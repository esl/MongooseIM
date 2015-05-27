%%%----------------------------------------------------------------------
%%% File    : mod_roster_mnesia.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_last mnesia backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------
-module(mod_roster_http).

-include_lib("eunit/include/eunit.hrl").

%% from mod_roster_mnesia

-include("mod_roster.hrl").
-include("jlib.hrl").

-behaviour(mod_roster).

%% API
-export([init/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_by_jid_t/3,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         get_roster_by_jid_with_groups_t/3,
         remove_user/2,
         update_roster_t/4,
         del_roster_t/3,
         read_subscription_and_groups/3]).

-export([raw_to_record/2]).

-spec init(ejabberd:server(), list()) -> no_return().
init(Host, _Opts) ->
    RosterOpts = ejabberd_config:get_local_option(roster_opts, Host),
    {_, RosterHost} = lists:keyfind(host, 1, RosterOpts),
    PoolSize = proplists:get_value(connection_pool_size, RosterOpts, 10),
    Opts = proplists:get_value(connection_opts, RosterOpts, []),
    ChildMods = [fusco],
    ChildMFA = {fusco, start_link, [RosterHost, Opts]},
    {ok, _} = supervisor:start_child(ejabberd_sup,
                                     {{ejabberd_roster_http_sup, Host},
                                      {cuesport, start_link,
                                       [pool_name(Host), PoolSize, ChildMods, ChildMFA]},
                                      transient, 2000, supervisor, [cuesport | ChildMods]}),
    ok.

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver())
-> binary() | error.
read_roster_version(LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
        [#roster_version{version = V}] -> V;
        [] -> error
    end.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    US = {LUser, LServer},
    if InTransaction ->
           mnesia:write(#roster_version{us = US, version = Ver});
       true ->
           mnesia:dirty_write(#roster_version{us = US,
                                              version = Ver})
    end.

get_roster(LocalUser, LocalServer) ->
    {ok, JSONRoster} = make_req(get, undefined, LocalUser, LocalServer, undefined),
    mochijson2:decode(JSONRoster).

get_roster_by_jid_t(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
        [] ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        [I] ->
            I#roster{jid = LJID, name = <<"">>, groups = [],
                     xs = []}
    end.

get_subscription_lists(_, LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
        Items when is_list(Items) -> Items;
        _ -> []
    end.

roster_subscribe_t(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
        [] ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        [I] -> I
    end.

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    mod_roster:send_unsubscription_to_rosteritems(LUser, LServer),
    F = fun () ->
                lists:foreach(fun (R) -> mnesia:delete_object(R) end,
                              mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

update_roster_t(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

del_roster_t(LUser, LServer, LJID) ->
    mnesia:delete({roster, {LUser, LServer, LJID}}).


read_subscription_and_groups(LUser, LServer, LJID) ->
    case catch mnesia:dirty_read(roster,
                                 {LUser, LServer, LJID})
    of
        [#roster{subscription = Subscription,
                 groups = Groups}] ->
            {Subscription, Groups};
        _ -> error
    end.

raw_to_record(_, Item) -> Item.


%% ~ From mim_ct_rest

-spec pool_name(list()) -> atom().
pool_name(Host) ->
    list_to_atom("mod_roster_http_" ++ binary_to_list(Host)).

-spec existing_pool_name(list()) -> atom().
existing_pool_name(Host) ->
    list_to_existing_atom("mod_roster_http_" ++ binary_to_list(Host)).

get_path_prefix(RosterOpts) ->
    case lists:keyfind(path_prefix, 1, RosterOpts) of
	{_, Prefix} -> ejabberd_binary:string_to_binary(Prefix);
	false -> <<"/roster/">>
    end.

%% From ejabberd_auth_http

-spec make_req(post | get, binary(), binary(), binary(), binary()) ->
    {ok, Body :: binary()} | {error, term()}.
make_req(_, _, LUser, LServer, _) when LUser == error orelse LServer == error ->
    {error, {prep_failed, LUser, LServer}};
make_req(Method, _Path, LUser, LServer, _Password) ->
    AuthOpts = ejabberd_config:get_local_option(roster_opts, LServer),
    PathPrefix = case lists:keyfind(path_prefix, 1, AuthOpts) of
                     {_, Prefix} ->
			 ejabberd_binary:string_to_binary(Prefix);
                     false ->
			 <<"/roster/">>
                 end,
    LUserE = list_to_binary(http_uri:encode(binary_to_list(LUser))),
    LServerE = list_to_binary(http_uri:encode(binary_to_list(LServer))),
    AtE = list_to_binary(http_uri:encode("@")),
    %% PasswordE = list_to_binary(http_uri:encode(binary_to_list(Password))),
    %% Query = <<"user=", LUserE/binary, "&server=", LServerE/binary, "&pass=", PasswordE/binary>>,
    JID = <<LUserE/binary, AtE/binary, LServerE/binary>>,
    %% Header = [{<<"Authorization">>, <<"Basic ", BasicAuth64/binary>>}],
    Header = [],
    Connection = cuesport:get_worker(existing_pool_name(LServer)),
			
    ?debugFmt("request with path ~p and JID ~p.~n", [PathPrefix, JID]),
    {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} = 
	case Method of
	    %% get -> fusco:request(Connection, <<PathPrefix/binary, Path/binary, "?", Query/binary>>,
	    %% 			 "GET", Header, "", 2, 5000);
	    %% get ->
	    %% 	fusco:request(Connection, <<PathPrefix/binary, JID/binary>>,
	    %% 		      "GET", Header, "", 2, 5000);
	    get ->
		fusco:request(Connection, <<PathPrefix/binary, JID/binary>>,
			      "GET", Header, "", 2, 5000)
	    %% post -> fusco:request(Connection, <<PathPrefix/binary, Path/binary>>,
	    %% 			  "POST", Header, Query, 2, 5000)
	end,
    
    ?debugFmt("Request result: ~s: ~p", [Code, RespBody]),
    case Code of
        <<"409">> -> {error, conflict};
        <<"404">> -> {error, not_found};
        <<"401">> -> {error, not_authorized};
        <<"403">> -> {error, not_allowed};
        <<"400">> -> {error, RespBody};
        <<"204">> -> {ok, <<"">>};
        <<"201">> -> {ok, created};
        <<"200">> -> {ok, RespBody}
    end.
