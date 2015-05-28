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

-include("mod_roster.hrl").
-include("jlib.hrl").

-behaviour(mod_roster).

-backend('HTTP').

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
init(_Host, _Opts) ->
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

get_roster(User, Domain) ->
    %% TODO fetch from config
    URL = "http://localhost:7654",
    Options = [],
    {ok, Client} = fusco:start(URL, Options),
    {ok, Response} = fusco:request(Client, <<"/roster/",Domain/binary,"/",User/binary>>, "GET", [], [], 1, 1000),
    DecodedJson = mochijson2:decode(body(Response)),
    Contacts = extract_contacts(DecodedJson),
    lists:map(fun proplist_to_roster/1, Contacts).

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


%% AUXILIARY

body({_, _, Body, _, _}) ->
    Body.

extract_contacts(JSONStruct) ->
    {struct, FieldsPropList} = JSONStruct,
    Items = proplists:get_value(<<"items">>, FieldsPropList),
    lists:map(fun ({struct, ItemFields}) ->
		      ItemFields end,
	      Items).

proplist_to_roster(Contact) ->
    Jid = ensure_field(<<"jid">>, Contact),
    [User, Domain] = binary:split(Jid, <<"@">>),
    Name = proplists:get_value(<<"name">>, Contact, <<"">>),
    Subscription = field_to_atom(<<"subscription">>, Contact, <<"none">>),
    Ask = field_to_atom(<<"ask">>, Contact, <<"none">>),
    Groups = proplists:get_value(<<"groups">>, Contact, []),
    Askmessage = proplists:get_value(<<"askmessage">>, Contact, <<"">>),
    #roster{
       usj = {User, Domain, Jid},
       jid = Jid,
       name = Name,
       subscription = Subscription,
       ask = Ask,
       groups = Groups,
       askmessage = Askmessage,
       %% TODO: check, what that is and what should we set here
       xs = []
      }.

ensure_field(FieldNameBinary, Contact) ->
    case proplists:get_value(FieldNameBinary, Contact) of
	undefined ->
	    error("Field ~p must be defined", [FieldNameBinary]);
	Value -> Value
    end.

field_to_atom(FieldNameBinary, Contact, Default) ->
    binary_to_existing_atom(
      proplists:get_value(FieldNameBinary, Contact, Default),
      utf8
     ).
