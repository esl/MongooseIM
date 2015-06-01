%%%----------------------------------------------------------------------
%%% File    : mod_roster_http.erl
%%% Authors : Joseph Yiasemides Tomasz Kowal <{joseph.yiasemides,tomasz.kowal}>@erlang-solutions.com>
%%% Purpose : mod_roster http backend
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

-define(HOST, <<"localhost">>).

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
    HTTPRosterOpts = ejabberd_config:get_local_option(http_roster_opts, Host),
    ok = ensure_defined_field(address, HTTPRosterOpts),
    ok = ensure_defined_field(port, HTTPRosterOpts).

ensure_defined_field(Field, HTTPRosterOpts) ->
    case proplists:get_value(Field, HTTPRosterOpts) of
	undefined ->
	    error("Check configuration for mod_roster_http backend");
	_ ->
	    ok
    end.
 
-spec read_roster_version(ejabberd:luser(), ejabberd:lserver())
-> binary() | error.
read_roster_version(LUser, LServer) ->
    ok.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    ok.

get_roster(User, Domain) ->
    Opts = ejabberd_config:get_local_option(http_roster_opts, Domain),

    Address = proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    Path = list_to_binary(proplists:get_value(path, Opts, "/roster/")),

    URL = "http://"++Address++":"++Port,
    Options = [],

    {ok, Client} = fusco:start(URL, Options),
    {ok, Response} = fusco:request(Client, <<Path/binary,Domain/binary,"/",User/binary>>, "GET", [], [], 1, 1000),

    DecodedJson = mochijson2:decode(body(Response)),
    Contacts = extract_contacts(DecodedJson),
    ProplistToRoster = fun(Contact) -> proplist_to_roster(User, Domain, Contact) end,
    lists:map(ProplistToRoster, Contacts).

get_roster_by_jid_t(User, Domain, {_U, _D, _R} = PossibleContactJID) ->
    get_single_item({User, Domain}, PossibleContactJID),
    ok.

get_subscription_lists(_, LUser, LServer) ->
    [].

roster_subscribe_t(_LUser, _LServer, _LJID, Item) ->
    ok.

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    ok.

remove_user(LUser, LServer) ->
    ok.

update_roster_t(_LUser, _LServer, _LJID, Item) ->
    ok.

del_roster_t(LUser, LServer, LJID) ->
    ok.


read_subscription_and_groups(LUser, LServer, LJID) ->
    ok.

raw_to_record(_, Item) -> Item.


%% AUXILIARY

get_single_item({User, Domain} = Owner, {_U, _D, _R} = ContactJID) ->
    Opts = ejabberd_config:get_local_option(http_roster_opts, Domain),

    Address = proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    Path = list_to_binary(proplists:get_value(path, Opts, "/roster/")),

    URL = "http://"++Address++":"++Port,
    Options = [],

    {ok, Client} = fusco:start(URL, Options),
    {ok, Response} = fusco:request(Client, <<Path/binary,Domain/binary,"/",User/binary>>, "GET", [], [], 1, 1000),

    DecodedJson = mochijson2:decode(body(Response)),
    Contacts = extract_contacts(DecodedJson),
    case contact_in_list(ContactJID, Contacts) of
	flase ->
	    #roster{usj = {User, Domain, ContactJID},
		    us = {User, Domain}, jid = ContactJID};
	true ->
	    proplist_to_roster(
	      User, Domain,
	      [{<<"jid">>, ContactJID},
	       {<<"name">>, <<"">>},
	       {<<"groups">>, []},
	       {<<"xs">>, []}])
    end.

contact_in_list(ContactJID, List) ->
    lists:keymember(ContactJID, #roster.jid, List).


body({_, _, Body, _, _}) ->
    Body.

extract_contacts(JSONStruct) ->
    {struct, FieldsPropList} = JSONStruct,
    Items = proplists:get_value(<<"items">>, FieldsPropList),
    lists:map(fun ({struct, ItemFields}) ->
		      ItemFields end,
	      Items).

proplist_to_roster(LocalUser, LocalUserDomain, Contact) ->
    Jid = ensure_field(<<"jid">>, Contact),
    %% TODO: What about resource?
    [User, Domain] = binary:split(Jid, <<"@">>),
    Name = proplists:get_value(<<"name">>, Contact, <<>>),
    Subscription = field_to_atom(<<"subscription">>, Contact, <<"none">>),
    Ask = field_to_atom(<<"ask">>, Contact, <<"none">>),
    Groups = proplists:get_value(<<"groups">>, Contact, []),
    Askmessage = proplists:get_value(<<"askmessage">>, Contact, <<"">>),
    #roster{
       usj = {LocalUser, LocalUserDomain, {User, Domain, <<>>}},
       us = {LocalUser, LocalUserDomain},
       jid = {User, Domain, <<>>},
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
