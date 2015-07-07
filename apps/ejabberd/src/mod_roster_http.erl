%%%----------------------------------------------------------------------
%%% File    : mod_roster_http.erl
%%% Authors : Joseph Yiasemides & Tomasz Kowal
%%% Purpose : Interface with an HTTP endpoint to do rostering
%%%
%%% MongooseIM, Copyright (C) 2015 Erlang Solutions Ltd.
%%%----------------------------------------------------------------------

-module(mod_roster_http).

-include_lib("eunit/include/eunit.hrl").

-include("mod_roster.hrl").
-include("jlib.hrl").

-behaviour(mod_roster).

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


%% API

init(Host, _Opts) ->
    HTTPRosterOpts = ejabberd_config:get_local_option(http_roster_opts, Host),
    ok = ensure_defined_field(address, HTTPRosterOpts),
    ok = ensure_defined_field(port, HTTPRosterOpts).
 
read_roster_version(LUser, LServer) ->
    ok.

write_roster_version(LUser, LServer, InTransaction, Ver) ->
    ok.

get_roster(User, Domain) ->
    fetch_entire_roster(User, Domain).

get_roster_by_jid_t(User, Domain, {_U, _D, _R} = Contact) ->
    get_single_item({User, Domain}, Contact).

get_subscription_lists(_Acc, User, Domain) ->
    fetch_entire_roster(User, Domain).

roster_subscribe_t(User, Domain, {_U, _D, _R} = Contact, Item) ->
    put_single_item(User, Domain, Contact, Item),
    ok.
 
get_roster_by_jid_with_groups_t(User, Domain, Contact) ->
    Roster = fetch_entire_roster(User, Domain),
    case lists:filter(fun (Item) -> same_JID_tuple(Contact, Item) end, Roster) of
	[] ->
	    proplist_to_roster(User, Domain, [{<<"jid">>, Contact}]);
	[Item] ->
	    Item
    end.

remove_user(LUser, LServer) ->
    ok.

update_roster_t(_LUser, _LServer, _LJID, Item) ->
    ok.

del_roster_t(LUser, LServer, LJID) ->
    ok.

read_subscription_and_groups(User, Domain, Contact) ->
    Roster = fetch_entire_roster(User, Domain),
    case lists:filter(fun (Item) -> same_JID_tuple(Contact, Item) end, Roster) of
        [#roster{subscription = Subscription, groups = Groups}] ->
            {Subscription, Groups};
        _Other ->
	    error
    end.

raw_to_record(_, Item) -> Item.


%% AUXILIARY

%%TBC
put_single_item(User, Domain, Contact, Item) ->
    #roster{
       usj = {User, Domain, {U, D, R} = Contact},
       us = {User, Domain},
       jid = {U, D, R} = Contact,
       name = Name,
       subscription = Subscription,
       ask = Ask,
       groups = Groups,
       askmessage = Askmessage,
       %% TODO: check, what that is and what should we set here
       xs = []} = Item.

fetch_entire_roster(User, Domain) ->
    Opts = ejabberd_config:get_local_option(http_roster_opts, Domain),
    Address = proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    Path = list_to_binary(proplists:get_value(path, Opts, "/roster/")),
    URLHead = "http://" ++ Address ++ ":" ++ Port,
    Options = [],

    URLTail = <<Path/binary,Domain/binary,"/",User/binary>>,
    {ok, Client} = fusco:start(URLHead, Options),
    {ok, Response} =
	fusco:request(Client, URLTail, "GET", [], [], 1, 1000),
    ok = fusco:disconnect(Client),
    DecodedJson = mochijson2:decode(body(Response)),
    Contacts = extract_contacts(DecodedJson),
    ProplistToRoster = fun(Contact) -> proplist_to_roster(User, Domain, Contact) end,
    lists:map(ProplistToRoster, Contacts).

proplist_to_roster(LocalUser, LocalUserDomain, Contact) ->
    Jid = ensure_field(<<"jid">>, Contact),
    %% TODO: split at a backslash for the resource?
    [User, Domain] = binary:split(Jid, <<"@">>),
    Name = proplists:get_value(<<"name">>, Contact, <<>>),
    Subscription = field_to_existing_atom(<<"subscription">>, Contact, <<"none">>),
    Ask = field_to_existing_atom(<<"ask">>, Contact, <<"none">>),
    Groups = proplists:get_value(<<"groups">>, Contact, []),
    Askmessage = proplists:get_value(<<"askmessage">>, Contact, <<>>),
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

get_single_item({User, Domain} = _Owner, {_U, _D, _R} = Contact) ->
    Roster = fetch_entire_roster(User, Domain),
    case lists:filter(fun (Item) -> same_JID_tuple(Contact, Item) end, Roster) of
	[] ->
	    proplist_to_roster(User, Domain, [{<<"jid">>, Contact}]);
	[Item] ->
	    Item#roster{jid = Contact, name = <<"">>, groups = [], xs = []}
    end.

extract_contacts(JSONStruct) ->
    {struct, FieldsPropList} = JSONStruct,
    Items = proplists:get_value(<<"items">>, FieldsPropList),
    lists:map(fun ({struct, ItemFields}) ->
		      ItemFields end,
	      Items).

ensure_defined_field(FieldName, HTTPRosterOpts) ->
    case proplists:get_value(FieldName, HTTPRosterOpts) of
	undefined ->
	    error("Check configuration for mod_roster_http backend");
	_ ->
	    ok
    end.

ensure_field(FieldNameBinary, Contact) ->
    case proplists:get_value(FieldNameBinary, Contact) of
        undefined ->
            error("Field ~p must be defined", [FieldNameBinary]);
        Value -> Value
    end.

contact_in_roster({_U, _D, _R} = Contact, Roster) ->
    lists:keymember(Contact, #roster.jid, Roster).

field_to_existing_atom(FieldNameBinary, Contact, Default) ->
    binary_to_existing_atom(
      proplists:get_value(FieldNameBinary, Contact, Default),
      utf8
     ).

same_JID_tuple(A, B) ->
    A#roster.jid == B#roster.jid.

body({_, _, Body, _, _}) ->
    Body.
