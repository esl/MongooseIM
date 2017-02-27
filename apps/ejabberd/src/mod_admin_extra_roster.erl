%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_roster.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_admin_extra_roster).
-author('badlop@process-one.net').
-export([
    commands/0,

    add_rosteritem/7,
    delete_rosteritem/4,
    process_rosteritems/5,
    get_roster/2,
    push_roster/3,
    push_roster_all/1,
    push_alltoall/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-type simple_roster() :: {User :: ejabberd:user(),
                         Server :: ejabberd:server(),
                         Group :: binary(),
                         Nick :: binary()}.
-type jids_nick_subs_ask_grp() :: {Jids :: list(),
                                   Nick :: binary(),
                                   Subs :: subs(),
                                   _Ask,
                                   _Group}.
-type subs() :: atom() | binary().
-type push_action() :: remove
                     | none
                     | {add, Nick :: binary(), Subs :: subs(),
                        Group :: binary() | string()}.

-type delete_action() :: {'delete', Subs :: [atom()], Asks :: [atom()],
                                    [ejabberd:user()], Contacts :: [binary()]}.
-type list_action() :: {'list', Subs :: [atom()], Asks :: [atom()],
                                [ejabberd:user()], Contacts :: [binary()]}.


%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    [
        #ejabberd_commands{name = add_rosteritem, tags = [roster],
                           desc = "Add an item to a user's roster (supports ODBC)",
                           module = ?MODULE, function = add_rosteritem,
                           args = [{localuser, binary}, {localserver, binary},
                                   {user, binary}, {server, binary},
                                   {nick, binary}, {group, binary},
                                   {subs, binary}],
                           result = {res, restuple}},
        %%{"", "subs= none, from, to or both"},
        %%{"", "example: add-roster peter localhost mike server.com MiKe Employees both"},
        %%{"", "will add mike@server.com to peter@localhost roster"},
        #ejabberd_commands{name = delete_rosteritem, tags = [roster],
                           desc = "Delete an item from a user's roster (supports ODBC)",
                           module = ?MODULE, function = delete_rosteritem,
                           args = [{localuser, binary}, {localserver, binary},
                                   {user, binary}, {server, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = process_rosteritems, tags = [roster],
                           desc = "List or delete rosteritems that"
                                  " match filtering options (Mnesia only!)",
                           longdesc = "Explanation of each argument:\n"
                           " - action: what to do with each rosteritem that "
                           "matches all the filtering options\n"
                           " - subs: subscription type\n"
                           " - asks: pending subscription\n"
                           " - users: the JIDs of the local user\n"
                           " - contacts: the JIDs of the contact in the roster\n"
                           "\n"
                           "Allowed values in the arguments:\n"
                           "  ACTION = list | delete\n"
                           "  SUBS = SUB[:SUB]* | any\n"
                           "  SUB = none | from | to | both\n"
                           "  ASKS = ASK[:ASK]* | any\n"
                           "  ASK = none | out | in\n"
                           "  USERS = JID[:JID]* | any\n"
                           "  CONTACTS = JID[:JID]* | any\n"
                           "  JID = characters valid in a JID, and can use the "
                           "Regular expression syntax:"
                           " http://www.erlang.org/doc/man/re.html#id212737\n"
                           "\n"
                           "This example will list roster items with subscription "
                           "'none', 'from' or 'to' that have any ask property, of "
                           "local users which JID is in the virtual host "
                           "'example.org' and that the contact JID is either a "
                           "bare server name (without user part) or that has a "
                           "user part and the server part contains the word 'icq'"
                           ":\n  list none:from:to any *@example.org *:*@*icq*",
                           module = ?MODULE, function = process_rosteritems,
                           args = [{action, string}, {subs, string},
                                   {asks, string}, {users, string},
                                   {contacts, string}],
                           result = {res, binary}},
        #ejabberd_commands{name = get_roster, tags = [roster],
                           desc = "Get roster of a local user",
                           module = ?MODULE, function = get_roster,
                           args = [{user, binary}, {host, binary}],
                           result = {contacts, {list, {contact, {tuple, [
                                {jid, binary},
                                {nick, binary},
                                {subscription, binary},
                                {ask, binary},
                                {group, binary}
                                ]}}}}},
        #ejabberd_commands{name = push_roster, tags = [roster],
                           desc = "Push template roster from file to a user",
                           module = ?MODULE, function = push_roster,
                           args = [{file, string}, {user, binary}, {host, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = push_roster_all, tags = [roster],
                           desc = "Push template roster from file to all those users",
                           module = ?MODULE, function = push_roster_all,
                           args = [{file, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = push_roster_alltoall, tags = [roster],
                           desc = "Add all the users to all the users of Host in Group",
                           module = ?MODULE, function = push_alltoall,
                           args = [{host, binary}, {group, binary}],
                           result = {res, rescode}}
        ].

%%%
%%% Roster
%%%

-spec add_rosteritem(LocalUser :: ejabberd:user(),
                     LocalServer :: ejabberd:server(),
                     User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Nick :: binary(),
                     Group :: binary() | string(),
                     Subs :: subs()) -> {Res, string()} when
    Res :: user_doest_not_exist | error | bad_subs | ok.
add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
    case ejabberd_auth:is_user_exists(LocalUser, LocalServer) of
        true ->
            case subscribe(LocalUser, LocalServer, User, Server, Nick, Group, Subs, []) of
                {atomic, _} ->
                    do_add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs);
                Other ->
                    {error, io_lib:format("~p", [Other])}
            end;
        false ->
            {user_does_not_exist,
             io_lib:format("Cannot add the item because user ~s@~s does not exist",
                           [LocalUser, LocalServer])}
    end.

do_add_rosteritem(LocalUser, LocalServer, User, Server, Nick, Group, Subs) ->
    case lists:member(Subs, possible_subs_binary()) of
        true ->
            push_roster_item(LocalUser, LocalServer, User, Server, {add, Nick, Subs, Group}),
            {ok, io_lib:format("Added the item to the roster of ~s@~s", [LocalUser, LocalServer])};
        false ->
            {bad_subs, io_lib:format("Sub ~s is incorrect."
                                     " Choose one of the following:~nnone~nfrom~nto~nboth",
                                     [binary_to_list(Subs)])}
    end.


%% @doc returns result of mnesia or odbc transaction
-spec subscribe(LocalUser :: ejabberd:user(),
                LocalServer :: ejabberd:server(),
                User :: ejabberd:user(),
                Server :: ejabberd:server(),
                Nick :: binary(),
                Group :: binary() | string(),
                Subs :: subs(),
                _Xattrs :: [jlib:binary_pair()]) -> any().
subscribe(LU, LS, User, Server, Nick, Group, SubscriptionS, _Xattrs) ->
    ItemEl = build_roster_item(User, Server, {add, Nick, SubscriptionS, Group}),
    QueryEl = #xmlel{ name = <<"query">>,
                      attrs = [{<<"xmlns">>, <<"jabber:iq:roster">>}],
                      children = [ItemEl]},
    mod_roster:set_items(LU, LS, QueryEl).


-spec delete_rosteritem(LocalUser :: ejabberd:user(),
                        LocalServer :: ejabberd:server(),
                        User :: ejabberd:user(),
                        Server :: ejabberd:server()) -> {Res, string()} when
    Res :: ok | error | user_does_not_exist.
delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case ejabberd_auth:is_user_exists(LocalUser, LocalServer) of
        true ->
            case unsubscribe(LocalUser, LocalServer, User, Server) of
                {atomic, ok} ->
                    push_roster_item(LocalUser, LocalServer, User, Server, remove),
                    {ok, io_lib:format("The item removed from roster of ~s@~s",
                                       [LocalUser, LocalServer])};
                Other ->
                    {error, io_lib:format("~p", [Other])}
            end;
        false ->
            {user_does_not_exist,
             io_lib:format("Cannot delete the item because user ~s@~s doest not exist",
                           [LocalUser, LocalServer])}
    end.


%% @doc returns result of mnesia or odbc transaction
-spec unsubscribe(LocalUser :: ejabberd:user(),
                  LocalServer :: ejabberd:server(),
                  User :: ejabberd:user(),
                  Server :: ejabberd:server()) -> any().
unsubscribe(LU, LS, User, Server) ->
    ItemEl = build_roster_item(User, Server, remove),
    QueryEl = #xmlel{ name = <<"query">>,
              attrs = [{<<"xmlns">>, <<"jabber:iq:roster">>}],
              children = [ItemEl]},
    mod_roster:set_items(LU, LS, QueryEl).

%% -----------------------------
%% Get Roster
%% -----------------------------

-spec get_roster(ejabberd:user(), ejabberd:server()) -> [jids_nick_subs_ask_grp()].
get_roster(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Items = ejabberd_hooks:run_fold(roster_get, Server, [], [{LUser, LServer}]),
    make_roster(Items).


%% @doc Note: if a contact is in several groups, the contact is returned
%% several times, each one in a different group.
-spec make_roster([mod_roster:roster()]) -> [jids_nick_subs_ask_grp()].
make_roster(Roster) ->
    lists:foldl(
        fun(Item, Res) ->
                JIDS = jid:to_binary(Item#roster.jid),
                Nick = Item#roster.name,
                Subs = atom_to_list(Item#roster.subscription),
                Ask = atom_to_list(Item#roster.ask),
                Groups = case Item#roster.groups of
                    [] -> [""];
                    Gs -> Gs
                end,
                ItemsX = [{JIDS, Nick, Subs, Ask, Group}
                          || Group <- Groups],
                ItemsX ++ Res
        end,
        [],
        Roster).


%%-----------------------------
%% Push Roster from file
%%-----------------------------

-spec push_roster(file:name(), ejabberd:user(), ejabberd:server()) -> 'ok'.
push_roster(File, User, Server) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_roster({User, Server, <<"">>, User}, roster_list_to_binary(Roster)).


-spec push_roster_all(file:name()) -> 'ok'.
push_roster_all(File) ->
    {ok, [Roster]} = file:consult(File),
    subscribe_all(roster_list_to_binary(Roster)).


-spec roster_list_to_binary([mod_roster:roster()]) -> [simple_roster()].
roster_list_to_binary(Roster) ->
    [{
        ejabberd_binary:string_to_binary(Usr),
        ejabberd_binary:string_to_binary(Srv),
        ejabberd_binary:string_to_binary(Grp),
        ejabberd_binary:string_to_binary(Nick)} || {Usr, Srv, Grp, Nick} <- Roster].


-spec subscribe_all([simple_roster()]) -> 'ok'.
subscribe_all(Roster) ->
    subscribe_all(Roster, Roster).
subscribe_all([], _) ->
    ok;
subscribe_all([User1 | Users], Roster) ->
    subscribe_roster(User1, Roster),
    subscribe_all(Users, Roster).


-spec subscribe_roster(simple_roster(), [simple_roster()]) -> 'ok'.
subscribe_roster(_, []) ->
    ok;
%% Do not subscribe a user to itself
subscribe_roster({Name, Server, Group, Nick}, [{Name, Server, _, _} | Roster]) ->
    subscribe_roster({Name, Server, Group, Nick}, Roster);
%% Subscribe Name2 to Name1
subscribe_roster({Name1, Server1, Group1, Nick1}, [{Name2, Server2, Group2, Nick2} | Roster]) ->
    subscribe(Name1, Server1, Name2, Server2, Nick2, Group2, <<"both">>, []),
    subscribe_roster({Name1, Server1, Group1, Nick1}, Roster).


-spec push_alltoall(ejabberd:server(), binary()) -> 'ok'.
push_alltoall(S, G) ->
    Users = ejabberd_auth:get_vh_registered_users(S),
    Users2 = build_list_users(G, Users, []),
    subscribe_all(Users2),
    ok.


-spec build_list_users(Group :: binary(),
                       [ejabberd:simple_bare_jid()],
                       Res :: [simple_roster()]) -> [].
build_list_users(_Group, [], Res) ->
    Res;
build_list_users(Group, [{User, Server}|Users], Res) ->
    build_list_users(Group, Users, [{User, Server, Group, User}|Res]).


%% @spec(LU, LS, U, S, Action) -> ok
%%       Action = {add, Nick, Subs, Group} | remove
%% @doc Push to the roster of account LU@LS the contact U@S.
%% The specific action to perform is defined in Action.
-spec push_roster_item(ejabberd:luser(), ejabberd:lserver(), ejabberd:user(),
        ejabberd:server(), Action :: push_action()) -> 'ok'.
push_roster_item(LU, LS, U, S, Action) ->
    lists:foreach(fun(R) ->
                push_roster_item(LU, LS, R, U, S, Action)
        end, ejabberd_sm:get_user_resources(LU, LS)).


-spec push_roster_item(ejabberd:luser(), ejabberd:lserver(), ejabberd:user(),
        ejabberd:user(), ejabberd:server(), Action :: push_action()) -> mongoose_acc:t().
push_roster_item(LU, LS, R, U, S, Action) ->
    LJID = jid:make(LU, LS, R),
    BroadcastEl = build_broadcast(U, S, Action),
    ejabberd_sm:route(LJID, LJID, BroadcastEl),
    Item = build_roster_item(U, S, Action),
    ResIQ = build_iq_roster_push(Item),
    ejabberd_router:route(LJID, LJID, ResIQ).

-spec build_roster_item(ejabberd:user(), ejabberd:server(), push_action()
                       ) -> jlib:xmlel().
build_roster_item(U, S, {add, Nick, Subs, Group}) ->
    #xmlel{ name = <<"item">>,
       attrs = [{<<"jid">>, jid:to_binary(jid:make(U, S, <<"">>))},
                {<<"name">>, Nick},
                {<<"subscription">>, Subs}],
       children = [#xmlel{name = <<"group">>, children = [#xmlcdata{content = Group}]}]
      };
build_roster_item(U, S, remove) ->
    #xmlel{ name = <<"item">>,
       attrs = [{<<"jid">>, jid:to_binary(jid:make(U, S, <<"">>))},
                {<<"subscription">>, <<"remove">>}]}.


-spec build_iq_roster_push(jlib:xmlcdata() | jlib:xmlel()) -> jlib:xmlel().
build_iq_roster_push(Item) ->
    #xmlel{ name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>}, {<<"id">>, <<"push">>}],
           children = [#xmlel{ name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_ROSTER}],
                               children = [Item]}] }.

-spec build_broadcast(U :: ejabberd:user(), S :: ejabberd:server(),
                      push_action()) -> ejabberd_c2s:broadcast().
build_broadcast(U, S, {add, _Nick, Subs, _Group}) ->
    build_broadcast(U, S, list_to_existing_atom(binary_to_list(Subs)));
build_broadcast(U, S, remove) ->
    build_broadcast(U, S, none);
%% Subs = both | from | to | none
build_broadcast(U, S, SubsAtom) when is_atom(SubsAtom) ->
    {broadcast, {item, {U, S, <<"">>}, SubsAtom}}.

%%-----------------------------
%% Purge roster items
%%-----------------------------

-spec process_rosteritems(Act :: string(), SubsS :: string(), AsksS :: string(),
        UsersS :: string(), ContactsS :: string()) -> binary().
process_rosteritems(ActionS, SubsS, AsksS, UsersS, ContactsS) ->
    Action = case ActionS of
        "list" -> list;
        "delete" -> delete
    end,

    Subs = lists:foldl(
            fun(any, _) -> [none, from, to, both];
                (Sub, Subs) -> [Sub | Subs]
            end,
            [],
            [list_to_existing_atom(S) || S <- string:tokens(SubsS, ":")]
            ),

    Asks = lists:foldl(
            fun(any, _) -> [none, out, in];
                (Ask, Asks) -> [Ask | Asks]
            end,
            [],
            [list_to_existing_atom(S) || S <- string:tokens(AsksS, ":")]
            ),

    Users = lists:foldl(
            fun(<<"any">>, _) -> [<<".*">>, <<".*@.*">>];
                (U, Us) -> [U | Us]
            end,
            [],
            [ejabberd_binary:string_to_binary(S) || S <- string:tokens(UsersS, ":")]
            ),

    Contacts = lists:foldl(
            fun(<<"any">>, _) -> [<<".*">>, <<".*@.*">>];
                (U, Us) -> [U | Us]
            end,
            [],
            [ejabberd_binary:string_to_binary(S) || S <- string:tokens(ContactsS, ":")]
            ),

    case validate_regexps(Users ++ Contacts) of
        <<>> ->
            Options = {Action, Subs, Asks, Users, Contacts},

            case mnesia:table_info(roster, size) of
                0 ->
                    <<"Roster table is empty.\n">>;
                NumRosteritems ->
                    Msg1 = <<"There are ", (integer_to_binary(NumRosteritems))/binary,
                             " roster items in total.\n">>,
                    Key = mnesia:dirty_first(roster),
                    Msg2 = rip(Key, Options, <<>>),
                    <<Msg1/binary, Msg2/binary>>
            end;
        ErrorMsg ->
            ErrorMsg
    end.

validate_regexps(ListOfRegexps) ->
    lists:foldl(
      fun(RegExp, MsgAcc) ->
              case re:compile(RegExp) of
                  {ok, _} -> MsgAcc;
                  {error, Error} ->
                      NewErr = iolist_to_binary(io_lib:format("Wrong regexp ~p: ~p~n",
                                                              [RegExp, Error])),
                      <<MsgAcc/binary, NewErr/binary>>
              end
      end, <<>>, ListOfRegexps).

-spec rip('$end_of_table'  | any(), delete_action()  | list_action(), binary()) -> binary().
rip('$end_of_table', _Options, Acc) ->
    Acc;
rip(Key, Options, Acc) ->
    KeyNext = mnesia:dirty_next(roster, Key),
    {Action, _, _, _, _} = Options,
    Msg = case decide_rip(Key, Options) of
              true ->
                  apply_action(Action, Key);
              false ->
                  <<>>
          end,
    rip(KeyNext, Options, <<Acc/binary, Msg/binary>>).

apply_action(list, Key) ->
    {User, Server, JID} = Key,
    {RUser, RServer, _} = JID,
    <<"Matches: ", User/binary, "@", Server/binary, " ", RUser/binary, "@", RServer/binary, "\n">>;
apply_action(delete, Key) ->
    Msg = apply_action(list, Key),
    mnesia:dirty_delete(roster, Key),
    Msg.

decide_rip(Key, {_Action, Subs, Asks, User, Contact}) ->
    case catch mnesia:dirty_read(roster, Key) of
        [RI] ->
            lists:member(RI#roster.subscription, Subs)
            andalso lists:member(RI#roster.ask, Asks)
            andalso decide_rip_jid(RI#roster.us, User)
            andalso decide_rip_jid(RI#roster.jid, Contact);
        _ ->
            false
    end.

%% Returns true if the server of the JID is included in the servers
decide_rip_jid({UName, UServer, _UResource}, MatchList) ->
    decide_rip_jid({UName, UServer}, MatchList);
decide_rip_jid({UName, UServer}, MatchList) ->
    lists:any(
        fun(MatchString) ->
                MJID = jid:from_binary(MatchString),
                MName = MJID#jid.luser,
                MServer = MJID#jid.lserver,
                IsServer = is_regexp_match(UServer, MServer),
                case {MName, UName} of
                    {<<>>, <<>>} -> IsServer;
                    {<<>>, _} -> false;
                    _ -> IsServer andalso is_regexp_match(UName, MName)
                end
        end,
        MatchList).

is_regexp_match(String, RegExp) ->
    case catch re:run(String, RegExp) of
        nomatch ->
            false;
        {match, List} ->
            Size = length(binary_to_list(String)),
            case lists:member({0, Size}, List) of
                true ->
                    true;
                false ->
                    false
            end;
        Error ->
            ?ERROR_MSG("Wrong regexp ~p: ~p", [RegExp, Error]),
            false
    end.

possible_subs_binary() ->
    [<<"none">>, <<"from">>, <<"to">>, <<"both">>].

