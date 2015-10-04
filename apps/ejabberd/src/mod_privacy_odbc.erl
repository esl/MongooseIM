%%%----------------------------------------------------------------------
%%% Copyright notice from original mod_privacy
%%%
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%%----------------------------------------------------------------------

-module(mod_privacy_odbc).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').

-behaviour(mod_privacy).

-export([init/2,
         get_default_list/2,
         get_list_names/2,
         get_privacy_list/3,
         forget_default_list/2,
         set_default_list/3,
         remove_privacy_list/3,
         replace_privacy_list/4,
         remove_user/2,
         block_user/3,
         unblock_user/3,
         unblock_all/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-define(LIST_NAME, <<"block">>).

init(_Host, _Opts) ->
    ok.

get_default_list(LUser, LServer) ->
    case get_default_list_name(LUser, LServer) of
        none ->
            {error, not_found};
        Default ->
            case get_privacy_list(LUser, LServer, Default) of
                {ok, List} ->
                    {ok, {Default, List}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_list_names(LUser, LServer) ->
    Default = get_default_list_name(LUser, LServer),
    Names = get_list_names_only(LUser, LServer),
    {ok, {Default, Names}}.

get_default_list_name(LUser, LServer) ->
    case catch sql_get_default_privacy_list(LUser, LServer) of
        {selected, [<<"name">>], []} ->
            none;
        {selected, [<<"name">>], [{DefName}]} ->
            DefName;
        _ ->
            none
    end.

get_list_names_only(LUser, LServer) ->
    case catch sql_get_privacy_list_names(LUser, LServer) of
    {selected, [<<"name">>], Names} ->
        [Name || {Name} <- Names];
    _ ->
        []
    end.


get_privacy_list(LUser, LServer, Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
        {selected, [<<"id">>], []} ->
            {error, not_found};
        {selected, [<<"id">>], [{ID}]} ->
            case catch sql_get_privacy_list_data_by_id(ID, LServer) of
                {selected, [<<"t">>, <<"value">>, <<"action">>,
                        <<"ord">>, <<"match_all">>,
                        <<"match_iq">>, <<"match_message">>,
                        <<"match_presence_in">>, <<"match_presence_out">>],
                 RItems} ->
                    Items = lists:map(fun raw_to_item/1, RItems),
                    {ok, Items};
                Other ->
                    {error, Other}
            end;
        Other2 ->
            {error, Other2}
    end.

%% @doc Set no default list for user.
forget_default_list(LUser, LServer) ->
    case catch sql_unset_default_privacy_list(LUser, LServer) of
    {'EXIT', Reason} ->
        {error, Reason};
    {error, Reason} ->
        {error, Reason};
    _ ->
        ok
    end.

set_default_list(LUser, LServer, Name) ->
    F = fun() ->
        case sql_get_privacy_list_names_t(LUser) of
            {selected, [<<"name">>], []} ->
                {error, not_found};
            {selected, [<<"name">>], Names} ->
                case lists:member({Name}, Names) of
                    true ->
                        sql_set_default_privacy_list(LUser, Name),
                        ok;
                    false ->
                        {error, not_found}
                end
        end
        end,
    case odbc_queries:sql_transaction(LServer, F) of
        {atomic, ok} ->
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

remove_privacy_list(LUser, LServer, Name) ->
     F = fun() ->
            case sql_get_default_privacy_list_t(LUser) of
                {selected, [<<"name">>], []} ->
                    sql_remove_privacy_list(LUser, Name),
                    ok;
                {selected, [<<"name">>], [{Default}]} ->
                    if
                        Name == Default ->
                            {error, conflict};
                        true ->
                            sql_remove_privacy_list(LUser, Name),
                            ok
                    end
            end
        end,
    case odbc_queries:sql_transaction(LServer, F) of
        {atomic, {error, _} = Error} ->
            Error;
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

replace_privacy_list(LUser, LServer, Name, List) ->
    RItems = lists:map(fun item_to_raw/1, List),
    F = fun() ->
        ID = case sql_get_privacy_list_id_t(LUser, Name) of
            {selected, [<<"id">>], []} ->
                sql_add_privacy_list(LUser, Name),
                {selected, [<<"id">>], [{I}]} =
                sql_get_privacy_list_id_t(LUser, Name),
                I;
            {selected, [<<"id">>], [{I}]} ->
                I
            end,
        sql_set_privacy_list(ID, RItems),
        ok
    end,
    case odbc_queries:sql_transaction(LServer, F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

remove_user(LUser, LServer) ->
    sql_del_privacy_lists(LUser, LServer).

%% XEP 0191 Blocking Command
block_user(LUser, LServer, JIDList) ->
    F = fun() ->
        case get_default_list_name(LUser, LServer) of
            none ->
                sql_add_privacy_list(LUser, ?LIST_NAME),
                {selected, [<<"id">>], [{I}]} =
                sql_get_privacy_list_id_t(LUser, ?LIST_NAME),
                ZippedWithNumbers = lists:zip(JIDList, lists:seq(1,length(JIDList))),
                Items = lists:map(fun parse_jid_to_listitem/1, ZippedWithNumbers),
                RItems = lists:map(fun item_to_raw/1, Items),
                sql_set_privacy_list(I, RItems),
                sql_set_default_privacy_list(LUser, ?LIST_NAME),
                {?LIST_NAME,Items};
            Name ->
                BlockeeList = lists:map(fun jlib:jid_to_binary/1, JIDList),
                %% FilteredJids Contains JIDs that are ready to be BLOCK
                FilteredList = lists:filter(fun(J) -> not check_contact_is_blocked(LUser, J, Name) end, BlockeeList),
                Num = case sql_get_latest_block_ord(LUser, Name) of
                    {selected, [<<"ord">>], [{Number}]} ->
                        Number;
                    _ ->
                        <<"1">>
                    end,
                NewNumber = binary_to_integer(Num),
                FilteredNormalized1 = lists:map(fun jlib:binary_to_jid/1, FilteredList),
                FilteredNormalized2 = lists:map(fun jlib:jid_to_lower/1, FilteredNormalized1),
                ZippedWithNumbers = lists:zip(FilteredNormalized2, lists:seq(1,length(FilteredNormalized2))),
                FilteredItemList = lists:map(fun parse_jid_to_listitem/1, ZippedWithNumbers),
                RenumberedItems = renumber_items(FilteredItemList, NewNumber),
                RIRenumberedItems = lists:map(fun item_to_raw/1, RenumberedItems),
                ID = case sql_get_privacy_list_id_t(LUser, Name) of
                    {selected, [<<"id">>], [{I}]} ->
                        I
                end,
                sql_block_contacts(ID, RIRenumberedItems),
                {Name, RenumberedItems}
        end
    end,
    case odbc_queries:sql_transaction(LServer, F) of
        {atomic, {ListName, UniqueJIDs}} ->
            {ok, {ListName, UniqueJIDs}};
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

unblock_user(LUser, LServer, JIDList) ->
    F = fun () ->
        case get_default_list_name(LUser, LServer) of
        none ->
            {error, no_default_list};
        Name ->
            BlockeeList = lists:map(fun jlib:jid_to_binary/1, JIDList),
            FilteredList = lists:filter(fun(J) -> check_contact_is_blocked(LUser, J, Name) end, BlockeeList),   %% Contains JIDs that are ready to be UNBLOCKED
            ID = case sql_get_privacy_list_id_t(LUser, Name) of
                {selected, [<<"id">>], []} ->
                    {error, cannot_create_list};
                {selected, [<<"id">>], [{I}]} ->
                    I
            end,
            FilteredNormalized1 = lists:map(fun jlib:binary_to_jid/1, FilteredList),
            FilteredNormalized2 = lists:map(fun jlib:jid_to_lower/1, FilteredNormalized1),
            ZippedWithNumbers = lists:zip(FilteredNormalized2, lists:seq(1,length(FilteredNormalized2))),
            FilteredItemList = lists:map(fun parse_jid_to_listitem/1, ZippedWithNumbers),
            RIFiltered = lists:map(fun item_to_raw/1, FilteredItemList),
            sql_unblock_contacts(ID, RIFiltered),
            FilteredList
    end
    end,
    case odbc_queries:sql_transaction(LServer, F) of
        {atomic, {error, Reason}} ->
            {error, Reason};
        {atomic, UniqueJIDs} ->
            {ok, UniqueJIDs};
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

unblock_all(LUser, LServer) ->
        case get_default_list_name(LUser, LServer) of
        none ->
            {error, no_default_list};
        Name ->
            case replace_privacy_list(LUser, LServer, Name, []) of
                ok ->
                    {ok, Name};
                {error, Reason} ->
                    {error, Reason}
            end
        end.

renumber_items(Items, Offset) ->
    lists:map(fun(Item = #listitem{order =  Order}) -> Item#listitem{order = Offset+Order} end, Items).

check_contact_is_blocked(LUser, LContact, ListName) ->
    case sql_check_contact_blocked(LUser, LContact, ListName) of                            %% TODO remember about HEAD
        {selected, [<<"value">>], [{_}]} ->
            true;
        {selected, [<<"value">>], []} ->
            false
    end.

parse_jid_to_listitem({JID, Number}) ->
    #listitem{type=jid, action=deny, value=JID, order =  Number, match_all = true,
              match_iq = true,
              match_message = true,
              match_presence_in = true,
              match_presence_out = true}.

%% Records are broken or smth?
raw_to_item({BType, BValue, BAction, BOrder, BMatchAll, BMatchIQ,
         BMatchMessage, BMatchPresenceIn, BMatchPresenceOut}) ->
    {Type, Value} =
    case BType of
        <<"n">> ->
        {none, none};
        <<"j">> ->
        case jlib:binary_to_jid(BValue) of
            #jid{} = JID ->
            {jid, jlib:jid_tolower(JID)}
        end;
        <<"g">> ->
        {group, BValue};
        <<"s">> ->
        case BValue of
            <<"none">> ->
            {subscription, none};
            <<"both">> ->
            {subscription, both};
            <<"from">> ->
            {subscription, from};
            <<"to">> ->
            {subscription, to}
        end
    end,
    Action =
    case BAction of
        <<"a">> -> allow;
        <<"d">> -> deny
    end,
    Order = binary_to_integer(BOrder),
    MatchAll = ejabberd_odbc:to_bool(BMatchAll),
    MatchIQ = ejabberd_odbc:to_bool(BMatchIQ),
    MatchMessage = ejabberd_odbc:to_bool(BMatchMessage),
    MatchPresenceIn = ejabberd_odbc:to_bool(BMatchPresenceIn),
    MatchPresenceOut =  ejabberd_odbc:to_bool(BMatchPresenceOut),
    #listitem{type = Type,
          value = Value,
          action = Action,
          order = Order,
          match_all = MatchAll,
          match_iq = MatchIQ,
          match_message = MatchMessage,
          match_presence_in = MatchPresenceIn,
          match_presence_out = MatchPresenceOut
         }.

item_to_raw(#listitem{type = Type,
              value = Value,
              action = Action,
              order = Order,
              match_all = MatchAll,
              match_iq = MatchIQ,
              match_message = MatchMessage,
              match_presence_in = MatchPresenceIn,
              match_presence_out = MatchPresenceOut
             }) ->
    {BType, BValue} =
    case Type of
        none ->
        {<<"n">>, <<"">>};
        jid ->
        {<<"j">>, ejabberd_odbc:escape(jlib:jid_to_binary(Value))};
        group ->
        {<<"g">>, ejabberd_odbc:escape(Value)};
        subscription ->
        case Value of
            none ->
            {<<"s">>, <<"none">>};
            both ->
            {<<"s">>, <<"both">>};
            from ->
            {<<"s">>, <<"from">>};
            to ->
            {<<"s">>, <<"to">>}
        end
    end,
    BAction =
    case Action of
        allow -> <<"a">>;
        deny -> <<"d">>
    end,
    BOrder = integer_to_binary(Order),
    BMatchAll = boolean_to_binary_number(MatchAll),
    BMatchIQ = boolean_to_binary_number(MatchIQ),
    BMatchMessage = boolean_to_binary_number(MatchMessage),
    BMatchPresenceIn = boolean_to_binary_number(MatchPresenceIn),
    BMatchPresenceOut = boolean_to_binary_number(MatchPresenceOut),
    [BType, BValue, BAction, BOrder, BMatchAll, BMatchIQ,
     BMatchMessage, BMatchPresenceIn, BMatchPresenceOut].

boolean_to_binary_number(true) -> <<"1">>;
boolean_to_binary_number(_)   -> <<"0">>.

sql_get_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_default_privacy_list(LServer, Username).

sql_get_default_privacy_list_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_default_privacy_list_t(Username).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_privacy_list_names(LServer, Username).

sql_get_privacy_list_names_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:get_privacy_list_names_t(Username).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_id(LServer, Username, SName).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_id_t(Username, SName).

sql_get_privacy_list_data_by_id(ID, LServer) when is_integer(ID) ->
    odbc_queries:get_privacy_list_data_by_id(LServer, integer_to_binary(ID));
sql_get_privacy_list_data_by_id(ID, LServer) ->
    odbc_queries:get_privacy_list_data_by_id(LServer, ID).

sql_set_default_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:set_default_privacy_list(Username, SName).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:unset_default_privacy_list(LServer, Username).

sql_remove_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:remove_privacy_list(Username, SName).

sql_add_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:add_privacy_list(Username, SName).

sql_set_privacy_list(ID, RItems) when is_integer(ID)->
    odbc_queries:set_privacy_list(integer_to_binary(ID), RItems);
sql_set_privacy_list(ID, RItems) ->
    odbc_queries:set_privacy_list(ID, RItems).

sql_del_privacy_lists(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    Server = ejabberd_odbc:escape(LServer),
    odbc_queries:del_privacy_lists(LServer, Server, Username).


%% XEP 0191 Blocking Commands
sql_check_contact_blocked(LUser, LContact, ListName) ->
    Username = ejabberd_odbc:escape(LUser),
    Contact = ejabberd_odbc:escape(LContact),
    Name = ejabberd_odbc:escape(ListName),
    odbc_queries:check_contact_blocked(Username, Contact, Name).

sql_get_latest_block_ord(LUser, ListName) ->
    Username = ejabberd_odbc:escape(LUser),
    Name = ejabberd_odbc:escape(ListName),
    odbc_queries:get_latest_ord(Username, Name).

sql_block_contacts(ID, ContactList) ->
    odbc_queries:block_contacts(ID, ContactList).

sql_unblock_contacts(ID, ContactList) ->
    odbc_queries:unblock_contacts(ID, ContactList).