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
         remove_user/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

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
        {selected, []} ->
            none;
        {selected, [{DefName}]} ->
            DefName;
        _ ->
            none
    end.

get_list_names_only(LUser, LServer) ->
    case catch sql_get_privacy_list_names(LUser, LServer) of
    {selected, Names} ->
        [Name || {Name} <- Names];
    _ ->
        []
    end.


get_privacy_list(LUser, LServer, Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
        {selected, []} ->
            {error, not_found};
        {selected, [{ID}]} ->
            case catch sql_get_privacy_list_data_by_id(ID, LServer) of
                {selected, RItems} ->
                    Items = raw_to_items(RItems),
                    {ok, Items};
                Other ->
                    {error, Other}
            end;
        Other2 ->
            {error, Other2}
    end.

raw_to_items(RItems) ->
    lists:map(fun raw_to_item/1, RItems).

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
    case rdbms_queries:sql_transaction(
           LServer, fun() -> set_default_list_t(LUser, Name) end) of
        {atomic, ok} ->
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec set_default_list_t(LUser :: jid:luser(), Name :: binary()) -> ok | {error, not_found}.
set_default_list_t(LUser, Name) ->
    case sql_get_privacy_list_names_t(LUser) of
        {selected, []} ->
            {error, not_found};
        {selected, Names} ->
            case lists:member({Name}, Names) of
                true ->
                    sql_set_default_privacy_list(LUser, Name),
                    ok;
                false ->
                    {error, not_found}
            end
    end.

remove_privacy_list(LUser, LServer, Name) ->
     F = fun() ->
            case sql_get_default_privacy_list_t(LUser) of
                {selected, []} ->
                    sql_remove_privacy_list(LUser, Name),
                    ok;
                {selected, [{Name}]} ->
                    {error, conflict};
                {selected, [{_Default}]} ->
                    sql_remove_privacy_list(LUser, Name)
            end
        end,
    case rdbms_queries:sql_transaction(LServer, F) of
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
            {selected, []} ->
                sql_add_privacy_list(LUser, Name),
                {selected, [{I}]} =
                sql_get_privacy_list_id_t(LUser, Name),
                I;
            {selected, [{I}]} ->
                I
            end,
        sql_set_privacy_list(ID, RItems),
        ok
    end,
    case rdbms_queries:sql_transaction(LServer, F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

remove_user(LUser, LServer) ->
    sql_del_privacy_lists(LUser, LServer).


raw_to_item({BType, BValue, BAction, BOrder, BMatchAll, BMatchIQ,
         BMatchMessage, BMatchPresenceIn, BMatchPresenceOut}) ->
    {Type, Value} =
    case BType of
        <<"n">> ->
        {none, none};
        <<"j">> ->
        case jid:from_binary(BValue) of
            #jid{} = JID ->
            {jid, jid:to_lower(JID)}
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
        <<"d">> -> deny;
        <<"b">> -> block
    end,
    Order = mongoose_rdbms:result_to_integer(BOrder),
    MatchAll = mongoose_rdbms:to_bool(BMatchAll),
    MatchIQ = mongoose_rdbms:to_bool(BMatchIQ),
    MatchMessage = mongoose_rdbms:to_bool(BMatchMessage),
    MatchPresenceIn = mongoose_rdbms:to_bool(BMatchPresenceIn),
    MatchPresenceOut =  mongoose_rdbms:to_bool(BMatchPresenceOut),
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
        {<<"j">>, mongoose_rdbms:escape(jid:to_binary(Value))};
        group ->
        {<<"g">>, mongoose_rdbms:escape(Value)};
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
        deny -> <<"d">>;
        block -> <<"b">>
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
    Username = mongoose_rdbms:escape(LUser),
    rdbms_queries:get_default_privacy_list(LServer, Username).

sql_get_default_privacy_list_t(LUser) ->
    Username = mongoose_rdbms:escape(LUser),
    rdbms_queries:get_default_privacy_list_t(Username).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    rdbms_queries:get_privacy_list_names(LServer, Username).

sql_get_privacy_list_names_t(LUser) ->
    Username = mongoose_rdbms:escape(LUser),
    rdbms_queries:get_privacy_list_names_t(Username).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = mongoose_rdbms:escape(LUser),
    SName = mongoose_rdbms:escape(Name),
    rdbms_queries:get_privacy_list_id(LServer, Username, SName).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = mongoose_rdbms:escape(LUser),
    SName = mongoose_rdbms:escape(Name),
    rdbms_queries:get_privacy_list_id_t(Username, SName).

sql_get_privacy_list_data_by_id(ID, LServer) when is_integer(ID) ->
    rdbms_queries:get_privacy_list_data_by_id(LServer, integer_to_binary(ID));
sql_get_privacy_list_data_by_id(ID, LServer) ->
    rdbms_queries:get_privacy_list_data_by_id(LServer, ID).

sql_set_default_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape(LUser),
    SName = mongoose_rdbms:escape(Name),
    rdbms_queries:set_default_privacy_list(Username, SName).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    rdbms_queries:unset_default_privacy_list(LServer, Username).

sql_remove_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape(LUser),
    SName = mongoose_rdbms:escape(Name),
    rdbms_queries:remove_privacy_list(Username, SName).

sql_add_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape(LUser),
    SName = mongoose_rdbms:escape(Name),
    rdbms_queries:add_privacy_list(Username, SName).

sql_set_privacy_list(ID, RItems) when is_integer(ID)->
    rdbms_queries:set_privacy_list(integer_to_binary(ID), RItems);
sql_set_privacy_list(ID, RItems) ->
    rdbms_queries:set_privacy_list(ID, RItems).

sql_del_privacy_lists(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    Server = mongoose_rdbms:escape(LServer),
    rdbms_queries:del_privacy_lists(LServer, Server, Username).
