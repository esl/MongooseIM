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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_privacy_rdbms).
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
    try sql_get_default_privacy_list(LUser, LServer) of
        {selected, []} ->
            none;
        {selected, [{DefName}]} ->
            DefName;
        Other ->
            ?ERROR_MSG("event=get_default_list_name_failed "
                       "user=~ts server=~ts result=~1000p",
                       [LUser, LServer, Other]),
            none
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("event=get_default_list_name_failed "
                       "user=~ts server=~ts"
                       "reason=~p:~p stacktrace=~1000p",
                       [LUser, LServer,
                        Class, Reason, StackTrace]),
            none
    end.

get_list_names_only(LUser, LServer) ->
    try sql_get_privacy_list_names(LUser, LServer) of
        {selected, Names} ->
            [Name || {Name} <- Names];
        Other ->
            ?ERROR_MSG("event=get_list_names_only_failed "
                       "user=~ts server=~ts result=~1000p",
                       [LUser, LServer, Other]),
            []
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("event=get_list_names_only_failed "
                       "user=~ts server=~ts"
                       "reason=~p:~p stacktrace=~1000p",
                       [LUser, LServer,
                        Class, Reason, StackTrace]),
            []
    end.


get_privacy_list(LUser, LServer, Name) ->
    try sql_get_privacy_list_id(LUser, LServer, Name) of
        {selected, []} ->
            {error, not_found};
        {selected, [{ID}]} ->
            IntID = mongoose_rdbms:result_to_integer(ID),
            get_privacy_list_by_id(LUser, LServer, Name, IntID, LServer);
        Other ->
            ?ERROR_MSG("event=get_privacy_list_failed "
                       "user=~ts server=~ts listname=~ts result=~1000p",
                       [LUser, LServer, Name, Other]),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("event=get_privacy_list_failed "
                       "user=~ts server=~ts listname=~ts"
                       "reason=~p:~p stacktrace=~1000p",
                       [LUser, LServer, Name,
                        Class, Reason, StackTrace]),
            {error, Reason}
    end.

get_privacy_list_by_id(LUser, LServer, Name, ID, LServer) when is_integer(ID) ->
    try sql_get_privacy_list_data_by_id(ID, LServer) of
        {selected, RItems} ->
            Items = raw_to_items(RItems),
            {ok, Items};
        Other ->
            ?ERROR_MSG("event=get_privacy_list_by_id_failed "
                       "user=~ts server=~ts listname=~ts id=~p result=~1000p",
                       [LUser, LServer, Name, ID, Other]),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("event=get_privacy_list_by_id_failed "
                       "user=~ts server=~ts listname=~ts id=~p"
                       "reason=~p:~p stacktrace=~1000p",
                       [LUser, LServer, Name, ID,
                        Class, Reason, StackTrace]),
            {error, Reason}
    end.

raw_to_items(RItems) ->
    lists:map(fun raw_to_item/1, RItems).

%% @doc Set no default list for user.
forget_default_list(LUser, LServer) ->
    try sql_unset_default_privacy_list(LUser, LServer) of
        {updated, _} ->
            ok;
        Other ->
            ?ERROR_MSG("event=forget_default_list_failed "
                       "user=~ts server=~ts result=~1000p",
                       [LUser, LServer, Other]),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?ERROR_MSG("event=forget_default_list_failed "
                       "user=~ts server=~ts"
                       "reason=~p:~p stacktrace=~1000p",
                       [LUser, LServer,
                        Class, Reason, StackTrace]),
            {error, Reason}
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
        sql_set_privacy_list(mongoose_rdbms:result_to_integer(ID), RItems),
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

-spec item_to_raw(mod_privacy:list_item()) -> list(mongoose_rdbms:escaped_value()).
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
        {<<"n">>, <<>>};
        jid ->
        {<<"j">>, jid:to_binary(Value)};
        group ->
        {<<"g">>, Value};
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
    SType = mongoose_rdbms:escape_string(BType),
    SValue = mongoose_rdbms:escape_string(BValue),
    BAction =
    case Action of
        allow -> <<"a">>;
        deny -> <<"d">>;
        block -> <<"b">>
    end,
    SAction = mongoose_rdbms:escape_string(BAction),
    SOrder = mongoose_rdbms:escape_integer(Order),
    SMatchAll = mongoose_rdbms:escape_boolean(MatchAll),
    SMatchIQ = mongoose_rdbms:escape_boolean(MatchIQ),
    SMatchMessage = mongoose_rdbms:escape_boolean(MatchMessage),
    SMatchPresenceIn = mongoose_rdbms:escape_boolean(MatchPresenceIn),
    SMatchPresenceOut = mongoose_rdbms:escape_boolean(MatchPresenceOut),
    [SType, SValue, SAction, SOrder, SMatchAll, SMatchIQ,
     SMatchMessage, SMatchPresenceIn, SMatchPresenceOut].

sql_get_default_privacy_list(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:get_default_privacy_list(LServer, Username).

sql_get_default_privacy_list_t(LUser) ->
    Username = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:get_default_privacy_list_t(Username).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:get_privacy_list_names(LServer, Username).

sql_get_privacy_list_names_t(LUser) ->
    Username = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:get_privacy_list_names_t(Username).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SName = mongoose_rdbms:escape_string(Name),
    rdbms_queries:get_privacy_list_id(LServer, Username, SName).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SName = mongoose_rdbms:escape_string(Name),
    rdbms_queries:get_privacy_list_id_t(Username, SName).

sql_get_privacy_list_data_by_id(ID, LServer) when is_integer(ID) ->
    rdbms_queries:get_privacy_list_data_by_id(LServer, mongoose_rdbms:escape_integer(ID)).

sql_set_default_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SName = mongoose_rdbms:escape_string(Name),
    rdbms_queries:set_default_privacy_list(Username, SName).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:unset_default_privacy_list(LServer, Username).

sql_remove_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SName = mongoose_rdbms:escape_string(Name),
    rdbms_queries:remove_privacy_list(Username, SName).

sql_add_privacy_list(LUser, Name) ->
    Username = mongoose_rdbms:escape_string(LUser),
    SName = mongoose_rdbms:escape_string(Name),
    rdbms_queries:add_privacy_list(Username, SName).

sql_set_privacy_list(ID, RItems) when is_integer(ID)->
    rdbms_queries:set_privacy_list(mongoose_rdbms:escape_integer(ID), RItems).

sql_del_privacy_lists(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    Server = mongoose_rdbms:escape_string(LServer),
    rdbms_queries:del_privacy_lists(LServer, Server, Username).
