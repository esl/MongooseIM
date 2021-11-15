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
-behaviour(mod_privacy_backend).

-export([init/2,
         get_default_list/3,
         get_list_names/3,
         get_privacy_list/4,
         set_default_list/4,
         forget_default_list/3,
         remove_privacy_list/4,
         replace_privacy_list/5,
         remove_user/3,
         remove_domain/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

prepare_queries(HostType) ->
    %% Queries to privacy_list table
    mongoose_rdbms:prepare(privacy_list_get_id, privacy_list, [server, username, name],
                           <<"SELECT id FROM privacy_list WHERE server=? AND username=? AND name=?">>),
    mongoose_rdbms:prepare(privacy_list_get_names, privacy_list, [server, username],
                           <<"SELECT name FROM privacy_list WHERE server=? AND username=?">>),
    mongoose_rdbms:prepare(privacy_list_delete_by_name, privacy_list, [server, username, name],
                           <<"DELETE FROM privacy_list WHERE server=? AND username=? AND name=?">>),
    mongoose_rdbms:prepare(privacy_list_delete_multiple, privacy_list, [server, username],
                           <<"DELETE FROM privacy_list WHERE server=? AND username=?">>),
    mongoose_rdbms:prepare(privacy_list_insert, privacy_list, [server, username, name],
                           <<"INSERT INTO privacy_list(server, username, name) VALUES (?, ?, ?)">>),
    %% Queries to privacy_default_list table
    mongoose_rdbms:prepare(privacy_default_get_name, privacy_default_list, [server, username],
                           <<"SELECT name FROM privacy_default_list WHERE server=? AND username=?">>),
    mongoose_rdbms:prepare(privacy_default_delete, privacy_default_list, [server, username],
                           <<"DELETE from privacy_default_list WHERE server=? AND username=?">>),
    prepare_default_list_upsert(HostType),
    %% Queries to privacy_list_data table
    mongoose_rdbms:prepare(privacy_data_get_by_id, privacy_list_data, [id],
                           <<"SELECT ord, t, value, action, match_all, match_iq, "
                             "match_message, match_presence_in, match_presence_out "
                             "FROM privacy_list_data "
                             "WHERE id=? ORDER BY ord">>),
    mongoose_rdbms:prepare(delete_data_by_id, privacy_list_data, [id],
                           <<"DELETE FROM privacy_list_data WHERE id=?">>),
    mongoose_rdbms:prepare(privacy_data_delete, privacy_list_data, [id, ord],
                           <<"DELETE FROM privacy_list_data WHERE id=? AND ord=?">>),
    mongoose_rdbms:prepare(privacy_data_update, privacy_list_data,
                           [t, value, action, match_all, match_iq,
                            match_message, match_presence_in, match_presence_out, id, ord],
                           <<"UPDATE privacy_list_data SET "
                             "t=?, value=?, action=?, match_all=?, match_iq=?, "
                             "match_message=?, match_presence_in=?, match_presence_out=? "
                             " WHERE id=? AND ord=?">>),
    mongoose_rdbms:prepare(privacy_data_insert, privacy_list_data,
                           [id, ord, t, value, action, match_all, match_iq,
                            match_message, match_presence_in, match_presence_out],
                           <<"INSERT INTO privacy_list_data("
                              "id, ord, t, value, action, match_all, match_iq, "
                              "match_message, match_presence_in, match_presence_out) "
                              "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
    %% This query uses multiple tables
    mongoose_rdbms:prepare(privacy_data_delete_user, privacy_list, [server, username],
                           <<"DELETE FROM privacy_list_data WHERE id IN "
                             "(SELECT id FROM privacy_list WHERE server=? AND username=?)">>),
    %% delete domain queries
    mongoose_rdbms:prepare(privacy_default_delete_domain, privacy_default_list, [server],
                           <<"DELETE from privacy_default_list WHERE server=?">>),
    mongoose_rdbms:prepare(privacy_list_delete_domain, privacy_list, [server],
                           <<"DELETE FROM privacy_list WHERE server=?">>),
    mongoose_rdbms:prepare(privacy_data_delete_domain, privacy_list, [server],
                           <<"DELETE FROM privacy_list_data WHERE id IN "
                             "(SELECT id FROM privacy_list WHERE server=?)">>),
    ok.

prepare_default_list_upsert(HostType) ->
    Fields = [<<"name">>],
    Filter = [<<"server">>, <<"username">>],
    rdbms_queries:prepare_upsert(HostType, privacy_default_upsert, privacy_default_list,
                                 Filter ++ Fields, Fields, Filter).

default_list_upsert(HostType, LServer, LUser, Name) ->
    InsertParams = [LServer, LUser, Name],
    UpdateParams = [Name],
    UniqueKeyValues = [LServer, LUser],
    rdbms_queries:execute_upsert(HostType, privacy_default_upsert,
                                 InsertParams, UpdateParams, UniqueKeyValues).

get_default_list(HostType, LUser, LServer) ->
    case get_default_list_name(HostType, LUser, LServer) of
        none ->
            {error, not_found};
        Default ->
            case get_privacy_list(HostType, LUser, LServer, Default) of
                {ok, List} ->
                    {ok, {Default, List}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_list_names(HostType, LUser, LServer) ->
    Default = get_default_list_name(HostType, LUser, LServer),
    Names = get_list_names_only(HostType, LUser, LServer),
    {ok, {Default, Names}}.

get_default_list_name(HostType, LUser, LServer) ->
    try execute_privacy_default_get_name(HostType, LServer, LUser) of
        {selected, []} ->
            none;
        {selected, [{DefName}]} ->
            DefName;
        Other ->
            ?LOG_ERROR(#{what => privacy_get_default_list_name_failed,
                         user => LUser, server => LServer, reason => Other}),
            none
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => privacy_get_default_list_name_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            none
    end.

get_list_names_only(HostType, LUser, LServer) ->
    try execute_privacy_list_get_names(HostType, LServer, LUser) of
        {selected, Names} ->
            [Name || {Name} <- Names];
        Other ->
            ?LOG_ERROR(#{what => privacy_get_list_names_only_failed,
                         user => LUser, server => LServer, reason => Other}),
            []
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => privacy_get_list_names_only_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            []
    end.

get_privacy_list(HostType, LUser, LServer, Name) ->
    try execute_privacy_list_get_id(HostType, LServer, LUser, Name) of
        {selected, []} ->
            {error, not_found};
        {selected, [{ID}]} ->
            IntID = mongoose_rdbms:result_to_integer(ID),
            get_privacy_list_by_id(HostType, LUser, LServer, Name, IntID, LServer);
        Other ->
            ?LOG_ERROR(#{what => privacy_get_privacy_list_failed,
                         user => LUser, server => LServer, list_name => Name,
                         reason => Other}),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => privacy_get_privacy_list_failed,
                         user => LUser, server => LServer, list_name => Name,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason}
    end.

get_privacy_list_by_id(HostType, LUser, LServer, Name, ID, LServer) when is_integer(ID) ->
    try execute_privacy_data_get_by_id(HostType, ID) of
        {selected, Rows} ->
            {ok, raw_to_items(Rows)};
        Other ->
            ?LOG_ERROR(#{what => privacy_get_privacy_list_by_id_failed,
                         user => LUser, server => LServer, list_name => Name, list_id => ID,
                         reason => Other}),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => privacy_get_privacy_list_by_id_failed,
                         user => LUser, server => LServer, list_name => Name, list_id => ID,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason}
    end.

%% @doc Set no default list for user.
forget_default_list(HostType, LUser, LServer) ->
    try execute_privacy_default_delete(HostType, LServer, LUser) of
        {updated, _} ->
            ok;
        Other ->
            ?LOG_ERROR(#{what => privacy_forget_default_list_failed,
                         user => LUser, server => LServer, reason => Other}),
            {error, Other}
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => privacy_forget_default_list_failed,
                         user => LUser, server => LServer,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            {error, Reason}
    end.

set_default_list(HostType, LUser, LServer, Name) ->
    F = fun() -> set_default_list_t(HostType, LServer, LUser, Name) end,
    case rdbms_queries:sql_transaction(HostType, F) of
        {atomic, ok} ->
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

set_default_list_t(HostType, LServer, LUser, Name) ->
    case execute_privacy_list_get_names(HostType, LServer, LUser) of
        {selected, []} ->
            {error, not_found};
        {selected, Names} ->
            case lists:member({Name}, Names) of
                true ->
                    default_list_upsert(HostType, LServer, LUser, Name),
                    ok;
                false ->
                    {error, not_found}
            end
    end.

remove_privacy_list(HostType, LUser, LServer, Name) ->
     F = fun() ->
            case execute_privacy_default_get_name(HostType, LServer, LUser) of
                {selected, [{Name}]} -> %% Matches Name variable
                    {error, conflict};
                {selected, _} ->
                    execute_privacy_list_delete_by_name(HostType, LServer, LUser, Name),
                    ok
            end
        end,
    case rdbms_queries:sql_transaction(HostType, F) of
        {atomic, {error, _} = Error} ->
            Error;
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

replace_privacy_list(HostType, LUser, LServer, Name, List) ->
    Rows = lists:map(fun item_to_raw/1, List),
    F = fun() ->
        ResultID = case execute_privacy_list_get_id(HostType, LServer, LUser, Name) of
            {selected, []} ->
                execute_privacy_list_insert(HostType, LServer, LUser, Name),
                {selected, [{I}]} = execute_privacy_list_get_id(HostType, LServer, LUser, Name),
                I;
            {selected, [{I}]} ->
                I
            end,
        ID = mongoose_rdbms:result_to_integer(ResultID),
        replace_data_rows(HostType, ID, Rows),
        ok
    end,
    {atomic, ok} = mongoose_rdbms:transaction_with_delayed_retry(HostType, F, #{retries => 5, delay => 100}),
    ok.

remove_domain(HostType, LServer) ->
    F = fun() -> remove_domain_t(HostType, LServer) end,
    rdbms_queries:sql_transaction(HostType, F).

remove_domain_t(HostType, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_data_delete_domain, [LServer]),
    mongoose_rdbms:execute_successfully(HostType, privacy_list_delete_domain, [LServer]),
    mongoose_rdbms:execute_successfully(HostType, privacy_default_delete_domain, [LServer]).

remove_user(HostType, LUser, LServer) ->
    F = fun() -> remove_user_t(HostType, LUser, LServer) end,
    rdbms_queries:sql_transaction(HostType, F).

remove_user_t(HostType, LUser, LServer) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_data_delete_user, [LServer, LUser]),
    mongoose_rdbms:execute_successfully(HostType, privacy_list_delete_multiple, [LServer, LUser]),
    execute_privacy_default_delete(HostType, LServer, LUser).

execute_privacy_list_get_id(HostType, LServer, LUser, Name) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_list_get_id, [LServer, LUser, Name]).

execute_privacy_default_get_name(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_default_get_name, [LServer, LUser]).

execute_privacy_list_get_names(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_list_get_names, [LServer, LUser]).

execute_privacy_data_get_by_id(HostType, ID) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_data_get_by_id, [ID]).

execute_privacy_default_delete(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_default_delete, [LServer, LUser]).

execute_privacy_list_delete_by_name(HostType, LServer, LUser, Name) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_list_delete_by_name, [LServer, LUser, Name]).

execute_privacy_list_insert(HostType, LServer, LUser, Name) ->
    mongoose_rdbms:execute_successfully(HostType, privacy_list_insert, [LServer, LUser, Name]).

execute_delete_data_by_id(HostType, ID) ->
    mongoose_rdbms:execute_successfully(HostType, delete_data_by_id, [ID]).

replace_data_rows(HostType, ID, []) when is_integer(ID) ->
    %% Just remove the data, nothing should be inserted
    execute_delete_data_by_id(HostType, ID);
replace_data_rows(HostType, ID, Rows) when is_integer(ID) ->
    {selected, OldRows} = execute_privacy_data_get_by_id(HostType, ID),
    New = lists:sort(Rows),
    Old = lists:sort([tuple_to_list(Row) || Row <- OldRows]),
    Diff = diff_rows(ID, New, Old, []),
    F = fun({Q, Args}) -> mongoose_rdbms:execute_successfully(HostType, Q, Args) end,
    lists:foreach(F, Diff),
    ok.

%% We assume that there are no record duplicates with the same Order.
%% It's checked in the main module for the New argument.
%% It's checked by the database for the Old argument.
diff_rows(ID, [H|New], [H|Old], Ops) ->
    diff_rows(ID, New, Old, Ops); %% Not modified
diff_rows(ID, [NewH|NewT] = New, [OldH|OldT] = Old, Ops) ->
    NewOrder = hd(NewH),
    OldOrder = hd(OldH),
    if NewOrder =:= OldOrder ->
           Op = {privacy_data_update, tl(NewH) ++ [ID, OldOrder]},
           diff_rows(ID, NewT, OldT, [Op|Ops]);
       NewOrder > OldOrder ->
           Op = {privacy_data_delete, [ID, OldOrder]},
           diff_rows(ID, New, OldT, [Op|Ops]);
       true ->
           Op = {privacy_data_insert, [ID|NewH]},
           diff_rows(ID, NewT, Old, [Op|Ops])
    end;
diff_rows(ID, [], [OldH|OldT], Ops) ->
    OldOrder = hd(OldH),
    Op = {privacy_data_delete, [ID, OldOrder]},
    diff_rows(ID, [], OldT, [Op|Ops]);
diff_rows(ID, [NewH|NewT], [], Ops) ->
    Op = {privacy_data_insert, [ID|NewH]},
    diff_rows(ID, NewT, [], [Op|Ops]);
diff_rows(_ID, [], [], Ops) ->
    Ops.

%% Encoding/decoding pure functions

raw_to_items(Rows) ->
    [raw_to_item(Row) || Row <- Rows].

raw_to_item({ExtOrder, ExtType, ExtValue, ExtAction,
             ExtMatchAll, ExtMatchIQ, ExtMatchMessage,
             ExtMatchPresenceIn, ExtMatchPresenceOut}) ->
    Type = decode_type(mongoose_rdbms:character_to_integer(ExtType)),
    #listitem{type = Type,
          value = decode_value(Type, ExtValue),
          action = decode_action(mongoose_rdbms:character_to_integer(ExtAction)),
          order = mongoose_rdbms:result_to_integer(ExtOrder),
          match_all = mongoose_rdbms:to_bool(ExtMatchAll),
          match_iq = mongoose_rdbms:to_bool(ExtMatchIQ),
          match_message = mongoose_rdbms:to_bool(ExtMatchMessage),
          match_presence_in = mongoose_rdbms:to_bool(ExtMatchPresenceIn),
          match_presence_out = mongoose_rdbms:to_bool(ExtMatchPresenceOut)}.

%% Encodes for privacy_data_insert query (but without ID)
-spec item_to_raw(mod_privacy:list_item()) -> list(term()).
item_to_raw(#listitem{type = Type,
              value = Value,
              action = Action,
              order = Order,
              match_all = MatchAll,
              match_iq = MatchIQ,
              match_message = MatchMessage,
              match_presence_in = MatchPresenceIn,
              match_presence_out = MatchPresenceOut}) ->
    ExtType = encode_type(Type),
    ExtValue = encode_value(Type, Value),
    ExtAction = encode_action(Action),
    Bools = [MatchAll, MatchIQ, MatchMessage, MatchPresenceIn, MatchPresenceOut],
    ExtBools = [encode_boolean(X) || X <- Bools],
    [Order, ExtType, ExtValue, ExtAction | ExtBools].

encode_boolean(true) -> 1;
encode_boolean(false) -> 0.

encode_action(allow) -> <<"a">>;
encode_action(deny)  -> <<"d">>;
encode_action(block) -> <<"b">>.

decode_action($a) -> allow;
decode_action($d) -> deny;
decode_action($b) -> block.

encode_subscription(none) -> <<"none">>;
encode_subscription(both) -> <<"both">>;
encode_subscription(from) -> <<"from">>;
encode_subscription(to)   -> <<"to">>.

decode_subscription(<<"none">>) -> none;
decode_subscription(<<"both">>) -> both;
decode_subscription(<<"from">>) -> from;
decode_subscription(<<"to">>)   -> to.

encode_type(none)         -> <<"n">>;
encode_type(jid)          -> <<"j">>;
encode_type(group)        -> <<"g">>;
encode_type(subscription) -> <<"s">>.

decode_type($n) -> none;
decode_type($j) -> jid;
decode_type($g) -> group;
decode_type($s) -> subscription.

encode_value(none, _Value)          -> <<>>;
encode_value(jid, Value)            -> jid:to_binary(Value);
encode_value(group, Value)          -> Value;
encode_value(subscription, Value)   -> encode_subscription(Value).

decode_value(none, _)               -> none;
decode_value(jid, BinJid)           -> jid:to_lower(jid:from_binary(BinJid));
decode_value(group, Group)          -> Group;
decode_value(subscription, ExtSub)  -> decode_subscription(ExtSub).
