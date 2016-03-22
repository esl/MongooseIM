%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_privacy_cassandra).
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

-export([prepared_queries/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

prepared_queries() ->
    [{set_default_list_query, "INSERT INTO privacy_default_list (username, listname) VALUES (?,?)"},
     {get_default_list_query, "SELECT listname FROM privacy_default_list WHERE username = ?"},
     {forget_default_list_query, "DELETE FROM privacy_default_list WHERE username = ?"},

     {add_list_query, "INSERT INTO privacy_list (username, listname) VALUES (?,?)"},
     {get_list_names_only_query, "SELECT listname FROM privacy_list WHERE username = ?"},
     {remove_list_query, "DELETE FROM privacy_list WHERE username = ? AND listname = ?"},
     {remove_lists_query, "DELETE FROM privacy_list WHERE username = ?"},

     {add_list_item_ts_query, "INSERT INTO privacy_item (username, listname, itemname, value) VALUES (?,?,?,?) USING TIMESTAMP ?"},
     {get_list_items_query, "SELECT itemname, value FROM privacy_item WHERE username = ? AND listname = ?"},
     {remove_all_items_query, "DELETE FROM privacy_item WHERE username = ?"},
     {remove_list_items_query, "DELETE FROM privacy_item WHERE username = ? AND listname = ?"},
     {remove_list_items_ts_query, "DELETE FROM privacy_item USING TIMESTAMP ? WHERE username = ? AND listname = ?"}].


init(_Host, Opts) ->
    compile_params_module(Opts),
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

-spec get_default_list_name(ejabberd:luser(), ejabberd:lserver()) -> binary() | none.
get_default_list_name(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_default_list_query, Params),
    case Res of
        {ok, [[Name]]} ->
            Name;
        {ok, []} ->
            none;
        {error, Other} ->
            ?ERROR_MSG("issue=\"get_default_list_name failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            none
    end.

-spec get_list_names_only(ejabberd:luser(), ejabberd:lserver()) -> list(binary()).
get_list_names_only(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_list_names_only_query, Params),
    case Res of
        {ok, Rows} ->
            rows_to_names(Rows);
        {error, Other} ->
            ?ERROR_MSG("issue=\"get_list_names_only failed\", user=~ts, reason=~1000p",
                       [LUser, Other]),
            []
    end.

get_privacy_list(LUser, LServer, Name) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser, Name],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_list_items_query, Params),
    case Res of
        {ok, []} ->
            case list_exists(LUser, LServer, Name) of
                true ->
                    {ok, []};
                false ->
                    {error, not_found}
            end;
        {ok, Rows} ->
            Val = rows_to_privacy_list(Rows),
            {ok, Val};
        {error, Other} ->
            ?ERROR_MSG("issue=\"get_privacy_list failed\", user=~ts, listname=~ts, reason=~1000p",
                       [LUser, Name, Other]),
            {error, Other}
    end.

forget_default_list(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, forget_default_list_query, Params),
    handle_empty_result(Res, forget_default_list).

set_default_list(LUser, LServer, Name) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser, Name],
    case list_exists(LUser, LServer, Name) of
        true ->
            Res = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, set_default_list_query, Params),
            handle_empty_result(Res, set_default_list);
        false ->
            {error, not_found}
    end.

remove_privacy_list(LUser, LServer, Name) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser, Name],
    Queries = [{remove_list_items_query, Params},
               {remove_list_query, Params}],
    Res = mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries),
    handle_empty_result(Res, remove_privacy_list).

replace_privacy_list(LUser, LServer, Name, List) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Now = mongoose_cassandra:now_timestamp(),
    Next = Now + 1,
    EncodedList = [mongoose_privacy_serializer:encode(Item) || Item <- List],
    Queries =
        %% Remove old items
        [{remove_list_items_ts_query, [Now, LUser, Name]}]
        %% Save list name
        ++ [{add_list_query, [LUser, Name]}]
        %% Add new items
        ++ [{add_list_item_ts_query, [LUser, Name, ItemName, Value, Next]} || {ItemName, Value} <- EncodedList],
    Res = mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries),
    handle_empty_result(Res, replace_privacy_list).

remove_user(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    Queries = [{forget_default_list_query, Params},
               {remove_all_items_query, Params},
               {remove_lists_query, Params}],
    Res = mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries),
    handle_empty_result(Res, remove_user).

handle_empty_result({ok, []}, _Pos) -> ok;
handle_empty_result({error, Other}, Pos) ->
    ?ERROR_MSG("issue=\"handle_empty_result failed\", position=~p, reason=~1000p",
               [Pos, Other]),
    {error, Other};
handle_empty_result(Other, Pos) -> handle_empty_result({error, Other}, Pos).

list_exists(LUser, LServer, Name) ->
    Names = get_list_names_only(LUser, LServer),
    lists:member(Name, Names).

rows_to_privacy_list(Rows) ->
    Items = [row_to_privacy_item(Row) || Row <- Rows],
    sort_items(Items).

row_to_privacy_item([ItemName, ItemValue]) ->
    mongoose_privacy_serializer:decode({ItemName, ItemValue}).

sort_items(Items) ->
    lists:keysort(#listitem.order, Items).

rows_to_names(Rows) ->
    [Name || [Name] <- Rows].


%% ----------------------------------------------------------------------
%% Dynamic params module

compile_params_module(Params) ->
    CodeStr = params_helper(Params),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_privacy_cassandra_params.erl", Code).

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_privacy_cassandra_params).~n"
        "-compile(export_all).~n"
        "pool_name() -> ~p.~n",
        [proplists:get_value(pool_name, Params, default)
        ]))).

-spec pool_name(binary(), binary()) -> term().
pool_name(_LServer, _LUser) ->
    mod_privacy_cassandra_params:pool_name().
