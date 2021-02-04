%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_last rdbms backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------


-module(mod_last_rdbms).

-behaviour(mod_last).

-include("mod_last.hrl").
-include("mongoose.hrl").

%% API
-export([init/2,
         get_last/2,
         count_active_users/2,
         set_last_info/4,
         remove_user/2]).

-spec init(jid:server(), list()) -> ok.
init(Host, _Opts) ->
    prepare_queries(Host),
    ok.

%% Prepared query functions
prepare_queries(Host) ->
    mongoose_rdbms:prepare(last_select, last, [username],
                           <<"SELECT seconds, state FROM last WHERE username=?">>),
    mongoose_rdbms:prepare(last_count_active, last, [seconds],
                           <<"SELECT COUNT(*) FROM last WHERE seconds > ?">>),
    mongoose_rdbms:prepare(last_delete, last, [username],
                           <<"delete from last where username=?">>),
    rdbms_queries:prepare_upsert(Host, last_upsert, last,
                                 [<<"username">>, <<"seconds">>, <<"state">>],
                                 [<<"seconds">>, <<"state">>],
                                 [<<"username">>]).

execute_get_last(LServer, LUser) ->
    mongoose_rdbms:execute_successfully(LServer, last_select, [LUser]).

execute_count_active_users(LServer, Seconds) ->
    mongoose_rdbms:execute_successfully(LServer, last_count_active, [Seconds]).

execute_remove_user(LServer, LUser) ->
    mongoose_rdbms:execute_successfully(LServer, last_delete, [LUser]).

execute_upsert_last(Host, LUser, Seconds, State) ->
    InsertParams = [LUser, Seconds, State],
    UpdateParams = [Seconds, State],
    UniqueKeyValues = [LUser],
    rdbms_queries:execute_upsert(Host, last_upsert, InsertParams, UpdateParams, UniqueKeyValues).

%% API functions
-spec get_last(jid:luser(), jid:lserver()) ->
    {ok, non_neg_integer(), binary()} | not_found.
get_last(LUser, LServer) ->
    Result = execute_get_last(LServer, LUser),
    decode_last_result(Result).

-spec count_active_users(jid:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, Seconds) ->
    Result = execute_count_active_users(LServer, Seconds),
    mongoose_rdbms:selected_to_integer(Result).

-spec set_last_info(jid:luser(), jid:lserver(),
                    non_neg_integer(), binary()) -> ok | {error, term()}.
set_last_info(LUser, LServer, Seconds, State) ->
    wrap_rdbms_result(execute_upsert_last(LServer, LUser, Seconds, State)).

-spec remove_user(jid:luser(), jid:lserver()) -> ok | {error, term()}.
remove_user(LUser, LServer) ->
    wrap_rdbms_result(execute_remove_user(LServer, LUser)).

%% Helper functions
decode_last_result({selected, []}) ->
    not_found;
decode_last_result({selected, [{DbSeconds, State}]}) ->
    Seconds = mongoose_rdbms:result_to_integer(DbSeconds),
    {ok, Seconds, State}.

-spec wrap_rdbms_result({error, term()} | any()) -> ok | {error, term()}.
wrap_rdbms_result({error, _} = Error) -> Error;
wrap_rdbms_result(_) -> ok.
