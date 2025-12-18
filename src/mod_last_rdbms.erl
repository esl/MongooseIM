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

-behaviour(mod_last_backend).

-include("mongoose.hrl").
-include("session.hrl").
-include("mongoose_logger.hrl").

%% API
-export([init/2,
         get_last/3,
         count_active_users/3,
         set_last_info/5,
         session_cleanup/5,
         remove_user/3,
         remove_domain/2,
         sessions_cleanup/2]).

-type host_type() :: mongooseim:host_type().

-spec init(host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

%% Prepared query functions
prepare_queries(HostType) ->
    mongoose_rdbms:prepare(last_select, last, [server, username],
                           <<"SELECT seconds, state FROM last WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(last_count_active, last, [server, seconds],
                           <<"SELECT COUNT(*) FROM last WHERE server = ? AND seconds > ?">>),
    mongoose_rdbms:prepare(last_delete, last, [server, username],
                           <<"DELETE FROM last WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(last_remove_domain, last, [server],
                            <<"DELETE FROM last WHERE server = ?">>),
    Ins = [<<"server">>, <<"username">>, <<"seconds">>, <<"state">>],
    Upd = [<<"seconds">>, <<"state">>],
    Key = [<<"server">>, <<"username">>],
    rdbms_queries:prepare_upsert(HostType, last_upsert, last, Ins, Upd, Key),
    rdbms_queries:prepare_upsert_many(HostType, 10, last_upsert_many10, last, Ins, Upd, Key),
    rdbms_queries:prepare_upsert_many(HostType, 100, last_upsert_many100, last, Ins, Upd, Key),
    ok.

-spec execute_get_last(host_type(), jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_get_last(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, last_select, [LServer, LUser]).

-spec execute_count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) ->
          mongoose_rdbms:query_result().
execute_count_active_users(HostType, LServer, Seconds) ->
    mongoose_rdbms:execute_successfully(HostType, last_count_active, [LServer, Seconds]).

-spec execute_remove_user(host_type(), jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_remove_user(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, last_delete, [LServer, LUser]).

-spec execute_upsert_last(host_type(), jid:lserver(), jid:luser(),
                          mod_last:timestamp(), mod_last:status()) ->
          mongoose_rdbms:query_result().
execute_upsert_last(HostType, LServer, LUser, Seconds, State) ->
    InsertParams = [LServer, LUser, Seconds, State],
    UpdateParams = [Seconds, State],
    rdbms_queries:execute_upsert(HostType, last_upsert, InsertParams, UpdateParams).

%% API functions
-spec get_last(host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | not_found.
get_last(HostType, LUser, LServer) ->
    Result = execute_get_last(HostType, LServer, LUser),
    decode_last_result(Result).

-spec count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) -> non_neg_integer().
count_active_users(HostType, LServer, Seconds) ->
    Result = execute_count_active_users(HostType, LServer, Seconds),
    mongoose_rdbms:selected_to_integer(Result).

-spec session_cleanup(host_type(), jid:luser(), jid:lserver(), mod_last:timestamp(), mod_last:status()) ->
          ok | {error, term()}.
session_cleanup(_HostType, _LUser, _LServer, _Seconds, _State) ->
    %% Cleaning is done in sessions_cleanup
    ok.

-spec set_last_info(host_type(), jid:luser(), jid:lserver(), mod_last:timestamp(), mod_last:status()) ->
          ok | {error, term()}.
set_last_info(HostType, LUser, LServer, Seconds, State) ->
    wrap_rdbms_result(execute_upsert_last(HostType, LServer, LUser, Seconds, State)).

-spec remove_user(host_type(), jid:luser(), jid:lserver()) -> ok | {error, term()}.
remove_user(HostType, LUser, LServer) ->
    wrap_rdbms_result(execute_remove_user(HostType, LServer, LUser)).

-spec remove_domain(host_type(), jid:lserver()) -> ok | {error, term()}.
remove_domain(HostType, Domain) ->
    mongoose_rdbms:execute(HostType, last_remove_domain, [Domain]).

%% Helper functions
decode_last_result({selected, []}) ->
    not_found;
decode_last_result({selected, [{DbSeconds, State}]}) ->
    Seconds = mongoose_rdbms:result_to_integer(DbSeconds),
    {ok, Seconds, State}.

-spec wrap_rdbms_result({error, term()} | any()) -> ok | {error, term()}.
wrap_rdbms_result({error, _} = Error) -> Error;
wrap_rdbms_result(_) -> ok.

-spec sessions_cleanup(mongooseim:host_type(), [ejabberd_sm:session()]) -> ok.
sessions_cleanup(HostType, Sessions) ->
    Seconds = erlang:system_time(second),
    %% server, username, seconds, state
    Records = [[S, U, Seconds, <<>>] || #session{usr = {U, S, _}} <- Sessions],
    UpdateParams = [Seconds, <<>>],
    AllTasks = prepare_cleanup_tasks(Records),
    RunTaskF = fun({Count, QueryName, InsertParams}) ->
                   run_upsert(HostType, Count, QueryName, InsertParams, UpdateParams)
               end,
    run_tasks_in_parallel(RunTaskF, AllTasks).

prepare_cleanup_tasks(Records) ->
    %% PgSQL would complain if there are duplicates (i.e. when there are two sessions
    %% with the same name but different resources)
    Records2 = lists:usort(Records),
    {Singles, Many100} = bucketize(100, Records2),
    {Singles2, Many10} = bucketize(10, Singles),
    [{100, last_upsert_many100, lists:append(Batch)} || Batch <- Many100] ++
    [{10, last_upsert_many10, lists:append(Batch)} || Batch <- Many10] ++
    [{1, last_upsert, Rec} || Rec <- Singles2].

run_tasks_in_parallel(RunTaskF, AllTasks) ->
    Workers = 8,
    TasksForWorkers = spread(Workers, AllTasks),
    RunTasksF = fun(Tasks) -> lists:map(RunTaskF, Tasks) end,
    Results = mongoose_lib:pmap(RunTasksF, TasksForWorkers, timer:minutes(1)),
    [check_result(Res) || Res <- Results],
    ok.

run_upsert(HostType, 1, QueryName, InsertParams = [_S, _U|_], UpdateParams) ->
    {updated, _} = rdbms_queries:execute_upsert(HostType, QueryName,
                                                InsertParams, UpdateParams);
run_upsert(HostType, _Count, QueryName, InsertParams, UpdateParams) ->
    %% MySQL replace returns wrong numbers
    {updated, _} = rdbms_queries:execute_upsert_many(HostType, QueryName,
                                                     InsertParams, UpdateParams).

check_result({ok, Results}) ->
    lists:foreach(fun({updated, _}) -> ok end, Results);
check_result({error, Reason}) ->
    ?LOG_ERROR(#{what => session_cleanup_failed, reason => Reason}).

%% Create chunks of size N
bucketize(N, Records) ->
    bucketize(N, Records, []).

bucketize(N, Records, Acc) ->
    try
        lists:split(N, Records)
    of
        {Batch, Records2} ->
            bucketize(N, Records2, [Batch | Acc])
    catch error:badarg ->
        {Records, lists:reverse(Acc)}
    end.

%% Create N chunks
%% Spread elements into buckets one element at a time before moving to the next bucket
spread(N, Tasks) ->
    Buckets = lists:duplicate(N, []),
    spread(lists:reverse(Tasks), Buckets, []).

spread([Task | Tasks], [Bucket | Buckets], Acc) ->
    spread(Tasks, Buckets, [[Task | Bucket] | Acc]);
spread([], Buckets, Acc) ->
    Acc ++ lists:reverse(Buckets);
spread(Tasks, [], Acc) ->
    spread(Tasks, lists:reverse(Acc), []).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
bucketize_test_() ->
    [?_assertEqual({[10], [[1, 2, 3], [4, 5, 6], [7, 8, 9]]}, bucketize(3, lists:seq(1, 10)))].

spread_test_() ->
    [?_assertEqual([[1, 4, 7, 10], [2, 5, 8], [3, 6, 9]], spread(3, lists:seq(1, 10))),
     ?_assertEqual([[1, 6], [2, 7], [3, 8], [4, 9], [5, 10]], spread(5, lists:seq(1, 10))),
     ?_assertEqual([[1, 3, 5, 7, 9], [2, 4, 6, 8, 10]], spread(2, lists:seq(1, 10)))].
-endif.
