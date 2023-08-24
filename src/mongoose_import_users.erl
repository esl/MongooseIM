-module(mongoose_import_users).

-export([run/1]).

-define(REGISTER_WORKERS_NUM, 10).

-type summary() :: #{reason() => [jid:jid() | binary()]}.
-type reason() :: ok | exists | not_allowed | invalid_jid | null_password |
                  limit_per_domain_exceeded | bad_csv.

-export_type([summary/0, reason/0]).

-spec run(file:name()) -> {ok, summary()} | {error, file_not_found}.
run(Filename) ->
    case filelib:is_file(Filename) of
        true ->
            {ok, CsvStream} = erl_csv:decode_new_s(Filename),
            Workers = spawn_link_workers(),
            WorkersQueue = queue:from_list(Workers),
            {ok, do_import(CsvStream, WorkersQueue)};
        false ->
            {error, file_not_found}
    end.

-spec do_import(erl_csv:csv_stream(), queue:queue()) -> summary().
do_import(stream_end, WQueue) ->
    Workers = queue:to_list(WQueue),
    lists:foldl(fun accumulate_results/2, #{}, Workers);
do_import(Stream, WQueue) ->
    {ok, Decoded, MoreStream} = erl_csv:decode_s(Stream),
    WQueue1 = send_job_to_next_worker(Decoded, WQueue),
    do_import(MoreStream, WQueue1).

-spec spawn_link_workers() -> [pid()].
spawn_link_workers() ->
    Manager = self(),
    [spawn_link(fun() -> registrator_proc(Manager) end)
     || _ <- lists:seq(1, ?REGISTER_WORKERS_NUM)].

-spec accumulate_results(pid(), summary()) -> summary().
accumulate_results(Pid, Map) ->
    Results = get_results_from_registrator(Pid),
    maps:merge_with(
        fun(_Key, List1, List2) -> List1 ++ List2 end,
        Map,
        Results).

-spec get_results_from_registrator(pid()) -> summary().
get_results_from_registrator(Pid) ->
    Pid ! get_result,
    receive
        {result, Result} -> Result
    end.

-spec send_job_to_next_worker([binary()], queue:queue()) -> queue:queue().
send_job_to_next_worker([], WQueue) ->
    WQueue;
send_job_to_next_worker([Record], WQueue) ->
    {{value, Worker}, Q1} = queue:out(WQueue),
    Worker ! {process, Record},
    queue:in(Worker, Q1).

-spec registrator_proc(pid()) -> ok.
registrator_proc(Manager) ->
    registrator_proc(Manager, #{}).

-spec registrator_proc(pid(), summary()) -> ok.
registrator_proc(Manager, Map) ->
    receive
        {process, Data} ->
            {Reason, User} = do_register(Data),
            Map2 = maps:update_with(Reason, fun(List) -> [User | List] end, [User], Map),
            registrator_proc(Manager, Map2);
        get_result ->
            Manager ! {result, Map}
    end,
    ok.

-spec do_register([binary()]) -> {reason(), jid:user() | binary()}.
do_register([User, Host, Password] = List) ->
    JID = jid:make_bare(User, Host),
    case ejabberd_auth:try_register(JID, Password) of
        {error, invalid_jid} -> {invalid_jid, join(List)};
        {error, Reason} -> {Reason, JID};
        _ -> {ok, JID}
    end;
do_register(List) ->
    {bad_csv, join(List)}.

-spec join([binary()]) -> binary().
join(Record) ->
    JoinBinary = fun(Elem, <<"">>) -> Elem;
                    (Elem, Acc) -> <<Elem/binary, ",", Acc/binary>>
                 end,
    lists:foldr(JoinBinary, <<"">>, Record).
