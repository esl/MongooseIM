-author("bartlomiej.gorny@erlang-solutions.com").

%% for worker_pool
-record(mongoose_worker_pool, {name :: atom(),
    selection_strategy :: atom(),
    extra :: term(),
    request_timeout :: pos_integer(),
    pool_timeout :: pos_integer() }).
