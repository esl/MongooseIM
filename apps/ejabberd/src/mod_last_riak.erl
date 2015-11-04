%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Pawel Pikula <pawel.pikula@erlang-solutions.com>
%%% Purpose : mod_last mnesia backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------

-module(mod_last_riak).

-behaviour(mod_last).

-include("mod_last.hrl").
-include("ejabberd.hrl").

-define(TIMESTAMP_IDX, {integer_index, "timestamp"}).

%% API
-export([init/2,
         get_last/2,
         count_active_users/2,
         set_last_info/4,
         remove_user/2]).

-spec init(ejabberd:server(), list()) ->ok.
init(_VHost, _Opts) ->
    %% we are using commong riak pool
    ok.

-spec get_last(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    case mongoose_riak:get(bucket_type(LServer), LUser) of
        {ok, Obj} ->
            Status = riakc_obj:get_value(Obj),
            MD = riakc_obj:get_update_metadata(Obj),
            [Timestamp] = riakc_obj:get_secondary_index(MD, ?TIMESTAMP_IDX),
            {ok, Timestamp, Status};
        {error, notfound} ->
            not_found;
        {error, Reason} ->
            {error, Reason}
    end.

-spec count_active_users(ejabberd:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, TimeStamp) ->
    Idx = {index, bucket_type(LServer), ?TIMESTAMP_IDX, TimeStamp+1, infinity()},
    RedMF = {modfun, riak_kv_mapreduce, reduce_count_inputs},
    Red = [{reduce, RedMF, [{reduce_phase_batch_size, 1000}], true}],
    {ok, [{0, [Count]}]}  = mongoose_riak:mapred(Idx, Red),
    Count.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(), non_neg_integer(),
                    binary()) -> ok | {error, term()}.
set_last_info(LUser, LServer, Timestamp, Status) ->
    Obj = riakc_obj:new(bucket_type(LServer), LUser, Status),
    MD = riakc_obj:get_update_metadata(Obj),
    MDWithIndex = riakc_obj:set_secondary_index(MD, [{?TIMESTAMP_IDX, [Timestamp]}]),
    FinalObj = riakc_obj:update_metadata(Obj, MDWithIndex),

    mongoose_riak:put(FinalObj).

-spec remove_user(ejabberd:luser(), ejabberd:lserver())  -> ok.
remove_user(LUser, LServer) ->
    mongoose_riak:delete(bucket_type(LServer), LUser).

bucket_type(LServer) -> {<<"last">>, LServer}.

-spec infinity() -> non_neg_integer().
infinity() ->
    % almost infinity ... Wed, 16 Nov 5138 09:46:39 GMT
    % I think Mongoose won't live so long ;)
    99999999999.
