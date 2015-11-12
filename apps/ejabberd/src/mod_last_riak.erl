%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Pawel Pikula <pawel.pikula@erlang-solutions.com>
%%% Purpose : mod_last riak backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------
%%% @doc Riak backend for last activity XEP
%%%
%%% The backend uses the existing riak connection pool, which is "globally" defined in
%%% the ejebberd.cfg file. Therefore, we don't need to start anything in the init
%%% function.
%%%
%%% The module follows the approach taken by the other riak backends - it creates
%%% the following bucket {<<"last">>, <<"example.com">>} for each xmpp domain.
%%% The <<"last">> bucket type has the following props :'{"props":{"last_write_wins":true}}'
%%%
%%% Basically, there are 3 operations the backend need to provide set/get and count
%%% the number of users which has been active since given date. However, the count
%%% query can be only called using mongooseimctl.
%%%
%%% Username is used as a key and the value is just a user status, which is just
%%% a binary. Moreover, we have a secondary integer index on the timestamp, which
%%% is used in the count active users query.
%%%
%%% Data Layout:
%%% KV: {Username:binary, Status:binary}
%%% 2i: [Timestamp:integer]
%%%
%%% Set/Get are rather simple operations - they map to riak's put/get functions.
%%% The count query uses mapred task to calculate the number of users satisfying
%%% the criteria. As the input timestamp index range is used. The approach is
%%% described here: http://docs.basho.com/riak/latest/dev/using/2i/#Querying
%%% in the *Count Bucket Objects via $bucket Index* section. The alternative approach
%%% could be the "riakc_pb_socket:get_index_range" function
%%%
%%% The pros and cons of the both approaches have been discussed here:
%%% https://github.com/esl/MongooseIM/pull/567

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
    %% we are using common riak pool
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
