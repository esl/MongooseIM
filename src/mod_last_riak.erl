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
%%% the mongooseim.toml file. Therefore, we don't need to start anything in the init
%%% function.
%%%
%%% The module follows the approach taken by the other riak backends - it creates
%%% the following bucket `{<<"last">>, <<"example.com">>}' for each xmpp domain.
%%% The `<<"last">>' bucket type has the following props: `{"props":{"last_write_wins":true}}'
%%%
%%% Basically, there are 3 operations the backend need to provide set/get and count
%%% the number of users which has been active since given date. However, the count
%%% query can be only called using `mongooseimctl'.
%%%
%%% Username is used as a key and the value is just a user status, which is just
%%% a binary. Moreover, we have a secondary integer index on the timestamp, which
%%% is used in the count active users query.
%%%
%%% ```
%%% Data Layout:
%%% KV: {Username:binary, Status:binary}
%%% 2i: [Timestamp:integer]
%%% '''
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
%%% @end
-module(mod_last_riak).

-behaviour(mod_last_backend).

-include("mongoose.hrl").

-define(TIMESTAMP_IDX, {integer_index, "timestamp"}).

%% API
-export([init/2,
         get_last/3,
         count_active_users/3,
         set_last_info/5,
         remove_user/3,
         remove_domain/2]).

-type host_type() :: mongooseim:host_type().

-spec init(host_type(), gen_mod:module_opts()) -> ok.
init(_VHost, _Opts) ->
    %% we are using common riak pool
    ok.

-spec get_last(host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.
get_last(HostType, LUser, LServer) ->
    case mongoose_riak:get(bucket_type(HostType, LServer), LUser) of
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

-spec count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) -> non_neg_integer().
count_active_users(HostType, LServer, TimeStamp) ->
    Idx = {index, bucket_type(HostType, LServer), ?TIMESTAMP_IDX, TimeStamp+1, infinity()},
    RedMF = {modfun, riak_kv_mapreduce, reduce_count_inputs},
    Red = [{reduce, RedMF, [{reduce_phase_batch_size, 1000}], true}],
    {ok, [{0, [Count]}]}  = mongoose_riak:mapred(Idx, Red),
    Count.

-spec set_last_info(host_type(), jid:luser(), jid:lserver(),
                    mod_last:timestamp(), mod_last:status()) ->
          ok | {error, term()}.
set_last_info(HostType, LUser, LServer, Timestamp, Status) ->
    Obj = riakc_obj:new(bucket_type(HostType, LServer), LUser, Status),
    MD = riakc_obj:get_update_metadata(Obj),
    MDWithIndex = riakc_obj:set_secondary_index(MD, [{?TIMESTAMP_IDX, [Timestamp]}]),
    FinalObj = riakc_obj:update_metadata(Obj, MDWithIndex),

    mongoose_riak:put(FinalObj).

-spec remove_user(host_type(), jid:luser(), jid:lserver()) -> ok | {error, term()}.
remove_user(HostType, LUser, LServer) ->
    mongoose_riak:delete(bucket_type(HostType, LServer), LUser).

% Implementation only for RDBMS backends
-spec remove_domain(host_type(), jid:lserver()) -> ok.
remove_domain(_HostType, _Domain) ->
    ok.

-spec bucket_type(host_type(), jid:lserver()) -> riakc_obj:bucket().
bucket_type(HostType, LServer) ->
    {gen_mod:get_module_opt(HostType, mod_last, [riak, bucket_type]), LServer}.

-spec infinity() -> non_neg_integer().
infinity() ->
    % almost infinity ... Wed, 16 Nov 5138 09:46:39 GMT
    % I think Mongoose won't live so long ;)
    99999999999.
