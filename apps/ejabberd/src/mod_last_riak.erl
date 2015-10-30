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

%% API
-export([init/2,
         get_last/2,
         count_active_users/3,
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
            Value = riakc_obj:get_value(Obj),
            {Timestamp, Status} = binary_to_term(Value),
            {ok, Timestamp, Status};
        {error, notfound} ->
            not_found;
        {error, Reason} ->
            {error, Reason}
    end.

-spec count_active_users(ejabberd:lserver(), non_neg_integer(), '<' | '>') ->
    non_neg_integer().
count_active_users(_LServer, _TimeStamp, _Comparator) ->
    ?ERROR_MSG("cannot count active users in the riak backend", []),
    0.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(), non_neg_integer(),
                    binary()) -> ok | {error, term()}.
set_last_info(LUser, LServer, Timestamp, Status) ->
    BinTimestampStatus = term_to_binary({Timestamp, Status}),
    Obj = riakc_obj:new(bucket_type(LServer), LUser, BinTimestampStatus),
    mongoose_riak:put(Obj).

-spec remove_user(ejabberd:luser(), ejabberd:lserver())  -> ok.
remove_user(LUser, LServer) ->
    mongoose_riak:delete(bucket_type(LServer), LUser).

bucket_type(LServer) -> {<<"last">>, LServer}.
