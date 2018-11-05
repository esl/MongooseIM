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
init(_Host, _Opts) ->
    ok.

-spec get_last(jid:luser(), jid:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    case catch rdbms_queries:get_last(LServer, Username) of
        {selected, []} ->
            not_found;
        {selected, [{STimeStamp, Status}]} ->
            case catch mongoose_rdbms:result_to_integer(STimeStamp) of
                TimeStamp when is_integer(TimeStamp) ->
                    {ok, TimeStamp, Status};
                Reason ->
                    {error, {invalid_timestamp, Reason}}
            end;
        Reason -> {error, {invalid_result, Reason}}
    end.

-spec count_active_users(jid:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, TimeStamp) ->
    TimeStampBin = integer_to_binary(TimeStamp),
    WhereClause = <<"where seconds > ", TimeStampBin/binary >>,
    case rdbms_queries:count_records_where(LServer, <<"last">>, WhereClause) of
        {selected, [{Count}]} ->
            mongoose_rdbms:result_to_integer(Count);
        _ ->
            0
    end.

-spec set_last_info(jid:luser(), jid:lserver(),
                    non_neg_integer(), binary()) ->
    ok | {error, term()}.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    Username = mongoose_rdbms:escape_string(LUser),
    Seconds = mongoose_rdbms:escape_integer(TimeStamp),
    State = mongoose_rdbms:escape_string(Status),
    wrap_rdbms_result(rdbms_queries:set_last_t(LServer, Username, Seconds, State)).

-spec remove_user(jid:luser(), jid:lserver()) -> ok | {error, term()}.
remove_user(LUser, LServer) ->
    Username = mongoose_rdbms:escape_string(LUser),
    wrap_rdbms_result(rdbms_queries:del_last(LServer, Username)).

-spec wrap_rdbms_result({error, term()} | any()) -> ok | {error, term()}.
wrap_rdbms_result({error, _} = Error) -> Error;
wrap_rdbms_result(_) -> ok.

