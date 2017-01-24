%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_last odbc backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------


-module(mod_last_odbc).

-behaviour(mod_last).

-include("mod_last.hrl").
-include("ejabberd.hrl").

%% API
-export([init/2,
         get_last/2,
         count_active_users/2,
         set_last_info/4,
         remove_user/2]).

-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec get_last(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_last(LServer, Username) of
        {selected, []} ->
            not_found;
        {selected, [{STimeStamp, Status}]} ->
            case catch ejabberd_odbc:result_to_integer(STimeStamp) of
                TimeStamp when is_integer(TimeStamp) ->
                    {ok, TimeStamp, Status};
                Reason ->
                    {error, {invalid_timestamp, Reason}}
            end;
        Reason -> {error, {invalid_result, Reason}}
    end.

-spec count_active_users(ejabberd:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, TimeStamp) ->
    TimeStampBin = integer_to_binary(TimeStamp),
    WhereClause = <<"where seconds > ", TimeStampBin/binary >>,
    case odbc_queries:count_records_where(LServer, <<"last">>, WhereClause) of
        {selected, [{Count}]} ->
            ejabberd_odbc:result_to_integer(Count);
        _ ->
            0
    end.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(),
                    non_neg_integer(), binary()) ->
    ok | {error, term()}.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    Username = ejabberd_odbc:escape(LUser),
    Seconds = ejabberd_odbc:escape(integer_to_binary(TimeStamp)),
    State = ejabberd_odbc:escape(Status),
    wrap_odbc_result(odbc_queries:set_last_t(LServer, Username, Seconds, State)).

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok | {error, term()}.
remove_user(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    wrap_odbc_result(odbc_queries:del_last(LServer, Username)).

-spec wrap_odbc_result({error, term()} | any()) -> ok | {error, term()}.
wrap_odbc_result({error, _} = Error) -> Error;
wrap_odbc_result(_) -> ok.

