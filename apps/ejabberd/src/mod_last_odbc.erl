%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_last mnesia backend (XEP-0012)
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2014      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------


-module(mod_last_odbc).

-behaviour(mod_last).

-include("mod_last.hrl").

%% API
-export([init/2,
    get_last/2,
    set_last_info/4,
    remove_user/2]).

-spec init(ejabberd:server(), list()) -> no_return().
init(_Host, _Opts) ->
    ok.

-spec get_last(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_last(LServer, Username) of
        {selected, [<<"seconds">>, <<"state">>], []} ->
            not_found;
        {selected, [<<"seconds">>, <<"state">>],
            [{STimeStamp, Status}]} ->
            case catch binary_to_integer(STimeStamp) of
                TimeStamp when is_integer(TimeStamp) ->
                    {ok, TimeStamp, Status};
                Reason -> {error, {invalid_timestamp, Reason}}
            end;
        Reason -> {error, {invalid_result, Reason}}
    end.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(),
                    non_neg_integer(), binary()) ->
    {atomic, ok} | {error, term()}.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    Username = ejabberd_odbc:escape(LUser),
    Seconds = ejabberd_odbc:escape(integer_to_binary(TimeStamp)),
    State = ejabberd_odbc:escape(Status),
    odbc_queries:set_last_t(LServer, Username, Seconds, State).

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_last(LServer, Username).