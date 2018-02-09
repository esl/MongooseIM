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


-module(mod_last_mnesia).

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
    mnesia:create_table(last_activity,
                        [{disc_copies, [node()]},
                         {attributes,
                          record_info(fields, last_activity)}]),
    ok.

-spec get_last(jid:luser(), jid:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
        {'EXIT', Reason} -> {error, Reason};
        [] -> not_found;
        [#last_activity{timestamp = TimeStamp,
            status = Status}] ->
            {ok, TimeStamp, Status}
    end.

-spec count_active_users(jid:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, TimeStamp) ->
    MS = [{{last_activity, {'_', LServer}, '$1', '_'},
        [{'>', '$1', TimeStamp}],
        [true]}],
    ets:select_count(last_activity, MS).

-spec set_last_info(jid:luser(), jid:lserver(),
                    non_neg_integer(), binary()) -> ok | {error, term()}.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    US = {LUser, LServer},
    F = fun() ->
        mnesia:write(#last_activity{us = US,
            timestamp = TimeStamp,
            status = Status})
    end,
    wrap_transaction_result(mnesia:transaction(F)).

-spec remove_user(jid:luser(), jid:lserver()) -> ok.
remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() -> mnesia:delete({last_activity, US}) end,
    wrap_transaction_result(mnesia:transaction(F)).

-spec wrap_transaction_result({atomic, ok | term()} | term()) -> ok | {error, term()}.
wrap_transaction_result({atomic, ok}) -> ok;
wrap_transaction_result({atomic, Error}) -> {error, Error};
wrap_transaction_result(Error) -> {error, Error}.


