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

-behaviour(mod_last_backend).

-include("mod_last.hrl").
-include("mongoose.hrl").

%% API
-export([init/2,
         get_last/3,
         count_active_users/3,
         set_last_info/5,
         remove_user/3,
         remove_domain/2]).

-type host_type() :: mongooseim:host_type().

-spec init(host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(last_activity,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, last_activity)}]),
    ok.

-spec get_last(host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | {error, term()} | not_found.
get_last(_HostType, LUser, LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
        {'EXIT', Reason} -> {error, Reason};
        [] -> not_found;
        [#last_activity{timestamp = TimeStamp,
            status = Status}] ->
            {ok, TimeStamp, Status}
    end.

-spec count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) -> non_neg_integer().
count_active_users(_HostType, LServer, TimeStamp) ->
    MS = [{{last_activity, {'_', LServer}, '$1', '_'},
        [{'>', '$1', TimeStamp}],
        [true]}],
    ets:select_count(last_activity, MS).

-spec set_last_info(host_type(), jid:luser(), jid:lserver(),
                    mod_last:timestamp(), mod_last:status()) -> ok | {error, term()}.
set_last_info(_HostType, LUser, LServer, TimeStamp, Status) ->
    US = {LUser, LServer},
    F = fun() ->
        mnesia:write(#last_activity{us = US,
            timestamp = TimeStamp,
            status = Status})
    end,
    wrap_transaction_result(mnesia:transaction(F)).

-spec remove_user(host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() -> mnesia:delete({last_activity, US}) end,
    wrap_transaction_result(mnesia:transaction(F)).

% Implementation only for RDBMS backends
-spec remove_domain(host_type(), jid:lserver()) -> ok.
remove_domain(_HostType, _Domain) ->
    ok.

-spec wrap_transaction_result({atomic, ok | term()} | term()) -> ok | {error, term()}.
wrap_transaction_result({atomic, ok}) -> ok;
wrap_transaction_result({atomic, Error}) -> {error, Error};
wrap_transaction_result(Error) -> {error, Error}.


