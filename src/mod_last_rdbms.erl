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

-behaviour(mod_last_backend).

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
init(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

%% Prepared query functions
prepare_queries(HostType) ->
    mongoose_rdbms:prepare(last_select, last, [server, username],
                           <<"SELECT seconds, state FROM last WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(last_count_active, last, [server, seconds],
                           <<"SELECT COUNT(*) FROM last WHERE server = ? AND seconds > ?">>),
    mongoose_rdbms:prepare(last_delete, last, [server, username],
                           <<"DELETE FROM last WHERE server = ? AND username = ?">>),
    mongoose_rdbms:prepare(last_remove_domain, last, [server],
                            <<"DELETE FROM last WHERE server = ?">>),
    rdbms_queries:prepare_upsert(HostType, last_upsert, last,
                                 [<<"server">>, <<"username">>, <<"seconds">>, <<"state">>],
                                 [<<"seconds">>, <<"state">>],
                                 [<<"server">>, <<"username">>]).

-spec execute_get_last(host_type(), jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_get_last(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, last_select, [LServer, LUser]).

-spec execute_count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) ->
          mongoose_rdbms:query_result().
execute_count_active_users(HostType, LServer, Seconds) ->
    mongoose_rdbms:execute_successfully(HostType, last_count_active, [LServer, Seconds]).

-spec execute_remove_user(host_type(), jid:lserver(), jid:luser()) -> mongoose_rdbms:query_result().
execute_remove_user(HostType, LServer, LUser) ->
    mongoose_rdbms:execute_successfully(HostType, last_delete, [LServer, LUser]).

-spec execute_upsert_last(host_type(), jid:lserver(), jid:luser(),
                          mod_last:timestamp(), mod_last:status()) ->
          mongoose_rdbms:query_result().
execute_upsert_last(HostType, LServer, LUser, Seconds, State) ->
    InsertParams = [LServer, LUser, Seconds, State],
    UpdateParams = [Seconds, State],
    UniqueKeyValues = [LServer, LUser],
    rdbms_queries:execute_upsert(HostType, last_upsert,
                                 InsertParams, UpdateParams, UniqueKeyValues).

%% API functions
-spec get_last(host_type(), jid:luser(), jid:lserver()) ->
    {ok, mod_last:timestamp(), mod_last:status()} | not_found.
get_last(HostType, LUser, LServer) ->
    Result = execute_get_last(HostType, LServer, LUser),
    decode_last_result(Result).

-spec count_active_users(host_type(), jid:lserver(), mod_last:timestamp()) -> non_neg_integer().
count_active_users(HostType, LServer, Seconds) ->
    Result = execute_count_active_users(HostType, LServer, Seconds),
    mongoose_rdbms:selected_to_integer(Result).

-spec set_last_info(host_type(), jid:luser(), jid:lserver(),
                    mod_last:timestamp(), mod_last:status()) ->
          ok | {error, term()}.
set_last_info(HostType, LUser, LServer, Seconds, State) ->
    wrap_rdbms_result(execute_upsert_last(HostType, LServer, LUser, Seconds, State)).

-spec remove_user(host_type(), jid:luser(), jid:lserver()) -> ok | {error, term()}.
remove_user(HostType, LUser, LServer) ->
    wrap_rdbms_result(execute_remove_user(HostType, LServer, LUser)).

-spec remove_domain(host_type(), jid:lserver()) -> ok | {error, term()}.
remove_domain(HostType, Domain) ->
    mongoose_rdbms:execute(HostType, last_remove_domain, [Domain]).

%% Helper functions
decode_last_result({selected, []}) ->
    not_found;
decode_last_result({selected, [{DbSeconds, State}]}) ->
    Seconds = mongoose_rdbms:result_to_integer(DbSeconds),
    {ok, Seconds, State}.

-spec wrap_rdbms_result({error, term()} | any()) -> ok | {error, term()}.
wrap_rdbms_result({error, _} = Error) -> Error;
wrap_rdbms_result(_) -> ok.
