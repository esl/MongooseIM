%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Assigns archive integer identifiers (multihost version).
%%%
%%% This module supports several hosts.
%%%
%%% Archive id is assigned based on user_name and host.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_rdbms_user).
-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1, hooks/1, supported_features/0]).

%% ejabberd handlers
-export([archive_id/3,
         remove_archive/3]).

%% For debugging ONLY
-export([create_user_archive/3]).
-ignore_xref([create_user_archive/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

%% ----------------------------------------------------------------------
%% gen_mod callbacks
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    prepare_queries(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [{Hook, HostType, Fun, #{}, N}
     || {true, Hook, Fun, N} <- hooks2(HostType)].

hooks2(HostType) ->
    %% FIXME the auto_remove option is missing from the config spec
    AR = gen_mod:get_module_opt(HostType, ?MODULE, auto_remove, false),
    PM = gen_mod:get_module_opt(HostType, ?MODULE, pm, false),
    MUC = gen_mod:get_module_opt(HostType, ?MODULE, muc, false),
    [{PM, mam_archive_id, fun ?MODULE:archive_id/3, 50},
     {PM and AR, mam_remove_archive, fun ?MODULE:remove_archive/3, 90},
     {MUC, mam_muc_archive_id, fun ?MODULE:archive_id/3, 50},
     {MUC and AR, mam_muc_remove_archive, fun ?MODULE:remove_archive/3, 90}].

prepare_queries(HostType) ->
    mongoose_rdbms:prepare(mam_user_select, mam_server_user, [server, user_name],
                            <<"SELECT id FROM mam_server_user WHERE server=? AND user_name=?">>),
    mongoose_rdbms:prepare(mam_user_remove, mam_server_user, [server, user_name],
                            <<"DELETE FROM mam_server_user WHERE server=? AND user_name=?">>),
    rdbms_queries:prepare_upsert(HostType, mam_user_upsert, mam_server_user,
                                 [<<"server">>, <<"user_name">>],
                                 [],
                                 [<<"server">>, <<"user_name">>]),

    ok.

%%====================================================================
%% API
%%====================================================================
-spec archive_id(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:archive_id() | undefined,
    Params :: map(),
    Extra :: gen_hook:extra().
archive_id(undefined,
           #{owner := #jid{lserver = LServer, luser = LUser}},
           #{host_type := HostType}) ->
    {ok, query_archive_id(HostType, LServer, LUser)};
archive_id(ArcID, _Params, _Extra) ->
    {ok, ArcID}.

-spec remove_archive(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_archive(Acc,
               #{owner := #jid{lserver = LServer, luser = LUser}},
               #{host_type := HostType}) ->
    execute_user_remove(HostType, LServer, LUser),
    {ok, Acc}.

%%====================================================================
%% Internal functions
%%====================================================================

execute_user_remove(HostType, LServer, LUser) ->
    {updated, _} =
        mongoose_rdbms:execute(HostType, mam_user_remove, [LUser, LServer]).

-spec query_archive_id(mongooseim:host_type(), jid:lserver(), jid:luser()) -> integer().
query_archive_id(HostType, LServer, LUser) ->
    Tries = 5,
    query_archive_id(HostType, LServer, LUser, Tries).

query_archive_id(HostType, LServer, LUser, 0) ->
    ?LOG_ERROR(#{what => query_archive_id_failed,
                 host => HostType, server => LServer, user => LUser}),
    error(query_archive_id_failed);
query_archive_id(HostType, LServer, LUser, Tries) when Tries > 0 ->
    Result = mongoose_rdbms:execute(HostType, mam_user_select, [LServer, LUser]),
    case Result of
        {selected, [{IdBin}]} ->
            mongoose_rdbms:result_to_integer(IdBin);
        {selected, []} ->
            %% The user is not found
            create_user_archive(HostType, LServer, LUser),
            query_archive_id(HostType, LServer, LUser, Tries - 1)
    end.

-spec create_user_archive(mongooseim:host_type(), jid:lserver(), jid:luser()) -> ok.
create_user_archive(HostType, LServer, LUser) ->
    InsertParams = [LServer, LUser],
    UpdateParams = [], %% On duplicate key do nothing

    case rdbms_queries:execute_upsert(HostType, mam_user_upsert, InsertParams, UpdateParams) of
        {updated, _} ->
            ok;
        _Error ->
            error(create_user_archive_failed)
    end.
