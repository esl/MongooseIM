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

%% gen_mod handlers
-export([start/2, stop/1, supported_features/0]).

%% ejabberd handlers
-export([archive_id/3,
         remove_archive/4]).

%% For debugging ONLY
-export([create_user_archive/3]).

-ignore_xref([archive_id/3, create_user_archive/3, remove_archive/4, start/2,
              stop/1, supported_features/0]).

-include("mongoose.hrl").
-include("jlib.hrl").

%% ----------------------------------------------------------------------
%% gen_mod callbacks
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    prepare_queries(),
    ejabberd_hooks:add(hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [{Hook, HostType, ?MODULE, Fun, N}
     || {true, Hook, Fun, N} <- hooks2(HostType)].

hooks2(HostType) ->
    %% FIXME the auto_remove option is missing from the config spec
    AR = gen_mod:get_module_opt(HostType, ?MODULE, auto_remove, false),
    PM = gen_mod:get_module_opt(HostType, ?MODULE, pm, false),
    MUC = gen_mod:get_module_opt(HostType, ?MODULE, muc, false),
    [{PM, mam_archive_id, archive_id, 50},
     {PM and AR, mam_remove_archive, remove_archive, 90},
     {MUC, mam_muc_archive_id, archive_id, 50},
     {MUC and AR, mam_muc_remove_archive, remove_archive, 90}].

prepare_queries() ->
    mongoose_rdbms:prepare(mam_user_insert, mam_server_user, [server, user_name],
                            <<"INSERT INTO mam_server_user (server, user_name) VALUES (?, ?)">>),
    mongoose_rdbms:prepare(mam_user_select, mam_server_user, [server, user_name],
                            <<"SELECT id FROM mam_server_user WHERE server=? AND user_name=?">>),
    mongoose_rdbms:prepare(mam_user_remove, mam_server_user, [server, user_name],
                            <<"DELETE FROM mam_server_user WHERE server=? AND user_name=?">>),
    ok.

%%====================================================================
%% API
%%====================================================================
-spec archive_id(ArcID :: undefined | mod_mam_pm:archive_id(),
                 HostType :: mongooseim:host_type(),
                 ArchiveJID :: jid:jid()) -> mod_mam_pm:archive_id().
archive_id(undefined, HostType, _ArcJID=#jid{lserver = LServer, luser = LUser}) ->
    query_archive_id(HostType, LServer, LUser);
archive_id(ArcID, _Host, _ArcJID) ->
    ArcID.

-spec remove_archive(Acc :: map(), HostType :: mongooseim:host_type(),
                     ArchiveID :: mod_mam_pm:archive_id(),
                     ArchiveJID :: jid:jid()) -> map().
remove_archive(Acc, HostType, _ArcID, _ArcJID = #jid{lserver = LServer, luser = LUser}) ->
    execute_user_remove(HostType, LServer, LUser),
    Acc.

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
    Res = mongoose_rdbms:execute(HostType, mam_user_insert, [LServer, LUser]),
    case Res of
        {updated, 1} ->
            ok;
        _ ->
            %% There is a common race condition case
            %% Duplicate entry ... for key 'uc_mam_server_user_name'.
            %% In this case Res can de:
            %% - {error, duplicate_key}
            %% - {error, "[FreeTDS][SQL Server]Violation of UNIQUE KEY constraint" ++ _}
            %% Let's ignore the errors and just retry in query_archive_id
            ?LOG_WARNING(#{what => create_user_archive_failed, reason => Res,
                           user => LUser, host => HostType, server => LServer}),
            ok
    end.
