-module(mongoose_graphql_mnesia_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, mnesia, <<"setMaster">>, #{<<"node">> := Node}) ->
    case mnesia_api:set_master(Node) of
        {ok, _} -> {ok, "Master node set"};
        {error, Reason} -> make_error({error, Reason}, #{node => Node})
    end;
execute(_Ctx, mnesia, <<"dump">>, #{<<"path">> := Path}) ->
    case mnesia_api:dump_mnesia(binary_to_list(Path)) of
        {ok, _} -> {ok, "Mnesia successfully dumped"};
        {error, Error} -> make_error(Error, #{path => Path})
    end;
execute(_Ctx, mnesia, <<"dumpTable">>, #{<<"path">> := Path, <<"table">> := Table}) ->
    case mnesia_api:dump_table(binary_to_list(Path), binary_to_list(Table)) of
        {ok, _} -> {ok, "Mnesia table successfully dumped"};
        {error, Error} -> make_error(Error, #{path => Path, table => Table})
    end;
execute(_Ctx, mnesia, <<"backup">>, #{<<"path">> := Path}) ->
    case mnesia_api:backup_mnesia(binary_to_list(Path)) of
        {ok, _} -> {ok, "Mnesia was successfully backuped"};
        {error, Error} -> make_error(Error, #{path => Path})
    end;
execute(_Ctx, mnesia, <<"restore">>, #{<<"path">> := Path}) ->
    case mnesia_api:restore_mnesia(binary_to_list(Path)) of
        {ok, _} -> {ok, "Mnesia was successfully restored"};
        {error, Error} -> make_error(Error, #{path => Path})
    end;
execute(_Ctx, mnesia, <<"installFallback">>, #{<<"path">> := Path}) ->
    case mnesia_api:install_fallback_mnesia(Path) of
        {ok, _} -> {ok, "Fallback installed"};
        Error -> make_error(Error, #{path => Path})
    end.
