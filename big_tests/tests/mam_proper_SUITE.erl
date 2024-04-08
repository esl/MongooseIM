-module(mam_proper_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).
-import(domain_helper, [host_type/0]).

%% Common Test init/teardown functions
suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, async_writer}].

groups() ->
    [{async_writer, [], [async_writer_store]}].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    C1 = dynamic_modules:save_modules(host_type(), C),
    escalus:init_per_suite(C1).

end_per_suite(C) ->
    dynamic_modules:restore_modules(C),
    escalus:end_per_suite(C).

init_per_group(G, _C) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true -> dynamic_modules:ensure_modules(host_type(), required_modules(G));
        false -> {skip, "rdbms not enabled"}
    end.

required_modules(_G) ->
    [{mod_mam, mam_helper:config_opts(#{pm => #{}})}].

end_per_group(_G, C) ->
    C.

init_per_testcase(Name, C) ->
    escalus:init_per_testcase(Name, C).

end_per_testcase(Name, C) ->
    escalus:end_per_testcase(Name, C).

%% Common Test Cases
async_writer_store(C) ->
    run_prop(async_writer_store, async_writer_store_prop(C)).

%% Proper Test Cases
async_writer_store_prop(_C) ->
    HostType = domain_helper:host_type(),
    ?FORALL(ParamsList, params_list(), async_writer_store_check(HostType, ParamsList)).

async_writer_store_check(HostType, ParamsList) ->
    clean_db(HostType),
    Name = prepare_insert(length(ParamsList)),
    Rows = [prepare_message(HostType, Params) || Params <- ParamsList],
    case execute(HostType, Name, lists:append(Rows)) of
        {updated, _} -> true;
        Other ->
            ct:pal("ParamsList ~p~nOther ~p", [ParamsList, Other]),
            false
    end.

%% Proper Type Generators
usernames() ->
   [<<"alice">>, <<"bob">>, <<"bob.1">>, <<"bob44">>, <<"a">>, <<"xd">>,
    <<"admin">>].

resources() ->
    [<<"res1">>, <<"PICKLE">>, <<"1">>, <<"xdxd">>].

origin_id() ->
    oneof([none, <<"orig_id">>,
           proper_types:non_empty(strip_nulls(proper_unicode:utf8(5)))]).

strip_nulls(Gen) ->
    ?LET(X, Gen, do_strip_nulls(X)).

do_strip_nulls(B) ->
    %% PostgreSQL doesn't support storing NULL (\0x00) characters in text fields
    List = unicode:characters_to_list(B),
    List2 = [X || X <- List, X =/= 0],
    unicode:characters_to_binary(List2).

username() ->
    ?SUCHTHAT(U, proper_types:non_empty(oneof(usernames())),
              jid:nodeprep(U) =/= error).

hostname() ->
    proper_types:oneof([<<"localhost">>, <<"otherhost">>, <<"example.com">>]).

resource() ->
    ?SUCHTHAT(R, proper_types:non_empty(oneof(resources())),
              jid:resourceprep(R) =/= error).

jid() ->
    ?SUCHTHAT(Jid, maybe_invalid_jid(), Jid =/= error).

maybe_invalid_jid() ->
    ?LET({U, S, R},
         {username(), hostname(), resource()},
         make_jid(U, S, R)).

make_jid(U, S, R) when is_binary(U), is_binary(S), is_binary(R) ->
    mongoose_helper:make_jid(U, S, R).

direction() ->
    proper_types:oneof([incoming, outgoing]).

body() ->
    strip_nulls(proper_unicode:utf8(1000)).

packet(To, Body) ->
    escalus_stanza:chat_to(jid:to_binary(To), Body).

%% Generates mod_mam:archive_message_params()
params() ->
    ?LET({MessId, ArcId, LocalJid, RemoteJid,
          OriginId, Dir, Body, SenderId, IsGroupChat},
         {non_neg_integer(), non_neg_integer(), jid(), jid(),
          origin_id(), direction(), body(), non_neg_integer(), boolean()},
         #{message_id => MessId, archive_id => ArcId,
           local_jid => LocalJid, remote_jid => RemoteJid,
           source_jid => choose_source_jid(Dir, LocalJid, RemoteJid),
           origin_id => OriginId, direction => Dir,
           packet => packet(choose_dest_jid(Dir, LocalJid, RemoteJid), Body),
           sender_id => SenderId, is_groupchat => IsGroupChat}).

choose_source_jid(incoming, _LocalJid, RemoteJid) -> RemoteJid;
choose_source_jid(outgoing, LocalJid, _RemoteJid) -> LocalJid.

choose_dest_jid(outgoing, _LocalJid, RemoteJid) -> RemoteJid;
choose_dest_jid(incoming, LocalJid, _RemoteJid) -> LocalJid.

params_list() ->
    ?LET(ParamsList, proper_types:non_empty(proper_types:list(params())),
         with_unique_message_id(ParamsList)).

with_unique_message_id(ParamsList) ->
    Pairs = [{MessId, Params} || #{message_id := MessId} = Params <- ParamsList],
    %% Removes duplicates
    maps:values(maps:from_list(Pairs)).

%% Proper Helper Functions
run_prop(PropName, Property) ->
    Opts = [verbose, long_result, {numtests, 30}],
    ?assertEqual(true, quickcheck(PropName, Property, Opts)).

quickcheck(PropName, Property, Opts) ->
    proper:quickcheck(proper:conjunction([{PropName, Property}]), Opts).

%% Helper Functions
prepare_insert(Len) ->
    Name = multi_name(insert_mam_message_prop, Len),
    rpc(mim(), mod_mam_rdbms_arch, prepare_insert, [Name, Len]),
    Name.

multi_name(Name, Times) ->
    list_to_atom(atom_to_list(Name) ++ integer_to_list(Times)).

prepare_message(HostType, Params) ->
    rpc(mim(), mod_mam_rdbms_arch, prepare_message, [HostType, Params]).

execute(HostType, BatchName, Args) ->
    rpc(mim(), mongoose_rdbms, execute, [HostType, BatchName, Args]).

clean_db(HostType) ->
    Q = <<"DELETE FROM mam_message">>,
    rpc(mim(), mongoose_rdbms, sql_query, [HostType, Q]).
