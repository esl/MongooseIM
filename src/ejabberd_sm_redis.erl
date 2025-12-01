%%%-------------------------------------------------------------------
%%% @author Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Implementation of Redis-based session manager
%%%
%%% @end
%%% Created : 17 Nov 2011 by Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_redis).

-include("mongoose.hrl").
-include("session.hrl").

-behavior(ejabberd_sm_backend).
-export([init/1,
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         set_session/4,
         delete_session/4,
         cleanup/1,
         maybe_initial_cleanup/2,
         total_count/0,
         unique_count/0,
         parse_session_key/1]).

-ignore_xref([maybe_initial_cleanup/2, parse_session_key/1]).

-spec init(map()) -> ok.
init(_Opts) ->
    %% Clean current node's sessions from previous life
    {Elapsed, Count} = timer:tc(?MODULE, maybe_initial_cleanup, [node(), true]),
    ?LOG_NOTICE(#{what => sm_cleanup_initial,
                  text => <<"SM cleanup on start took">>,
                  duration => erlang:round(Elapsed / 1000),
                  removed_sessions_count => Count}),
    ok.

-spec get_sessions() -> [ejabberd_sm:session()].
get_sessions() ->
    Keys = mongoose_redis:cmd(["KEYS", hash(<<"*">>)]),
    lists:flatmap(fun(K) ->
                          Sessions = mongoose_redis:cmd(["SMEMBERS", K]),
                          lists:map(fun(S) ->
                                            binary_to_term(S)
                                    end,
                                    Sessions)
                  end, Keys).

-spec get_sessions(jid:server()) -> [ejabberd_sm:session()].
get_sessions(Server) ->
    Keys = mongoose_redis:cmd(["KEYS", hash(Server)]),
    lists:flatmap(fun(K) ->
                          Sessions = mongoose_redis:cmd(["SMEMBERS", K]),
                          lists:map(fun(S) ->
                                            binary_to_term(S)
                                    end,
                                    Sessions)
                  end, Keys).

-spec get_sessions(jid:user(), jid:server()) -> [ejabberd_sm:session()].
get_sessions(User, Server) ->
    Sessions = mongoose_redis:cmd(["SMEMBERS", hash(User, Server)]),

    lists:map(fun(S) -> binary_to_term(S) end, Sessions).

-spec get_sessions(jid:user(), jid:server(), jid:resource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    Sessions = mongoose_redis:cmd(["SMEMBERS", hash(User, Server, Resource)]),

    lists:map(fun(S) -> binary_to_term(S) end, Sessions).

-spec set_session(User :: jid:luser(),
                  Server :: jid:lserver(),
                  Resource :: jid:lresource(),
                  Session :: ejabberd_sm:session()) -> ok | {error, term()}.
set_session(User, Server, Resource, Session) ->
    OldSessions = get_sessions(User, Server, Resource),
    Node = sid_to_node(Session#session.sid),
    case lists:keysearch(Session#session.sid, #session.sid, OldSessions) of
        {value, OldSession} ->
            BOldSession = term_to_binary(OldSession),
            BSession = term_to_binary(Session),
            mongoose_redis:cmds([["SADD", n(Node), hash_v2(User, Server, Resource, Session#session.sid)],
                                 ["SREM", hash(User, Server), BOldSession],
                                 ["SREM", hash(User, Server, Resource), BOldSession],
                                 ["SADD", hash(User, Server), BSession],
                                 ["SADD", hash(User, Server, Resource), BSession]]);
        false ->
            BSession = term_to_binary(Session),
            mongoose_redis:cmds([["SADD", n(Node), hash_v2(User, Server, Resource, Session#session.sid)],
                                 ["SADD", hash(User, Server), BSession],
                                 ["SADD", hash(User, Server, Resource), BSession]])
    end.

-spec delete_session(SID :: ejabberd_sm:sid(),
                     User :: jid:user(),
                     Server :: jid:server(),
                     Resource :: jid:resource()) -> ok.
delete_session(SID, User, Server, Resource) ->
    Sessions = get_sessions(User, Server, Resource),
    case lists:keysearch(SID, #session.sid, Sessions) of
        {value, Session} ->
            BSession = term_to_binary(Session),
            mongoose_redis:cmds([["SREM", hash(User, Server), BSession],
                                 ["SREM", hash(User, Server, Resource), BSession],
                                 ["SREM", n(sid_to_node(SID)), hash_v2(User, Server, Resource, SID)]]);
        false ->
            ok
    end.

-spec cleanup(atom()) -> ok.
cleanup(Node) ->
    maybe_initial_cleanup(Node, false),
    ok.

-spec maybe_initial_cleanup(atom(), boolean()) -> non_neg_integer().
maybe_initial_cleanup(Node, Initial) ->
    Hashes = mongoose_redis:cmd(["SMEMBERS", n(Node)]),
    mongoose_redis:cmd(["DEL", n(Node)]),
    Sessions = lists:map(fun(H) ->
                          {U, S, R, SID} = parse_session_key(H),
                          delete_session(SID, U, S, R),
                          case Initial of
                              true ->
                                  ok;
                              false ->
                                  Session = #session{usr = {U, S, R}, sid = SID},
                                  ejabberd_sm:session_cleanup(Session),
                                  Session
                          end
                  end, Hashes),
    ejabberd_sm:sessions_cleanup(Sessions),
    length(Sessions).

-spec total_count() -> integer().
total_count() ->
    {Counts, _} = rpc:multicall(ejabberd_sm, get_node_sessions_number, []),
    lists:sum([Count || Count <- Counts, is_integer(Count)]).

-spec unique_count() -> integer().
unique_count() ->
    length(mongoose_redis:cmd(["KEYS", "s2:*"])).

%% Internal functions

-spec hash(binary()) -> iolist().
hash(Val1) ->
    ["s3:*:", Val1, ":*"].

-spec hash(binary(), binary()) -> iolist().
hash(Val1, Val2) ->
    ["s2:", Val1, ":", Val2].

-spec hash(binary(), binary(), binary()) -> iolist().
hash(Val1, Val2, Val3) ->
    ["s3:", Val1, ":", Val2, ":", Val3].

-spec n(atom()) -> iolist().
n(Node) ->
    ["n:", atom_to_list(Node)].

sid_to_node(SID) ->
    {_, Pid} = SID,
    node(Pid).

%% New format (s5:) uses hex-encoded resource to handle colons in resource
-spec hash_v2(binary(), binary(), binary(), ejabberd_sm:sid()) -> iolist().
hash_v2(User, Server, Resource, SID) ->
    ["s5:", User, ":", Server, ":", encode_resource(Resource), ":", term_to_binary(SID)].

-spec encode_resource(binary()) -> binary().
encode_resource(Resource) ->
    binary:encode_hex(Resource, lowercase).

-spec decode_resource(binary()) -> binary().
decode_resource(HexResource) ->
    binary:decode_hex(HexResource).

%% Parse session key supporting both old (s4:) and new (s5:) formats
-spec parse_session_key(binary()) -> {binary(), binary(), binary(), ejabberd_sm:sid()}.
parse_session_key(<<"s5:", Rest/binary>>) ->
    %% New format: s5:User:Server:HexEncodedResource:BinarySID
    %% HexResource contains only [0-9a-f], so we can safely split on first 3 colons
    [User, Rest1] = binary:split(Rest, <<":">>),
    [Server, Rest2] = binary:split(Rest1, <<":">>),
    [HexResource, BinarySID] = binary:split(Rest2, <<":">>),
    Resource = decode_resource(HexResource),
    SID = binary_to_term(BinarySID),
    {User, Server, Resource, SID};
parse_session_key(<<"s4:", _/binary>> = Key) ->
    %% Old format: s4:User:Server:Resource:BinarySID (Resource may contain colons)
    [_, User, Server, Resource | SIDEncoded] = binary:split(Key, <<":">>, [global]),
    %% Add possible removed ":" from encoded SID
    SID = binary_to_term(mongoose_bin:join(SIDEncoded, <<":">>)),
    {User, Server, Resource, SID}.
