%% Library functions without side effects.
%% These functions do not change the state of the system or send any messages.
%% These functions do not write into Mnesia/CETS or read from it.
%% They could read the configuration table though.
%% There is one hook `mongoose_hooks:s2s_allow_host', that could cause some side effects
%% (it depends on the hook handlers).
-module(mongoose_s2s_lib).
-export([make_from_to/2,
         domain_utf8_to_ascii/2,
         check_shared_secret/2,
         choose_pid/2,
         need_more_connections/2,
         needed_extra_connections_number_if_allowed/2,
         allow_host/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type fromto() :: ejabberd_s2s:fromto().
-type s2s_pids() :: ejabberd_s2s:s2s_pids().

-define(DEFAULT_MAX_S2S_CONNECTIONS, 1).
-define(DEFAULT_MAX_S2S_CONNECTIONS_PER_NODE, 1).

-spec make_from_to(From :: jid:jid(), To :: jid:jid()) ->  fromto().
make_from_to(#jid{lserver = FromServer}, #jid{lserver = ToServer}) ->
    {FromServer, ToServer}.

%% Converts a UTF-8 domain to ASCII (IDNA)
-spec domain_utf8_to_ascii(string() | jid:lserver(), binary) -> jid:lserver() | false;
                          (string() | jid:lserver(), string) -> string() | false.
domain_utf8_to_ascii(Domain, binary) ->
    Result = domain_utf8_to_ascii(Domain),
    false =/= Result andalso list_to_binary(Result);
domain_utf8_to_ascii(Domain, string) ->
    domain_utf8_to_ascii(Domain).

-spec domain_utf8_to_ascii(string() | jid:lserver()) -> string() | false.
domain_utf8_to_ascii(Domain) ->
    case catch idna:utf8_to_ascii(Domain) of
        {'EXIT', _} ->
            false;
        AsciiDomain ->
            AsciiDomain
    end.

-spec check_shared_secret(HostType, StoredSecretResult) -> ok | {update, NewSecret} when
    HostType :: mongooseim:host_type(),
    StoredSecretResult :: {ok, ejabberd_s2s:base16_secret()} | {error, not_found},
    NewSecret :: ejabberd_s2s:base16_secret().
check_shared_secret(HostType, StoredSecretResult) ->
    %% register_secret is replicated across all nodes.
    %% So, when starting a node with updated secret in the config,
    %% we would replace stored secret on all nodes at once.
    %% There could be a small race condition when dialback key checks would get rejected,
    %% But there would not be conflicts when some nodes have one secret stored and others - another.
    case {StoredSecretResult, get_shared_secret_from_config(HostType)} of
        {{error, not_found}, {ok, Secret}} ->
            %% Write the secret from the config into Mnesia/CETS for the first time
            {update, Secret};
        {{error, not_found}, {error, not_found}} ->
            %% Write a random secret into Mnesia/CETS for the first time
            {update, make_random_secret()};
        {{ok, Secret}, {ok, Secret}} ->
            %% Config matches Mnesia/CETS
            ok;
        {{ok, _OldSecret}, {ok, NewSecret}} ->
            ?LOG_INFO(#{what => overwrite_secret_from_config}),
            {update, NewSecret};
        {{ok, _OldSecret}, {error, not_found}} ->
            %% Keep the secret already stored in Mnesia/CETS
            ok
    end.

-spec make_random_secret() -> ejabberd_s2s:base16_secret().
make_random_secret() ->
    binary:encode_hex(crypto:strong_rand_bytes(10), lowercase).

-spec get_shared_secret_from_config(mongooseim:host_type()) ->
    {ok, ejabberd_s2s:base16_secret()} | {error, not_found}.
get_shared_secret_from_config(HostType) ->
    mongoose_config:lookup_opt([{s2s, HostType}, shared]).

%% Prefers the local connection (i.e. not on the remote node)
-spec choose_pid(From :: jid:jid(), Pids :: s2s_pids()) -> pid().
choose_pid(From, [_|_] = Pids) ->
    Pids1 = case filter_local_pids(Pids) of
                [] -> Pids;
                FilteredPids -> FilteredPids
            end,
    % Use sticky connections based on the JID of the sender
    % (without the resource to ensure that a muc room always uses the same connection)
    Pid = lists:nth(erlang:phash2(jid:to_bare(From), length(Pids1)) + 1, Pids1),
    ?LOG_DEBUG(#{what => s2s_choose_pid, from => From, s2s_pid => Pid}),
    Pid.

%% Returns only pids from the current node.
-spec filter_local_pids(s2s_pids()) -> s2s_pids().
filter_local_pids(Pids) ->
    Node = node(),
    [Pid || Pid <- Pids, node(Pid) == Node].

-spec max_s2s_connections(fromto()) -> pos_integer().
max_s2s_connections(FromTo) ->
    match_integer_acl_rule(FromTo, max_s2s_connections,
                           ?DEFAULT_MAX_S2S_CONNECTIONS).

-spec max_s2s_connections_per_node(fromto()) -> pos_integer().
max_s2s_connections_per_node(FromTo) ->
    match_integer_acl_rule(FromTo, max_s2s_connections_per_node,
                           ?DEFAULT_MAX_S2S_CONNECTIONS_PER_NODE).

-spec match_integer_acl_rule(fromto(), atom(), integer()) -> term().
match_integer_acl_rule({FromServer, ToServer}, Rule, Default) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(FromServer),
    ToServerJid = jid:make(<<>>, ToServer, <<>>),
    case acl:match_rule(HostType, Rule, ToServerJid) of
        Int when is_integer(Int) -> Int;
        _ -> Default
    end.

-spec needed_extra_connections_number_if_allowed(fromto(), s2s_pids()) -> non_neg_integer().
needed_extra_connections_number_if_allowed(FromTo, OldCons) ->
    case is_s2s_allowed_for_host(FromTo, OldCons) of
        true ->
            needed_extra_connections_number(FromTo, OldCons);
        false ->
            0
    end.

%% Checks:
%% - if the host is not a service
%% - and host policy (allowlist or denylist)
-spec is_s2s_allowed_for_host(fromto(), _OldConnections :: s2s_pids()) -> boolean().
is_s2s_allowed_for_host(_FromTo, [_|_]) ->
    true; %% Has outgoing connections established, skip the check
is_s2s_allowed_for_host(FromTo, []) ->
    not is_service(FromTo) andalso allow_host(FromTo).

%% Checks if the s2s host is not in the denylist or is in the allowlist
%% Runs a hook
-spec allow_host(fromto()) -> boolean().
allow_host({FromServer, ToServer}) ->
    case mongoose_domain_api:get_host_type(FromServer) of
        {error, not_found} ->
            false;
        {ok, HostType} ->
            case mongoose_config:lookup_opt([{s2s, HostType}, host_policy, ToServer]) of
                {ok, allow} ->
                    true;
                {ok, deny} ->
                    false;
                {error, not_found} ->
                    mongoose_config:get_opt([{s2s, HostType}, default_policy]) =:= allow
                        andalso mongoose_hooks:s2s_allow_host(FromServer, ToServer) =:= allow
            end
    end.

-spec need_more_connections(fromto(), s2s_pids()) -> boolean().
need_more_connections(FromTo, Connections) ->
    needed_extra_connections_number(FromTo, Connections) > 0.

-spec needed_extra_connections_number(fromto(), s2s_pids()) -> non_neg_integer().
needed_extra_connections_number(FromTo, Connections) ->
    MaxConnections = max_s2s_connections(FromTo),
    MaxConnectionsPerNode = max_s2s_connections_per_node(FromTo),
    LocalPids = filter_local_pids(Connections),
    lists:min([MaxConnections - length(Connections),
               MaxConnectionsPerNode - length(LocalPids)]).

%% Returns true if the destination must be considered as a service.
-spec is_service(ejabberd_s2s:fromto()) -> boolean().
is_service({FromServer, ToServer} = _FromTo) ->
    case mongoose_config:lookup_opt({route_subdomains, FromServer}) of
        {ok, s2s} -> % bypass RFC 3920 10.3
            false;
        {error, not_found} ->
            Hosts = ?MYHOSTS,
            P = fun(ParentDomain) -> lists:member(ParentDomain, Hosts) end,
            lists:any(P, parent_domains(ToServer))
    end.

-spec parent_domains(jid:lserver()) -> [jid:lserver()].
parent_domains(Domain) ->
    parent_domains(Domain, [Domain]).

parent_domains(<<>>, Acc) ->
    lists:reverse(Acc);
parent_domains(<<$., Rest/binary>>, Acc) ->
    parent_domains(Rest, [Rest | Acc]);
parent_domains(<<_, Rest/binary>>, Acc) ->
    parent_domains(Rest, Acc).
