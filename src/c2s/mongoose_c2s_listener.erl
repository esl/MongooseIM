-module(mongoose_c2s_listener).

-include("mongoose.hrl").

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% Hook handlers
-export([handle_user_open_session/3]).

-export([instrumentation/0]).
-ignore_xref([instrumentation/0]).

-type options() :: #{module := ?MODULE,
                     atom() => any()}.

-spec instrumentation(_) -> [mongoose_instrument:spec()].
instrumentation(_) ->
    lists:foldl(fun instrumentation/2, instrumentation(), ?ALL_HOST_TYPES).

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{c2s_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {c2s_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {c2s_tls_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {c2s_tls_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {c2s_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {c2s_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}}].

-spec instrumentation(_, _) -> [mongoose_instrument:spec()].
instrumentation(HostType, Acc) when is_binary(HostType) ->
    [{c2s_message_processed, #{host_type => HostType},
      #{metrics => #{time => histogram}}},
     {c2s_auth_failed, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {c2s_element_in, #{host_type => HostType},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}},
     {c2s_element_out, #{host_type => HostType},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}} | Acc].

%% mongoose_listener
-spec start_listener(options()) -> ok.
start_listener(#{module := ?MODULE} = Opts) ->
    HostTypes = ?ALL_HOST_TYPES,
    TransportOpts = mongoose_listener:prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    maybe_add_access_check(HostTypes, Opts, ListenerId),
    ChildSpec = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, ?MODULE, Opts),
    ChildSpec1 = ChildSpec#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    mongoose_listener_sup:start_child(ChildSpec1).

%% Hooks and handlers
-spec handle_user_open_session(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
handle_user_open_session(Acc, #{c2s_data := StateData},
                         #{host_type := HostType, listener_id := ListenerId}) ->
    ListenerOpts = mongoose_c2s:get_listener_opts(StateData),
    case mongoose_listener:listener_id(ListenerOpts) of
        ListenerId ->
            Jid = mongoose_c2s:get_jid(StateData),
            LServer = mongoose_c2s:get_lserver(StateData),
            #{access := Access} = ListenerOpts,
            case acl:match_rule(HostType, LServer, Access, Jid) of
                allow ->
                    case mongoose_hooks:session_opening_allowed_for_user(HostType, Jid) of
                        allow -> {ok, Acc};
                        _ -> {stop, Acc}
                    end;
                deny ->
                    {stop, Acc}
            end;
        _Other ->
            {ok, Acc}
    end.

%% ranch_protocol
-spec start_link(ranch:ref(), module(), mongoose_c2s:listener_opts()) -> {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_c2s:start_link({mongoose_c2s_ranch, {Transport, Ref}, Opts}, ProcessOpts).

%% supervisor
maybe_add_access_check(_, #{access := all}, _) ->
    ok;
maybe_add_access_check(HostTypes, _, ListenerId) ->
    AclHooks = [ {user_open_session, HostType, fun ?MODULE:handle_user_open_session/3,
                  #{listener_id => ListenerId}, 10}
                 || HostType <- HostTypes ],
    gen_hook:add_handlers(AclHooks).
