-module(mongoose_c2s_listener).

-include("mongoose.hrl").

-behaviour(mongoose_listener).
-export([start_listener/1,
         instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% Hook handlers
-export([handle_user_open_session/3]).

-type options() :: #{module := module(),
                     atom() => any()}.

-spec instrumentation(options()) -> [mongoose_instrument:spec()].
instrumentation(_Opts) ->
    [{c2s_tcp_data_out, #{},
      #{metrics => #{byte_size => spiral}}},
     {c2s_tls_data_out, #{},
      #{metrics => #{byte_size => spiral}}},
     {c2s_tcp_data_in, #{},
      #{metrics => #{byte_size => spiral}}},
     {c2s_tls_data_in, #{},
      #{metrics => #{byte_size => spiral}}}].

%% mongoose_listener
-spec start_listener(options()) -> ok.
start_listener(#{module := Module} = Opts) ->
    HostTypes = ?ALL_HOST_TYPES,
    TransportOpts = mongoose_listener:prepare_socket_opts(Opts),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    maybe_add_access_check(HostTypes, Opts, ListenerId),
    ChildSpec = ranch:child_spec(ListenerId, ranch_tcp, TransportOpts, Module, Opts),
    ChildSpec1 = ChildSpec#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    mongoose_listener_sup:start_child(ChildSpec1).

%% Hooks and handlers
-spec handle_user_open_session(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
handle_user_open_session(Acc, #{c2s_data := StateData},
                         #{host_type := HostType, listener_id := ListenerId}) ->
    ListenerOpts = mongoose_c2s:get_listener_opts(StateData),
    case mongoose_listener_config:listener_id(ListenerOpts) of
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
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    mongoose_c2s:start_link({mongoose_c2s_ranch, {Transport, Ref}, Opts}, [{hibernate_after, HibernateAfterTimeout}]).

%% supervisor
maybe_add_access_check(_, #{access := all}, _) ->
    ok;
maybe_add_access_check(HostTypes, _, ListenerId) ->
    AclHooks = [ {user_open_session, HostType, fun ?MODULE:handle_user_open_session/3,
                  #{listener_id => ListenerId}, 10}
                 || HostType <- HostTypes ],
    gen_hook:add_handlers(AclHooks).
