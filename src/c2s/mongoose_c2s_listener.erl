-module(mongoose_c2s_listener).

-include("mongoose.hrl").

-behaviour(mongoose_listener).
-export([listener_spec/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

%% Hook handlers
-export([handle_user_open_session/3]).

-export([instrumentation/0]).
-ignore_xref([instrumentation/0]).

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
-spec listener_spec(mongoose_listener:options()) -> supervisor:child_spec().
listener_spec(Opts) ->
    maybe_add_access_check_hooks(Opts),
    mongoose_listener:child_spec(Opts).

%% ranch_protocol
-spec start_link(ranch:ref(), mongoose_listener:transport_module(), mongoose_listener:options()) ->
    {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_c2s:start_link({Transport, Ref, Opts}, ProcessOpts).

%% Hooks and handlers
-spec maybe_add_access_check_hooks(mongoose_listener:options()) -> ok.
maybe_add_access_check_hooks(#{access := all}) ->
    ok;
maybe_add_access_check_hooks(Opts) ->
    ListenerId = mongoose_listener:listener_id(Opts),
    AclHooks = [ {user_open_session, HostType, fun ?MODULE:handle_user_open_session/3,
                  #{listener_id => ListenerId}, 10}
                 || HostType <- ?ALL_HOST_TYPES ],
    gen_hook:add_handlers(AclHooks).

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
