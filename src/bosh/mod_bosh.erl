-module(mod_bosh).
-moduledoc """
BOSH support for MongooseIM. Requires mongoose_bosh_handler to handle incoming connections.
""".

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-xep([{xep, 206}, {version, "1.4"}]).
-xep([{xep, 124}, {version, "1.11.2"}]).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% mongoose_module_metrics callbacks
-export([config_metrics/1]).

%% Hook handlers
-export([node_cleanup/3]).

%% Internal API
-export([get_session_socket/1, store_session/3, delete_session/1]).

%% API for testing and debugging
-export([get_sessions/0]).
-ignore_xref([get_sessions/0]).

-include("mongoose_config_spec.hrl").
-include("mod_bosh.hrl").

-type socket() :: #bosh_socket{}.
-type session() :: #bosh_session{sid :: sid(), socket :: pid()}.
-type sid() :: binary().

-export_type([session/0, sid/0, socket/0]).

%% gen_mod callbacks

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_bosh_backend:start(HostType, Opts),
    {ok, _Pid} = mod_bosh_socket:start_supervisor(HostType).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ok = mod_bosh_socket:stop_supervisor(HostType).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{node_cleanup_for_host_type, HostType, fun ?MODULE:node_cleanup/3, #{}, 50}].

-spec node_cleanup(map(), #{node := node()}, gen_hook:extra()) -> {ok, map()}.
node_cleanup(Acc, #{node := Node}, #{host_type := HostType}) ->
    Res = mod_bosh_backend:node_cleanup(HostType, Node),
    {ok, Acc#{?MODULE => Res}}.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"backend">> => #option{type = atom,
                                                validate = {module, service_bosh}},
                       <<"inactivity">> => #option{type = int_or_infinity,
                                                   validate = positive},
                       <<"max_wait">> => #option{type = int_or_infinity,
                                                 validate = positive},
                       <<"server_acks">> => #option{type = boolean},
                       <<"max_pause">> => #option{type = integer,
                                                  validate = positive}
                      },
             defaults = #{<<"backend">> => mnesia,
                          <<"inactivity">> => 30, % seconds
                          <<"max_wait">> => infinity, % seconds
                          <<"server_acks">> => false,
                          <<"max_pause">> => 120} % seconds
            }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% mongoose_module_metrics callbacks

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).

%% Internal API

-spec get_session_socket(sid()) -> {ok, pid()} | {error, item_not_found}.
get_session_socket(Sid) ->
    case service_bosh_backend:get_session(Sid) of
        [BS] ->
            {ok, BS#bosh_session.socket};
        [] ->
            {error, item_not_found}
    end.

-spec store_session(mongooseim:host_type(), sid(), pid()) -> any().
store_session(HostType, Sid, Socket) ->
    mod_bosh_backend:create_session(HostType, #bosh_session{sid = Sid, socket = Socket}).

-spec delete_session(sid()) -> any().
delete_session(Sid) ->
    service_bosh_backend:delete_session(Sid).

%% API for testing and debugging

-spec get_sessions() -> [session()].
get_sessions() ->
    service_bosh_backend:get_sessions().
