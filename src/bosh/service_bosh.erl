-module(service_bosh).
-moduledoc "BOSH session management for MongooseIM".

-xep([{xep, 206}, {version, "1.4"}]).
-xep([{xep, 124}, {version, "1.11.2"}]).

-behaviour(mongoose_service).

-export([start/1, stop/0, config_spec/0]).

%% Hook handlers
-export([node_cleanup/3]).

%% Internal API
-export([get_session_socket/1, store_session/2, delete_session/1, is_host_type_allowed/1]).

%% API for testing and debugging
-export([get_sessions/0]).
-ignore_xref([get_sessions/0]).

-include("mongoose_config_spec.hrl").
-include("mongoose_bosh.hrl").

-type session() :: #bosh_session{sid :: sid(), socket :: pid()}.
-type sid() :: binary().

-export_type([session/0, sid/0]).

-spec start(mongoose_service:options()) -> ok.
start(Opts) ->
    service_bosh_backend:start(Opts),
    {ok, _Pid} = mongoose_bosh_socket:start_supervisor(),
    gen_hook:add_handlers(hooks()).

-spec stop() -> ok.
stop() ->
    mongoose_bosh_socket:stop_supervisor(),
    gen_hook:delete_handlers(hooks()),
    ok.

-spec hooks() -> gen_hook:hook_list().
hooks() ->
    [{node_cleanup, global, fun ?MODULE:node_cleanup/3, #{}, 50}].

-spec node_cleanup(map(), #{node := node()}, gen_hook:extra()) -> {ok, map()}.
node_cleanup(Acc, #{node := Node}, _) ->
    Res = service_bosh_backend:node_cleanup(Node),
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
                                                  validate = positive},
                       <<"host_types">> => #list{items = #option{type = binary,
                                                                 validate = non_empty},
                                                 validate = unique}
                      },
             defaults = #{<<"backend">> => mnesia,
                          <<"inactivity">> => 30, % seconds
                          <<"max_wait">> => infinity, % seconds
                          <<"server_acks">> => false,
                          <<"max_pause">> => 120} % seconds
            }.

-spec get_session_socket(sid()) -> {ok, pid()} | {error, item_not_found}.
get_session_socket(Sid) ->
    case service_bosh_backend:get_session(Sid) of
        [BS] ->
            {ok, BS#bosh_session.socket};
        [] ->
            {error, item_not_found}
    end.

-spec store_session(sid(), pid()) -> any().
store_session(Sid, Socket) ->
    service_bosh_backend:create_session(#bosh_session{sid = Sid, socket = Socket}).

-spec delete_session(sid()) -> any().
delete_session(Sid) ->
    service_bosh_backend:delete_session(Sid).

-spec get_sessions() -> [session()].
get_sessions() ->
    service_bosh_backend:get_sessions().

-spec is_host_type_allowed(mongooseim:host_type()) -> boolean().
is_host_type_allowed(HostType) ->
    case mongoose_config:lookup_opt([services, ?MODULE]) of
        {ok, #{host_types := HostTypes}} ->
            lists:member(HostType, HostTypes);
        {ok, #{}} ->
            true; % allow by default
        {error, not_found} ->
            false % service_bosh is not enabled
    end.
