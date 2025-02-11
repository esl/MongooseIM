%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc).
-author('alexey@process-one.net').
-xep([{xep, 45}, {version, "1.34.5"}]).
-xep([{xep, 249}, {version, "1.2"}]).
-behaviour(gen_server).
-behaviour(gen_mod).
-behaviour(mongoose_packet_handler).
-behaviour(mongoose_module_metrics).
-behaviour(mongoose_instrument_probe).

%% API
-export([start_link/2,
         start/2,
         stop/1,
         supported_features/0,
         config_spec/0,
         instrumentation/1,
         process_room_affiliation/1,
         room_destroyed/4,
         store_room/4,
         restore_room/3,
         forget_room/3,
         create_instant_room/6,
         broadcast_service_message/2,
         can_use_nick/4,
         room_jid_to_pid/1,
         get_vh_rooms/2,
         default_host/0]).
-export([server_host_to_muc_host/2]).

%% For testing purposes only
-export([register_room/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% packet handler callback
-export([process_packet/5]).

%% mongoose_instrument_probe callback
-export([probe/2]).

%% Hooks handlers
-export([is_muc_room_owner/3,
         can_access_room/3,
         remove_domain/3,
         acc_room_affiliations/3,
         can_access_identity/3,
         disco_local_items/3,
         node_cleanup_for_host_type/3]).

-export([config_metrics/1]).

-ignore_xref([broadcast_service_message/2, create_instant_room/6,
              register_room/4, restore_room/3, start_link/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_config_spec.hrl").
-include("mod_muc_room.hrl").

-export_type([access/0,
             room/0,
             nick/0,
             packet/0,
             role/0,
             affiliation/0
            ]).

-type role() :: moderator | participant | visitor | none.
-type affiliation() :: admin | owner | member | outcast | none.
-type room() :: binary().
-type nick() :: binary().
-type room_host() :: jid:simple_bare_jid().
-type packet() :: exml:element().
-type from_to_packet() ::
        {From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
         Packet :: packet()}.
-type access() :: {_AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent}.
-type host_type() :: mongooseim:host_type().
-type muc_host() :: jid:lserver().

-include("mod_muc.hrl").

-type muc_room() :: #muc_room{
    name_host    :: room_host(),
    opts         :: list()
}.

-type muc_online_room() :: #muc_online_room{
    name_host :: room_host(),
    host_type :: host_type(),
    pid       :: pid()
}.
-export_type([muc_online_room/0]).

-type room_event_data() :: #{
                  from_nick := nick(),
                  from_jid := jid:jid(),
                  room_jid := jid:jid(),
                  affiliation := affiliation(),
                  role := role(),
                  timestamp := integer()
       }.
-export_type([room_event_data/0]).

-record(muc_state, {host_type           :: host_type(),
                    subdomain_pattern   :: mongoose_subdomain_utils:subdomain_pattern(),
                    access,
                    history_size        :: integer(),
                    default_room_opts   :: list(),
                    room_shaper         :: mongoose_shaper:shaper(),
                    http_auth_pool      :: mongoose_http_client:pool(),
                    hibernated_room_check_interval :: timeout(),
                    hibernated_room_timeout :: timeout() }).

-type state() :: #muc_state{}.

-export_type([muc_room/0]).

-define(PROCNAME, ejabberd_mod_muc).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(host_type(), map())
            -> ignore | {error, _} | {ok, pid()}.
start_link(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, {HostType, Opts}, []).

-spec start(host_type(), _) -> ok.
start(HostType, Opts) when is_map(Opts) ->
    mod_muc_online_backend:start(HostType, Opts),
    start_supervisor(HostType),
    start_server(HostType, Opts),
    assert_server_running(HostType),
    ok.

-spec stop(host_type()) -> ok.
stop(HostType) ->
    stop_supervisor(HostType),
    stop_gen_server(HostType),
    mod_muc_online_backend:stop(HostType),
    ok.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

start_server(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [HostType, Opts]},
         temporary,
         1000,
         worker,
         [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(ChildSpec).

assert_server_running(HostType) ->
    true = is_pid(whereis(gen_mod:get_module_proc(HostType, ?PROCNAME))).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, mod_muc}},
                 <<"online_backend">> => #option{type = atom,
                                                 validate = {module, mod_muc_online}},
                 <<"host">> => #option{type = string,
                                       validate = subdomain_template,
                                       process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                 <<"access">> => #option{type = atom,
                                         validate = access_rule},
                 <<"access_create">> => #option{type = atom,
                                                validate = access_rule},
                 <<"access_admin">> => #option{type = atom,
                                               validate = access_rule},
                 <<"access_persistent">> => #option{type = atom,
                                                    validate = access_rule},
                 <<"history_size">> => #option{type = integer,
                                               validate = non_negative},
                 <<"room_shaper">> => #option{type = atom,
                                              validate = shaper},
                 <<"max_room_id">> => #option{type = int_or_infinity,
                                              validate = non_negative},
                 <<"max_room_name">> => #option{type = int_or_infinity,
                                                validate = non_negative},
                 <<"max_room_desc">> => #option{type = int_or_infinity,
                                                validate = non_negative},
                 <<"min_message_interval">> => #option{type = integer,
                                                       validate = non_negative},
                 <<"min_presence_interval">> => #option{type = integer,
                                                        validate = non_negative},
                 <<"max_users">> => #option{type = integer,
                                            validate = positive},
                 <<"max_users_admin_threshold">> => #option{type = integer,
                                                            validate = positive},
                 <<"user_message_shaper">> => #option{type = atom,
                                                      validate = shaper},
                 <<"user_presence_shaper">> => #option{type = atom,
                                                       validate = shaper},
                 <<"max_user_conferences">> => #option{type = integer,
                                                       validate = non_negative},
                 <<"http_auth_pool">> => #option{type = atom,
                                                 validate = pool_name},
                 <<"load_permanent_rooms_at_startup">> => #option{type = boolean},
                 <<"hibernate_timeout">> => #option{type = int_or_infinity,
                                                    validate = non_negative},
                 <<"hibernated_room_check_interval">> => #option{type = int_or_infinity,
                                                                 validate = non_negative},
                 <<"hibernated_room_timeout">> => #option{type = int_or_infinity,
                                                          validate = non_negative},
                 <<"default_room">> => default_room_config_spec()
                },
       defaults = defaults()
      }.

defaults() ->
    #{<<"backend">> => mnesia,
      <<"online_backend">> => mnesia,
      <<"host">> => default_host(),
      <<"access">> => all,
      <<"access_create">> => all,
      <<"access_admin">> => none,
      <<"access_persistent">> => all,
      <<"history_size">> => 20,
      <<"room_shaper">> => none,
      <<"max_room_id">> => infinity,
      <<"max_room_name">> => infinity,
      <<"max_room_desc">> => infinity,
      <<"min_message_interval">> => 0,
      <<"min_presence_interval">> => 0,
      <<"max_users">> => ?MAX_USERS_DEFAULT,
      <<"max_users_admin_threshold">> => 5,
      <<"user_message_shaper">> => none,
      <<"user_presence_shaper">> => none,
      <<"max_user_conferences">> => 10,
      <<"http_auth_pool">> => none,
      <<"load_permanent_rooms_at_startup">> => false,
      <<"hibernate_timeout">> => timer:seconds(90),
      <<"hibernated_room_check_interval">> => infinity,
      <<"hibernated_room_timeout">> => infinity,
      <<"default_room">> => keys_as_atoms(default_room_opts())}.

keys_as_atoms(Map) ->
    maps:from_list([{binary_to_atom(K), V} || {K, V} <- maps:to_list(Map)]).

default_room_config_spec() ->
    #section{
       items = #{<<"title">> => #option{type = binary},
                 <<"description">> => #option{type = binary},
                 <<"allow_change_subj">> => #option{type = boolean},
                 <<"allow_query_users">> => #option{type = boolean},
                 <<"allow_private_messages">> => #option{type = boolean},
                 <<"allow_visitor_status">> => #option{type = boolean},
                 <<"allow_visitor_nickchange">> => #option{type = boolean},
                 <<"public">> => #option{type = boolean},
                 <<"public_list">> => #option{type = boolean},
                 <<"persistent">> => #option{type = boolean},
                 <<"moderated">> => #option{type = boolean},
                 <<"members_by_default">> => #option{type = boolean},
                 <<"members_only">> => #option{type = boolean},
                 <<"allow_user_invites">> => #option{type = boolean},
                 <<"allow_multiple_sessions">> => #option{type = boolean},
                 <<"password_protected">> => #option{type = boolean},
                 <<"password">> => #option{type = binary},
                 <<"anonymous">> => #option{type = boolean},
                 <<"max_users">> => #option{type = integer,
                                            validate = positive},
                 <<"logging">> => #option{type = boolean},
                 <<"maygetmemberlist">> => #list{items = #option{type = atom,
                                                                 validate = non_empty}},
                 <<"affiliations">> => #list{items = default_room_affiliations_spec()},
                 <<"subject">> => #option{type = binary},
                 <<"subject_author">> => #option{type = binary}
                },
       defaults = default_room_opts()
      }.

default_room_opts() ->
    X = #config{},
    #{<<"title">> => X#config.title,
      <<"description">> => X#config.description,
      <<"allow_change_subj">> => X#config.allow_change_subj,
      <<"allow_query_users">> => X#config.allow_query_users,
      <<"allow_private_messages">> => X#config.allow_private_messages,
      <<"allow_visitor_status">> => X#config.allow_visitor_status,
      <<"allow_visitor_nickchange">> => X#config.allow_visitor_nickchange,
      <<"public">> => X#config.public,
      <<"public_list">> => X#config.public_list,
      <<"persistent">> => X#config.persistent,
      <<"moderated">> => X#config.moderated,
      <<"members_by_default">> => X#config.members_by_default,
      <<"members_only">> => X#config.members_only,
      <<"allow_user_invites">> => X#config.allow_user_invites,
      <<"allow_multiple_sessions">> => X#config.allow_multiple_sessions,
      <<"password_protected">> => X#config.password_protected,
      <<"password">> => X#config.password,
      <<"anonymous">> => X#config.anonymous,
      <<"max_users">> => X#config.max_users,
      <<"logging">> => X#config.logging,
      <<"maygetmemberlist">> => X#config.maygetmemberlist,
      <<"affiliations">> => [],
      <<"subject">> => <<>>,
      <<"subject_author">> => <<>>}.

default_room_affiliations_spec() ->
    #section{
       items = #{<<"user">> => #option{type = binary,
                                       validate = non_empty},
                 <<"server">> => #option{type = binary,
                                         validate = domain},
                 <<"resource">> => #option{type = binary},
                 <<"affiliation">> => #option{type = atom,
                                              validate = non_empty}},
       required = all,
       process = fun ?MODULE:process_room_affiliation/1
      }.

process_room_affiliation(#{user := User, server := Server, resource := Res, affiliation := Aff}) ->
    {{User, Server, Res}, Aff}.

stop_gen_server(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:call(Proc, stop),
    %% Proc can still be alive because of a race condition
    ejabberd_sup:stop_child(Proc).

%% @doc This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
-spec room_destroyed(host_type(), jid:server(), room(), pid()) -> 'ok'.
room_destroyed(HostType, MucHost, Room, Pid) ->
    mod_muc_online_backend:room_destroyed(HostType, MucHost, Room, Pid).

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
%% XXX Only used from tests.
-spec create_instant_room(jid:lserver(), MucHost :: jid:lserver(), Name :: room(),
    From :: jid:jid(), Nick :: nick(), Opts :: list()) -> any().
create_instant_room(ServerHost, MucHost, Name, From, Nick, Opts) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(ServerHost),
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:call(Proc, {create_instant, ServerHost, MucHost, Name, From, Nick, Opts}).

-spec store_room(host_type(), jid:server(), room(), list()) ->
    {error, _} | ok.
store_room(HostType, MucHost, Name, Opts) ->
    mod_muc_backend:store_room(HostType, MucHost, Name, Opts).

-spec restore_room(host_type(), muc_host(), room()) ->
        {error, _} | {ok, _}.
restore_room(HostType, MucHost, Name) ->
    mod_muc_backend:restore_room(HostType, MucHost, Name).

-spec forget_room(host_type(), jid:server(), room()) -> ok | {error, term()}.
forget_room(HostType, MucHost, Name) ->
    %% Removes room from DB, even if it's already removed.
    Result = mod_muc_backend:forget_room(HostType, MucHost, Name),
    case Result of
        ok ->
            %% TODO This hook should be renamed to forget_room_hook.
            %% We also need to think how to remove stopped rooms
            %% (i.e. in case we want to expose room removal over REST or SQS).
            %%
            %% In some _rare_ cases this hook can be called more than once for the same room.
            mongoose_hooks:forget_room(HostType, MucHost, Name);
        _ ->
            %% Room is not removed or we don't know.
            %% XXX Handle this case better.
            ok
    end,
    Result.

%% For rooms
-spec process_iq_disco_items(MucHost :: jid:server(), From :: jid:jid(),
        To :: jid:jid(), jlib:iq()) -> mongoose_acc:t().
process_iq_disco_items(MucHost, From, To, #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                                 attrs = #{<<"xmlns">> => ?NS_DISCO_ITEMS},
                                 children = iq_disco_items(MucHost, From, Lang, Rsm)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res)).

-spec can_use_nick(host_type(), jid:server(), jid:jid(), nick()) -> boolean().
can_use_nick(_HostType, _Host, _JID, <<>>) ->
    false;
can_use_nick(HostType, MucHost, JID, Nick) ->
    mod_muc_backend:can_use_nick(HostType, MucHost, JID, Nick).

set_nick(_HostType, _MucHost, _From, <<>>) ->
    {error, should_not_be_empty};
set_nick(HostType, MucHost, From, Nick) ->
    mod_muc_backend:set_nick(HostType, MucHost, From, Nick).

unset_nick(HostType, MucHost, From) ->
    mod_muc_backend:unset_nick(HostType, MucHost, From).

get_nick(HostType, MucHost, From) ->
    mod_muc_backend:get_nick(HostType, MucHost, From).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init({host_type(), map()}) -> {ok, state()}.
init({HostType, Opts}) ->
    mod_muc_backend:init(HostType, Opts),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    #{access := Access,
      access_create := AccessCreate,
      access_admin := AccessAdmin,
      access_persistent := AccessPersistent,
      http_auth_pool := HttpAuthPool,
      history_size := HistorySize,
      default_room := DefRoomOpts,
      room_shaper := RoomShaper,
      hibernated_room_check_interval := CheckInterval,
      hibernated_room_timeout := HibernatedTimeout,
      host := SubdomainPattern,
      load_permanent_rooms_at_startup := LoadPermRoomsAtStartup} = Opts,
    State = #muc_state{host_type = HostType,
                       subdomain_pattern = SubdomainPattern,
                       access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
                       default_room_opts = maps:to_list(DefRoomOpts),
                       history_size = HistorySize,
                       room_shaper = RoomShaper,
                       http_auth_pool = HttpAuthPool,
                       hibernated_room_check_interval = CheckInterval,
                       hibernated_room_timeout = HibernatedTimeout},
    %% Hooks
    gen_hook:add_handlers(hooks(HostType)),
    %% Handler
    PacketHandler = mongoose_packet_handler:new(?MODULE, #{state => State}),
    case SubdomainPattern of
       {prefix, _} -> ok;
       _ -> ?LOG_WARNING(#{what => muc_host_pattern_missing,
                           host_type => HostType,
                           subdomain_pattern => SubdomainPattern,
                           text => <<"Only one MUC domain would work with this host type">>})
    end,
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern, PacketHandler),
    %% Loading
    case LoadPermRoomsAtStartup of
        false ->
            ?LOG_INFO(#{what => load_permanent_rooms_at_startup, skip => true,
                        text => <<"Skip loading permanent rooms at startup. "
                                  "Each room is loaded when someone access the room">>});
        true ->
            ?LOG_WARNING(#{what => load_permanent_rooms_at_startup_is_deprecated, skip => false,
                           text => <<"Loading permanent rooms at startup is deprecated. "
                                     "The option is ignored.">>})
    end,
    set_persistent_rooms_timer(State),
    {ok, State}.

set_persistent_rooms_timer(#muc_state{hibernated_room_check_interval = infinity}) ->
    ok;
set_persistent_rooms_timer(#muc_state{hibernated_room_check_interval = Timeout}) ->
    timer:send_after(Timeout, stop_hibernated_persistent_rooms).

handle_call(stop, _From, State) ->
    gen_hook:delete_handlers(hooks(State#muc_state.host_type)),
    {stop, normal, ok, State};
handle_call({create_instant, ServerHost, MucHost, Room, From, Nick, Opts},
            _From,
            #muc_state{host_type = HostType,
                       access = Access,
                       default_room_opts = DefOpts,
                       history_size = HistorySize,
                       room_shaper = RoomShaper,
                       http_auth_pool = HttpAuthPool} = State) ->
    ?LOG_DEBUG(#{what => muc_create_instant, room => Room, sub_host => MucHost}),
    NewOpts = case Opts of
                  default -> DefOpts;
                  _ -> Opts
              end,
    try
        {ok, Pid} = mod_muc_room:start_new(HostType,
                                           MucHost, ServerHost, Access,
                                           Room, HistorySize,
                                           RoomShaper, HttpAuthPool, From,
                                           Nick, [{instant, true}|NewOpts]),
        register_room_or_stop_if_duplicate(HostType, MucHost, Room, Pid),
        {reply, ok, State}
    catch Class:Reason:Stacktrace ->
              Err = #{what => muc_create_instant_failed,
                      server => ServerHost, host_type => HostType,
                      room => Room, from_jid => From,
                      class => Class, reason => Reason,
                      stacktrace => Stacktrace},
              ?LOG_ERROR(Err),
              {reply, {error, Err}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop_hibernated_persistent_rooms,
            #muc_state{host_type = HostType,
                       hibernated_room_timeout = Timeout} = State)
  when is_integer(Timeout) ->
    handle_stop_hibernated_persistent_rooms(HostType, Timeout),
    set_persistent_rooms_timer(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_stop_hibernated_persistent_rooms(HostType, Timeout) ->
    ?LOG_INFO(#{what => muc_stop_hibernated_persistent_rooms, host_type => HostType,
               text => <<"Closing hibernated persistent rooms">>}),
    try
        Supervisor = gen_mod:get_module_proc(HostType, ejabberd_mod_muc_sup),
        Now = os:timestamp(),
        [stop_if_hibernated(Pid, Now, Timeout * 1000) ||
         {undefined, Pid, worker, _} <- supervisor:which_children(Supervisor)]
    catch Error:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => stop_hibernated_persistent_rooms_failed,
                           error => Error, reason => Reason,
                           stacktrace => Stacktrace,
                           host_type => HostType})
    end.

stop_if_hibernated(Pid, Now, Timeout) ->
    stop_if_hibernated(Pid, Now, Timeout, erlang:process_info(Pid, current_function)).

stop_if_hibernated(Pid, Now, Timeout, {current_function, {erlang, hibernate, 3}}) ->
    {dictionary, Dictionary} = erlang:process_info(Pid, dictionary),
    LastHibernated = lists:keyfind(hibernated, 1, Dictionary),
    stop_if_hibernated_for_specified_time(Pid, Now, Timeout, LastHibernated),
    ok;
stop_if_hibernated(_, _, _, _) ->
    ok.

stop_if_hibernated_for_specified_time(_Pid, _, _, false) ->
    ok;
stop_if_hibernated_for_specified_time(Pid, Now, Timeout, {hibernated, LastHibernated}) ->
    TimeDiff = timer:now_diff(Now, LastHibernated),
    case TimeDiff >= Timeout of
        true ->
            Pid ! stop_persistent_room_process;
        _ ->
            ok
    end.

terminate(_Reason, #muc_state{host_type = HostType,
                              subdomain_pattern = SubdomainPattern}) ->
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec start_supervisor(host_type()) -> {error, _}
                                           | {ok, undefined | pid()}
                                           | {ok, undefined | pid(), _}.
start_supervisor(HostType) ->
    ChildSpec = sup_spec(HostType),
    ejabberd_sup:start_child(ChildSpec).

sup_spec(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ejabberd_mod_muc_sup),
    ejabberd_sup:template_supervisor_spec(Proc, mod_muc_room).

-spec stop_supervisor(jid:server()) -> ok | {error, Reason}
    when Reason :: not_found | restarting | running | simple_one_for_one.
stop_supervisor(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ejabberd_mod_muc_sup),
    ejabberd_sup:stop_child(Proc).

-spec process_packet(Acc :: mongoose_acc:t(),
                     From :: jid:jid(),
                     To :: jid:simple_jid() | jid:jid(),
                     El :: exml:element(),
                     #{state := state()}) -> mongoose_acc:t().
process_packet(Acc, From, To, El, #{state := State}) ->
    {AccessRoute, _, _, _} = State#muc_state.access,
    ServerHost = make_server_host(To, State),
    HostType = State#muc_state.host_type,
    case acl:match_rule(HostType, ServerHost, AccessRoute, From) of
        allow ->
            {Room, MucHost, _} = jid:to_lower(To),
            route_to_room(MucHost, Room, {From, To, Acc, El}, State),
            Acc;
        _ ->
            Lang = exml_query:attr(El, <<"xml:lang">>, <<>>),
            ErrText = <<"Access denied by service policy">>,
            ejabberd_router:route_error_reply(To, From, Acc,
                                              mongoose_xmpp_errors:forbidden(Lang, ErrText))
    end.


-spec route_to_room(jid:lserver(), room(), from_to_packet(), state()) -> ok | pid().
route_to_room(_MucHost, <<>>, {_, To, _Acc, _} = Routed, State) ->
    {_, _, Nick} = jid:to_lower(To),
    route_by_nick(Nick, Routed, State);
route_to_room(MucHost, Room, Routed, #muc_state{} = State) ->
    HostType = State#muc_state.host_type,
    case find_room_pid(HostType, MucHost, Room) of
        {error, not_found} ->
            case get_registered_room_or_route_error(MucHost, Room, Routed, State) of
                {ok, Pid} ->
                    route_to_online_room(Pid, Routed);
                {route_error, _ErrText} ->
                    ok
            end;
        {ok, Pid} ->
            route_to_online_room(Pid, Routed)
    end.

route_to_online_room(Pid, {From, To, Acc, Packet}) ->
    ?LOG_DEBUG(#{what => muc_route_to_online_room, room_pid => Pid, acc => Acc}),
    {_, _, Nick} = jid:to_lower(To),
    ok = mod_muc_room:route(Pid, From, Nick, Acc, Packet).

-spec get_registered_room_or_route_error(muc_host(), room(), from_to_packet(), state()) -> {ok, pid()} | {route_error, binary()}.
get_registered_room_or_route_error(MucHost, Room, {From, To, Acc, Packet}, State) ->
    case {Packet#xmlel.name, exml_query:attr(Packet, <<"type">>, <<>>)} of
        {<<"presence">>, <<>>} ->
            get_registered_room_or_route_error_from_presence(MucHost, Room, From, To, Acc, Packet, State);
        _ ->
            get_registered_room_or_route_error_from_packet(MucHost, Room, From, To, Acc, Packet, State)
    end.

get_registered_room_or_route_error_from_presence(MucHost, Room, From, To, Acc,
        Packet, #muc_state{host_type = HostType, access = Access} = State) ->
    {_, AccessCreate, _, _} = Access,
    ServerHost = make_server_host(To, State),
    case check_user_can_create_room(HostType, ServerHost, AccessCreate, From, Room) of
        ok ->
            #muc_state{history_size = HistorySize,
                       room_shaper = RoomShaper,
                       http_auth_pool = HttpAuthPool,
                       default_room_opts = DefRoomOpts} = State,
            {_, _, Nick} = jid:to_lower(To),
            ServerHost = make_server_host(To, State),
            Result = start_room(HostType, ServerHost, MucHost, Access, Room,
                                       HistorySize, RoomShaper, HttpAuthPool,
                                       From, Nick, DefRoomOpts, Acc),
            case Result of
                {ok, Pid} ->
                    register_room_or_stop_if_duplicate(HostType, MucHost, Room, Pid);
                {error, {failed_to_restore, Reason}} ->
                    %% Notify user about our backend module error
                    ?LOG_WARNING(#{what => muc_send_service_unavailable,
                                   text => <<"Failed to restore room">>,
                                   host_type => HostType,
                                   room => Room, sub_host => MucHost,
                                   reason => Reason, acc => Acc}),
                    Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
                    ErrText = <<"Service is temporary unavailable">>,
                    {Acc1, Err} = jlib:make_error_reply(
                            Acc, Packet, mongoose_xmpp_errors:service_unavailable(Lang, ErrText)),
                    ejabberd_router:route(To, From, Acc1, Err),
                    {route_error, ErrText}
            end;
        {error, Reason} ->
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            Policy = iolist_to_binary(io_lib:format("~p", [Reason])),
            ErrText = <<"Room creation is denied by service policy: ", Policy/binary>>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:not_allowed(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText}
    end.

get_registered_room_or_route_error_from_packet(MucHost, Room, From, To, Acc, Packet,
                                 #muc_state{host_type = HostType,
                                            access = Access} = State) ->
    ServerHost = make_server_host(To, State),
    case restore_room(HostType, MucHost, Room) of
        {error, room_not_found} ->
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            ErrText = <<"Conference room does not exist">>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:item_not_found(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText};
        {error, Reason} ->
            ?LOG_WARNING(#{what => muc_send_service_unavailable,
                           room => Room, host_type => HostType, sub_host => MucHost,
                           reason => Reason, acc => Acc}),
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            ErrText = <<"Service is temporary unavailable">>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:service_unavailable(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText};
        {ok, Opts} ->
            ?LOG_DEBUG(#{what => muc_restore_room, room => Room, room_opts => Opts}),
            #muc_state{history_size = HistorySize,
                       room_shaper = RoomShaper,
                       http_auth_pool = HttpAuthPool} = State,
            {ok, Pid} = mod_muc_room:start_restored(HostType,
                                           MucHost, ServerHost, Access,
                                           Room, HistorySize,
                                           RoomShaper, HttpAuthPool, Opts),
            register_room_or_stop_if_duplicate(HostType, MucHost, Room, Pid)
    end.

-spec route_by_nick(room(), from_to_packet(), state()) -> 'ok' | pid().
route_by_nick(<<>>, {_, _, _, Packet} = Routed, State) ->
    #xmlel{name = Name} = Packet,
    route_by_type(Name, Routed, State);
route_by_nick(_Nick, {From, To, Acc, Packet}, _State) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            Acc;
        <<"result">> ->
            Acc;
        _ ->
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:item_not_found()),
            ejabberd_router:route(To, From, Acc1, Err)
    end.

-spec route_by_type(binary(), from_to_packet(), state()) -> ok | pid().
route_by_type(<<"iq">>, {From, To, Acc, Packet}, #muc_state{} = State) ->
    HostType = State#muc_state.host_type,
    MucHost = To#jid.lserver,
    case jlib:iq_query_info(Packet) of
        #iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS, lang = Lang} = IQ ->
            IdentityXML = mongoose_disco:identities_to_xml([identity(Lang)]),
            FeatureXML =  mongoose_disco:get_muc_features(HostType, From, To, <<>>, Lang,
                                                          features()),
            InfoXML = mongoose_disco:get_info(HostType, ?MODULE, <<>>, Lang),
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = #{<<"xmlns">> => XMLNS},
                                         children = IdentityXML ++ FeatureXML ++ InfoXML}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ ->
            proc_lib:spawn(fun() -> process_iq_disco_items(MucHost, From, To, IQ) end);
        #iq{type = get, xmlns = ?NS_REGISTER = XMLNS, lang = Lang} = IQ ->
            Result = iq_get_register_info(HostType, MucHost, From, Lang),
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = #{<<"xmlns">> => XMLNS},
                                         children = Result}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = set,
            xmlns = ?NS_REGISTER = XMLNS,
            lang = Lang,
            sub_el = SubEl} = IQ ->
            case process_iq_register_set(HostType, MucHost, From, SubEl, Lang) of
                {result, IQRes} ->
                    Res = IQ#iq{type = result,
                                sub_el = [#xmlel{name = <<"query">>,
                                                 attrs = #{<<"xmlns">> => XMLNS},
                                                 children = IQRes}]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
                {error, Error} ->
                    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, Error),
                    ejabberd_router:route(To, From, Acc1, Err)
            end;
        #iq{type = get, xmlns = ?NS_VCARD = XMLNS, lang = Lang} = IQ ->
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"vCard">>,
                                         attrs = #{<<"xmlns">> => XMLNS},
                                         children = iq_get_vcard(Lang)}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_MUC_UNIQUE} = IQ ->
           Res = IQ#iq{type = result,
                       sub_el = [#xmlel{name = <<"unique">>,
                                        attrs = #{<<"xmlns">> => ?NS_MUC_UNIQUE},
                                        children = [iq_get_unique(From)]}]},
           ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{} ->
            ?LOG_INFO(#{what => muc_ignore_unknown_iq, acc => Acc}),
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet,
                mongoose_xmpp_errors:feature_not_implemented(<<"en">>, <<"From mod_muc">>)),
            ejabberd_router:route(To, From, Acc1, Err);
        Other ->
            ?LOG_INFO(#{what => muc_failed_to_parse_iq, acc => Acc, reason => Other}),
            ok
    end;
route_by_type(<<"message">>, {From, To, Acc, Packet},
              #muc_state{host_type = HostType,
                         access = {_, _, AccessAdmin, _}} = State) ->
    MucHost = To#jid.lserver,
    ServerHost = make_server_host(To, State),
    case exml_query:attr(Packet, <<"type">>) of
        <<"error">> ->
            ok;
        _ ->
            case acl:match_rule(HostType, ServerHost, AccessAdmin, From) of
                allow ->
                    Msg = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
                    broadcast_service_message(MucHost, Msg);
                _ ->
                    Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
                    ErrTxt = <<"Only service administrators are allowed to send service messages">>,
                    Err = mongoose_xmpp_errors:forbidden(Lang, ErrTxt),
                    {Acc1, ErrorReply} = jlib:make_error_reply(Acc, Packet, Err),
                    ejabberd_router:route(To, From, Acc1, ErrorReply)
            end
    end;
route_by_type(<<"presence">>, _Routed, _State) ->
    ok.

-spec check_user_can_create_room(host_type(), jid:lserver(),
        allow | atom(), jid:jid(), room()) -> ok | {error, term()}.
check_user_can_create_room(HostType, ServerHost, AccessCreate, From, RoomID) ->
    case acl:match_rule(HostType, ServerHost, AccessCreate, From) of
        allow ->
            MaxLen = gen_mod:get_module_opt(HostType, mod_muc, max_room_id),
            case (size(RoomID) =< MaxLen) of
                true -> ok;
                false -> {error, room_id_too_long}
            end;
        _ ->
            ?LOG_WARNING(#{what => check_user_can_create_room_failed,
                           host_type => HostType,
                           server => ServerHost,
                           access_create => AccessCreate,
                           from_jid => From,
                           room_id => RoomID}),
            {error, no_matching_acl_rule}
    end.

-spec start_room(HostType :: host_type(), ServerHost :: jid:lserver(),
        MucHost :: muc_host(), Access :: access(), room(),
        HistorySize :: undefined | integer(), RoomShaper :: mongoose_shaper:shaper(),
        HttpAuthPool :: none | mongoose_http_client:pool(), From :: jid:jid(), nick(),
        DefRoomOpts :: undefined | [any()], Acc :: mongoose_acc:t())
            -> {error, {failed_to_restore, Reason :: term()}} | {ok, pid()}.
start_room(HostType, ServerHost, MucHost, Access, Room,
               HistorySize, RoomShaper, HttpAuthPool, From,
               Nick, DefRoomOpts, Acc) ->
    case mod_muc_backend:restore_room(HostType, MucHost, Room) of
        {error, room_not_found} ->
            ?LOG_DEBUG(#{what => muc_start_new_room, acc => Acc,
                         room => Room, host_type => HostType, sub_host => MucHost}),
            mod_muc_room:start_new(HostType,
                               MucHost, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, HttpAuthPool, From,
                               Nick, DefRoomOpts);
        {error, Reason} ->
            {error, {failed_to_restore, Reason}};
        {ok, Opts} ->
            ?LOG_DEBUG(#{what => muc_restore_room, acc => Acc, room => Room,
                         host_type => HostType, sub_host => MucHost, room_opts => Opts}),
            mod_muc_room:start_restored(HostType,
                               MucHost, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, HttpAuthPool, Opts)
    end.

register_room_or_stop_if_duplicate(HostType, MucHost, Room, Pid) ->
    case register_room(HostType, MucHost, Room, Pid) of
        ok ->
            {ok, Pid};
        {exists, OldPid} ->
            mod_muc_room:stop(Pid),
            {ok, OldPid};
        {error, Reason} ->
            error({failed_to_register, MucHost, Room, Pid, Reason})
    end.

-spec register_room(HostType :: host_type(), jid:server(), room(),
                    pid()) -> ok | {exists, pid()} | {error, term()}.
register_room(HostType, MucHost, Room, Pid) ->
    mod_muc_online_backend:register_room(HostType, MucHost, Room, Pid).

-spec room_jid_to_pid(RoomJID :: jid:jid()) -> {ok, pid()} | {error, not_found}.
room_jid_to_pid(#jid{luser = Room, lserver = MucHost}) ->
    case mongoose_domain_api:get_subdomain_host_type(MucHost) of
        {ok, HostType} ->
            find_room_pid(HostType, MucHost, Room);
        _ ->
            {error, not_found}
    end.

find_room_pid(HostType, MucHost, Room) ->
    mod_muc_online_backend:find_room_pid(HostType, MucHost, Room).

-spec default_host() -> mongoose_subdomain_utils:subdomain_pattern().
default_host() ->
    mongoose_subdomain_utils:make_subdomain_pattern(<<"conference.@HOST@">>).

identity(Lang) ->
    #{category => <<"conference">>,
      type => <<"text">>,
      name => service_translations:do(Lang, <<"Chatrooms">>)}.

features() ->
    [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_MUC, ?NS_MUC_UNIQUE, ?NS_REGISTER, ?NS_RSM, ?NS_VCARD, ?NS_CONFERENCE].

%% Disco for rooms
-spec iq_disco_items(muc_host(), jid:jid(), ejabberd:lang(),
                     Rsm :: none | jlib:rsm_in()) -> any().
iq_disco_items(MucHost, From, Lang, none) ->
    AllRooms = get_vh_rooms(MucHost) ++ get_persistent_vh_rooms(MucHost),
    Rooms = lists:ukeysort(1, lists:map(fun record_to_simple/1, AllRooms)),
    BareRooms = lists:filtermap(fun(Room) -> room_to_item(Room, MucHost, From, Lang) end, Rooms),
    lists:ukeysort(3, BareRooms);
iq_disco_items(MucHost, From, Lang, Rsm) ->
    {Rooms, RsmO} = get_vh_rooms(MucHost, Rsm),
    RsmOut = jlib:rsm_encode(RsmO),
    lists:filtermap(fun(Room) -> room_to_item(Room, MucHost, From, Lang) end, Rooms) ++ RsmOut.

room_to_item({{Name, _}, Pid}, MucHost, From, Lang) when is_pid(Pid) ->
     case catch gen_fsm_compat:sync_send_all_state_event(
                  Pid, {get_disco_item, From, Lang}, 100) of
         {item, Desc} ->
             {true,
              #xmlel{name = <<"item">>,
                     attrs = #{<<"jid">> => jid:to_binary({Name, MucHost, <<>>}),
                               <<"name">> => Desc}}};
         _ ->
             false
     end;
room_to_item({{Name, _}, _}, MucHost, _, _) ->
     {true,
     #xmlel{name = <<"item">>,
            attrs = #{<<"jid">> => jid:to_binary({Name, MucHost, <<>>}),
                      <<"name">> => Name}}
     }.
record_to_simple(#muc_online_room{name_host = Room, pid = Pid}) ->
    {Room, Pid};
record_to_simple(#muc_room{name_host = Room, opts = Opts}) ->
    {Room, Opts}.

-spec get_vh_rooms(muc_host(), jlib:rsm_in()) -> {list(), jlib:rsm_out()}.
get_vh_rooms(MucHost, #rsm_in{max=Max, direction=Direction, id=I, index=Index}) ->
    NonUndefMax = case Max of
        undefined -> 134217728;
        _ -> Max
    end,
    Rooms = get_vh_rooms(MucHost) ++ get_persistent_vh_rooms(MucHost),
    BareSortedRooms = lists:ukeysort(1, lists:map(fun record_to_simple/1, Rooms)),
    Count = erlang:length(BareSortedRooms),
    L2 = case {Index, Direction} of
        {undefined, undefined} ->
            lists:sublist(BareSortedRooms, 1, NonUndefMax);
        {undefined, aft} ->
            lists:sublist(
                lists:dropwhile(
                    fun({{Id, _}, _}) -> Id =< I end,
                    BareSortedRooms),
                1,
                NonUndefMax);
        {undefined,before} when I == <<>> ->
            lists:reverse(
                lists:sublist(
                    lists:reverse(BareSortedRooms), 1, NonUndefMax));
        {undefined, before} ->
            L = lists:takewhile(
                fun({{Id, _}, _}) -> Id < I end,
                BareSortedRooms),
            lists:reverse(
                lists:sublist(
                    lists:reverse(L), 1, NonUndefMax));
        {Index, _} when Index < 0 orelse Index > Count -> [];
        {Index, _} ->
            lists:sublist(BareSortedRooms, Index + 1, NonUndefMax);
         Input ->
             ?LOG_ERROR(#{what => muc_get_rooms_with_pagination_failed,
                          text => <<"Unexpected result in get_rooms_with_pagination">>,
                          reason => Input}),
             []
         end,
    case L2 of
        [] ->
            {L2, #rsm_out{count=Count}};
        _ ->
            H = hd(L2),
            NewIndex = get_room_pos(H, BareSortedRooms),

            {{F, _},_} = H,
            {{Last, _}, _} = lists:last(L2),
            {L2, #rsm_out{first=F, last=Last, count=Count, index=NewIndex}}
    end.

%% @doc Return the position of desired room in the list of rooms.
%% The room must exist in the list. The count starts in 0.
-spec get_room_pos({{binary(), any()}, any()}, [{{binary(), any()}, any}]) -> non_neg_integer().
get_room_pos(Desired, Rooms) ->
    get_room_pos(Desired, Rooms, 0).
get_room_pos({{NameHost, _}, _}, [{{NameHost, _}, _} | _], HeadPosition) ->
    HeadPosition;
get_room_pos(Desired, [_ | Rooms], HeadPosition) ->
    get_room_pos(Desired, Rooms, HeadPosition + 1).

%% @doc Get a pseudo unique Room Name. The Room Name is generated as a hash of
%%      the requester JID, the local time and a random salt.
%%
%%      `<<"pseudo">>' because we don't verify that there is not a room
%%      with the returned Name already created, nor mark the generated Name
%%      as `<<"already used">>'.  But in practice, it is unique enough. See
%%      http://xmpp.org/extensions/xep-0045.html#createroom-unique
-spec iq_get_unique(jid:jid()) -> exml:cdata().
iq_get_unique(From) ->
    Raw = [From, erlang:unique_integer(), mongoose_bin:gen_from_crypto()],
    #xmlcdata{content = mongoose_bin:encode_crypto(term_to_binary(Raw))}.

-spec iq_get_register_info(host_type(), jid:server(),
        jid:simple_jid() | jid:jid(), ejabberd:lang())
            -> [exml:element(), ...].
iq_get_register_info(HostType, MucHost, From, Lang) ->
    {Nick, Registered} =
        case catch get_nick(HostType, MucHost, From) of
            {'EXIT', _Reason} ->
                {<<>>, []};
            {error, _} ->
                {<<>>, []};
            {ok, N} ->
                {N, [#xmlel{name = <<"registered">>}]}
        end,
    ClientReqText = service_translations:do(
                      Lang, <<"You need a client that supports x:data to register the nickname">>),
    ClientReqEl = #xmlel{name = <<"instructions">>,
                         children = [#xmlcdata{content = ClientReqText}]},
    EnterNicknameText = service_translations:do(Lang, <<"Enter nickname you want to register">>),
    TitleText = <<(service_translations:do(Lang, <<"Nickname Registration at ">>))/binary,
                  MucHost/binary>>,
    NickField = #{type => <<"text-single">>,
                  label => service_translations:do(Lang, <<"Nickname">>),
                  var => <<"nick">>,
                  values => [Nick]},
    Registered ++ [ClientReqEl, mongoose_data_forms:form(#{title => TitleText,
                                                           instructions => EnterNicknameText,
                                                           fields => [NickField]})].

-spec iq_set_register_info(host_type(), jid:server(),
        jid:simple_jid() | jid:jid(), nick(), ejabberd:lang())
            -> {'error', exml:element()} | {'result', []}.
iq_set_register_info(HostType, MucHost, From, Nick, Lang) ->
    case set_nick(HostType, MucHost, From, Nick) of
        ok ->
            {result, []};
        {error, conflict} ->
            ErrText = <<"That nickname is registered by another person">>,
            {error, mongoose_xmpp_errors:conflict(Lang, ErrText)};
        {error, should_not_be_empty} ->
            ErrText = <<"You must fill in field \"Nickname\" in the form">>,
            {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)};
        {error, ErrorReason} ->
            ?LOG_ERROR(#{what => muc_iq_set_register_info_failed,
                         host_type => HostType, sub_host => MucHost,
                         from_jid => jid:to_binary(From), nick => Nick,
                         reason => ErrorReason}),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

-spec iq_set_unregister_info(host_type(), jid:server(),
        jid:simple_jid() | jid:jid(), ejabberd:lang())
            -> {'error', exml:element()} | {'result', []}.
iq_set_unregister_info(HostType, MucHost, From, _Lang) ->
    case unset_nick(HostType, MucHost, From) of
        ok ->
            {result, []};
        {error, ErrorReason} ->
            ?LOG_ERROR(#{what => muc_iq_set_unregister_info_failed,
                         host_type => HostType, sub_host => MucHost,
                         from_jid => jid:to_binary(From), reason => ErrorReason}),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

-spec process_iq_register_set(host_type(), jid:server(),
                              jid:jid(), exml:element(), ejabberd:lang())
            -> {'error', exml:element()} | {'result', []}.
process_iq_register_set(HostType, MucHost, From, SubEl, Lang) ->
    case exml_query:subelement(SubEl, <<"remove">>) of
        undefined ->
            case mongoose_data_forms:find_and_parse_form(SubEl) of
                #{type := <<"cancel">>} ->
                    {result, []};
                #{type := <<"submit">>, kvs := KVs} ->
                    process_register(HostType, MucHost, From, Lang, KVs);
                {error, Msg} ->
                    {error, mongoose_xmpp_errors:bad_request(Lang, Msg)};
                _ ->
                    {error, mongoose_xmpp_errors:bad_request(Lang, <<"Invalid form type">>)}
            end;
        _ ->
            iq_set_unregister_info(HostType, MucHost, From, Lang)
    end.

-spec process_register(HostType :: host_type(), MucHost :: jid:server(),
                       From :: jid:jid(), Lang :: ejabberd:lang(),
                       KVs :: mongoose_data_forms:kv_map()) ->
    {error, exml:element()} | {result, []}.
process_register(HostType, MucHost, From, Lang, #{<<"nick">> := [Nick]}) ->
    iq_set_register_info(HostType, MucHost, From, Nick, Lang);
process_register(_HostType, _MucHost, _From, Lang, #{}) ->
    ErrText = <<"You must fill in field \"Nickname\" in the form">>,
    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)}.

-spec iq_get_vcard(ejabberd:lang()) -> [exml:element(), ...].
iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_muc">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?MONGOOSE_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content =
                                  <<(service_translations:do(Lang, <<"ejabberd MUC module">>))/binary,
                                    "\nCopyright (c) 2003-2011 ProcessOne">>}]}].

-spec broadcast_service_message(muc_host(), binary() | string()) -> ok.
broadcast_service_message(MucHost, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
              gen_fsm_compat:send_all_state_event(
                Pid, {service_message, Msg})
      end, get_vh_rooms(MucHost)).

-spec get_vh_rooms(muc_host()) -> [muc_online_room()].
get_vh_rooms(MucHost) ->
    {ok, HostType} = mongoose_domain_api:get_subdomain_host_type(MucHost),
    mod_muc_online_backend:get_online_rooms(HostType, MucHost).

-spec get_persistent_vh_rooms(muc_host()) -> [muc_room()].
get_persistent_vh_rooms(MucHost) ->
    {ok, HostType} = mongoose_domain_api:get_subdomain_host_type(MucHost),
    case mod_muc_backend:get_rooms(HostType, MucHost) of
        {ok, List} ->
            List;
        {error, _} ->
            []
    end.

-spec node_cleanup(host_type(), node()) -> ok.
node_cleanup(HostType, Node) ->
    mod_muc_online_backend:node_cleanup(HostType, Node).

%%====================================================================
%% Hooks handlers
%%====================================================================

-spec is_muc_room_owner(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: #{room := jid:jid(), user := jid:jid()},
    Extra :: gen_hook:extra().
is_muc_room_owner(true, _, _) ->
    {ok, true};
is_muc_room_owner(_, #{room := Room, user := User}, _) ->
    Result = mod_muc_room:is_room_owner(Room, User) =:= {ok, true},
    {ok, Result}.

-spec can_access_room(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: #{room := jid:jid(), user := jid:jid()},
    Extra :: gen_hook:extra().
can_access_room(true, _, _) ->
    {ok, true};
can_access_room(_, #{room := Room, user := User}, _) ->
    Result = case mod_muc_room:can_access_room(Room, User) of
        {error, _} -> false;
        {ok, CanAccess} -> CanAccess
    end,
    {ok, Result}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_hooks:simple_acc(),
    Params :: #{domain := jid:lserver()},
    Extra :: gen_hook:extra().
 remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    MUCHost = server_host_to_muc_host(HostType, Domain),
    mod_muc_backend:remove_domain(HostType, MUCHost, Domain),
    {ok, Acc}.

-spec acc_room_affiliations(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{room := jid:jid()},
    Extra :: gen_hook:extra().
acc_room_affiliations(Acc, #{room := Room}, _) ->
    NewAcc = case mongoose_acc:get(?MODULE, {affiliations, Room}, {error, not_found}, Acc) of
        {error, _} ->
            case mod_muc_room:get_room_users(Room) of
                {error, not_found} ->
                    Acc;
                {ok, _Affs} = Res ->
                    mongoose_acc:set(?MODULE, {affiliations, Room}, Res, Acc)
            end;
        _Affs ->
            Acc
    end,
    {ok, NewAcc}.

-spec can_access_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: boolean(),
    Params :: #{room := jid:jid(), user := jid:jid()},
    Extra :: gen_hook:extra().
can_access_identity(true, _, _) ->
    {ok, true};
can_access_identity(_, #{room := Room, user := User}, _) ->
    Result = case mod_muc_room:can_access_identity(Room, User) of
        {error, _} -> false;
        {ok, CanAccess} -> CanAccess
    end,
    {ok, Result}.

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_items(Acc = #{host_type := HostType,
                          to_jid := #jid{lserver = ServerHost},
                          node := <<>>}, _, _) ->
    MUCHost = server_host_to_muc_host(HostType, ServerHost),
    Items = [#{jid => MUCHost, node => ?NS_MUC}],
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_local_items(Acc, _, _) ->
    {ok, Acc}.

-spec node_cleanup_for_host_type(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
node_cleanup_for_host_type(Acc, #{node := Node}, #{host_type := HostType}) ->
    node_cleanup(HostType, Node),
    {ok, Acc}.

-spec count_rooms(mongooseim:host_type()) -> mongoose_instrument:measurements().
count_rooms(HostType) ->
    try all_room_pids(HostType) of
        AllRooms ->
            InitialCounts = #{online => 0, hibernated => 0},
            lists:foldl(fun count_rooms/2, InitialCounts, AllRooms)
    catch exit:{noproc, _} ->
            #{}
    end.

all_room_pids(HostType) ->
    Supervisor = gen_mod:get_module_proc(HostType, ejabberd_mod_muc_sup),
    [Pid || {undefined, Pid, worker, _} <- supervisor:which_children(Supervisor)].

count_rooms(Pid, Counts = #{online := Online, hibernated := Hibernated}) ->
    case erlang:process_info(Pid, current_function) of
        {current_function, {erlang, hibernate, _}} ->
            #{online => Online + 1, hibernated => Hibernated + 1};
        _ ->
            Counts#{online := Online + 1}
    end.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend, online_backend]).

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{is_muc_room_owner, HostType, fun ?MODULE:is_muc_room_owner/3, #{}, 50},
     {can_access_room, HostType, fun ?MODULE:can_access_room/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
     {acc_room_affiliations, HostType, fun ?MODULE:acc_room_affiliations/3, #{}, 50},
     {can_access_identity, HostType, fun ?MODULE:can_access_identity/3, #{}, 50},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 250},
     {node_cleanup_for_host_type, HostType, fun ?MODULE:node_cleanup_for_host_type/3, #{}, 50}].

-spec instrumentation(host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_muc_deep_hibernations, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_muc_process_recreations, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_muc_hibernations, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_muc_rooms, #{host_type => HostType},
      #{probe => #{module => ?MODULE}, metrics => #{online => gauge, hibernated => gauge}}}].

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
          mongoose_instrument:measurements().
probe(mod_muc_rooms, #{host_type := HostType}) ->
    count_rooms(HostType).

subdomain_pattern(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, host).

server_host_to_muc_host(HostType, ServerHost) ->
    mongoose_subdomain_utils:get_fqdn(subdomain_pattern(HostType), ServerHost).

make_server_host(To, #muc_state{host_type = HostType,
                                subdomain_pattern = SubdomainPattern}) ->
    case SubdomainPattern of
        {prefix, _} ->
            mod_muc_light_utils:room_jid_to_server_host(To);
        {fqdn, _} ->
            HostType
    end.
