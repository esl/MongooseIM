%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (XEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_last).

-author('alexey@process-one.net').

-xep([{xep, 12}, {version, "2.0"}]).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% Gen_mod callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% IQ and hook handlers
-export([user_receive_iq/3,
         process_local_iq/5,
         process_sm_iq/5,
         remove_user/3,
         on_presence_update/3,
         session_cleanup/3,
         remove_domain/3]).

%% API
-export([store_last_info/5,
         get_last_info/3,
         count_active_users/3]).

-export([config_metrics/1]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-include("jlib.hrl").

%% ------------------------------------------------------------------
%% Backend callbacks

-export_type([timestamp/0, status/0]).

-type timestamp() :: non_neg_integer().
-type status() :: binary().

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc} = Opts) ->
    mod_last_backend:init(HostType, Opts),
    [gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_LAST, Component, Fn, #{}, IQDisc) ||
        {Component, Fn} <- iq_handlers()],
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_LAST, Component) ||
        {Component, _Fn} <- iq_handlers()],
    gen_hook:delete_handlers(hooks(HostType)).

iq_handlers() ->
    [{ejabberd_local, fun ?MODULE:process_local_iq/5},
     {ejabberd_sm, fun ?MODULE:process_sm_iq/5}].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {anonymous_purge_hook, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {unset_presence_hook, HostType, fun ?MODULE:on_presence_update/3, #{}, 50},
     {session_cleanup, HostType, fun ?MODULE:session_cleanup/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50}
    | c2s_hooks(HostType) ].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [
     {user_receive_iq, HostType, fun ?MODULE:user_receive_iq/3, #{}, 50}
    ].

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_last}}
                },
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"backend">> => mnesia
                   }
      }.

supported_features() -> [dynamic_domains].

%%%
%%% Uptime of ejabberd node
%%%
-spec process_local_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map())
        -> {mongoose_acc:t(), jlib:iq()}.
process_local_iq(Acc, _From, _To, #iq{type = Type, sub_el = SubEl} = IQ, _Extra) ->
    case Type of
        set ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
        get ->
            Sec = get_node_uptime(),
            {Acc, IQ#iq{type = result,
                sub_el =
                [#xmlel{name = <<"query">>,
                    attrs =
                    [{<<"xmlns">>, ?NS_LAST},
                        {<<"seconds">>,
                            integer_to_binary(Sec)}],
                    children = []}]}}
    end.

-spec get_node_uptime() -> non_neg_integer().
get_node_uptime() ->
    case mongoose_config:lookup_opt(node_start) of
        {ok, {node_start, Seconds}} ->
            erlang:system_time(second) - Seconds;
        {error, not_found} ->
            trunc(element(1, erlang:statistics(wall_clock))/1000)
    end.

%%%
%%% Serve queries about user last online
%%%
-spec process_sm_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq(Acc, From, To, #iq{type = get, sub_el = SubEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    case can_respond(HostType, From, To) of
        true ->
            UserListRecord = mongoose_hooks:privacy_get_user_list(HostType, To),
            {Res, Acc1} = mongoose_privacy:privacy_check_packet(Acc, To, UserListRecord, To, From, out),
            {Acc1, make_response(HostType, IQ, SubEl, To, Res)};
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]}}
    end.

can_respond(HostType, From, To) ->
    {Subscription, _Groups} = mongoose_hooks:roster_get_jid_info(HostType, To, From),
    MutualSubscription = Subscription =:= both,
    RequesterSubscribedToTarget = Subscription =:= from,
    QueryingSameUsersLast = jid:are_bare_equal(From, To),
    MutualSubscription or RequesterSubscribedToTarget or QueryingSameUsersLast.

-spec make_response(mongooseim:host_type(), jlib:iq(), SubEl :: 'undefined' | [exml:element()],
                    jid:jid(), allow | deny) -> jlib:iq().
make_response(_HostType, IQ, SubEl, _, deny) ->
    IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]};
make_response(HostType, IQ, SubEl, JID, allow) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    case ejabberd_sm:get_user_resources(JID) of
        [] ->
            case get_last(HostType, LUser, LServer) of
                {error, _Reason} ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]};
                not_found ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]};
                {ok, TimeStamp, Status} ->
                    TimeStamp2 = erlang:system_time(second),
                    Sec = TimeStamp2 - TimeStamp,
                    IQ#iq{type = result,
                        sub_el =
                        [#xmlel{name = <<"query">>,
                            attrs =
                            [{<<"xmlns">>, ?NS_LAST},
                                {<<"seconds">>,
                                    integer_to_binary(Sec)}],
                            children = [{xmlcdata, Status}]}]}
            end;
        _ ->
            IQ#iq{type = result,
                sub_el =
                [#xmlel{name = <<"query">>,
                    attrs =
                    [{<<"xmlns">>, ?NS_LAST},
                        {<<"seconds">>, <<"0">>}],
                    children = []}]}
    end.

-spec get_last_info(mongooseim:host_type(), jid:luser(), jid:lserver())
        -> 'not_found' | {'ok', timestamp(), status()}.
get_last_info(HostType, LUser, LServer) ->
    case get_last(HostType, LUser, LServer) of
        {error, _Reason} -> not_found;
        Res -> Res
    end.

%%%
%%% Hook handlers
%%%

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    R = mod_last_backend:remove_user(HostType, LUser, LServer),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, LUser, LServer}),
    {ok, Acc}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_hooks:simple_acc(),
    Params :: #{domain := jid:lserver()},
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_last_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

-spec user_receive_iq(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_receive_iq(Acc, _Params, _Extra) ->
    case mongoose_iq:info(Acc) of
        {#iq{type = get, xmlns = ?NS_LAST}, Acc1} ->
            maybe_forward_last(Acc1);
        {_, Acc1} ->
            {ok, Acc1}
    end.

-spec maybe_forward_last(mongoose_acc:t()) -> mongoose_c2s_hooks:result().
maybe_forward_last(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    {From, To, _} = mongoose_acc:packet(Acc),
    case can_respond(HostType, From, To) of
        true ->
            {ok, Acc};
        false ->
            {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:forbidden()),
            ejabberd_router:route(To, From, Acc1, Err),
            {stop, Acc}
    end.

-spec on_presence_update(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{jid := jid:jid(), status := status()},
    Extra :: gen_hook:extra().
on_presence_update(Acc, #{jid := #jid{luser = LUser, lserver = LServer}, status := Status}, _) ->
    {ok, store_last_info(Acc, LUser, LServer, Status)}.

-spec session_cleanup(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
session_cleanup(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, _) ->
    {ok, store_last_info(Acc, LUser, LServer, <<>>)}.

-spec store_last_info(mongoose_acc:t(), jid:luser(), jid:lserver(), status()) -> mongoose_acc:t().
store_last_info(Acc, LUser, LServer, Status) ->
    HostType = mongoose_acc:host_type(Acc),
    TimeStamp = erlang:system_time(second),
    store_last_info(HostType, LUser, LServer, TimeStamp, Status),
    Acc.

-spec store_last_info(mongooseim:host_type(), jid:luser(), jid:lserver(),
                      timestamp(), status()) -> ok.
store_last_info(HostType, LUser, LServer, TimeStamp, Status) ->
    case mod_last_backend:set_last_info(HostType, LUser, LServer, TimeStamp, Status) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => set_last_info_failed,
                         text => <<"Unexpected error while storing mod_last information">>,
                         user => LUser, server => LServer,
                         timestamp => TimeStamp, status => Status,
                         reason => Reason});
        ok ->
            ok
    end.

-spec get_last(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    {ok, timestamp(), status()} | {error, term()} | not_found.
get_last(HostType, LUser, LServer) ->
    mod_last_backend:get_last(HostType, LUser, LServer).

-spec count_active_users(mongooseim:host_type(), jid:lserver(), timestamp()) -> non_neg_integer().
count_active_users(HostType, LServer, Timestamp) ->
    mod_last_backend:count_active_users(HostType, LServer, Timestamp).

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
