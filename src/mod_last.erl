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
         config_spec/0,
         supported_features/0]).

%% IQ and hook handlers
-export([process_local_iq/5,
         process_sm_iq/5,
         remove_user/3,
         on_presence_update/5,
         session_cleanup/5,
         remove_domain/3,
         remove_unused_backend_opts/1]).

%% API
-export([store_last_info/5,
         get_last_info/3,
         count_active_users/3]).

-export([config_metrics/1]).

-ignore_xref([
    behaviour_info/1, on_presence_update/5, process_local_iq/4,
    process_sm_iq/4, remove_user/3, session_cleanup/5, remove_domain/3
]).

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
    ejabberd_hooks:add(hooks(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_LAST, Component) ||
        {Component, _Fn} <- iq_handlers()],
    ok.

iq_handlers() ->
    [{ejabberd_local, fun ?MODULE:process_local_iq/5},
     {ejabberd_sm, fun ?MODULE:process_sm_iq/5}].

hooks(HostType) ->
    [{remove_user, HostType, ?MODULE, remove_user, 50},
     {anonymous_purge_hook, HostType, ?MODULE, remove_user, 50},
     {unset_presence_hook, HostType, ?MODULE, on_presence_update, 50},
     {session_cleanup, HostType, ?MODULE, session_cleanup, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50}].

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_last}},
                 <<"riak">> => riak_config_spec()
                },
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"backend">> => mnesia
                   },
       process = fun ?MODULE:remove_unused_backend_opts/1
      }.

remove_unused_backend_opts(Opts = #{backend := riak}) -> Opts;
remove_unused_backend_opts(Opts) -> maps:remove(riak, Opts).

riak_config_spec() ->
    #section{items = #{<<"bucket_type">> => #option{type = binary,
                                                    validate = non_empty}
                      },
             defaults = #{<<"bucket_type">> => <<"last">>},
             include = always
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
    {Subscription, _Groups} = mongoose_hooks:roster_get_jid_info(HostType, To, From),
    MutualSubscription = Subscription == both,
    RequesterSubscribedToTarget = Subscription == from,
    QueryingSameUsersLast = (From#jid.luser == To#jid.luser) and
                            (From#jid.lserver == To#jid.lserver),
    case MutualSubscription or RequesterSubscribedToTarget or QueryingSameUsersLast of
        true ->
            UserListRecord = mongoose_hooks:privacy_get_user_list(HostType, To),
            {Acc1, Res} = mongoose_privacy:privacy_check_packet(Acc, To,
                                                                UserListRecord, To, From,
                                                                out),
            {Acc1, make_response(HostType, IQ, SubEl, To, Res)};
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]}}
    end.

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

-spec remove_user(mongoose_acc:t(), jid:user(), jid:server()) -> mongoose_acc:t().
remove_user(Acc, User, Server) ->
    HostType = mongoose_acc:host_type(Acc),
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    R = mod_last_backend:remove_user(HostType, LUser, LServer),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(), mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mod_last_backend:remove_domain(HostType, Domain),
    Acc.

-spec on_presence_update(mongoose_acc:t(), jid:luser(), jid:lserver(), jid:lresource(), status()) ->
          mongoose_acc:t().
on_presence_update(Acc, LUser, LServer, _Resource, Status) ->
    store_last_info(Acc, LUser, LServer, Status).

-spec session_cleanup(mongoose_acc:t(), jid:luser(), jid:lserver(), jid:lresource(),
                      ejabberd_sm:sid()) ->
          mongoose_acc:t().
session_cleanup(Acc, LUser, LServer, _LResource, _SID) ->
    store_last_info(Acc, LUser, LServer, <<>>).

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
