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

-export([
         start/2,
         stop/1,
         process_local_iq/4,
         process_sm_iq/4,
         on_presence_update/5,
         store_last_info/4,
         get_last_info/2,
         count_active_users/2,
         remove_user/3,
         session_cleanup/5
        ]).

-include("mongoose.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").
-include("mod_last.hrl").

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host    :: jid:server(),
    Opts    :: list().

-callback get_last(LUser, LServer) -> Result when
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    Reason  :: term(),
    Result  :: {ok, non_neg_integer(), binary()} | {error, Reason} | not_found.

-callback count_active_users(LServer, Timestamp) -> Result when
    LServer :: jid:lserver(),
    Timestamp :: non_neg_integer(),
    Result :: non_neg_integer().

-callback set_last_info(LUser, LServer, Timestamp, Status) -> Result when
    LUser   :: jid:luser(),
    LServer :: jid:lserver(),
    Timestamp :: non_neg_integer(),
    Status  :: binary(),
    Result  :: ok | {error, term()}.

-callback remove_user(LUser, LServer) -> ok | {error, term()} when
    LUser   :: jid:luser(),
    LServer :: jid:lserver().

-spec start(jid:server(), list()) -> 'ok'.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    gen_mod:start_backend_module(?MODULE, Opts, [get_last, set_last_info]),
    mod_last_backend:init(Host, Opts),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?NS_LAST, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_LAST, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_presence_update, 50),
    ejabberd_hooks:add(session_cleanup, Host, ?MODULE, session_cleanup, 50).

-spec stop(jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_presence_update, 50),
    ejabberd_hooks:delete(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_LAST).

%%%
%%% Uptime of ejabberd node
%%%
-spec process_local_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq())
        -> {mongoose_acc:t(), jlib:iq()}.
process_local_iq(_From, _To, Acc,
    #iq{type = Type, sub_el = SubEl} = IQ) ->
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
    case ejabberd_config:get_local_option(node_start) of
        {_, _, _} = StartNow ->
            now_to_seconds(p1_time_compat:timestamp()) - now_to_seconds(StartNow);
        _Undefined ->
            trunc(element(1, erlang:statistics(wall_clock))/1000)
    end.

-spec now_to_seconds(erlang:timestamp()) -> non_neg_integer().
now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

%%%
%%% Serve queries about user last online
%%%
-spec process_sm_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq(From, To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    User = To#jid.luser,
    Server = To#jid.lserver,
    {Subscription, _Groups} =
    ejabberd_hooks:run_fold(roster_get_jid_info, Server,
                            {none, []}, [User, Server, From]),
    MutualSubscription = Subscription == both,
    RequesterSubscribedToTarget = Subscription == from,
    QueryingSameUsersLast = (From#jid.luser == To#jid.luser) and
                            (From#jid.lserver == To#jid.lserver),
    case MutualSubscription or RequesterSubscribedToTarget or QueryingSameUsersLast of
        true ->
            UserListRecord = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                                                     #userlist{}, [User, Server]),
            ok,
            {Acc1, Res} = mongoose_privacy:privacy_check_packet(Acc, Server, User,
                                                             UserListRecord, To, From,
                                                             out),
            {Acc1, make_response(IQ, SubEl, User, Server, Res)};
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]}}
    end.

-spec make_response(jlib:iq(), SubEl :: 'undefined' | [exml:element()],
                    jid:luser(), jid:lserver(), allow | deny) -> jlib:iq().
make_response(IQ, SubEl, _, _, deny) ->
    IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]};
make_response(IQ, SubEl, LUser, LServer, allow) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
        [] ->
            case get_last(LUser, LServer) of
                {error, _Reason} ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]};
                not_found ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]};
                {ok, TimeStamp, Status} ->
                    TimeStamp2 = now_to_seconds(p1_time_compat:timestamp()),
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

get_last(LUser, LServer) ->
    mod_last_backend:get_last(LUser, LServer).

-spec count_active_users(jid:lserver(), non_neg_integer()) -> non_neg_integer().
count_active_users(LServer, Timestamp) ->
    mod_last_backend:count_active_users(LServer, Timestamp).

-spec on_presence_update(map(), jid:user(), jid:server(), jid:resource(),
                         Status :: binary()) -> map() | {error, term()}.
on_presence_update(Acc, LUser, LServer, _Resource, Status) ->
    TimeStamp = now_to_seconds(p1_time_compat:timestamp()),
    case store_last_info(LUser, LServer, TimeStamp, Status) of
        ok -> Acc;
        E -> E
    end.

-spec store_last_info(jid:user(), jid:server(), non_neg_integer(),
                      Status :: binary()) -> ok | {error, term()}.
store_last_info(LUser, LServer, TimeStamp, Status) ->
    mod_last_backend:set_last_info(LUser, LServer, TimeStamp, Status).

-spec get_last_info(jid:luser(), jid:lserver())
        -> 'not_found' | {'ok', integer(), string()}.
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
        {error, _Reason} -> not_found;
        Res -> Res
    end.

-spec remove_user(mongoose_acc:t(), jid:user(), jid:server()) -> mongoose_acc:t().
remove_user(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    R = mod_last_backend:remove_user(LUser, LServer),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

%% TODO fix
-spec session_cleanup(Acc :: map(), LUser :: jid:luser(), LServer :: jid:lserver(),
                      LResource :: jid:lresource(), SID :: ejabberd_sm:sid()) -> any().
session_cleanup(Acc, LUser, LServer, LResource, _SID) ->
    on_presence_update(Acc, LUser, LServer, LResource, <<>>).

