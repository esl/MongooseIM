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

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_local_iq/3,
         process_sm_iq/3,
         on_presence_update/4,
         store_last_info/4,
         get_last_info/2,
         count_active_users/3,
         remove_user/2]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").
-include("mod_last.hrl").

-define(BACKEND, (mod_last_backend:backend())).

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host    :: ejabberd:server(),
    Opts    :: list().

-callback get_last(LUser, LServer) -> Result when
    LUser   :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Reason  :: term(),
    Result  :: {ok, non_neg_integer(), binary()} | {error, Reason} | not_found.

-callback count_active_users(LServer, Timestamp, Comparator) -> Result when
    LServer :: ejabberd:lserver(),
    Timestamp :: non_neg_integer(),
    Comparator :: '<' | '>',
    Result :: non_neg_integer().

-callback set_last_info(LUser, LServer, Timestamp, Status) -> Result when
    LUser   :: ejabberd:luser(),
    LServer :: ejabberd:lserver(),
    Timestamp :: non_neg_integer(),
    Status  :: binary(),
    Result  :: {atomic, ok} | {error, term()}.

-callback remove_user(LUser, LServer) -> ok when
    LUser   :: ejabberd:luser(),
    LServer :: ejabberd:lserver().

-spec start(ejabberd:server(), list()) -> 'ok'.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    start_backend_module(Opts),
    ?BACKEND:init(Host, Opts),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?NS_LAST, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_LAST, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
        remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
        on_presence_update, 50).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
        remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
        ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
        ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
        ?NS_LAST).

%% ------------------------------------------------------------------
%% Dynamic modules

start_backend_module(Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    {Mod, Code} = dynamic_compile:from_string(mod_last_backend(Backend)),
    code:load_binary(Mod, "mod_last_backend.erl", Code).

-spec mod_last_backend(atom()) -> string().
mod_last_backend(Backend) when is_atom(Backend) ->
    lists:flatten(
        ["-module(mod_last_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            mod_last_",
            atom_to_list(Backend),
            ".\n"]).


%%%
%%% Uptime of ejabberd node
%%%
-spec process_local_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
        -> ejabberd:iq().
process_local_iq(_From, _To,
    #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
        set ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        get ->
            Sec = get_node_uptime(),
            IQ#iq{type = result,
                sub_el =
                [#xmlel{name = <<"query">>,
                    attrs =
                    [{<<"xmlns">>, ?NS_LAST},
                        {<<"seconds">>,
                            integer_to_binary(Sec)}],
                    children = []}]}
    end.

-spec get_node_uptime() -> non_neg_integer().
get_node_uptime() ->
    case ejabberd_config:get_local_option(node_start) of
        {_, _, _} = StartNow ->
            now_to_seconds(now()) - now_to_seconds(StartNow);
        _undefined ->
            trunc(element(1, erlang:statistics(wall_clock))/1000)
    end.

-spec now_to_seconds(erlang:timestamp()) -> non_neg_integer().
now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

%%%
%%% Serve queries about user last online
%%%
-spec process_sm_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
        -> ejabberd:iq().
process_sm_iq(From, To,
    #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
        set ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        get ->
            User = To#jid.luser,
            Server = To#jid.lserver,
            {Subscription, _Groups} =
                ejabberd_hooks:run_fold(roster_get_jid_info, Server,
                    {none, []}, [User, Server, From]),
            if (Subscription == both) or (Subscription == from) or
                (From#jid.luser == To#jid.luser) and
                    (From#jid.lserver == To#jid.lserver) ->
                UserListRecord =
                    ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                        #userlist{}, [User, Server]),
                case ejabberd_hooks:run_fold(privacy_check_packet,
                    Server, allow,
                    [User, Server, UserListRecord,
                        {To, From,
                            #xmlel{name = <<"presence">>,
                                attrs = [],
                                children = []}},
                        out])
                of
                    allow -> get_last_iq(IQ, SubEl, User, Server);
                    deny ->
                        IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
                end;
                true ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
            end
    end.

-spec get_last_iq(ejabberd:iq(), SubEl :: 'undefined' | [jlib:xmlel()],
                  ejabberd:luser(), ejabberd:lserver()) -> ejabberd:iq().
get_last_iq(IQ, SubEl, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
        [] ->
            case get_last(LUser, LServer) of
                {error, _Reason} ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
                not_found ->
                    IQ#iq{type = error,
                        sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
                {ok, TimeStamp, Status} ->
                    TimeStamp2 = now_to_seconds(now()),
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
    ?BACKEND:get_last(LUser, LServer).

-spec count_active_users(ejabberd:lserver(), non_neg_integer(), '<' | '>')
        -> non_neg_integer().
count_active_users(LServer, Timestamp, Comparator) ->
    ?BACKEND:count_active_users(LServer, Timestamp, Comparator).

-spec on_presence_update(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
                         Status :: binary()) -> {'aborted',_} | {'atomic',_}.
on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = now_to_seconds(now()),
    store_last_info(User, Server, TimeStamp, Status).

-spec store_last_info(ejabberd:user(), ejabberd:server(), erlang:timestamp(),
                      Status :: binary()) -> {'aborted',_} | {'atomic',_}.
store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    ?BACKEND:set_last_info(LUser, LServer, TimeStamp, Status).

-spec get_last_info(ejabberd:luser(), ejabberd:lserver())
        -> 'not_found' | {'ok',integer(),string()}.
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
        {error, _Reason} -> not_found;
        Res -> Res
    end.

-spec remove_user(ejabberd:user(), ejabberd:server())
        -> {'aborted',_} | {'atomic',_}.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    ?BACKEND:remove_user(LUser, LServer).
