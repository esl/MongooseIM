%%%----------------------------------------------------------------------
%%% File    : mod_sic.erl
%%% Author  : Karim Gemayel <karim.gemayel@process-one.net>
%%% Purpose : XEP-0279 Server IP Check
%%% Created : 6 Mar 2010 by Karim Gemayel <karim.gemayel@process-one.net>
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

-module(mod_sic).
-author('karim.gemayel@process-one.net').
-xep([{xep, 279}, {version, "0.2"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0
        ]).

%% IQ and hook handlers
-export([process_local_iq/5,
         process_sm_iq/5
        ]).

-ignore_xref([process_local_iq/5, process_sm_iq/5]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-define(NS_SIC, <<"urn:xmpp:sic:1">>).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc}) ->
    [gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_SIC, Component, Fn, #{}, IQDisc) ||
        {Component, Fn} <- iq_handlers()],
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_LAST, Component) ||
        {Component, _Fn} <- iq_handlers()],
    ok.

iq_handlers() ->
    [{ejabberd_local, fun ?MODULE:process_local_iq/5},
     {ejabberd_sm, fun ?MODULE:process_sm_iq/5}].

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc()},
             defaults = #{<<"iqdisc">> => one_queue},
             format_items = map}.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%
%%% IQ handlers
%%%

-spec process_local_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) 
        -> {mongoose_acc:t(), jlib:iq()}.
process_local_iq(Acc, #jid{} = JID, _To,
                 #iq{type = 'get', sub_el = _SubEl} = IQ, _Extra) ->
    {Acc, get_ip(JID, IQ)};
process_local_iq(Acc, _From, _To, #iq{type = 'set', sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}}.

-spec process_sm_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) 
        -> {mongoose_acc:t(), jlib:iq()}.
process_sm_iq(Acc,
              #jid{luser = LUser, lserver = LServer} = JID,
              #jid{luser = LUser, lserver = LServer},
              #iq{type = 'get', sub_el = _SubEl} = IQ, _Extra) ->
    {Acc, get_ip(JID, IQ)};
process_sm_iq(Acc, _From, _To, #iq{type = 'get', sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]}};
process_sm_iq(Acc, _From, _To, #iq{type = 'set', sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}}.

get_ip(JID, #iq{sub_el = #xmlel{} = SubEl} = IQ) ->
    case ejabberd_sm:get_session_ip(JID) of
        {IP, Port} when is_tuple(IP) ->
            IQ#iq{
              type = 'result',
              sub_el = [
                SubEl#xmlel{
                children = [
                    #xmlel{name = <<"ip">>,
                        children = [#xmlcdata{content = list_to_binary(inet_parse:ntoa(IP))}]},
                    #xmlel{name = <<"port">>,
                        children = [#xmlcdata{content = integer_to_binary(Port)}]}
                ]
                }]};
        _ ->
            IQ#iq{
              type = 'error',
              sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]
             }
    end.
