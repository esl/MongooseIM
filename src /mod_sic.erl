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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_sic).
-author('karim.gemayel@process-one.net').
-xep([{xep, 279}, {version, "0.2"}]).
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_local_iq/4,
         process_sm_iq/4
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(NS_SIC, <<"urn:xmpp:sic:1">>).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_SIC, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_SIC, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_SIC),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_SIC).


process_local_iq(#jid{user = User, server = Server, resource = Resource}, _To,
                 Acc, #iq{type = 'get', sub_el = _SubEl} = IQ) ->
    {Acc, get_ip({User, Server, Resource}, IQ)};

process_local_iq(_From, _To, Acc, #iq{type = 'set', sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}}.


process_sm_iq(#jid{user = User, server = Server, resource = Resource},
              #jid{user = User, server = Server},
              Acc,
              #iq{type = 'get', sub_el = _SubEl} = IQ) ->
    {Acc, get_ip({User, Server, Resource}, IQ)};

process_sm_iq(_From, _To, Acc, #iq{type = 'get', sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:forbidden()]}};

process_sm_iq(_From, _To, Acc, #iq{type = 'set', sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}}.

get_ip({User, Server, Resource},
       #iq{sub_el = #xmlel{} = SubEl} = IQ) ->
    case ejabberd_sm:get_session_ip(User, Server, Resource) of
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
