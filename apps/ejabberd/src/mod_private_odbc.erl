%%%----------------------------------------------------------------------
%%% File    : mod_private_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Private storage support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@process-one.net>
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

%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_odbc).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

init(_Host, _Opts) ->
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    F = fun() -> multi_set_data_t(LUser, LServer, NS2XML) end,
    {atomic, ok} = odbc_queries:sql_transaction(LServer, F),
    ok.

multi_set_data_t(LUser, LServer, NS2XML) ->
    SLUser = ejabberd_odbc:escape(LUser),
    [set_data_t(SLUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

set_data_t(SLUser, LServer, NS, XML) ->
    SNS = ejabberd_odbc:escape(NS),
    SData = ejabberd_odbc:escape(xml:element_to_binary(XML)),
    odbc_queries:set_private_data(LServer, SLUser, SNS, SData).

multi_get_data(LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    SLUser = ejabberd_odbc:escape(LUser),
    SNS = ejabberd_odbc:escape(NS),
    case catch odbc_queries:get_private_data(LServer, SLUser, SNS) of
        {selected, [<<"data">>], [{SData}]} ->
            #xmlel{} = xml_stream:parse_element(SData);
        _ ->
            Default
    end.

remove_user(LUser, LServer) ->
    SLUser = ejabberd_odbc:escape(LUser),
    odbc_queries:del_user_private_storage(LServer, SLUser).
