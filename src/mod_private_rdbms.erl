%%%----------------------------------------------------------------------
%%% File    : mod_private_rdbms.erl
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
-module(mod_private_rdbms).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-export([get_all_nss/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

init(_Host, _Opts) ->
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    F = fun() -> multi_set_data_t(LUser, LServer, NS2XML) end,
    case rdbms_queries:sql_transaction(LServer, F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {aborted, Reason};
        {error, Reason} -> {error, Reason}
    end.

multi_set_data_t(LUser, LServer, NS2XML) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    [set_data_t(SLUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

set_data_t(SLUser, LServer, NS, XML) ->
    SNS = mongoose_rdbms:escape_string(NS),
    SData = mongoose_rdbms:escape_string(exml:to_binary(XML)),
    rdbms_queries:set_private_data(LServer, SLUser, SNS, SData).

multi_get_data(LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    SNS = mongoose_rdbms:escape_string(NS),
    case catch rdbms_queries:get_private_data(LServer, SLUser, SNS) of
        {selected, [{SData}]} ->
            {ok, Elem} = exml:parse(SData),
            Elem;
        _ ->
            Default
    end.

get_all_nss(LUser, LServer) ->
    EscLUser = mongoose_rdbms:escape_string(LUser),
    {selected, Res} = rdbms_queries:get_all_roster_namespaces(LServer, EscLUser),
    Keys = lists:map(fun({R}) -> R end, Res),
    Keys.

remove_user(LUser, LServer) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:del_user_private_storage(LServer, SLUser).
