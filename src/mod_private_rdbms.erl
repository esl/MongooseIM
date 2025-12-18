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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_rdbms).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_private_backend).

-export([init/2,
         multi_set_data/4,
         multi_get_data/4,
         get_all_nss/3,
         remove_user/3,
         remove_domain/2]).

init(HostType, _Opts) ->
    mongoose_rdbms:prepare(private_select_data, private_storage,
                           [server, username, namespace],
                           <<"SELECT data FROM private_storage WHERE server=? AND username=? AND namespace=?">>),
    mongoose_rdbms:prepare(private_select_namespaces, private_storage,
                           [server, username],
                           <<"SELECT namespace FROM private_storage WHERE server=? AND username=?">>),
    mongoose_rdbms:prepare(private_remove_user, private_storage,
                           [server, username],
                           <<"DELETE FROM private_storage WHERE server=? AND username=?">>),
    mongoose_rdbms:prepare(private_remove_domain, private_storage,
                           [server],
                           <<"DELETE FROM private_storage WHERE server=?">>),
    rdbms_queries:prepare_upsert(HostType, private_upsert, private_storage,
                                 [<<"server">>, <<"username">>, <<"namespace">>, <<"data">>],
                                 [<<"data">>],
                                 [<<"server">>, <<"username">>, <<"namespace">>]),
    ok.

multi_set_data(HostType, LUser, LServer, NS2XML) ->
    NS2BinXML = make_xml_binary(NS2XML),
    F = fun() -> multi_set_data_t(HostType, LUser, LServer, NS2BinXML) end,
    case rdbms_queries:sql_transaction(HostType, F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {aborted, Reason};
        {error, Reason} -> {error, Reason}
    end.

multi_set_data_t(HostType, LUser, LServer, NS2XML) ->
    [upsert_data_t(HostType, LUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

upsert_data_t(HostType, LUser, LServer, NS, XML) ->
    InsertParams = [LServer, LUser, NS, XML],
    UpdateParams = [XML],
    rdbms_queries:execute_upsert(HostType, private_upsert, InsertParams, UpdateParams).

make_xml_binary(NS2XML) ->
    [{NS, exml:to_binary(XML)} || {NS, XML} <- NS2XML].

multi_get_data(HostType, LUser, LServer, NS2Def) ->
    [get_data(HostType, LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(HostType, LUser, LServer, NS, Default) ->
    Res = mongoose_rdbms:execute(HostType, private_select_data, [LServer, LUser, NS]),
    case Res of
        {selected, [{BinData}]} ->
            {ok, Elem} = exml:parse(BinData),
            Elem;
        _ ->
            Default
    end.

get_all_nss(HostType, LUser, LServer) ->
    {selected, Res} = mongoose_rdbms:execute(HostType, private_select_namespaces, [LServer, LUser]),
    lists:map(fun({R}) -> R end, Res).

remove_user(HostType, LUser, LServer) ->
    mongoose_rdbms:execute(HostType, private_remove_user, [LServer, LUser]).

remove_domain(HostType, LServer) ->
    mongoose_rdbms:execute(HostType, private_remove_domain, [LServer]).
