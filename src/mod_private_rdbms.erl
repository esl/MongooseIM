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
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-export([get_all_nss/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

init(Host, _Opts) ->
    mongoose_rdbms:prepare(private_select_data, private_storage,
                           [username, namespace],
                           <<"SELECT data FROM private_storage WHERE username=? AND namespace=?">>),
    mongoose_rdbms:prepare(private_select_namespaces, private_storage,
                           [username],
                           <<"SELECT namespace FROM private_storage WHERE username=?">>),
    mongoose_rdbms:prepare(private_remove_user, private_storage,
                           [username],
                           <<"DELETE FROM private_storage WHERE username=?">>),
    rdbms_queries:prepare_upsert(Host, private_upsert, private_storage,
                                 [<<"username">>, <<"namespace">>, <<"data">>],
                                 [<<"data">>],
                                 [<<"username">>, <<"namespace">>]),
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    NS2BinXML = make_xml_binary(NS2XML),
    F = fun() -> multi_set_data_t(LUser, LServer, NS2BinXML) end,
    case rdbms_queries:sql_transaction(LServer, F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {aborted, Reason};
        {error, Reason} -> {error, Reason}
    end.


multi_set_data_t(LUser, LServer, NS2XML) ->
    [upsert_data_t(LUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

upsert_data_t(LUser, Host, NS, XML) ->
    InsertParams = [LUser, NS, XML],
    UpdateParams = [XML],
    UniqueKeyValues = [LUser, NS],
    rdbms_queries:execute_upsert(Host, private_upsert, InsertParams, UpdateParams, UniqueKeyValues).


make_xml_binary(NS2XML) ->
    [{NS, exml:to_binary(XML)} || {NS, XML} <- NS2XML].

multi_get_data(LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    Res = mongoose_rdbms:execute(LServer, private_select_data, [LUser, NS]),
    case Res of
        {selected, [{BinData}]} ->
            {ok, Elem} = exml:parse(BinData),
            Elem;
        _ ->
            Default
    end.

get_all_nss(LUser, LServer) ->
    {selected, Res} = mongoose_rdbms:execute(LServer, private_select_namespaces, [LUser]),
    lists:map(fun({R}) -> R end, Res).

remove_user(LUser, LServer) ->
    mongoose_rdbms:execute(LServer, private_remove_user, [LUser]).
