%%%----------------------------------------------------------------------
%%% Old copyright notice from mod_private.erl
%%%
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
-module(mod_private_mnesia).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_private_backend).

-export([init/2,
         multi_set_data/4,
         multi_get_data/4,
         get_all_nss/3,
         remove_user/3,
         remove_domain/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(private_storage, {usns, xml}).

init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(private_storage,
        [{disc_only_copies, [node()]},
         {attributes, record_info(fields, private_storage)}]),
    ok.

multi_set_data(_HostType, LUser, LServer, NS2XML) ->
    F = fun() -> multi_set_data_t(LUser, LServer, NS2XML) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {aborted, Reason}
    end.

multi_set_data_t(LUser, LServer, NS2XML) ->
    [set_data_t(LUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

set_data_t(LUser, LServer, NS, XML) ->
    mnesia:write(#private_storage{usns = {LUser, LServer, NS}, xml = XML}).

multi_get_data(_HostType, LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

get_all_nss(_HostType, LUser, LServer) ->
    F = fun() ->
        select_namespaces_t(LUser, LServer)
    end,
    {atomic, NSs} = mnesia:transaction(F),
    NSs.

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    case mnesia:dirty_read(private_storage, {LUser, LServer, NS}) of
        [#private_storage{xml=XML}] -> XML;
        [] -> Default
    end.

remove_user(_HostType, LUser, LServer) ->
    F = fun() ->
                NSs = select_namespaces_t(LUser, LServer),
        [delete_record_t(LUser, LServer, NS) || NS <- NSs]
        end,
    mnesia:transaction(F).

%% There is no optimized way to remove a domain.
%% We expect, that domain removal process would call remove_user instead
%% for each user.
remove_domain(_HostType, _LServer) ->
    ok.

select_namespaces_t(LUser, LServer) ->
    Result = mnesia:select(
        private_storage,
        [{#private_storage{usns={LUser, LServer, '$1'}, _ = '_'},
         [],
         ['$$']}]),
    [NS || [NS] <- Result].

delete_record_t(LUser, LServer, NS) ->
    mnesia:delete({private_storage, {LUser, LServer, NS}}).
