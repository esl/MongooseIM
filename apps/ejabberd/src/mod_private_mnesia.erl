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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_mnesia).
-author('alexey@process-one.net').
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(private_storage, {usns, xml}).

init(_Host, _Opts) ->
    mnesia:create_table(private_storage,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, private_storage)}]),
    update_table(),
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    F = fun() -> multi_set_data_t(LUser, LServer, NS2XML) end,
    mnesia:transaction(F).

multi_set_data_t(LUser, LServer, NS2XML) ->
    [set_data_t(LUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    ok.

set_data_t(LUser, LServer, NS, XML) ->
    mnesia:write(#private_storage{usns = {LUser, LServer, NS}, xml = XML}).
    
multi_get_data(LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    case mnesia:dirty_read(private_storage, {LUser, LServer, NS}) of
        [#private_storage{xml=XML}] -> XML;
        [] -> Default
    end.

remove_user(LUser, LServer) ->
    F = fun() ->
		NSs = select_namespaces_t(LUser, LServer),
        [delete_record_t(LUser, LServer, NS) || NS <- NSs]
        end,
    mnesia:transaction(F).

select_namespaces_t(LUser, LServer) ->
    Result = mnesia:select(
        private_storage,
        [{#private_storage{usns={LUser, LServer, '$1'}, _ = '_'},
         [],
         ['$$']}]),
    [NS || [NS] <- Result].

delete_record_t(LUser, LServer, NS) ->
    mnesia:delete({private_storage, {LUser, LServer, NS}}).

update_table() ->
    Fields = record_info(fields, private_storage),
    case mnesia:table_info(private_storage, attributes) of
	Fields ->
	    ok;
	[userns, xml] ->
	    ?INFO_MSG("Converting private_storage table from "
		      "{user, default, lists} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_private_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, private_storage},
			      {attributes, record_info(fields, private_storage)}]),
	    mnesia:transform_table(private_storage, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_private_tmp_table),
			 mnesia:foldl(
			   fun(#private_storage{usns = {U, NS}} = R, _) ->
				   mnesia:dirty_write(
				     mod_private_tmp_table,
				     R#private_storage{usns = {U, Host, NS}})
			   end, ok, private_storage)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(private_storage),
	    F2 = fun() ->
			 mnesia:write_lock_table(private_storage),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_private_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_private_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating private_storage table", []),
	    mnesia:transform_table(private_storage, ignore, Fields)
    end.


