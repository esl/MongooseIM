%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @end
%%% ====================================================================


-module(node_hometree).
-behaviour(gen_pubsub_node).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-export([based_on/0, init/3, terminate/2, options/0, features/0,
         create_node_permission/6, node_to_path/1,
         path_to_node/1]).

based_on() ->  node_flat.

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    Owner = mod_pubsub:service_jid(Host),
    mod_pubsub:create_node(Host, ServerHost, <<"/home">>, Owner, <<"hometree">>),
    mod_pubsub:create_node(Host, ServerHost, <<"/home/", ServerHost/binary>>,
                           Owner, <<"hometree">>),
    ok.

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost).

options() ->
    node_flat:options().

features() ->
    node_flat:features().

%% @doc Checks if the current user has the permission to create the requested node
%% <p>In hometree node, the permission is decided by the place in the
%% hierarchy where the user is creating the node. The access parameter is also
%% checked. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
%% <p>This function also check that node can be created as a children of its
%% parent node</p>
create_node_permission(Host, _ServerHost, _Node, _ParentNode,
                       #jid{ luser = <<>>, lserver = Host, lresource = <<>> }, _Access) ->
    {result, true}; % pubsub service always allowed
create_node_permission(_Host, ServerHost, Node, _ParentNode,
                       #jid{ luser = LUser, lserver = LServer } = Owner, Access) ->
    case acl:match_rule(ServerHost, Access, Owner) of
        allow ->
            case node_to_path(Node) of
                [<<"home">>, LServer, LUser | _] -> {result, true};
                _ -> {result, false}
            end;
        _ -> {result, false}
    end.

%% @doc <p>Return the path of the node.</p>
node_to_path(Node) ->
    mongoose_bin:tokens(Node, <<"/">>).

path_to_node([]) -> <<>>;
path_to_node(Path) -> mongoose_bin:join([<<"">> | Path], <<"/">>).
