%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% This module provides an API for dealing with AMQP operations.
%% @end
%%==============================================================================

-module(mongoose_amqp).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("amqp_client/include/amqp_client.hrl").

-export([network_params/0, network_params/1, exchange_declare/2,
         exchange_declare_ok/0, exchange_delete/1, basic_publish/2,
         confirm_select/0, confirm_select_ok/0, message/1]).

-export_type([network_params/0, method/0, message/0]).


%%%===================================================================
%%% Types and definitions
%%%===================================================================

-type network_params() :: #amqp_params_network{}.

-type method() :: #'exchange.declare'{}
                | #'exchange.declare_ok'{}
                | #'exchange.delete'{}
                | #'basic.publish'{}
                | #'confirm.select'{}
                | #'confirm.select_ok'{}.

-type message() :: #amqp_msg{}.

-define(DEFAULT_PORT, 5672).

%%%===================================================================
%%% API
%%%===================================================================

-spec network_params() -> network_params().
network_params() ->
    network_params([]).

-spec network_params(proplists:proplist()) -> #amqp_params_network{}.
network_params(Opts) ->
    network_params(Opts, #amqp_params_network{}).

-spec exchange_declare(Exchange :: binary(), Type :: binary()) -> method().
exchange_declare(Exchange, Type) ->
    #'exchange.declare'{exchange = Exchange, type = Type}.

-spec exchange_declare_ok() -> method().
exchange_declare_ok() ->
    #'exchange.declare_ok'{}.

-spec exchange_delete(Exchange :: binary()) -> method().
exchange_delete(Exchange) ->
    #'exchange.delete'{exchange = Exchange}.

-spec basic_publish(Exchange :: binary(), RoutingKey :: binary()) -> method().
basic_publish(Exchange, RoutingKey) ->
    #'basic.publish'{exchange = Exchange, routing_key = RoutingKey}.

-spec confirm_select() -> method().
confirm_select() ->
    #'confirm.select'{}.

-spec confirm_select_ok() -> method().
confirm_select_ok() ->
    #'confirm.select_ok'{}.

-spec message(Payload :: binary()) -> message().
message(Payload) ->
    #amqp_msg{payload = Payload}.

%%%===================================================================
%%% Helpers
%%%===================================================================

network_params(Opts, #amqp_params_network{host = Host, username = UserName,
                                          password = Password}) ->
    #amqp_params_network{
       host = proplists:get_value(host, Opts, Host),
       port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
       username = proplists:get_value(username, Opts, UserName),
       password = proplists:get_value(password, Opts, Password)}.
