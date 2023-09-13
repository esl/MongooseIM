%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
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
%%==============================================================================
-module (mod_extdisco).
-author('jan.ciesla@erlang-solutions.com').

-xep([{xep, 215}, {version, "1.0.0"}]).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, supported_features/0, config_spec/0]).

-export([process_iq/5]).

-ignore_xref([process_iq/5]).

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_EXTDISCO, ejabberd_local,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_EXTDISCO, ejabberd_local).

supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                       <<"service">> => #list{items = service_config_spec()}},
             defaults = #{<<"iqdisc">> => no_queue,
                          <<"service">> => []}}.

service_config_spec() ->
    #section{items = #{<<"type">> => #option{type = atom,
                                             validate = non_empty},
                       <<"host">> => #option{type = binary,
                                             validate = non_empty},
                       <<"port">> => #option{type = integer,
                                             validate = port},
                       <<"transport">> => #option{type = binary,
                                                  validate = {enum, [<<"udp">>, <<"tcp">>]}},
                       <<"username">> => #option{type = binary,
                                           validate = non_empty},
                       <<"password">> => #option{type = binary,
                                                 validate = non_empty}
                      },
             required = [<<"type">>, <<"host">>]}.

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, _From, _To, #iq{type = get, sub_el = SubEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    {ResponseType, Response} = case request_type(SubEl) of
        all_services ->
            RequestedServices = get_external_services(HostType),
            {result, create_iq_response(RequestedServices)};
        {credentials, {Type, Host}} ->
            Services = get_external_services(HostType, Type),
            RequestedServices = lists:filter(fun(#{host := H}) -> H =:= Host end, Services),
            {result, create_iq_response_credentials(RequestedServices)};
        {selected_services, Type} ->
            RequestedServices = get_external_services(HostType, Type),
            {result, create_iq_response_services(RequestedServices, Type)};
        _ ->
            {error, [mongoose_xmpp_errors:bad_request()]}
    end,
    {Acc, IQ#iq{type = ResponseType, sub_el = Response}}.

request_type(#xmlel{name = <<"services">>} = Element) ->
    case exml_query:attr(Element, <<"type">>) of
        undefined -> all_services;
        ServiceType ->
            case catch binary_to_existing_atom(ServiceType, utf8) of
                {'EXIT', _} -> {error, bad_request};
                Type -> {selected_services, Type}
            end
    end;
request_type(#xmlel{name = <<"credentials">>, children = [Children]}) ->
    Host = exml_query:attr(Children, <<"host">>),
    case exml_query:attr(Children, <<"type">>) of
        undefined -> {error, bad_request};
        ServiceType ->
            case catch binary_to_existing_atom(ServiceType, utf8) of
                {'EXIT', _} -> {error, bad_request};
                Type -> {credentials, {Type, Host}}
            end
    end;
request_type(_) ->
    {error, bad_request}.

create_iq_response(Services) ->
    #xmlel{name = <<"services">>,
        attrs = [{<<"xmlns">>, ?NS_EXTDISCO}],
        children = prepare_services_element(Services)}.

create_iq_response_services(Services, Type) ->
    #xmlel{name = <<"services">>,
        attrs = [{<<"xmlns">>, ?NS_EXTDISCO}, {<<"type">>, atom_to_binary(Type, utf8)}],
        children = prepare_services_element(Services)}.

create_iq_response_credentials(Services) ->
    #xmlel{name = <<"credentials">>,
        attrs = [{<<"xmlns">>, ?NS_EXTDISCO}],
        children = prepare_services_element(Services)}.

get_external_services(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, service).

get_external_services(HostType, Type) ->
    [Service || Service = #{type := T} <- get_external_services(HostType), T =:= Type].

prepare_services_element(Services) ->
    [#xmlel{name = <<"service">>, attrs = make_attrs(Service)} || Service <- Services].

make_attrs(Service) ->
    [{atom_to_binary(Key), format_value(Key, Value)} || {Key, Value} <- maps:to_list(Service)].

format_value(port, Port) -> integer_to_binary(Port);
format_value(type, Type) -> atom_to_binary(Type);
format_value(_, Value) when is_binary(Value) -> Value.
