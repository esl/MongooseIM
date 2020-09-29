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

-xep([{xep, 215}, {version, "0.7"}]).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1]).

-export([process_iq/4]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec start(jid:server(), list()) -> ok.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    mod_disco:register_feature(Host, ?NS_EXTDISCO),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?NS_EXTDISCO, ?MODULE, process_iq, IQDisc).

-spec stop(jid:server()) -> ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_EXTDISCO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_EXTDISCO).

-spec process_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(_From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    {ResponseType, Response} = case request_type(SubEl) of
        all_services ->
            RequestedServices = get_external_services(),
            {result, create_iq_response(RequestedServices)};
        {credentials, {Type, Host}} ->
            Services = get_external_services(Type),
            RequestedServices = lists:filter(fun(Opts) ->
                gen_mod:get_opt(host, Opts, undefined) == binary_to_list(Host)
                end, Services),
            {result, create_iq_response_credentials(RequestedServices)};
        {selected_services, Type} ->
            RequestedServices = get_external_services(Type),
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

get_external_services() ->
    case gen_mod:get_module_opts(?MYNAME, ?MODULE) of
        [] -> [];
        ServicesWithOpts -> ServicesWithOpts
    end.

get_external_services(Type) ->
    [Opts || Opts <- get_external_services(), gen_mod:get_opt(type, Opts) == Type].

prepare_services_element(Services) ->
    lists:reverse(
      lists:foldl(
        fun(Opts, Acc) ->
                RequiredElements = required_elements(Opts),
                OptionalElements = optional_elements(Opts),
                NewResult = #xmlel{name = <<"service">>,
                                    attrs = RequiredElements ++ OptionalElements},
                [NewResult | Acc]
        end, [], Services)).

required_elements(Opts) ->
    Host = gen_mod:get_opt(host, Opts, <<"">>),
    Type = gen_mod:get_opt(type, Opts),
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"host">>, Host}].

optional_elements(Opts) ->
    Port = gen_mod:get_opt(port, Opts, undefined),
    Transport = gen_mod:get_opt(transport, Opts, undefined),
    Password = gen_mod:get_opt(password, Opts, undefined),
    Username = gen_mod:get_opt(username, Opts, undefined),
    Elements = [{<<"port">>, i2b(Port)},
                {<<"transport">>, Transport},
                {<<"password">>, Password},
                {<<"username">>, Username}],
    filter_undefined_elements(Elements).

filter_undefined_elements(Elements) ->
    lists:filter(fun({_, undefined}) -> false;
                    (_)              -> true
                 end, Elements).

i2b(X) when is_integer(X) -> integer_to_binary(X);
i2b(X) -> X.
