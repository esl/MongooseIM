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

-define(EXT_DISCO, <<"urn:xmpp:extdisco:2">>).

-spec start(jid:server(), list()) -> ok.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?EXT_DISCO),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?EXT_DISCO, ?MODULE, process_iq, IQDisc).

-spec stop(jid:server()) -> ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?EXT_DISCO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?EXT_DISCO).

-spec process_iq(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(_From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    Response = case request_type(SubEl) of
        all_services ->
            RequestedServices = get_external_services(),
            create_iq_response(RequestedServices);
        {credentials, {Type, Host}} ->
            Services = get_external_services([Type], []),
            RequestedServices = lists:filter(fun({_, Opts}) ->
                gen_mod:get_opt(host, Opts, undefined) == binary_to_list(Host)
                end, Services),
            create_iq_response_credentials(RequestedServices);
        {selected_services, Type} ->
            RequestedServices = get_external_services([Type], []),
            create_iq_response_services(RequestedServices, Type);
        _ ->
            []
    end,
    {Acc, IQ#iq{type = result, sub_el = Response}}.

request_type(#xmlel{name = <<"services">>} = Element) ->
    case exml_query:attr(Element, <<"type">>) of
        undefined -> all_services;
        Services -> {selected_services, binary_to_atom(Services, utf8)}
    end;
request_type(#xmlel{name = <<"credentials">>, children = [C]}) ->
    Host = exml_query:attr(C, <<"host">>),
    Type = exml_query:attr(C, <<"type">>),
    {credentials, {binary_to_atom(Type, utf8), Host}};
request_type(_) ->
    {error, unknown_request}.

create_iq_response(Services) ->
    #xmlel{name = <<"services">>,
        attrs = [{<<"xmlns">>, ?EXT_DISCO}],
        children = prepare_services_element(Services, [])}.

create_iq_response_services(Services, Type) ->
    #xmlel{name = <<"services">>,
        attrs = [{<<"xmlns">>, ?EXT_DISCO}, {<<"type">>, atom_to_binary(Type, utf8)}],
        children = prepare_services_element(Services, [])}.

create_iq_response_credentials(Services) ->
    #xmlel{name = <<"credentials">>,
        attrs = [{<<"xmlns">>, ?EXT_DISCO}],
        children = prepare_services_element(Services, [])}.

get_external_services() ->
    get_external_services([stun, turn], []).
get_external_services([], ServicesWithOpts) ->
    ServicesWithOpts;
get_external_services([Type | Types], ServicesWithOpts) ->
    case gen_mod:get_module_opt(?MYNAME, ?MODULE, Type, []) of
        [] -> get_external_services(Types, ServicesWithOpts);
        Opts -> get_external_services(Types, [{Type, Opts} | ServicesWithOpts])
    end.

prepare_services_element([], Result) ->
    Result;
prepare_services_element([{Type, Opts} | Services], Result) ->
    RequiredElements = required_elements(Type, Opts),
    OptionalElements = optional_elements(Opts),
    NewResult = [#xmlel{name = <<"service">>,
                        attrs = RequiredElements ++ OptionalElements}],
    prepare_services_element(Services, Result ++ NewResult).

required_elements(Type, Opts) ->
    Host = gen_mod:get_opt(host, Opts, <<"">>),
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"host">>, Host}].

optional_elements(Opts) ->
    Port = gen_mod:get_opt(port, Opts, undefined),
    Transport = gen_mod:get_opt(transport, Opts, undefined),
    Password = gen_mod:get_opt(password, Opts, undefined),
    Username = gen_mod:get_opt(username, Opts, undefined),
    Elements = [{<<"port">>, Port},
                {<<"transport">>, Transport},
                {<<"password">>, Password},
                {<<"username">>, Username}],
    filter_undefined_elements(Elements).

filter_undefined_elements(Elements) ->
    lists:filter(fun(Element) ->
        case Element of
            {_, undefined} -> false;
            _ -> true
        end
    end, Elements).
