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

-export([process_iq/5, validate_service_config/1]).

-export([get_external_services/2, get_external_services/3]).

-define(DEFAULT_TTL, 3600). %% in seconds

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
                       <<"secret">> => #option{type = binary,
                                               validate = non_empty},
                       <<"username">> => #option{type = binary,
                                                 validate = non_empty},
                       <<"password">> => #option{type = binary,
                                                 validate = non_empty}
                      },
             process = fun ?MODULE:validate_service_config/1,
             required = [<<"type">>, <<"host">>]}.

-spec validate_service_config(mongoose_config_parser_toml:config_part()) ->
    mongoose_config_parser_toml:config_part().
validate_service_config(ConfigPart) when is_map_key(secret, ConfigPart) andalso
                                         is_map_key(username, ConfigPart) ->
    error(#{what => invalid_service_config,
            text => <<"Cannot specify both 'secret' and 'username' in service config">>});
validate_service_config(ConfigPart) when is_map_key(secret, ConfigPart) andalso
                                         is_map_key(password, ConfigPart) ->
    error(#{what => invalid_service_config,
            text => <<"Cannot specify both 'secret' and 'password' in service config">>});
validate_service_config(ConfigPart) when is_map_key(secret, ConfigPart)  ->
    %% make TTL configurable if needed.
    maps:merge(#{ttl => ?DEFAULT_TTL}, ConfigPart);
validate_service_config(ConfigPart) ->
    ConfigPart.

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, From, _To, #iq{type = get, sub_el = SubEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    User = jid:to_bare_binary(From),
    {ResponseType, Response} = case request_type(SubEl) of
        all_services ->
            RequestedServices = get_external_services(HostType, User),
            {result, create_iq_response(RequestedServices)};
        {credentials, {Type, Host}} ->
            Services = get_external_services(HostType, Type, User),
            RequestedServices = lists:filter(fun(#{host := H}) -> H =:= Host end, Services),
            {result, create_iq_response_credentials(RequestedServices)};
        {selected_services, Type} ->
            RequestedServices = get_external_services(HostType, Type, User),
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
           attrs = #{<<"xmlns">> => ?NS_EXTDISCO},
           children = prepare_services_element(Services)}.

create_iq_response_services(Services, Type) ->
    #xmlel{name = <<"services">>,
           attrs = #{<<"xmlns">> => ?NS_EXTDISCO,
                     <<"type">> => atom_to_binary(Type, utf8)},
           children = prepare_services_element(Services)}.

create_iq_response_credentials(Services) ->
    #xmlel{name = <<"credentials">>,
           attrs = #{<<"xmlns">> => ?NS_EXTDISCO},
           children = prepare_services_element(Services)}.

get_external_services(HostType)->
    gen_mod:get_module_opt(HostType, ?MODULE, service).

get_external_services(HostType, User) ->
    [maybe_generate_credentials(Service, User)
        || Service <- get_external_services(HostType)].
get_external_services(HostType, Type, User) ->
    [maybe_generate_credentials(Service, User)
        || Service = #{type := T} <- get_external_services(HostType), T =:= Type].

prepare_services_element(Services) ->
    [#xmlel{name = <<"service">>, attrs = make_attrs(Service)} || Service <- Services].

maybe_generate_credentials(#{secret := Secret, ttl := TTL} = Service, User) ->
    ExpirationTime = erlang:system_time(second) + TTL,
    {TempUsername, TempPassword} = generate_credentials(User, Secret, ExpirationTime),
    Credentials = #{expires => ExpirationTime,
                    username => TempUsername,
                    password => TempPassword},
    NewService = maps:merge(maps:without([secret, ttl], Service), Credentials),
    NewService;
maybe_generate_credentials(Service, _) ->
    Service.

make_attrs(Service) ->
    #{atom_to_binary(Key) => format_value(Key, Value) || Key := Value <- Service}.

format_value(port, Port) -> integer_to_binary(Port);
format_value(type, Type) -> atom_to_binary(Type);
format_value(expires, UnixSeconds) ->
    list_to_binary(calendar:system_time_to_rfc3339(UnixSeconds, [{offset, "Z"}]));
format_value(_, Value) when is_binary(Value) -> Value.

%% see https://datatracker.ietf.org/doc/html/draft-uberti-behave-turn-rest-00#section-2.2
generate_credentials(Username, Secret, ExpirationTime) ->
    Timestamp = integer_to_binary(ExpirationTime),
    %% temporary-username = "timestamp:username"
    TempUsername = <<Timestamp/binary, ":", Username/binary>>,
    %% HMAC-SHA1(secret, temporary-username)
    Hmac = crypto:mac(hmac, sha, Secret, TempUsername),
    %% Base64 encode
    TempPassword = base64:encode(Hmac),
    {TempUsername, TempPassword}.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

generate_credentials_test_() ->
    %% Reference values are generated using this bash function:
    %%
    %%     function get_turn_credentials()
    %%     {
    %%       ## $1 - basic username
    %%       ## $2 - secret
    %%       ## $3 - timestamp
    %%       local username="${3}:${1}"
    %%       local secret="$2"
    %%       local password="$(echo -n "$username" | openssl dgst -hmac "$secret" -sha1 -binary | openssl base64)"
    %%       echo -e "username: '${username}'\npassword: '${password}'"
    %%     }
    %%
    %%     get_turn_credentials "dummy_user" "my shared secret" 100
    %%     get_turn_credentials "dummy_user@example.org" "secret123" 10000
    %%     get_turn_credentials "example.org" "super#secret" "$(($(date "+%s") + 3600))"
    %%     get_turn_credentials "example.org" "super#secret" "$(($(date "+%s") * 3))"

    [?_assertEqual(generate_credentials(~"dummy_user", ~"my shared secret", 100),
                   {~"100:dummy_user", ~"aBGRg1t/TGlc9+gkWObW2Li97Ew="}),
     ?_assertEqual(generate_credentials(~"dummy_user@example.org", ~"secret123", 10000),
                   {~"10000:dummy_user@example.org", ~"CKDrp61KWUnSXKusLAMKmL0qcQo="}),
     ?_assertEqual(generate_credentials(~"example.org", ~"super#secret", 1772853308),
                   {~"1772853308:example.org", ~"+THIMe4fdg70xzgyfQe1sXwl8ys="}),
     ?_assertEqual(generate_credentials(~"example.org", ~"super#secret", 5318549598),
                   {~"5318549598:example.org", ~"D8j9h+xPoWi8AL7Q5AfGywcRy10="})].

-endif.
