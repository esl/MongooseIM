%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

-module(mod_http_upload).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-xep([{xep, 363}, {version, "0.3.0"}]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-define(DEFAULT_TOKEN_BYTES, 32).
-define(DEFAULT_MAX_FILE_SIZE, 10 * 1024 * 1024). % 10 MB
-define(DEFAULT_SUBHOST, <<"upload.@HOST@">>).

-export([start/2, stop/1, process_iq/4, process_disco_iq/4, get_urls/5, config_spec/0]).

%% Hook handlers
-export([disco_local_items/1]).

-export([config_metrics/1]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback create_slot(UTCDateTime :: calendar:datetime(), UUID :: binary(),
                      Filename :: unicode:unicode_binary(), ContentType :: binary() | undefined,
                      Size :: pos_integer(), Opts :: proplists:proplist()) ->
    {PUTURL :: binary(), GETURL :: binary(), Headers :: #{binary() => binary()}}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SubHost = subhost(Host),
    %% TODO: Conversion of this module is not done, it doesn't support dynamic
    %%       domains yet. Only subdomain registration is done properly.
    SubdomainPattern = subdomain_pattern(Host),
    PacketHandler = mongoose_packet_handler:new(ejabberd_local),
    mongoose_domain_api:register_subdomain(Host, SubdomainPattern, PacketHandler),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_local_items, 90),
    gen_iq_handler:add_iq_handler(ejabberd_local, SubHost, ?NS_HTTP_UPLOAD_030,
                                  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, SubHost, ?NS_DISCO_INFO,
                                  ?MODULE, process_disco_iq, IQDisc),
    gen_mod:start_backend_module(?MODULE, with_default_backend(Opts), [create_slot]).


-spec stop(Host :: jid:server()) -> any().
stop(Host) ->
    SubHost = subhost(Host),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_local_items, 90),
    gen_iq_handler:remove_iq_handler(ejabberd_local, SubHost, ?NS_HTTP_UPLOAD_030),
    gen_iq_handler:remove_iq_handler(ejabberd_local, SubHost, ?NS_DISCO_INFO),
    SubdomainPattern = subdomain_pattern(Host),
    mongoose_domain_api:unregister_subdomain(Host, SubdomainPattern).


-spec process_iq(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq() | ignore}.
process_iq(_From, _To, Acc, IQ = #iq{type = set, lang = Lang, sub_el = SubEl}) ->
    Error = mongoose_xmpp_errors:not_allowed(Lang, <<"IQ set is not allowed for HTTP upload">>),
    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
process_iq(_From, _To = #jid{lserver = SubHost}, Acc, IQ = #iq{type = get, sub_el = Request}) ->
    {ok, HostType} = mongoose_domain_api:get_subdomain_host_type(SubHost),
    Res = case parse_request(Request) of
        {Filename, Size, ContentType} ->
            MaxFileSize = max_file_size(HostType),
            case MaxFileSize =:= undefined orelse Size =< MaxFileSize of
                true ->
                    UTCDateTime = calendar:universal_time(),
                    Token = generate_token(HostType),
                    Opts = module_opts(HostType),

                    {PutUrl, GetUrl, Headers} =
                        mod_http_upload_backend:create_slot(UTCDateTime, Token, Filename,
                                                            ContentType, Size, Opts),

                    compose_iq_reply(IQ, PutUrl, GetUrl, Headers);

                false ->
                    IQ#iq{type = error, sub_el = [file_too_large_error(MaxFileSize)]}
            end;

        bad_request ->
            IQ#iq{type = error, sub_el = [Request, mongoose_xmpp_errors:bad_request()]}
    end,
    {Acc, Res}.

-spec process_disco_iq(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                       IQ :: jlib:iq()) ->
          {mongoose_acc:t(), jlib:iq()}.
process_disco_iq(_From, _To, Acc, #iq{type = set, lang = Lang, sub_el = SubEl} = IQ) ->
    ErrorMsg = <<"IQ set is not allowed for service discovery">>,
    Error = mongoose_xmpp_errors:not_allowed(Lang, ErrorMsg),
    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
process_disco_iq(_From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    LServer = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    case Node of
        <<>> ->
            Identity = mongoose_disco:identities_to_xml(disco_identity(Lang)),
            Info = disco_info(LServer),
            Features = mongoose_disco:features_to_xml([?NS_HTTP_UPLOAD_030]),
            {Acc, IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
                                         children = Identity ++ Info ++ Features}]}};
        _ ->
            ErrorMsg = <<"Node is not supported by HTTP upload">>,
            Error = mongoose_xmpp_errors:item_not_found(Lang, ErrorMsg),
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
    end.

-spec get_urls(Host :: jid:lserver(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
                  {PutURL :: binary(), GetURL :: binary(), Headers :: #{binary() => binary()}}.
get_urls(Host, Filename, Size, ContentType, Timeout) ->
    UTCDateTime = calendar:universal_time(),
    Token = generate_token(Host),
    Opts = module_opts(Host),
    NewOpts = gen_mod:set_opt(expiration_time, Opts, Timeout),
    mod_http_upload_backend:create_slot(UTCDateTime, Token, Filename,
                                        ContentType, Size, NewOpts).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                  <<"host">> => #option{type = string,
                                        validate = subdomain_template,
                                        process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                  <<"backend">> => #option{type = atom,
                                           validate = {module, mod_http_upload}},
                  <<"expiration_time">> => #option{type = integer,
                                                   validate = positive},
                  <<"token_bytes">> => #option{type = integer,
                                               validate = positive},
                  <<"max_file_size">> => #option{type = integer,
                                                 validate = positive},
                  <<"s3">> => s3_spec()
        },
        required = [<<"s3">>]
    }.

s3_spec() ->
    #section{
        items = #{<<"bucket_url">> => #option{type = string,
                                              validate = url},
                  <<"add_acl">> => #option{type = boolean},
                  <<"region">> => #option{type = string},
                  <<"access_key_id">> => #option{type = string},
                  <<"secret_access_key">> => #option{type = string}
        },
        required = [<<"bucket_url">>, <<"region">>, <<"access_key_id">>, <<"secret_access_key">>]
    }.

-spec disco_local_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_local_items(Acc = #{to_jid := #jid{lserver = Host}, node := <<>>, lang := Lang}) ->
    mongoose_disco:add_items([#{jid => subhost(Host), name => my_disco_name(Lang)}], Acc);
disco_local_items(Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec disco_identity(ejabberd:lang()) -> [mongoose_disco:identity()].
disco_identity(Lang) ->
    [#{category => <<"store">>,
       type => <<"file">>,
       name => my_disco_name(Lang)}].

-spec disco_info(jid:lserver()) -> [exml:element()].
disco_info(SubHost) ->
    {ok, HostType} = mongoose_domain_api:get_subdomain_host_type(SubHost),
    case max_file_size(HostType) of
        undefined ->
            [];
        MaxFileSize ->
            MaxFileSizeBin = integer_to_binary(MaxFileSize),
            [get_disco_info_form(MaxFileSizeBin)]
    end.

-spec subhost(mongooseim:host_type()) -> mongooseim:domain_name().
subhost(HostType) ->
    %% TODO: this works only for statically configured hosts, when HostType =:= Domain.
    DefaultSubdomainPattern =
        mongoose_subdomain_utils:make_subdomain_pattern(?DEFAULT_SUBHOST),
    gen_mod:get_module_opt_subhost(HostType, ?MODULE, DefaultSubdomainPattern).

-spec subdomain_pattern(mongooseim:host_type()) ->
    mongoose_subdomain_utils:subdomain_pattern().
subdomain_pattern(HostType) ->
    DefaultSubdomainPattern =
        mongoose_subdomain_utils:make_subdomain_pattern(?DEFAULT_SUBHOST),
    gen_mod:get_module_opt(HostType, ?MODULE, host, DefaultSubdomainPattern).

-spec my_disco_name(ejabberd:lang()) -> binary().
my_disco_name(Lang) ->
    translate:translate(Lang, <<"HTTP File Upload">>).


-spec compose_iq_reply(IQ :: jlib:iq(),
		       PutUrl :: binary(),
		       GetUrl :: binary(),
                       Headers :: #{binary() => binary()}) ->
    Reply :: jlib:iq().
compose_iq_reply(IQ, PutUrl, GetUrl, Headers) ->
    Slot = #xmlel{
              name     = <<"slot">>,
              attrs    = [{<<"xmlns">>, ?NS_HTTP_UPLOAD_030}],
              children = [create_url_xmlel(<<"put">>, PutUrl, Headers),
                          create_url_xmlel(<<"get">>, GetUrl, #{})]},
    IQ#iq{type = result, sub_el =[Slot]}.


-spec token_bytes(jid:server()) -> pos_integer().
token_bytes(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, token_bytes, ?DEFAULT_TOKEN_BYTES).


-spec max_file_size(jid:server()) -> pos_integer() | undefined.
max_file_size(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_file_size, ?DEFAULT_MAX_FILE_SIZE).


-spec with_default_backend(Opts :: proplists:proplist()) -> proplists:proplist().
with_default_backend(Opts) ->
    case lists:keyfind(backend, 1, Opts) of
        false -> [{backend, s3} | Opts];
        _ -> Opts
    end.


-spec module_opts(jid:server()) -> proplists:proplist().
module_opts(Host) ->
    gen_mod:get_module_opts(Host, ?MODULE).


-spec generate_token(jid:server()) -> binary().
generate_token(Host) ->
    base16:encode(crypto:strong_rand_bytes(token_bytes(Host))).


-spec file_too_large_error(MaxFileSize :: non_neg_integer()) -> exml:element().
file_too_large_error(MaxFileSize) ->
    MaxFileSizeBin = integer_to_binary(MaxFileSize),
    MaxSizeEl = #xmlel{name = <<"max-file-size">>,
                       children = [#xmlcdata{content = MaxFileSizeBin}]},
    FileTooLargeEl = #xmlel{name = <<"file-too-large">>,
                            attrs = [{<<"xmlns">>, ?NS_HTTP_UPLOAD_030}],
                            children = [MaxSizeEl]},
    Error0 = mongoose_xmpp_errors:not_acceptable(),
    Error0#xmlel{children = [FileTooLargeEl | Error0#xmlel.children]}.


-spec parse_request(Request :: exml:element()) ->
    {Filename :: binary(), Size :: integer(), ContentType :: binary() | undefined} | bad_request.
parse_request(Request) ->
    Keys = [<<"filename">>, <<"size">>, <<"content-type">>],
    [Filename, SizeBin, ContentType] = [exml_query:attr(Request, K) || K <- Keys],
    Size = (catch erlang:binary_to_integer(SizeBin)),
    case is_nonempty_binary(Filename) andalso is_positive_integer(Size) of
        false -> bad_request;
        true -> {Filename, Size, ContentType}
    end.


-spec get_disco_info_form(MaxFileSizeBin :: binary()) -> exml:element().
get_disco_info_form(MaxFileSizeBin) ->
    #xmlel{name     = <<"x">>,
           attrs    = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
           children = [jlib:form_field({<<"FORM_TYPE">>, <<"hidden">>, ?NS_HTTP_UPLOAD_030}),
                       jlib:form_field({<<"max-file-size">>, MaxFileSizeBin})]}.


-spec header_to_xmlel({Key :: binary(), Value :: binary()}) -> exml:element().
header_to_xmlel({Key, Value}) ->
    #xmlel{name = <<"header">>,
           attrs = [{<<"name">>, Key}],
           children = [#xmlcdata{content = Value}]}.


-spec create_url_xmlel(Name :: binary(), Url :: binary(), Headers :: #{binary() => binary()}) ->
    exml:element().
create_url_xmlel(Name, Url, Headers) ->
    HeadersXml = [header_to_xmlel(H) || H <- maps:to_list(Headers)],
    #xmlel{name = Name, attrs = [{<<"url">>, Url}], children = HeadersXml}.


-spec is_nonempty_binary(term()) -> boolean().
is_nonempty_binary(<<_, _/binary>>) -> true;
is_nonempty_binary(_) -> false.


-spec is_positive_integer(term()) -> boolean().
is_positive_integer(X) when is_integer(X) -> X > 0;
is_positive_integer(_) -> false.

config_metrics(Host) ->
    OptsToReport = [{backend, s3}], % list of tuples {option, default_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
