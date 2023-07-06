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

-xep([{xep, 363}, {version, "1.1.0"}]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-define(DEFAULT_TOKEN_BYTES, 32).
-define(DEFAULT_MAX_FILE_SIZE, 10 * 1024 * 1024). % 10 MB
-define(DEFAULT_SUBHOST, <<"upload.@HOST@">>).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% IQ and hooks handlers
-export([process_iq/5,
         process_disco_iq/5,
         disco_local_items/3]).

%% API
-export([get_urls/5]).

%% mongoose_module_metrics callbacks
-export([config_metrics/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(HostType, Opts = #{iqdisc := IQDisc}) ->
    SubdomainPattern = subdomain_pattern(HostType),
    PacketHandler = mongoose_packet_handler:new(ejabberd_local),

    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern, PacketHandler),
    [gen_iq_handler:add_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace,
                                                 Component, Fn, #{}, IQDisc) ||
        {Component, Namespace, Fn} <- iq_handlers()],
    mod_http_upload_backend:init(HostType, Opts),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    SubdomainPattern = subdomain_pattern(HostType),

    [gen_iq_handler:remove_iq_handler_for_subdomain(HostType, SubdomainPattern, Namespace,
                                                    Component) ||
        {Component, Namespace, _Fn} <- iq_handlers()],

    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern),
    ok.

iq_handlers() ->
    [{ejabberd_local, ?NS_HTTP_UPLOAD_030, fun ?MODULE:process_iq/5},
     {ejabberd_local, ?NS_DISCO_INFO, fun ?MODULE:process_disco_iq/5}].

hooks(HostType) ->
    [{disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 90}].

%%--------------------------------------------------------------------
%% config_spec
%%--------------------------------------------------------------------

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                  <<"host">> => #option{type = binary,
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
        defaults = #{<<"iqdisc">> => one_queue,
                     <<"host">> => <<"upload.@HOST@">>,
                     <<"backend">> => s3,
                     <<"expiration_time">> => 60,
                     <<"token_bytes">> => 32,
                     <<"max_file_size">> => ?DEFAULT_MAX_FILE_SIZE
        },
        required = [<<"s3">>]
    }.

s3_spec() ->
    #section{
        items = #{<<"bucket_url">> => #option{type = binary,
                                              validate = url},
                  <<"add_acl">> => #option{type = boolean},
                  <<"region">> => #option{type = binary},
                  <<"access_key_id">> => #option{type = binary},
                  <<"secret_access_key">> => #option{type = binary}
        },
        defaults = #{<<"add_acl">> => false},
        required = [<<"bucket_url">>, <<"region">>, <<"access_key_id">>, <<"secret_access_key">>]
    }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%--------------------------------------------------------------------
%% IQ and hook handlers
%%--------------------------------------------------------------------

-spec process_iq(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                 IQ :: jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq() | ignore}.
process_iq(Acc, _From, _To, IQ = #iq{type = set, lang = Lang, sub_el = SubEl}, _Extra) ->
    Error = mongoose_xmpp_errors:not_allowed(Lang, <<"IQ set is not allowed for HTTP upload">>),
    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
process_iq(Acc,  _From, _To, IQ = #iq{type = get, sub_el = Request}, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = case parse_request(Request) of
        {Filename, Size, ContentType} ->
            Opts = module_opts(HostType),
            case get_urls_helper(HostType, Filename, Size, ContentType, Opts) of
                {PutUrl, GetUrl, Headers} ->
                    compose_iq_reply(IQ, PutUrl, GetUrl, Headers);
                file_too_large_error ->
                    IQ#iq{type = error, sub_el = [file_too_large_error(max_file_size(HostType))]}
            end;

        bad_request ->
            IQ#iq{type = error, sub_el = [Request, mongoose_xmpp_errors:bad_request()]}
    end,
    {Acc, Res}.

-spec process_disco_iq(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(),
                       IQ :: jlib:iq(), map()) ->
          {mongoose_acc:t(), jlib:iq()}.
process_disco_iq(Acc, _From, _To, #iq{type = set, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    ErrorMsg = <<"IQ set is not allowed for service discovery">>,
    Error = mongoose_xmpp_errors:not_allowed(Lang, ErrorMsg),
    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
process_disco_iq(Acc, _From, _To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    case Node of
        <<>> ->
            Identity = mongoose_disco:identities_to_xml(disco_identity(Lang)),
            Info = disco_info(mongoose_acc:host_type(Acc)),
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

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_items(Acc = #{host_type := HostType, to_jid := #jid{lserver = Domain}, node := <<>>, lang := Lang}, _, _) ->
    {ok, mongoose_disco:add_items([#{jid => subdomain(HostType, Domain), name => my_disco_name(Lang)}], Acc)};
disco_local_items(Acc, _, _) ->
    {ok, Acc}.

-spec get_urls(HostType :: mongooseim:host_type(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
                  file_too_large_error | {PutURL :: binary(), GetURL :: binary(),
                                          Headers :: #{binary() => binary()}}.
get_urls(HostType, Filename, Size, ContentType, Timeout) ->
    Opts = module_opts(HostType),
    get_urls_helper(HostType, Filename, Size, ContentType, Opts#{expiration_time := Timeout}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_urls_helper(HostType, Filename, Size, ContentType, Opts) ->
    MaxFileSize = max_file_size(HostType),
    case Size =< MaxFileSize of
        true ->
            UTCDateTime = calendar:universal_time(),
            Token = generate_token(HostType),
            mod_http_upload_backend:create_slot(HostType, UTCDateTime, Token, Filename,
                                                ContentType, Size, Opts);
        false ->
            file_too_large_error
    end.

-spec disco_identity(ejabberd:lang()) -> [mongoose_disco:identity()].
disco_identity(Lang) ->
    [#{category => <<"store">>,
       type => <<"file">>,
       name => my_disco_name(Lang)}].

-spec disco_info(mongooseim:host_type()) -> [exml:element()].
disco_info(HostType) ->
    MaxFileSize = max_file_size(HostType),
    MaxFileSizeBin = integer_to_binary(MaxFileSize),
    [get_disco_info_form(MaxFileSizeBin)].

-spec subdomain(mongooseim:host_type(), mongooseim:domain_name()) -> mongooseim:domain_name().
subdomain(HostType, Domain) ->
    SubdomainPattern = subdomain_pattern(HostType),
    mongoose_subdomain_utils:get_fqdn(SubdomainPattern, Domain).

-spec subdomain_pattern(mongooseim:host_type()) ->
    mongoose_subdomain_utils:subdomain_pattern().
subdomain_pattern(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, host).

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


-spec token_bytes(mongooseim:host_type()) -> pos_integer().
token_bytes(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, token_bytes).


-spec max_file_size(mongooseim:host_type()) -> pos_integer() | undefined.
max_file_size(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, max_file_size).

-spec module_opts(mongooseim:host_type())-> gen_mod:module_opts().
module_opts(HostType) ->
    gen_mod:get_module_opts(HostType, ?MODULE).


-spec generate_token(mongooseim:host_type()) -> binary().
generate_token(HostType) ->
    base16:encode(crypto:strong_rand_bytes(token_bytes(HostType))).


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
    Fields = [#{var => <<"max-file-size">>, values => [MaxFileSizeBin]}],
    mongoose_data_forms:form(#{type => <<"result">>, ns => ?NS_HTTP_UPLOAD_030, fields => Fields}).


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

config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
