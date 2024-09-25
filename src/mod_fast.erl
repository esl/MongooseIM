-module(mod_fast).
-xep([{xep, 484}, {version, "0.2.0"}]).
-behaviour(gen_mod).
-include("mongoose_ns.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0]).

%% hooks handlers
-export([sasl2_stream_features/3,
         sasl2_start/3,
         sasl2_success/3]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{sasl2_stream_features, HostType, fun ?MODULE:sasl2_stream_features/3, #{}, 50},
     {sasl2_start, HostType, fun ?MODULE:sasl2_start/3, #{}, 50},
     {sasl2_success, HostType, fun ?MODULE:sasl2_success/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        defaults = #{}
    }.

supported_features() -> [dynamic_domains].

-spec sasl2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
sasl2_stream_features(Acc, _, _) ->
    {ok, [fast() | Acc]}.

fast() ->
    #xmlel{name = <<"fast">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}],
           children = mechanisms_elems(mechanisms())}.

mechanisms_elems(Mechs) ->
    [#xmlel{name = <<"fast">>,
           children = [#xmlcdata{content = Mech}]} || Mech <- Mechs].

mechanisms() ->
    [<<"HT-SHA-256-ENDP">>, <<"HT-SHA-256-EXPR">>, <<"HT-SHA-256-NONE">>].

-spec sasl2_start(SaslAcc, #{stanza := exml:element()}, gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_start(SaslAcc, #{stanza := El}, _) ->
    case exml_query:path(El, [{element_with_ns, <<"request-token">>, ?NS_FAST}]) of
        undefined ->
            {ok, SaslAcc};
        Request ->
            Mech = exml_query:attr(Request, <<"mechanism">>),
            case Mech of
                <<"HT-SHA-256-ENDP">> ->
                    {ok, mod_sasl2:put_inline_request(SaslAcc, ?MODULE, Request)};
                _ ->
                     {ok, SaslAcc}
            end
    end.

-spec sasl2_success(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
sasl2_success(SaslAcc, _, #{host_type := HostType}) ->
    case mod_sasl2:get_inline_request(SaslAcc, ?MODULE, undefined) of
        undefined ->
            {ok, SaslAcc};
        #{request := Request} ->
            Response = make_fast_token_response(Request),
            SaslAcc2 = mod_sasl2:update_inline_request(SaslAcc, ?MODULE, Response, success),
            {ok, SaslAcc2}
    end.

make_fast_token_response(Request) ->
    Expire = <<"2020-03-12T14:36:15Z">>,
    Token = <<"WXZzciBwYmFmdmZnZiBqdmd1IGp2eXFhcmZm">>,
    #xmlel{name = <<"token">>,
           attrs = [{<<"xmlns">>, ?NS_FAST}, {<<"expire">>, Expire},
                    {<<"token">>, Token}]}.
