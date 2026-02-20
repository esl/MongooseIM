-module(mod_external_filter).

-author("pawel.dlugosz@erlang-solutions.com").

-behaviour(gen_mod).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod callbacks
-export([start/2, stop/1, deps/2, supported_features/0, config_spec/0, hooks/1]).
%% Hook handlers
-export([user_send_message/3]).

-record(request_params,
        {body :: binary(),
         packet :: exml:element(),
         from :: jid:jid(),
         to :: jid:jid(),
         id :: pos_integer()}).

-type request_params() :: #request_params{}.

-define(QUERY_DOCUMENT,
        <<"mutation verifyMessage(
               $messageBody: String!,
               $rawMessage: String!,
               $receiver: String!,
               $sender: String!,
               $messageId: String!) {
           verify(
             messageBody: $messageBody
             rawMessage: $rawMessage
             receiver: $receiver
             sender: $sender
             messageId: $messageId
           ) {
             action
           }
         }">>).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, _Opts) ->
    [{mod_stanzaid, #{}, hard}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"pool_tag">> => #option{type = atom, validate = pool_name}}}.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{user_send_message, HostType, fun ?MODULE:user_send_message/3, #{}, 3}].

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------

-spec user_send_message(mongoose_acc:t(),
                        mongoose_c2s_hooks:params(),
                        gen_hook:extra()) ->
                           mongoose_c2s_hooks:result().
user_send_message(Acc, _, _) ->
    case fetch_message(Acc) of
        {ok, Params} ->
            call_filter(Acc, Params);
        no_body ->
            {ok, Acc}
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec fetch_message(mongoose_acc:t()) -> {ok, request_params()} | no_body.
fetch_message(Acc) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case exml_query:subelement(Packet, <<"body">>) of
        #xmlel{} = BodyElem ->
            Body = exml_query:cdata(BodyElem),
            Id = fetch_stable_id(Acc),
            Params =
                #request_params{body = Body,
                                packet = Packet,
                                from = From,
                                to = To,
                                id = Id},
            {ok, Params};
        undefined ->
            no_body
    end.

-spec fetch_stable_id(mongoose_acc:t()) -> pos_integer().
fetch_stable_id(Acc) ->
    case mongoose_acc:get(stable_stanza_id, value, undefined, Acc) of
        undefined ->
            error(stable_stanza_id_not_found);
        Id ->
            Id
    end.

-spec call_filter(Acc, request_params()) -> {ok, Acc} | {stop, Acc}.
call_filter(Acc, Params) ->
    HostType = mongoose_acc:host_type(Acc),
    Query = create_query(Params),
    case make_request(HostType, Query) of
        allow ->
            {ok, Acc};
        block ->
            {stop, Acc}
    end.

-spec create_query(request_params()) -> iodata().
create_query(#request_params{body = Body,
                             packet = Packet,
                             from = From,
                             to = To,
                             id = Id}) ->
    Query =
        #{query => ?QUERY_DOCUMENT,
          variables =>
              #{messageBody => Body,
                rawMessage => exml:to_binary(Packet),
                sender => jid:to_binary(From),
                receiver => jid:to_binary(To),
                messageId => integer_to_binary(Id)}},
    jiffy:encode(Query).

-spec make_request(mongooseim:host_type(), binary()) -> allow | block.
make_request(HostType, Query) ->
    PoolTag = gen_mod:get_module_opt(HostType, ?MODULE, pool_tag),
    Headers =
        [{<<"Accept">>, <<"application/json">>},
         {<<"Content-Type">>, <<"application/graphql-response+json">>}],
    case mongoose_http_client:post(HostType, PoolTag, <<>>, Headers, Query) of
        {ok, {<<"200">>, RawResponse}} ->
            Response = jiffy:decode(RawResponse, [return_maps]),
            parse_response(Response);
        {ok, {Code, RawResponse}} ->
            ?LOG_WARNING(#{what => external_filter_server_error,
                           response_code => Code,
                           response_body => RawResponse,
                           host_type => HostType}),
            allow;
        {error, Reason} ->
            ?LOG_ERROR(#{what => external_filter_call_crashed,
                         reason => Reason,
                         host_type => HostType}),
            allow
    end.

parse_response(#{<<"data">> := #{<<"verify">> := #{<<"action">> := <<"BLOCK">>}}}) ->
    block;
parse_response(#{<<"data">> := #{<<"verify">> := #{<<"action">> := <<"ALLOW">>}}}) ->
    allow;
parse_response(#{<<"errors">> := Errors}) ->
    ?LOG_ERROR(#{what => external_filter_bad_request, errors => Errors}),
    allow.
