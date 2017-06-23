%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

-module(mod_foreign).
-author('szymon.mentel@erlang-solutions.com').
-behaviour(gen_mod).

-include("jlib.hrl").
-include("ejabberd.hrl").

-define(DEFAULT_SUBHOST, <<"foreign.@HOST@">>).

-export([start/2, stop/1]).

%% IQ handler
-export([iq_handler/3]).

%% Publishing
-export([publish_to_pubsub/4]).


%% Hook implementations
-export([get_disco_items/5]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type foreign_request() :: jlib:xmlel().
-type foreign_response() :: jlib:xmlel().

-type publish_service() :: pubsub. %% | muc | muc_light.
-type publish_context() :: {publish_service(),
                            Host :: ejabberd:server(),
                            PublishServiceEntityName :: binary()}.
-type publish_item() :: {publish, publish_context(), on_request() | on_response()}.

-type on_request() :: fun((foreign_request()) -> term()).
-type on_response() :: fun((foreign_response()) -> term()).

-export_type([foreign_request/0, on_response/0]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback make_request(ejabberd:server(),
                       foreign_request(),
                       on_response()) -> ok | {error, term()}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SubHost = subhost(Host),
    mod_disco:register_subhost(Host, SubHost),
    mongoose_subhosts:register(Host, SubHost),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_disco_items, 90),
    gen_iq_handler:add_iq_handler(ejabberd_local, SubHost,
                                  ?NS_FOREIGN_EVENT, ?MODULE, iq_handler, IQDisc),
    start_backends(Host, proplists:get_value(backends, Opts, [])).

-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    SubHost = subhost(Host),
    stop_backends(Host),
    gen_iq_handler:remove_iq_handler(ejabberd_local, SubHost, ?NS_FOREIGN_EVENT),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_disco_items, 90),
    mongoose_subhosts:unregister(SubHost),
    mod_disco:unregister_subhost(Host, SubHost).

-spec get_disco_items(Acc :: term(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                      Node :: binary(), ejabberd:lang()) -> {result, [jlib:xmlel()]} | term().
get_disco_items({result, Nodes}, _From, #jid{lserver = Host} = _To, <<"">>, Lang) ->
    Item = #xmlel{name  = <<"item">>,
                  attrs = [{<<"jid">>, subhost(Host)}, {<<"name">>, my_disco_name(Lang)}]},
    {result, [Item | Nodes]};
get_disco_items(empty, From, To, Node, Lang) ->
    get_disco_items({result, []}, From, To, Node, Lang);
get_disco_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec iq_handler(From :: jid(), To :: jid(), iq()) -> iq() | ignore.
iq_handler(_From, _To, #iq{type = get} = IQ) ->
    IQ#iq{type = error, sub_el = ?ERR_NOT_ALLOWED};
iq_handler(From = #jid{lserver = Host},
           _To,
           #iq{type = set, sub_el = ForeignEvent} = IQ) ->
    case parse_foreign_event(From, ForeignEvent) of
        {ok, Type, Request, OnReqPublish, OnRespPublish} ->
            maybe_dispatch_request(Type,
                                   Host,
                                   Request,
                                   OnReqPublish,
                                   OnRespPublish,
                                   IQ);
        error ->
            IQ#iq{type = error, sub_el = ?ERR_BAD_REQUEST}
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec subhost(Host :: ejabberd:server()) -> binary().
subhost(Host) ->
    gen_mod:get_module_opt_subhost(Host, ?MODULE, ?DEFAULT_SUBHOST).


-spec my_disco_name(ejabberd:lang()) -> binary().
my_disco_name(Lang) ->
    translate:translate(Lang, <<"Foreign-Event">>).

%%--------------------------------------------------------------------
%% Dispatching
%%--------------------------------------------------------------------

-spec maybe_dispatch_request(Host:: ejabberd:server(),
                             Type :: binary(),
                             Request :: xmlel(),
                             OnRequestPublishes :: [publish_item()],
                             OnResponsePublishes :: [publish_item()],
                             iq()) -> iq().
maybe_dispatch_request(<<"http">>, Host, Request, OnReqPublish, OnRespPublish, IQ) ->
    publish_request(OnReqPublish, Request),
    case mod_foreign_http:make_request(Host,
                                       Request,
                                       publish_response_fun(OnRespPublish))
    of
        ok ->
            IQ#iq{type = result, sub_el = []};
        error ->
            IQ#iq{type = error, sub_el = ?ERR_BAD_REQUEST}
    end;
maybe_dispatch_request(_, _, _, _, _, IQ) ->
    IQ#iq{type = error, sub_el = ?ERR_FEATURE_NOT_IMPLEMENTED}.


%%--------------------------------------------------------------------
%% IQ parsing
%%--------------------------------------------------------------------

-spec parse_foreign_event(jid(), xmlel()) -> Result when
      Result :: error
              | {ok, ReqType, Request, OnRequestPublishes, OnResponsePublishes},
      ReqType :: binary(),
      Request :: xmlel(),
      OnRequestPublishes :: [publish_item()],
      OnResponsePublishes :: [publish_item()].
parse_foreign_event(From, #xmlel{name = <<"foreign-event">>} = ForeignEvent) ->
    VerifiedRequest = case exml_query:subelement(ForeignEvent, <<"request">>) of
                          undefined ->
                              error;
                          Request ->
                              verify_request(Request)
                      end,
    Publish = case exml_query:paths(ForeignEvent, [{element, <<"publish">>}]) of
                  undefined ->
                      error;
                  PublishNodes ->
                      parse_publish_nodes(From, PublishNodes)
              end,
    case {VerifiedRequest, Publish} of
        {X, Y} when X == error orelse Y == error ->
            error;
        {{ok, VType, VRequest}, {ok, OnRequest, OnResponse}} ->
            {ok, VType, VRequest, OnRequest, OnResponse}
    end;
parse_foreign_event(_, _) -> error.

-spec verify_request(xmlel()) ->
    {ok, Type :: binary(), Request :: xmlel()} | error.
verify_request(Request) ->
    case parse_type(Request) of
        {ok, Type} ->
            {ok, Type, Request};
        error ->
            error
    end.

-spec parse_publish_nodes(jid(), [xmlel()]) -> Result when
      Result :: {ok, OnRequestPublishes, OnResponsePublishes} | error,
      OnRequestPublishes :: [publish_item()],
      OnResponsePublishes :: [publish_item()].
parse_publish_nodes(From, PublishNodes) ->
    parse_publish_nodes(From, PublishNodes, {[], []}).

-spec parse_publish_nodes(jid(), [xmlel()],
                          {[publish_item()], [publish_item()]}) ->
    error | {ok, [publish_item()], [publish_item()]}.
parse_publish_nodes(_From, [], {OnReqPubs, OnRespPubs}) ->
    {ok, OnReqPubs, OnRespPubs};
parse_publish_nodes(From, [El | Rest], {OnReqPubs, OnRespPubs}) ->
    Type = exml_query:attr(El, <<"type">>, undefined),
    ToService = exml_query:attr(El, <<"to">>, undefined),
    ServiceEntityName = exml_query:attr(El, <<"name">>, undefined),
    Publish = mk_publish(publish_service(ToService),
                         From,
                         ServiceEntityName),
    case {Type, Publish} of
        {T, P} when T == undefined orelse P == undefined ->
            error;
        {<<"request">>, P} ->
            parse_publish_nodes(From, Rest, {[P | OnReqPubs], OnRespPubs});
        {<<"response">>, P} ->
            parse_publish_nodes(From, Rest, {OnReqPubs, [P | OnRespPubs]})
    end.

-spec publish_service(binary()) -> publish_service().
publish_service(<<"pubsub">>) -> pubsub;
publish_service(_) -> undefined.

-spec mk_publish(publish_service(), jid(), mod_pubsub:nodeId()) ->
                        publish_item() | undefined.
mk_publish(pubsub, #jid{lserver = Host} = From, NodeName) ->
    {publish,
     _Context = {pubusb, Host, NodeName},
     fun(Payload) ->
            ?MODULE:publish_to_pubsub(Host, From, NodeName, [Payload])
     end};
mk_publish(_, _, _) -> undefined.

-spec parse_type(xmlel()) -> {ok, Type :: binary()} | error.
parse_type(Request) ->
    case exml_query:attr(Request, <<"type">>) of
        undefined ->
            error;
        Type ->
            {ok, Type}
    end.

%%--------------------------------------------------------------------
%% Publishing
%%--------------------------------------------------------------------

-spec publish_to_pubsub(ejabberd:server(), jlib:jid(), mod_pubsub:nodeId(),
                        mod_pubsub:payload()) -> {result, [xmlel(), ...]}.
publish_to_pubsub(Host, From, Node, Payload) ->
    SubHost = pubsub_subhost(Host),
    {result, _} = mod_pubsub:publish_item(SubHost, Host, Node, From, <<>>, Payload).

-spec pubsub_subhost(ejabberd:server()) -> ejabberd:server().
pubsub_subhost(Host) ->
    gen_mod:get_module_opt_subhost(Host, mod_pubsub, mod_pubsub:default_host()).

-spec publish_response_fun([publish_item()]) -> ok.
publish_response_fun(OnRespPublish) ->
    fun(Response) ->
            lists:foreach(fun({publish, _Context, Fun}) -> Fun(Response) end,
                          OnRespPublish)
    end.

-spec publish_request([publish_item()], xmlel()) -> ok.
publish_request(OnReqPublish, Request) ->
    lists:foreach(fun({publish, _Context, Fun}) -> 
                          Fun(#xmlel{name = <<"foreign-event">>,
                                     attrs = [{<<"xmlns">>, ?NS_FOREIGN_EVENT}],
                                     children = [Request]})
                  end,
                  OnReqPublish).

%%--------------------------------------------------------------------
%% Backends
%%--------------------------------------------------------------------

start_backends(Host, BackendsOpts) ->
    mod_foreign_http:start(Host, proplists:get_value(http, BackendsOpts, [])).

stop_backends(Host) ->
    mod_foreign_http:stop(Host).
