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
-type on_response() :: fun((foreign_response()) -> term()).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback make_request(foreign_request(), on_response()) -> ok | {error, term()}.

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
                                  ?NS_FOREIGN_EVENT, ?MODULE, iq_handler, IQDisc).


-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    SubHost = subhost(Host),
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
iq_handler(_From, _To, #iq{type = set, sub_el = ForeignEvent} = IQ) ->
    case parse_foreign_event(ForeignEvent) of
        {ok, Type, Request, _Publish} ->
            maybe_dispatch_request(Type, Request, IQ);
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

-spec maybe_dispatch_request(Type :: binary(), Request :: xmlel(), iq()) -> iq().
maybe_dispatch_request(<<"http">>, Request, IQ) ->
    mod_foreign_http:make_request(Request, fun(_) -> ok end),
    IQ#iq{type = result, sub_el = []};
maybe_dispatch_request(_, _, IQ) ->
    IQ#iq{type = error, sub_el = ?ERR_FEATURE_NOT_IMPLEMENTED}.

%%--------------------------------------------------------------------
%% IQ parsing
%%--------------------------------------------------------------------

-spec parse_foreign_event(xmlel()) ->
    {ok, Type :: binary(), Request :: xmlel(), PublishNodes :: [xmlel()]} | error.
parse_foreign_event(#xmlel{name = <<"foreign-event">>} = ForeignEvent) ->
    case exml_query:subelement(ForeignEvent, <<"request">>) of
        undefined ->
            error;
        Request ->
            verify_request(Request)
    end;
parse_foreign_event(_) -> error.

-spec verify_request(xmlel()) ->
    {ok, Type :: binary(), PublishNodes :: [xmlel()]} | error.
verify_request(Request) ->
    case parse_type(Request) of
        {ok, Type} ->
            {ok, Type, Request, []};
        error ->
            error
    end.

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

publish_to_pubsub(Host, From, Node, Payload) ->
    SubHost = pubsub_subhost(Host),
    {result, _} = mod_pubsub:publish_item(SubHost, Host, Node, From, <<>>, Payload).

pubsub_subhost(Host) ->
    gen_mod:get_module_opt_subhost(Host, mod_pubsub, mod_pubsub:default_host()).
