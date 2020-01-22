%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of XEP-0357
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push).
-author('rafal.slota@erlang-solutions.com').
-behavior(gen_mod).
-behavior(mod_event_pusher).
-behaviour(mongoose_module_metrics).
-xep([{xep, 357}, {version, "0.2.1"}]).

-include("mod_event_pusher_events.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-define(SESSION_KEY, publish_service).

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod behaviour
-export([start/2, stop/1]).

%% mod_event_pusher behaviour
-export([push_event/3]).

%% Hooks and IQ handlers
-export([iq_handler/4,
         remove_user/3]).

%% Plugin utils
-export([cast/3]).
-export([virtual_pubsub_hosts/1]).
-export([disable_node/3]).

%% Debug & testing
-export([add_virtual_pubsub_host/2]).

%% Types
-type publish_service() :: {PubSub :: jid:jid(), Node :: pubsub_node(), Form :: form()}.
-type pubsub_node() :: binary().
-type form_field() :: {Name :: binary(), Value :: binary()}.
-type form() :: [form_field()].

-export_type([pubsub_node/0, form_field/0, form/0]).
-export_type([publish_service/0]).

%%--------------------------------------------------------------------
%% DB backend behaviour definition
%%--------------------------------------------------------------------

-callback init(Host :: jid:server(), Opts :: list()) -> ok.

-callback enable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                 Node :: pubsub_node(), Form :: form()) ->
    ok | {error, Reason :: term()}.

-callback disable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                  Node :: pubsub_node()) ->
    ok | {error, Reason :: term()}.

-callback get_publish_services(User :: jid:jid()) ->
    {ok, [publish_service()]} | {error, Reason :: term()}.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_event_pusher_push starting on host ~p", [Host]),

    expand_and_store_virtual_pubsub_hosts(Host, Opts),

    WpoolOpts = [{strategy, available_worker} | gen_mod:get_opt(wpool, Opts, [])],
    {ok, _} = mongoose_wpool:start(generic, Host, pusher_push, WpoolOpts),

    gen_mod:start_backend_module(?MODULE, Opts, [enable, disable, get_publish_services]),
    mod_event_pusher_push_backend:init(Host, Opts),

    mod_event_pusher_push_plugin:init(Host),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?NS_PUSH),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),

    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 90),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 90),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PUSH),
    mod_disco:unregister_feature(Host, ?NS_PUSH),

    mongoose_wpool:stop(generic, Host, pusher_push),
    ok.

%%--------------------------------------------------------------------
%% mod_event_pusher callbacks
%%--------------------------------------------------------------------
-spec push_event(Acc :: mongoose_acc:t(), Host :: jid:lserver(),
                 Event :: mod_event_pusher:event()) -> mongoose_acc:t().
push_event(Acc, Host, Event = #chat_event{direction = out, to = To,
                                          type = Type}) when Type =:= groupchat;
                                                             Type =:= chat ->
    BareRecipient = jid:to_bare(To),
    do_push_event(Acc, Host, Event, BareRecipient);
push_event(Acc, Host, Event = #unack_msg_event{to = To}) ->
    BareRecipient = jid:to_bare(To),
    do_push_event(Acc, Host, Event, BareRecipient);
push_event(Acc, _, _) ->
    Acc.

%%--------------------------------------------------------------------
%% Hooks and IQ handlers
%%--------------------------------------------------------------------
-spec remove_user(Acc :: mongoose_acc:t(), LUser :: binary(), LServer :: binary()) ->
    mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    R = mod_event_pusher_push_backend:disable(jid:make_noprep(LUser, LServer, <<>>),
                                              undefined, undefined),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, LUser, LServer}),
    Acc.

-spec iq_handler(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq() | ignore}.
iq_handler(_From, _To, Acc, IQ = #iq{type = get, sub_el = SubEl}) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
iq_handler(From, _To, Acc, IQ = #iq{type = set, sub_el = Request}) ->
    Res = case parse_request(Request) of
              {enable, BarePubSubJID, Node, FormFields} ->
                  maybe_enable_node(From, BarePubSubJID, Node, FormFields, IQ);
              {disable, BarePubsubJID, Node} ->
                  ok = disable_node(From, BarePubsubJID, Node),
                  IQ#iq{type = result, sub_el = []};
              bad_request ->
                  IQ#iq{type = error, sub_el = [Request, mongoose_xmpp_errors:bad_request()]}
          end,
    {Acc, Res}.

%%--------------------------------------------------------------------
%% Plugin utils API
%%--------------------------------------------------------------------
-spec disable_node(UserJID :: jid:jid(), BarePubSubJID :: jid:jid(),
                   Node :: pubsub_node()) -> ok | {error, Reason :: term()}.
disable_node(UserJID, BarePubSubJID, Node) ->
    BareUserJID = jid:to_bare(UserJID),
    maybe_remove_push_node_from_sessions_info(BareUserJID, BarePubSubJID, Node),
    mod_event_pusher_push_backend:disable(BareUserJID, BarePubSubJID, Node).

-spec cast(Host :: jid:server(), F :: function(), A :: [any()]) -> any().
cast(Host, F, A) ->
    mongoose_wpool:cast(generic, Host, pusher_push, {erlang, apply, [F,A]}).

-spec virtual_pubsub_hosts(jid:server()) -> [jid:server()].
virtual_pubsub_hosts(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, []).

%%--------------------------------------------------------------------
%% Debug & testing API
%%--------------------------------------------------------------------
-spec add_virtual_pubsub_host(Host :: jid:server(), VirtualHost :: jid:server()) -> any().
add_virtual_pubsub_host(Host, VirtualHost) ->
    %% add_virtual_pubsub_host/2 is non-atomic interface, so execution in parallel
    %% environment can result in race conditions.
    VHosts0 = virtual_pubsub_hosts(Host),
    VHosts = lists:usort(gen_mod:make_subhosts(VirtualHost, Host) ++ VHosts0),
    gen_mod:set_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, VHosts).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------
-spec expand_and_store_virtual_pubsub_hosts(Host :: jid:server(), Opts :: list()) -> any().
expand_and_store_virtual_pubsub_hosts(Host, Opts) ->
    ExpandedVHosts = lists:usort([SubHost || Spec <- gen_mod:get_opt(virtual_pubsub_hosts, Opts, []),
                                             SubHost <- gen_mod:make_subhosts(Spec, Host)]),
    gen_mod:set_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, ExpandedVHosts).

-spec do_push_event(mongoose_acc:t(), jid:server(), mod_event_pusher:event(), jid:jid()) ->
    mongoose_acc:t().
do_push_event(Acc, Host, Event, BareRecipient) ->
    case mod_event_pusher_push_plugin:prepare_notification(Host, Acc, Event) of
        skip -> Acc;
        Payload ->
            {ok, Services} = mod_event_pusher_push_backend:get_publish_services(BareRecipient),
            FilteredService = mod_event_pusher_push_plugin:should_publish(Host, Acc, Event,
                                                                          Services),
            mod_event_pusher_push_plugin:publish_notification(Host, Acc, Event,
                                                              Payload, FilteredService)
    end.

-spec parse_request(Request :: exml:element()) ->
    {enable, jid:jid(), pubsub_node(), form()} |
    {disable, jid:jid(), pubsub_node() | undefined} |
    bad_request.
parse_request(#xmlel{name = <<"enable">>} = Request) ->
    JID = jid:from_binary(exml_query:attr(Request, <<"jid">>, <<>>)),
    Node = exml_query:attr(Request, <<"node">>, <<>>), %% Treat unset node as empty - both forbidden
    Form = exml_query:subelement(Request, <<"x">>),

    case {JID, Node, parse_form(Form)} of
        {_, _, invalid_form}            -> bad_request;
        {_, <<>>, _}                    -> bad_request;
        {error, _, _}                   -> bad_request;
        {#jid{lserver = <<>>}, _, _}    -> bad_request;
        {JID, Node, FormFields} ->
            {enable, jid:to_bare(JID), Node, FormFields}
    end;
parse_request(#xmlel{name = <<"disable">>} = Request) ->
    JID = jid:from_binary(exml_query:attr(Request, <<"jid">>, <<>>)),
    Node = exml_query:attr(Request, <<"node">>, undefined),

    case {JID, Node} of
        {error, _}                  -> bad_request;
        {_, <<>>}                   -> bad_request; %% Node may not be set, but shouldn't be empty
        {#jid{lserver = <<>>}, _}   -> bad_request;
        {JID, Node} ->
            {disable, jid:to_bare(JID), Node}
    end;
parse_request(_) ->
    bad_request.

-spec parse_form(undefined | exml:element()) -> invalid_form | form().
parse_form(undefined) ->
    [];
parse_form(Form) ->
    IsForm = ?NS_XDATA == exml_query:attr(Form, <<"xmlns">>),
    IsSubmit = <<"submit">> == exml_query:attr(Form, <<"type">>, <<"submit">>),

    FieldsXML = exml_query:subelements(Form, <<"field">>),
    Fields = [{exml_query:attr(Field, <<"var">>),
               exml_query:path(Field, [{element, <<"value">>}, cdata])} || Field <- FieldsXML],
    {[{_, FormType}], CustomFields} = lists:partition(
                                        fun({Name, _}) ->
                                                Name == <<"FORM_TYPE">>
                                        end, Fields),
    IsFormTypeCorrect = ?NS_PUBSUB_PUB_OPTIONS == FormType,

    case IsForm andalso IsSubmit andalso IsFormTypeCorrect of
        true ->
            CustomFields;
        false ->
            invalid_form
    end.

-spec maybe_enable_node(jid:jid(), jid:jid(), pubsub_node(), form(), jlib:iq()) -> jlib:iq().
maybe_enable_node(#jid{lserver = Host} = From, BarePubSubJID, Node, FormFields, IQ) ->
    AllKnownDomains = ejabberd_router:dirty_get_all_domains() ++ virtual_pubsub_hosts(Host),
    case lists:member(BarePubSubJID#jid.lserver, AllKnownDomains) of
        true ->
            ok = mod_event_pusher_push_backend:enable(jid:to_bare(From), BarePubSubJID, Node, FormFields),
            store_session_info(From, {BarePubSubJID, Node, FormFields}),
            IQ#iq{type = result, sub_el = []};
        false ->
            NewSubEl = [IQ#iq.sub_el, mongoose_xmpp_errors:remote_server_not_found()],
            IQ#iq{type = error, sub_el = NewSubEl}
    end.

-spec store_session_info(jid:jid(), publish_service()) -> any().
store_session_info(Jid, Service) ->
    ejabberd_sm:store_info(Jid, {?SESSION_KEY, Service}).

-spec maybe_remove_push_node_from_sessions_info(jid:jid(), jid:jid(), pubsub_node()) -> ok.
maybe_remove_push_node_from_sessions_info(From, PubSubJid, Node) ->
    AllSessions = ejabberd_sm:get_raw_sessions(From),
    find_and_remove_push_node(From, AllSessions, PubSubJid, Node).

-spec find_and_remove_push_node(jid:jid(), [ejabberd_sm:session()],
                                jid:jid(), pubsub_node()) -> ok.
find_and_remove_push_node(_From, [], _,_) ->
    ok;
find_and_remove_push_node(From, [RawSession | Rest], PubSubJid, Node) ->
    case my_push_node(RawSession, PubSubJid, Node) of
        true ->
            LResource  = mongoose_session:get_resource(RawSession),
            JID = jid:replace_resource(From, LResource),
            ejabberd_sm:remove_info(JID, ?SESSION_KEY),
            find_and_remove_push_node(From, Rest, PubSubJid, Node);
        false ->
            find_and_remove_push_node(From, Rest, PubSubJid, Node)
    end.

-spec my_push_node(ejabberd_sm:session(), jid:jid(), pubsub_node()) -> boolean().
my_push_node(RawSession, PubSubJid, Node) ->
    SInfo = mongoose_session:get_info(RawSession),
    case lists:keyfind(?SESSION_KEY, 1, SInfo) of
        {?SESSION_KEY, {PubSubJid, Node, _}} ->
            true;
        {?SESSION_KEY, {PubSubJid, _, _}} when Node =:= undefined ->
            %% The node is undefined which means that a user wants to
            %% disable all the push nodes for the specified service
            true;
        _ ->
            false
    end.
