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
-xep([{xep, 357}, {version, "0.2.1"}]).

-include("mod_event_pusher_events.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hooks and IQ handlers
-export([iq_handler/4,
         handle_publish_response/4,
         remove_user/3,
         push_event/3]).

%% Plugin utils
-export([cast/3, cast/4]).
-export([virtual_pubsub_hosts/1]).

%% Debug & testing
-export([add_virtual_pubsub_host/2]).

%% Types
-export_type([pubsub_node/0, form_field/0, form/0]).
-export_type([publish_service/0]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-callback init(Host :: jid:server(), Opts :: list()) -> ok.
-callback enable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                 Node :: pubsub_node(), Form :: form()) ->
    ok | {error, Reason :: term()}.
-callback disable(UserJID :: jid:jid(), PubsubJID :: jid:jid(),
                  Node :: pubsub_node()) -> ok | {error, Reason :: term()}.
-callback get_publish_services(User :: jid:jid()) ->
    {ok, [publish_service()]} |
    {error, Reason :: term()}.

%% Types
-type publish_service() :: {PubSub :: jid:jid(), Node :: pubsub_node(), Form :: form()}.
-type pubsub_node()        :: binary().
-type form_field()  :: {Name :: binary(), Value :: binary()}.
-type form()        :: [form_field()].

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_event_pusher_push starting on host ~p", [Host]),

    expand_and_store_virtual_pubsub_hosts(Host, Opts),

    WpoolOpts = [{strategy, available_worker} | gen_mod:get_opt(wpool, Opts, [])],
    {ok, _} = mongoose_wpool:start(generic, Host, pusher_push, WpoolOpts),

    gen_mod:start_backend_module(?MODULE, Opts, [enable, disable, get_publish_services]),
    mod_event_pusher_push_backend:init(Host, Opts),

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
%% Hooks
%%--------------------------------------------------------------------

push_event(Acc, Host, Event = #chat_event{type = chat, direction = in}) ->
    do_push_event(Acc, Host, Event);
push_event(Acc, Host, Event = #chat_event{type = groupchat, direction = out}) ->
    do_push_event(Acc, Host, Event);
push_event(Acc, _, _) ->
    Acc.

do_push_event(Acc, Host, #chat_event{from = From, to = To, packet = Packet}) ->
    %% First condition means that we won't try to push messages without
    %% <body/> element because it is required later in payload generation.
    %% In such case we don't care about plugin's decision, so it is checked
    %% as a second condition.
    %% Messages with empty body will still be pushed.
    exml_query:subelement(Packet, <<"body">>) /= undefined
    andalso mod_event_pusher_push_plugin:should_publish(Host, From, To, Packet)
    andalso publish_message(Acc, From, To, Packet).

%% Hook 'remove_user'
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
                  ok = mod_event_pusher_push_backend:disable(
                         jid:to_bare(From), BarePubsubJID, Node),
                  maybe_remove_push_node_from_session_info(From, Node),
                  IQ#iq{type = result, sub_el = []};
              bad_request ->
                  IQ#iq{type = error, sub_el = [Request, mongoose_xmpp_errors:bad_request()]}
          end,
    {Acc, Res}.

maybe_enable_node(#jid{lserver = Host} = From, BarePubSubJID, Node, FormFields, IQ) ->
    AllKnownDomains = ejabberd_router:dirty_get_all_domains() ++ virtual_pubsub_hosts(Host),
    case lists:member(BarePubSubJID#jid.lserver, AllKnownDomains) of
        true ->
            ok = mod_event_pusher_push_backend:enable(jid:to_bare(From), BarePubSubJID, Node, FormFields),
            ejabberd_sm:store_info(From#jid.luser, From#jid.lserver, From#jid.lresource,
                                  {push_notifications, {Node, FormFields}}),
            IQ#iq{type = result, sub_el = []};
        false ->
            NewSubEl = [IQ#iq.sub_el, mongoose_xmpp_errors:remote_server_not_found()],
            IQ#iq{type = error, sub_el = NewSubEl}
    end.

%%--------------------------------------------------------------------
%% Router callbacks
%%--------------------------------------------------------------------

-spec handle_publish_response(Recipient :: jid:jid(), PubsubJID :: jid:jid(),
                              Node :: pubsub_node(), Result :: timeout | jlib:iq()) -> ok.
handle_publish_response(_Recipient, _PubsubJID, _Node, timeout) ->
    ok;
handle_publish_response(_Recipient, _PubsubJID, _Node, #iq{type = result}) ->
    ok;
handle_publish_response(Recipient, PubsubJID, Node, #iq{type = error, sub_el = Els}) ->
    [Error | _ ] = [Err || #xmlel{name = <<"error">>} = Err <- Els],
    case exml_query:attr(Error, <<"type">>) of
        <<"cancel">> ->
            %% We disable the push node in case the error type is cancel
            ejabberd_sm:remove_info(Recipient#jid.luser, Recipient#jid.lserver, Recipient#jid.lresource,
                                    push_notifications),
            BareRecipient = jid:to_bare(Recipient),
            mod_event_pusher_push_backend:disable(BareRecipient, PubsubJID, Node);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec publish_message(Acc :: mongoose_acc:t(), From :: jid:jid(), To :: jid:jid(), Packet :: exml:element()) -> ok.
publish_message(Acc, From, To, Packet) ->
    ?DEBUG("Handle push notification ~p", [{From, To, Packet}]),

    BareRecipient = jid:to_bare(To),
    {ok, Services} = mod_event_pusher_push_backend:get_publish_services(BareRecipient),
    mod_event_pusher_push_plugin:publish_notification(Acc, From, To, Packet, Services).

%%--------------------------------------------------------------------
%% Plugin utils
%%--------------------------------------------------------------------

-spec cast(Host :: jid:server(), F :: atom(), A :: [any()]) -> any().
cast(Host, F, A) ->
    cast(Host, ?MODULE, F, A).

-spec cast(Host :: jid:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
cast(Host, M, F, A) ->
    mongoose_wpool:cast(generic, Host, pusher_push, {M, F, A}).

-spec virtual_pubsub_hosts(jid:server()) -> [jid:server()].
virtual_pubsub_hosts(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, []).

%%--------------------------------------------------------------------
%% Debug & testing
%%--------------------------------------------------------------------

-spec add_virtual_pubsub_host(Host :: jid:server(), VirtualHost :: jid:server()) -> any().
add_virtual_pubsub_host(Host, VirtualHost) ->
    VHosts0 = virtual_pubsub_hosts(Host),
    VHosts = lists:usort(gen_mod:make_subhosts(VirtualHost, Host) ++ VHosts0),
    gen_mod:set_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, VHosts).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------
-spec maybe_remove_push_node_from_session_info(jid:jid(), pubsub_node() | undefined) ->
    ok.
maybe_remove_push_node_from_session_info(From, undefined) ->
    %% The node is undefined which means that a user wants to disable all push nodes
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    AllResources = ejabberd_sm:get_user_present_resources(LUser, LServer),
    [ejabberd_sm:remove_info(LUser, LServer, Resource, push_notifications) ||
     {_, Resource} <- AllResources],
    ok;
maybe_remove_push_node_from_session_info(From, Node) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    LResource = From#jid.lresource,
    case my_push_node(LUser, LServer, LResource, Node) of
        true ->
            ejabberd_sm:remove_info(LUser, LServer, LResource, push_notifications);
        false ->
            find_and_remove_push_node_from_other_session(From, Node)
    end.

my_push_node(LUser, LServer, LResource, Node) ->
    {_SUser, _SID, _, SInfo} = ejabberd_sm:get_session(LUser, LServer, LResource),
    case lists:keyfind(push_notifications, 1, SInfo) of
        {push_notifications, {Node, _}} ->
            true;
        _ ->
            false
    end.

find_and_remove_push_node_from_other_session(From, Node) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    AllResources = ejabberd_sm:get_user_present_resources(LUser, LServer),
    AllResourcesButMine = [LResource || {_, LResource} <- AllResources,
                                        LResource /= From#jid.lresource],
    find_and_remove_push_node(LUser, LServer, AllResourcesButMine, Node).

find_and_remove_push_node(_LUser, _LServer, [], _Node) ->
    ok;
find_and_remove_push_node(LUser, LServer, [LResource | Rest], Node) ->
    case my_push_node(LUser, LServer, LResource, Node) of
        true ->
            ejabberd_sm:remove_info(LUser, LServer, LResource, push_notifications);
        false ->
            find_and_remove_push_node(LUser, LServer, Rest, Node)
    end.

-spec expand_and_store_virtual_pubsub_hosts(Host :: jid:server(), Opts :: list()) -> any().
expand_and_store_virtual_pubsub_hosts(Host, Opts) ->
    ExpandedVHosts = lists:usort([SubHost || Spec <- gen_mod:get_opt(virtual_pubsub_hosts, Opts, []),
                                             SubHost <- gen_mod:make_subhosts(Spec, Host)]),
    gen_mod:set_module_opt(Host, ?MODULE, normalized_virtual_pubsub_hosts, ExpandedVHosts).

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

