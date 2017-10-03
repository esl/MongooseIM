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
-module(mod_push).
-author('rafal.slota@erlang-solutions.com').
-behavior(gen_mod).
-xep([{xep, 357}, {version, "0.2.1"}]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hooks and IQ handlers
-export([iq_handler/3,
         handle_publish_response/4,
         filter_packet/1,
         remove_user/3]).

%% Types
-export_type([pubsub_node/0, form_field/0, form/0]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(PUSH_FORM_TYPE, <<"urn:xmpp:push:summary">>).

-callback init(Host :: ejabberd:server(), Opts :: list()) -> ok.
-callback enable(UserJID :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                 Node :: mod_push:pubsub_node(), Form :: mod_push:form()) ->
    ok | {error, Reason :: term()}.
-callback disable(UserJID :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                  Node :: mod_push:pubsub_node()) -> ok | {error, Reason :: term()}.
-callback get_publish_services(User :: ejabberd:jid()) ->
    {ok, [{PubSub :: ejabberd:jid(), Node :: mod_push:node(), Form :: mod_push:form()}]} |
    {error, Reason :: term()}.

%% Types
-type pubsub_node()        :: binary().
-type form_field()  :: {Name :: binary(), Value :: binary()}.
-type form()        :: [form_field()].

%%--------------------------------------------------------------------
%% Module callbacks
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?INFO_MSG("mod_push starting on host ~p", [Host]),

    {ok, _} = wpool_sup:start_pool(gen_mod:get_module_proc(Host, ?MODULE),
                                    gen_mod:get_opt(wpool, Opts, [])),

    gen_mod:start_backend_module(?MODULE, Opts, []),
    mod_push_backend:init(Host, Opts),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?NS_PUSH),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),

    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 90),

    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 90),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PUSH),
    mod_disco:unregister_feature(Host, ?NS_PUSH),

    wpool_sup:stop_pool(gen_mod:get_module_proc(Host, ?MODULE)),

    ok.

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------

%% Hook 'remove_user'
-spec remove_user(Acc :: mongoose_acc:t(), LUser :: binary(), LServer :: binary()) ->
    mongoose_acc:t().
remove_user(Acc, LUser, LServer) ->
    R = mod_push_backend:disable(jid:make_noprep(LUser, LServer, <<>>), undefined, undefined),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, LUser, LServer}),
    Acc.

%% Hook 'filter_packet'
-type fpacket() :: {From :: ejabberd:jid(),
                    To :: ejabberd:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: jlib:xmlel()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->
    drop;
filter_packet({From, To = #jid{lserver = Host}, Acc, Packet}) ->
    ?DEBUG("Receive packet~n    from ~p ~n    to ~p~n    packet ~p.",
           [From, To, Acc]),
    PacketType = mongoose_acc:get(type, Acc),
    case lists:member(PacketType, [<<"chat">>, <<"groupchat">>]) of
        true ->
            case mod_push_plugin:should_publish(Host, From, To, Packet) of
                true ->
                    publish_message(From, To, Packet);
                false ->
                    skip
            end;
        false ->
            skip
    end,
    {From, To, Acc, Packet}.

-spec iq_handler(From :: ejabberd:jid(), To :: ejabberd:jid(), IQ :: ejabberd:iq()) ->
                        ejabberd:iq() | ignore.
iq_handler(_From, _To, IQ = #iq{type = get, sub_el = SubEl}) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
iq_handler(From, _To, IQ = #iq{type = set, sub_el = Request}) ->
    case parse_request(Request) of
        {enable, BarePubsubJID, Node, FormFields} ->
            ok = mod_push_backend:enable(jid:to_bare(From), BarePubsubJID, Node, FormFields),
            IQ#iq{type = result, sub_el = []};
        {disable, BarePubsubJID, Node} ->
            ok = mod_push_backend:disable(jid:to_bare(From), BarePubsubJID, Node),
            IQ#iq{type = result, sub_el = []};
        bad_request ->
            IQ#iq{type = error, sub_el = [Request, ?ERR_BAD_REQUEST]}
    end.


%%--------------------------------------------------------------------
%% Router callbacks
%%--------------------------------------------------------------------

-spec handle_publish_response(BareRecipient :: ejabberd:jid(), PubsubJID :: ejabberd:jid(),
                              Node :: pubsub_node(), Result :: timeout | iq()) -> ok.
handle_publish_response(_BareRecipient, _PubsubJID, _Node, timeout) ->
    ok;
handle_publish_response(_BareRecipient, _PubsubJID, _Node, #iq{type = result}) ->
    ok;
handle_publish_response(BareRecipient, PubsubJID, Node, #iq{type = error}) ->
    %% @todo: maybe filter only some errors? e.g. internal server error may be temporary and
    %%        should not disable notifications
    mod_push_backend:disable(BareRecipient, PubsubJID, Node),
    ok.

%%--------------------------------------------------------------------
%% Module API
%%--------------------------------------------------------------------

-spec publish_message(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> ok.
publish_message(From, To = #jid{lserver = Host}, Packet) ->
    ?DEBUG("Handle push notification ~p", [{From, To, Packet}]),

    BareRecipient = jid:to_bare(To),
    {ok, Services} = mod_push_backend:get_publish_services(BareRecipient),
    lists:foreach(
      fun({PubsubJID, Node, Form}) ->
              Stanza = push_notification_iq(Host, From, Packet, Node, Form),
              ResponseHandler =
                  fun(Response) ->
                          cast(Host, handle_publish_response,
                               [BareRecipient, PubsubJID, Node, Response])
                  end,
              cast(Host, ejabberd_local, route_iq, [To, PubsubJID, Stanza, ResponseHandler])
      end, Services),

    ok.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec parse_request(Request :: exml:element()) ->
    {enable, ejabberd:jid(), pubsub_node(), form()} |
    {disable, ejabberd:jid(), pubsub_node()} |
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

-spec parse_form(undefined | jlib:xmlel()) -> invalid_form | form().
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

-spec push_notification_iq(Host :: ejabberd:server(), From :: ejabberd:jid(),
                           Packet :: jlib:xmlel(), Node :: pubsub_node(), Form :: form()) -> iq().
push_notification_iq(Host, From, Packet, Node, Form) ->
    ContentFields =
        [
         {<<"FORM_TYPE">>, ?PUSH_FORM_TYPE},
         {<<"message-count">>, <<"1">>},
         {<<"last-message-sender">>, mod_push_plugin:sender_id(Host, From, Packet)},
         {<<"last-message-body">>, exml_query:cdata(exml_query:subelement(Packet, <<"body">>))}
        ],

    #iq{type = set, sub_el = [
        #xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}], children = [
            #xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}], children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = [{<<"xmlns">>, ?NS_PUSH}], children = [make_form(ContentFields)]}
                ]}
            ]}
        ] ++ maybe_publish_options(Form)}
    ]}.

    -spec maybe_publish_options(form()) -> [jlib:xmlel()].
maybe_publish_options([]) ->
    [];
maybe_publish_options(FormFields) ->
    [#xmlel{name = <<"publish-options">>,
            children = [
                        make_form([{<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS}] ++ FormFields)
                       ]}].

-spec make_form(form()) -> jlib:xmlel().
make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Field) || Field <- Fields]}.

-spec make_form_field(form_field()) -> jlib:xmlel().
make_form_field({Name, Value}) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

-spec cast(Host :: ejabberd:server(), F :: atom(), A :: [any()]) -> any().
cast(Host, F, A) ->
    cast(Host, ?MODULE, F, A).

-spec cast(Host :: ejabberd:server(), M :: atom(), F :: atom(), A :: [any()]) -> any().
cast(Host, M, F, A) ->
    wpool:cast(gen_mod:get_module_proc(Host, ?MODULE), {M, F, A}, available_worker).
