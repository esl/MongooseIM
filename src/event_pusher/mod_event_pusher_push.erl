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
-behaviour(mongoose_module_metrics).
-xep([{xep, 357}, {version, "0.4.1"}]).

-include("mod_event_pusher_events.hrl").
-include("mongoose.hrl").
-include("session.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-define(SESSION_KEY, publish_service).

%%--------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------

%% gen_mod behaviour
-export([start/2, stop/1, hooks/1, config_spec/0]).

%% mongoose_module_metrics behaviour
-export([config_metrics/1]).

%% hook handlers
-export([push_event/3]).

%% Hooks and IQ handlers
-export([iq_handler/4,
         remove_user/3]).

%% Plugin utils
-export([cast/3]).
-export([is_virtual_pubsub_host/3]).
-export([disable_node/4]).

-ignore_xref([iq_handler/4]).

%% Types
-type publish_service() :: {PubSub :: jid:jid(), Node :: pubsub_node(), Form :: form()}.
-type pubsub_node() :: binary().
-type form() :: #{binary() => binary()}.

-export_type([pubsub_node/0, form/0]).
-export_type([publish_service/0]).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    ?LOG_INFO(#{what => event_pusher_starting, host_type => HostType}),
    start_pool(HostType, Opts),
    mod_event_pusher_push_backend:init(HostType, Opts),
    mod_event_pusher_push_plugin:init(HostType, Opts),
    init_iq_handlers(HostType, Opts),
    ok.

start_pool(HostType, #{wpool := WpoolOpts}) ->
    {ok, _} = mongoose_wpool:start(generic, HostType, pusher_push, maps:to_list(WpoolOpts)).

init_iq_handlers(HostType, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, HostType, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostType, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostType, ?NS_PUSH),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostType, ?NS_PUSH),

    mongoose_wpool:stop(generic, HostType, pusher_push),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 90},
     {push_event, HostType, fun ?MODULE:push_event/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    VirtPubSubHost = #option{type = string, validate = subdomain_template,
                             process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
    #section{
        items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                  <<"backend">> => #option{type = atom, validate = {module, ?MODULE}},
                  <<"wpool">> => wpool_spec(),
                  <<"plugin_module">> => #option{type = atom, validate = module},
                  <<"virtual_pubsub_hosts">> => #list{items = VirtPubSubHost}},
        defaults = #{<<"iqdisc">> => one_queue,
                     <<"backend">> => mnesia,
                     <<"plugin_module">> => mod_event_pusher_push_plugin:default_plugin_module(),
                     <<"virtual_pubsub_hosts">> => []}
    }.

wpool_spec() ->
    Wpool = mongoose_config_spec:wpool(#{<<"strategy">> => available_worker}),
    Wpool#section{include = always}.

%%--------------------------------------------------------------------
%% Hooks and IQ handlers
%%--------------------------------------------------------------------
-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{jid := jid:jid()},
      Extra :: map().
remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, _) ->
    R = mod_event_pusher_push_backend:disable(LServer, jid:make_noprep(LUser, LServer, <<>>)),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, LUser, LServer}),
    {ok, Acc}.

-spec push_event(mod_event_pusher:push_event_acc(), mod_event_pusher:push_event_params(),
                 gen_hook:extra()) -> {ok, mod_event_pusher:push_event_acc()}.
push_event(HookAcc, #{event := Event = #chat_event{direction = out, to = To, type = Type}}, _Extra)
  when Type =:= groupchat;
       Type =:= chat ->
    #{acc := Acc} = HookAcc,
    BareRecipient = jid:to_bare(To),
    NewAcc = do_push_event(Acc, Event, BareRecipient),
    {ok, HookAcc#{acc := NewAcc}};
push_event(HookAcc = #{acc := Acc}, #{event := Event = #unack_msg_event{to = To}}, _Extra) ->
    BareRecipient = jid:to_bare(To),
    #{acc := Acc} = HookAcc,
    NewAcc = do_push_event(Acc, Event, BareRecipient),
    {ok, HookAcc#{acc := NewAcc}};
push_event(HookAcc, _Params, _Extra) ->
    {ok, HookAcc}.

-spec iq_handler(From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq() | ignore}.
iq_handler(_From, _To, Acc, IQ = #iq{type = get, sub_el = SubEl}) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
iq_handler(From, _To, Acc, IQ = #iq{type = set, sub_el = Request}) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = case parse_request(Request) of
              {enable, BarePubSubJID, Node, FormFields} ->
                  ok = enable_node(HostType, From, BarePubSubJID, Node, FormFields),
                  store_session_info(From, {BarePubSubJID, Node, FormFields}, Acc),
                  IQ#iq{type = result, sub_el = []};
              {disable, BarePubsubJID, Node} ->
                  ok = disable_node(HostType, From, BarePubsubJID, Node),
                  IQ#iq{type = result, sub_el = []};
              bad_request ->
                  IQ#iq{type = error, sub_el = [Request, mongoose_xmpp_errors:bad_request()]}
          end,
    {Acc, Res}.

%%--------------------------------------------------------------------
%% Plugin utils API
%%--------------------------------------------------------------------
-spec disable_node(mongooseim:host_type(), UserJID :: jid:jid(), BarePubSubJID :: jid:jid(),
                   Node :: pubsub_node()) -> ok | {error, Reason :: term()}.
disable_node(HostType, UserJID, BarePubSubJID, Node) ->
    BareUserJID = jid:to_bare(UserJID),
    maybe_remove_push_node_from_sessions_info(BareUserJID, BarePubSubJID, Node),
    mod_event_pusher_push_backend:disable(HostType, BareUserJID, BarePubSubJID, Node).

-spec cast(mongooseim:host_type(), F :: function(), A :: [any()]) -> any().
cast(HostType, F, A) ->
    mongoose_wpool:cast(generic, HostType, pusher_push, {erlang, apply, [F, A]}).

-spec is_virtual_pubsub_host(HostType :: mongooseim:host_type(), %% recipient host type
                             RecipientDomain :: mongooseim:domain_name(),
                             VirtPubsubDomain :: mongooseim:domain_name()) -> boolean().
is_virtual_pubsub_host(HostType, RecipientDomain, VirtPubsubDomain) ->
    Templates = gen_mod:get_module_opt(HostType, ?MODULE, virtual_pubsub_hosts),
    PredFn = fun(Template) ->
                 mongoose_subdomain_utils:is_subdomain(Template,
                                                       RecipientDomain,
                                                       VirtPubsubDomain)
             end,
    lists:any(PredFn, Templates).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------
-spec do_push_event(mongoose_acc:t(), mod_event_pusher:event(), jid:jid()) -> mongoose_acc:t().
do_push_event(Acc, Event, BareRecipient) ->
    case mod_event_pusher_push_plugin:prepare_notification(Acc, Event) of
        skip -> Acc;
        Payload ->
            HostType = mongoose_acc:host_type(Acc),
            {ok, Services} = mod_event_pusher_push_backend:get_publish_services(HostType,
                                                                                BareRecipient),
            FilteredService = mod_event_pusher_push_plugin:should_publish(Acc, Event, Services),
            mod_event_pusher_push_plugin:publish_notification(Acc, Event, Payload, FilteredService)
    end.

-spec parse_request(Request :: exml:element()) ->
    {enable, jid:jid(), pubsub_node(), form()} |
    {disable, jid:jid(), pubsub_node() | undefined} |
    bad_request.
parse_request(#xmlel{name = <<"enable">>} = Request) ->
    JID = jid:from_binary(exml_query:attr(Request, <<"jid">>, <<>>)),
    Node = exml_query:attr(Request, <<"node">>, <<>>), %% Treat unset node as empty - both forbidden
    Form = mongoose_data_forms:find_form(Request),

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
    #{};
parse_form(Form) ->
    parse_form_fields(Form).

-spec parse_form_fields(exml:element()) -> invalid_form | form().
parse_form_fields(Form) ->
    case mongoose_data_forms:parse_form_fields(Form) of
        #{type := <<"submit">>, ns := ?NS_PUBSUB_PUB_OPTIONS, kvs := KVs} ->
            case maps:filtermap(fun(_, [V]) -> {true, V};
                                   (_, _) -> false
                                end, KVs) of
                ParsedKVs when map_size(ParsedKVs) < map_size(KVs) ->
                    invalid_form;
                ParsedKVs ->
                    ParsedKVs
            end;
        _ ->
            invalid_form
    end.

-spec enable_node(mongooseim:host_type(), jid:jid(), jid:jid(), pubsub_node(), form()) ->
    ok | {error, Reason :: term()}.
enable_node(HostType, From, BarePubSubJID, Node, FormFields) ->
    mod_event_pusher_push_backend:enable(HostType, jid:to_bare(From), BarePubSubJID, Node,
                                         FormFields).

-spec store_session_info(jid:jid(), publish_service(), mongoose_acc:t()) -> any().
store_session_info(Jid, Service, Acc) ->
    OriginSid = mongoose_acc:get(c2s, origin_sid, undefined, Acc),
    ejabberd_sm:store_info(Jid, OriginSid, ?SESSION_KEY, Service).

-spec maybe_remove_push_node_from_sessions_info(jid:jid(), jid:jid(), pubsub_node() | undefined) ->
          ok.
maybe_remove_push_node_from_sessions_info(From, PubSubJid, Node) ->
    AllSessions = ejabberd_sm:get_raw_sessions(From),
    find_and_remove_push_node(From, AllSessions, PubSubJid, Node).

-spec find_and_remove_push_node(jid:jid(), [ejabberd_sm:session()],
                                jid:jid(), pubsub_node() | undefined) -> ok.
find_and_remove_push_node(_From, [], _,_) ->
    ok;
find_and_remove_push_node(From, [RawSession | Rest], PubSubJid, Node) ->
    case my_push_node(RawSession, PubSubJid, Node) of
        true ->
            LResource  = mongoose_session:get_resource(RawSession),
            JID = jid:replace_resource(From, LResource),
            Sid = RawSession#session.sid,
            ejabberd_sm:remove_info(JID, Sid, ?SESSION_KEY),
            find_and_remove_push_node(From, Rest, PubSubJid, Node);
        false ->
            find_and_remove_push_node(From, Rest, PubSubJid, Node)
    end.

-spec my_push_node(ejabberd_sm:session(), jid:jid(), pubsub_node() | undfined) -> boolean().
my_push_node(RawSession, PubSubJid, Node) ->
    case mongoose_session:get_info(RawSession, ?SESSION_KEY, undefined) of
        {?SESSION_KEY, {PubSubJid, Node, _}} ->
            true;
        {?SESSION_KEY, {PubSubJid, _, _}} when Node =:= undefined ->
            %% The node is undefined which means that a user wants to
            %% disable all the push nodes for the specified service
            true;
        _ -> false
    end.

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
