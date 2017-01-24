%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @todo: write me!
%%% @end
%%%-------------------------------------------------------------------
-module(mod_push).
-author("Rafal Slota").
-behavior(gen_mod).
-xep([{xep, 357}, {version, "0.2.1"}]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% Hooks and IQ handlers
-export([handle_offline_message/3, iq_handler/3]).

-callback init(Host :: ejabberd:server(), Opts :: list()) -> ok.
-callback enable(User :: ejabberd:jid(), PubSub :: ejabberd:jid(),
                 Node :: binary(), Forms :: any()) -> ok.
-callback disable(User :: ejabberd:jid(), PubSub :: ejabberd:jid(), Node :: binary()) -> ok.
-callback get_publish_services(User :: ejabberd:jid()) ->
    [{PubSub :: ejabberd:jid(), Node :: binary()}].


-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    ?DEBUG("mod_push starting on host ~p", [Host]),

    gen_mod:start_backend_module(?MODULE, Opts, []),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_disco:register_feature(Host, ?NS_PUSH),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PUSH, ?MODULE,
                                  iq_handler, IQDisc),

    ejabberd_hooks:add(offline_groupchat_message_hook, Host, ?MODULE, handle_offline_message, 1),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, handle_offline_message, 1),

    ok.

stop(Host) ->

    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, handle_offline_message, 1),
    ejabberd_hooks:delete(offline_groupchat_message_hook, Host, ?MODULE, handle_offline_message, 1),

    ok.


-spec handle_offline_message(From :: ejabberd:jid(),
                             To :: ejabberd:jid(),
                             Packet :: jlib:xmlel()) -> ok.
handle_offline_message(From, To, Packet) ->




    ok.

-spec iq_handler(From :: ejabberd:jid(), To :: ejabberd:jid(), IQ :: ejabberd:iq()) ->
    ejabberd:iq() | ignore.
iq_handler(_From, _To, IQ = #iq{type = get, sub_el = SubEl}) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
iq_handler(_From, _To, IQ = #iq{type = set, sub_el = Request}) ->
    case parse_request(Request) of
        {enable, BareJID, Node, Form} ->


            IQ#iq{type = result, sub_el = []};
        {disable, BareJID, Node} ->
            IQ#iq{type = result, sub_el = []};
        bad_request ->
            IQ#iq{type = error, sub_el = [Request, ?ERR_BAD_REQUEST]}
    end.


-spec parse_request(Request :: exml:element()) ->
    bad_request.
parse_request(#xmlel{name = <<"enable">>} = Request) ->
    JID = jid:from_binary(exml_query:attr(Request, <<"jid">>, <<>>)),
    Node = exml_query:attr(Request, <<"node">>, <<>>), %% Treat unset node as empty - both forbidden
    Form = exml_query:subelement(Request, <<"x">>),

    case {JID, Node} of
        {_, <<>>}               -> bad_request;
        {#jid{luser = <<>>}}    -> bad_request;
        {#jid{lserver = <<>>}}  -> bad_request;
        {JID, Node} ->
            {enable, JID, Node, Form}
    end;
parse_request(#xmlel{name = <<"disable">>} = Request) ->
    JID = jid:from_binary(exml_query:attr(Request, <<"jid">>, <<>>)),
    Node = exml_query:attr(Request, <<"node">>, undefined),

    case {jid:to_bare(JID), Node} of
        {_, <<>>}               -> bad_request; %% Node may not be set, but shouldn't be empty
        {#jid{luser = <<>>}}    -> bad_request;
        {#jid{lserver = <<>>}}  -> bad_request;
        {BareJID, Node} ->
            {disable, BareJID, Node}
    end;
parse_request(_) ->
    bad_request.