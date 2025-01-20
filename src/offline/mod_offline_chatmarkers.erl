%%%----------------------------------------------------------------------------
%%% @copyright (C) 2020, Erlang Solutions Ltd.
%%% @doc
%%%   This module optimizes offline storage for chat markers in the next way:
%%%
%%%      1) It filters out chat marker packets processed by mod_smart_markers:
%%%
%%%          * These packets can be identified by the extra permanent Acc
%%%            timestamp field added by mod_smart_markers.
%%%
%%%          * These packets are not going to mod_offline (notice the
%%%            difference in priorities for the offline_message hook handlers)
%%%
%%%          * The information about these chat markers is stored in DB,
%%%            timestamp added by mod_smart_markers is important here!
%%%
%%%      2) After all the offline messages are inserted by mod_offline (notice
%%%         the difference in priorities for the resend_offline_messages hook
%%%         handlers), this module adds the latest chat markers as the last
%%%         offline messages:
%%%
%%%          * It extracts chat markers data stored for the user in the DB
%%%            (with timestamps)
%%%
%%%          * Requests cached chat markers from mod_smart_markers that has
%%%            timestamp older or equal to the stored one.
%%%
%%%          * Generates and inserts chat markers as the last offline messages
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(mod_offline_chatmarkers).
-xep([{xep, 160}, {version, "1.0.1"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod handlers
%% gen_mod API
-export([start/2]).
-export([stop/1]).
-export([hooks/1]).
-export([deps/2]).
-export([supported_features/0]).
-export([config_spec/0]).

%% Hook handlers
-export([inspect_packet/3,
         remove_user/3,
         pop_offline_messages/3]).

-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod callbacks
%% ------------------------------------------------------------------

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_, _)->
    [].  %% TODO: this need to be marked as required-to-be-configured
    % [{mod_smart_markers, [], hard}].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_offline_chatmarkers_backend:init(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    DefaultHooks = [
        {offline_message, HostType, fun ?MODULE:inspect_packet/3, #{}, 40},
        {resend_offline_messages, HostType, fun ?MODULE:pop_offline_messages/3, #{}, 60},
        {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50}
    ],
    case gen_mod:get_module_opt(HostType, ?MODULE, store_groupchat_messages) of
        true ->
            GroupChatHook = {offline_groupchat_message,
                             HostType, fun ?MODULE:inspect_packet/3, #{}, 40},
            [GroupChatHook | DefaultHooks];
        _ -> DefaultHooks
    end.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom,
                                           validate = {module, ?MODULE}},
                  <<"store_groupchat_messages">> => #option{type = boolean}
                 },
        defaults = #{<<"store_groupchat_messages">> => false,
                     <<"backend">> => rdbms
                    }
        }.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = User, lserver = Server}}, #{host_type := HostType}) ->
    mod_offline_chatmarkers_backend:remove_user(HostType, jid:make_bare(User, Server)),
    {ok, Acc}.

-spec pop_offline_messages(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
pop_offline_messages(Acc, #{jid := JID}, _Extra) ->
    {ok, mongoose_acc:append(offline, messages, offline_chatmarkers(Acc, JID), Acc)}.

-spec inspect_packet(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
inspect_packet(Acc, #{from := From, to := To, packet := Packet}, _Extra) ->
    case maybe_store_chat_marker(Acc, From, To, Packet) of
        true ->
            {stop, mongoose_acc:set(offline, stored, true, Acc)};
        false ->
            {ok, Acc}
    end.

maybe_store_chat_marker(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    case mongoose_acc:get(mod_smart_markers, timestamp, undefined, Acc) of
        undefined -> false;
        Timestamp when is_integer(Timestamp) ->
            Room = get_room(Acc, From),
            Thread = get_thread(Packet),
            mod_offline_chatmarkers_backend:maybe_store(HostType, To, Thread, Room, Timestamp),
            true
    end.

get_room(Acc, From) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"groupchat">> -> From;
        _ -> undefined
    end.

get_thread(El) ->
    case exml_query:path(El, [{element, <<"thread">>}, cdata]) of
        Thread when Thread =/= <<>> -> Thread;
        _ -> undefined
    end.

offline_chatmarkers(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, Rows} = mod_offline_chatmarkers_backend:get(HostType, JID),
    mod_offline_chatmarkers_backend:remove_user(HostType, JID),
    lists:concat([process_row(Acc, JID, R) || R <- Rows]).

process_row(Acc, Jid, {Thread, undefined, TS}) ->
    ChatMarkers = mod_smart_markers:get_chat_markers(Jid, Thread, TS),
    [build_one2one_chatmarker_msg(Acc, CM) || CM <- ChatMarkers];
process_row(Acc, Jid, {Thread, Room, TS}) ->
    ChatMarkers = mod_smart_markers:get_chat_markers(Room, Thread, TS),
    [build_room_chatmarker_msg(Acc, Jid, CM) || CM <- ChatMarkers].

build_one2one_chatmarker_msg(Acc, CM) ->
    #{from := From, to := To, thread := Thread,
      type := Type, id := Id, timestamp := TS} = CM,
    Children = thread(Thread) ++ marker(Type, Id),
    Attributes = #{<<"from">> => jid:to_binary(From),
                   <<"to">> => jid:to_binary(To)},
    Packet = #xmlel{name = <<"message">>, attrs = Attributes, children = Children},
    make_route_item(Acc, From, To, TS, Packet).

build_room_chatmarker_msg(Acc, To, CM) ->
    #{from := FromUser, to := Room, thread := Thread,
      type := Type, id := Id, timestamp := TS} = CM,
    FromUserBin = jid:to_bare_binary(FromUser),
    From = jid:make(Room#jid.luser, Room#jid.lserver, FromUserBin),
    FromBin = jid:to_binary(From),
    Children = thread(Thread) ++ marker(Type, Id),
    Attributes = #{<<"from">> => FromBin,
                   <<"to">> => jid:to_binary(To),
                   <<"type">> => <<"groupchat">>},
    Packet = #xmlel{name = <<"message">>, attrs = Attributes, children = Children},
    make_route_item(Acc, From, To, TS, Packet).

make_route_item(Acc, From, To, TS, Packet) ->
    NewStanzaParams = #{element => Packet, from_jid => From, to_jid => To},
    Acc1 = mongoose_acc:update_stanza(NewStanzaParams, Acc),
    Acc2 = mongoose_acc:set_permanent(mod_smart_markers, timestamp, TS, Acc1),
    {route, From, To, Acc2}.

marker(Type, Id) ->
    [#xmlel{name = atom_to_binary(Type, latin1),
            attrs = #{<<"xmlns">> => <<"urn:xmpp:chat-markers:0">>,
                      <<"id">> => Id}}].

thread(undefined) -> [];
thread(Thread) ->
    [#xmlel{name = <<"thread">>,
            children = [#xmlcdata{content = Thread}]}].
