%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_room.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Room logic for mod_muc_light
%%% Created : 9 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_room).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([handle_packet/3]).

%% Callbacks
-export([participant_limit_check/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_light.hrl").

-define(BACKEND, (mod_muc_light:backend())).
-define(BCASTER, (mod_muc_light:bcaster())).

-type packet_processing_result() :: {ok, noreply} | {ok, #xmlel{}}
                                    | {error, Reason :: term()}.

%%====================================================================
%% API
%%====================================================================

-spec handle_packet(jid(), jid(), #xmlel{}) -> ok.
handle_packet(From, RoomJID, Packet) ->
    AffiliationsRes = ?BACKEND:get_affiliated_users(RoomJID),
    Response = get_params_and_process_packet(From, RoomJID, Packet, AffiliationsRes),
    send_response(From, RoomJID, Packet, Response).

%%====================================================================
%% Packet handling
%%====================================================================

-spec get_params_and_process_packet(
        jid(), jid(), #xmlel{},{ok, affiliated_users()} | {error, term()}) ->
    packet_processing_result().
get_params_and_process_packet(_From, _RoomJID, _Packet, {error, Reason}) ->
    {error, Reason};
get_params_and_process_packet(From, RoomJID, Packet, {ok, Affiliations}) ->
    Auth = lists:keyfind(lower_nores(From), 1, Affiliations),
    Type = exml_query:path(Packet, [{attr, <<"type">>}]),
    classify_operation(From, RoomJID, Packet, Type, Auth, Affiliations).

-spec classify_operation(jid(), jid(), #xmlel{}, binary(),
                         false | affiliated_user(), affiliated_users()) ->
    packet_processing_result().
classify_operation(_From, _RoomJID, _Packet, _Type, false, _Affiliations) ->
    {error, registration_required};
classify_operation(_From, _RoomJID, #xmlel{ name = <<"iq">> }, <<"error">>, _Auth, _Affiliations) ->
    {ok, noreply};
classify_operation(From, RoomJID, #xmlel{ name = <<"iq">> } = IQ, Type, Auth, Affiliations) ->
    case exml_query:path(IQ, [{element, <<"query">>}, {attr, <<"xmlns">>}]) of
        ?NS_MUC_OWNER -> handle_config_iq(From, RoomJID, IQ, Type, Auth);
        ?NS_MUC_ADMIN -> handle_affiliation_iq(From, RoomJID, IQ, Type, Auth, Affiliations);
        _ -> handle_other_iq(RoomJID#jid.lserver, From, RoomJID, IQ)
    end;
classify_operation(From, RoomJID, #xmlel{ name = <<"message">> } = Msg,
                   <<"groupchat">>, _Auth, Affiliations) ->
    case exml_query:path(Msg, [{element, <<"body">>}]) of
        undefined -> handle_subject_message(From, RoomJID, Msg, Affiliations);
        _Body -> handle_body_message(From, RoomJID, Msg, Affiliations)
    end;
classify_operation(_From, _RoomJID, _Packet, _Type, _Auth, _Affiliations) ->
    {error, bad_request}.
    
%% --------- IQ handlers ---------

-spec handle_config_iq(jid(), jid(), #xmlel{},
                       binary(), affiliated_user()) ->
    packet_processing_result().
handle_config_iq(_From, RoomJID, IQ, <<"get">>, _) ->
    case ?BACKEND:get_configuration(RoomJID) of
        {ok, Config} ->
            ResultIQ1 = jlib:make_result_iq_reply(IQ),
            ResultIQ = ResultIQ1#xmlel{children = [mod_muc_light_utils:config_to_query(Config)]},
            {ok, ResultIQ};
        Error ->
            Error
    end;
handle_config_iq(_From, RoomJID, IQ, <<"set">>, {_, owner}) ->
    {ok, ConfigurationChange} = mod_muc_light_utils:iq_to_config(IQ, []),
    case ?BACKEND:set_configuration(RoomJID, ConfigurationChange) of
        ok -> {ok, (jlib:make_result_iq_reply(IQ))#xmlel{ children = [] }};
        Error -> Error
    end;
handle_config_iq(_From, _RoomJID, _IQ, <<"set">>, _) ->
    {error, not_allowed};
handle_config_iq(_From, _RoomJID, _IQ, <<"error">>, _) ->
    {ok, noreply};
handle_config_iq(_From, _RoomJID, _IQ, _Type, _FromAff) ->
    {error, bad_request}.

-spec handle_affiliation_iq(jid(), jid(), #xmlel{}, binary(),
                            affiliated_user(), affiliated_users()) ->
    packet_processing_result().
handle_affiliation_iq(_From, _RoomJID, IQ, <<"get">>, _, Affiliations) ->
    Query = exml_query:path(IQ, [{element, <<"query">>}]),
    QueryAffiliations = exml_query:paths(Query, [{element, <<"item">>}, {attr, <<"affiliation">>}]),
    AffiliationsFiltered
    = case {lists:member(<<"owner">>, QueryAffiliations),
            lists:member(<<"member">>, QueryAffiliations)} of
          {true, true} -> Affiliations;
          {true, false} -> [lists:keyfind(owner, 2, Affiliations)];
          {false, true} -> lists:keydelete(owner, 2, Affiliations)
      end,

    Items = affiliated_users_to_items(AffiliationsFiltered),
    Reply1 = jlib:make_result_iq_reply(IQ),
    {ok, Reply1#xmlel{ children = [Query#xmlel{ children = Items }]}};
handle_affiliation_iq(_From, RoomJID, IQ, <<"set">>, Auth, Affiliations) ->
    Items = exml_query:paths(IQ, [{element, <<"query">>}, {element, <<"item">>}]),
    AffiliationsToChange = items_to_affiliated_users(Items),
    IsAllowed = is_allowed_to_change_affiliated_users(
                  Auth, Affiliations, AffiliationsToChange, RoomJID),
    apply_affiliation_change(RoomJID, IQ, AffiliationsToChange, IsAllowed);
handle_affiliation_iq(_From, _RoomJID, _IQ, _Type, _Auth, _Affiliations) ->
    {error, not_allowed}.

-spec participant_limit_check(RoomJID :: jid(), NewAffiliations :: affiliated_users()) ->
    ok | {error, any()}.
participant_limit_check(RoomJID, Affiliations) ->
    case length(Affiliations) > mod_muc_light:get_service_opt(RoomJID, participant_limit, 30) of
        true -> {error, participant_limit_exceeded};
        false -> ok
    end.

-spec apply_affiliation_change(jid(), #xmlel{}, affiliated_users(), boolean()) ->
    packet_processing_result().
apply_affiliation_change(RoomJID, IQ, AffiliationsToChange, true) ->
    case ?BACKEND:modify_affiliated_users(RoomJID, AffiliationsToChange,
                                          fun ?MODULE:participant_limit_check/2) of
        {ok, NewAffiliations, ChangedAffiliations} ->
            case NewAffiliations of
                [] ->
                    ejabberd_hooks:run(forget_room, RoomJID#jid.lserver,
                                       [RoomJID#jid.lserver, RoomJID#jid.luser]),
                    ?BACKEND:destroy_room(RoomJID);
                _ -> my_room_will_go_on
            end,
            affiliation_change_bcast(RoomJID, IQ, NewAffiliations, ChangedAffiliations);
        Error ->
            Error
    end;
apply_affiliation_change(_RoomJID, _IQ, _Items, false) ->
    {error, not_allowed}.

-spec affiliation_change_bcast(jid(), #xmlel{}, affiliated_users(),
                               affiliated_users()) -> {ok, #xmlel{}}.
affiliation_change_bcast(RoomJID, IQ, NewAffiliations, ChangedAffiliations) ->
    %% Current occupants are notified first...
    ChangedAffMsg = make_affiliation_message(RoomJID, ChangedAffiliations),
    ?BCASTER:broadcast(nores(RoomJID), ChangedAffMsg, NewAffiliations),
    %% Now we tell the ones that were removed
    lists:foreach(
      fun
          ({_, none} = KickedUser) ->
              KickedMessage = make_affiliation_message(RoomJID, [KickedUser]),
              ?BCASTER:broadcast(nores(RoomJID), KickedMessage, [KickedUser]);
          (_) ->
              ignore
      end, ChangedAffiliations),
    {ok, (jlib:make_result_iq_reply(IQ))#xmlel{ children = [] }}.

handle_other_iq(Host, From, RoomJID, IQ) ->
    case mod_muc_iq:process_iq(Host, From, RoomJID, jlib:iq_query_info(IQ)) of
        ignore -> {ok, noreply};
        error -> {error, not_implemented};
        ResIQ -> {ok, jlib:iq_to_xml(ResIQ)}
    end.

%% --------- Message handlers ---------

-spec handle_subject_message(jid(), jid(), #xmlel{}, affiliated_users()) ->
    packet_processing_result().
handle_subject_message(From, RoomJID, Msg, Affiliations) ->
    Subject = exml_query:path(Msg, [{element, <<"subject">>}, cdata]),
    ?BACKEND:set_configuration(RoomJID, roomname, Subject),
    bcast_message(From, RoomJID, Msg, Affiliations),
    {ok, noreply}.

-spec handle_body_message(jid(), jid(), #xmlel{}, affiliated_users()) ->
    packet_processing_result().
handle_body_message(From, RoomJID, Msg, Affiliations) ->
    bcast_message(From, RoomJID, Msg, Affiliations),
    {ok, noreply}.

-spec bcast_message(jid(), jid(), #xmlel{}, affiliated_users()) -> ok.
bcast_message(From, RoomJID, Msg, Affiliations) ->
    MessageToSend = make_message_from_room(From, Msg),
    BCastFrom = rooms_user_jid(RoomJID, From),
    case ejabberd_hooks:run_fold(filter_room_packet, RoomJID#jid.lserver, MessageToSend,
                                 [jlib:jid_to_binary(lower_nores(From)), From, RoomJID]) of
        drop -> ok;
        FilteredMessage -> ?BCASTER:broadcast(BCastFrom, FilteredMessage, Affiliations)
    end,
    {ok, noreply}.

%%====================================================================
%% Response processing
%%====================================================================

-spec send_response(jid(), jid(), #xmlel{}, packet_processing_result()) -> ok.
send_response(_From, _RoomJID, _OriginalPacket, {ok, noreply}) ->
    ok;
send_response(From, RoomJID, _OriginalPacket, {ok, Packet}) ->
    ejabberd_router:route(RoomJID, From, Packet);
send_response(From, RoomJID, OriginalPacket, {error, Reason}) ->
    Reply = jlib:make_error_reply(OriginalPacket, reason_to_error_elem(Reason)),
    ejabberd_router:route(RoomJID, From, Reply).

-spec reason_to_error_elem(any()) -> #xmlel{}.
reason_to_error_elem(registration_required) -> ?ERR_REGISTRATION_REQUIRED;
reason_to_error_elem(bad_request) -> ?ERR_BAD_REQUEST;
reason_to_error_elem(not_exists) -> ?ERR_ITEM_NOT_FOUND;
reason_to_error_elem(not_allowed) -> ?ERR_NOT_ALLOWED;
reason_to_error_elem(not_implemented) -> ?ERR_FEATURE_NOT_IMPLEMENTED;
reason_to_error_elem(Reason) -> ?ERRT_BAD_REQUEST(<<"en">>, io_lib:format("~p", [Reason])).

%%====================================================================
%% Internal functions
%%====================================================================

-spec affiliated_users_to_items(affiliated_users()) -> [#xmlel{}].
affiliated_users_to_items(Affiliations) ->
    [ #xmlel{ name = <<"item">>,
              attrs = [{<<"affiliation">>, mod_muc_light_utils:aff2b(Affiliation)},
                       {<<"jid">>, jlib:jid_to_binary(JID)}] }
      || {JID, Affiliation} <- Affiliations ].

-spec items_to_affiliated_users([#xmlel{}]) -> affiliated_users().
items_to_affiliated_users(Items) ->
    lists:map(fun(Item) ->
                      BinJID = exml_query:path(Item, [{attr, <<"jid">>}]),
                      BinAff = exml_query:path(Item, [{attr, <<"affiliation">>}]),
                      {lower_nores(jlib:binary_to_jid(BinJID)), mod_muc_light_utils:b2aff(BinAff)}
              end, Items).

-spec is_allowed_to_change_affiliated_users(
        affiliated_user(), affiliated_users(), affiliated_users(), jid()) -> boolean().
is_allowed_to_change_affiliated_users(
  {_User, owner}, _CurrentAffiliations, _AffiliationsChange, _RoomJID) ->
    true;
is_allowed_to_change_affiliated_users(
  {User, member}, CurrentAffiliations, AffiliationsChange, RoomJID) ->
    EveryoneCanInvite = mod_muc_light:get_service_opt(RoomJID, everyone_can_invite, false),
    lists:all(
      fun ({UserMatch, none}) when UserMatch =:= User ->
              true;
          ({OtherUser, member}) ->
              (false == lists:keyfind(OtherUser, 1, CurrentAffiliations))
                andalso EveryoneCanInvite;
          (_) ->
              false
      end, AffiliationsChange);
is_allowed_to_change_affiliated_users(_, _, _, _) ->
    false.

-spec make_affiliation_message(jid(), affiliated_users()) -> #xmlel{}.
make_affiliation_message(RoomJID, ChangedAffiliations) ->
    XElem = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT}],
                    children = affiliated_users_to_items(ChangedAffiliations) },
    EmptyBodyForMAM = #xmlel{ name = <<"body">> },
    #xmlel{ name = <<"message">>,
            attrs = [{<<"from">>, jlib:jid_to_binary(nores(RoomJID))},
                     {<<"type">>, <<"groupchat">>}],
            children = [XElem, EmptyBodyForMAM] }.

-spec make_message_from_room(jid(), #xmlel{}) -> #xmlel{}.
make_message_from_room(FromJID, Packet) ->
    RoomBin = exml_query:path(Packet, [{attr, <<"to">>}]),
    UserBareBin = jlib:jid_to_binary(nores(FromJID)),
    NewFrom = <<RoomBin/binary, $/, UserBareBin/binary>>,
    NewAttrs = lists:keystore(<<"from">>, 1, Packet#xmlel.attrs,
                              {<<"from">>, NewFrom}),
    Packet#xmlel{ attrs = NewAttrs }.

%% --------- Utils ---------

-spec rooms_user_jid(jid(), jid()) -> jid().
rooms_user_jid(RoomJID, From) ->
    jlib:jid_replace_resource(RoomJID, jlib:jid_to_binary(lower_nores(From))).

%% Tiny helpers. I hate using these long-named functions from jlib. :)
-spec nores(jid()) -> jid().
nores(JID) -> jlib:jid_remove_resource(JID).

-spec lower_nores(jid()) -> {binary(), binary(), <<>>}.
lower_nores(JID) -> jlib:jid_remove_resource(jlib:jid_to_lower(JID)).
