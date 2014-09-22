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
    AffiliationsRes = ?BACKEND:get_affiliations(RoomJID),
    send_response(
      From, RoomJID, Packet,
      get_params_and_process_packet(From, RoomJID, Packet, AffiliationsRes)).

%%====================================================================
%% Packet handling
%%====================================================================

-spec get_params_and_process_packet(jid(), jid(), #xmlel{},
                                    {ok, affiliations()} | {error, term()}) ->
    packet_processing_result().
get_params_and_process_packet(_From, _RoomJID, _Packet, {error, Reason}) ->
    {error, Reason};
get_params_and_process_packet(From, RoomJID, Packet, {ok, Affiliations}) ->
    Auth = lists:keyfind(lower_nores(From), 1, Affiliations),
    Type = exml_query:path(Packet, [{attr, <<"type">>}]),
    classify_operation(From, RoomJID, Packet, Type, Auth, Affiliations).

-spec classify_operation(jid(), jid(), #xmlel{}, binary(),
                         false | affiliation_tuple(), affiliations()) ->
    packet_processing_result().
classify_operation(_From, _RoomJID, _Packet, _Type, false, _Affiliations) ->
    {error, registration_required};
classify_operation(_From, _RoomJID, #xmlel{ name = <<"iq">> }, <<"error">>,
                   _Auth, _Affiliations) ->
    {ok, noreply};
classify_operation(From, RoomJID, #xmlel{ name = <<"iq">> } = IQ, Type,
                   Auth, Affiliations) ->
    case exml_query:path(IQ, [{element, <<"query">>}, {attr, <<"xmlns">>}]) of
        ?NS_MUC_OWNER -> handle_config_iq(From, RoomJID, IQ, Type, Auth);
        ?NS_MUC_ADMIN -> handle_affiliation_iq(From, RoomJID, IQ, Type,
                                               Auth, Affiliations)
    end;
classify_operation(From, #jid{ lresource = <<>> } = RoomJID,
                   #xmlel{ name = <<"message">> } = Msg,
                   <<"groupchat">>, _Auth, Affiliations) ->
    case exml_query:path(Msg, [{element, <<"body">>}]) of
        undefined -> handle_subject_message(From, RoomJID, Msg, Affiliations);
        _Body -> handle_body_message(From, RoomJID, Msg, Affiliations)
    end;
classify_operation(_From, _RoomJID, _Packet, _Type, _Auth, _Affiliations) ->
    {error, bad_request}.
    
%% --------- IQ handlers ---------

-spec handle_config_iq(jid(), jid(), #xmlel{},
                       binary(), affiliation_tuple()) ->
    packet_processing_result().
handle_config_iq(_From, RoomJID, IQ, <<"get">>, _) ->
    case ?BACKEND:get_configuration(RoomJID) of
        {ok, Config} ->
            ResultIQ1 = jlib:make_result_iq_reply(IQ),
            ResultIQ = ResultIQ1#xmlel{
                         children = [mod_muc_light_utils:config_to_query(
                                       Config)]},
            {ok, ResultIQ};
        Error ->
            Error
    end;
handle_config_iq(_From, RoomJID, IQ, <<"set">>, {_, owner}) ->
    {ok, ConfigurationChange} = mod_muc_light_utils:iq_to_config(IQ, []),
    case ?BACKEND:set_configuration(RoomJID, ConfigurationChange) of
        ok ->
            {ok, (jlib:make_result_iq_reply(IQ))#xmlel{ children = [] }};
        Error ->
            Error
    end;
handle_config_iq(_From, _RoomJID, _IQ, <<"set">>, _) ->
    {error, not_allowed};
handle_config_iq(_From, _RoomJID, _IQ, <<"error">>, _) ->
    {ok, noreply};
handle_config_iq(_From, _RoomJID, _IQ, _Type, _FromAff) ->
    {error, bad_request}.

-spec handle_affiliation_iq(jid(), jid(), #xmlel{}, binary(),
                            affiliation_tuple(), affiliations()) ->
    packet_processing_result().
handle_affiliation_iq(_From, _RoomJID, IQ, <<"get">>, _, Affiliations) ->
    Query = exml_query:path(IQ, [{element, <<"query">>}]),
    QueryAffiliations
        = exml_query:paths(Query, [{element, <<"item">>},
                                    {attr, <<"affiliation">>}]),
    AffiliationsFiltered
        = case {lists:member(<<"owner">>, QueryAffiliations),
                lists:member(<<"member">>, QueryAffiliations)} of
              {true, true} -> Affiliations;
              {true, false} -> [lists:keyfind(owner, 2, Affiliations)];
              {false, true} -> lists:keydelete(owner, 2, Affiliations)
          end,

    Items = affiliations_to_items(AffiliationsFiltered),
    Reply1 = jlib:make_result_iq_reply(IQ),
    {ok, Reply1#xmlel{ children =
                       [Query#xmlel{ children = Items }]}};
handle_affiliation_iq(_From, RoomJID, IQ, <<"set">>, {_, owner}, _Affiliations) ->
    Items = exml_query:paths(IQ, [{element, <<"query">>}, {element, <<"item">>}]),
    case ?BACKEND:modify_affiliations(RoomJID, items_to_affiliations(Items)) of
        {ok, NewAffiliations, ChangedAffiliations} ->
            case NewAffiliations of
                [] -> ?BACKEND:destroy_room(RoomJID);
                _ -> my_room_will_go_on
            end,
            affiliation_change_bcast(RoomJID, IQ, NewAffiliations,
                                     ChangedAffiliations);
        Error ->
            Error
    end;
handle_affiliation_iq(_From, _RoomJID, _IQ, _Type, _Auth, _Affiliations) ->
    {error, not_allowed}.

-spec affiliation_change_bcast(jid(), #xmlel{}, affiliations(),
                               affiliations()) -> {ok, #xmlel{}}.
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

%% --------- Message handlers ---------

-spec handle_subject_message(jid(), jid(), #xmlel{}, affiliations()) ->
    packet_processing_result().
handle_subject_message(From, RoomJID, Msg, Affiliations) ->
    Subject = exml_query:path(Msg, [{element, <<"subject">>}, cdata]),
    ?BACKEND:set_configuration(RoomJID, roomname, Subject),
    bcast_message(From, RoomJID, Msg, Affiliations),
    {ok, noreply}.

-spec handle_body_message(jid(), jid(), #xmlel{}, affiliations()) ->
    packet_processing_result().
handle_body_message(From, RoomJID, Msg, Affiliations) ->
    bcast_message(From, RoomJID, Msg, Affiliations),
    {ok, noreply}.

-spec bcast_message(jid(), jid(), #xmlel{}, affiliations()) -> ok.
bcast_message(From, RoomJID, Msg, Affiliations) ->
    MessageToSend = make_message_from_room(From, Msg),
    BCastFrom = rooms_user_jid(RoomJID, From),
    ?BCASTER:broadcast(BCastFrom, MessageToSend, Affiliations).

%%====================================================================
%% Response processing
%%====================================================================

-spec send_response(jid(), jid(), #xmlel{}, packet_processing_result()) -> ok.
send_response(_From, _RoomJID, _OriginalPacket, {ok, noreply}) ->
    ok;
send_response(From, RoomJID, _OriginalPacket, {ok, Packet}) ->
    ejabberd_router:route(RoomJID, From, Packet);
send_response(From, RoomJID, OriginalPacket, {error, Reason}) ->
    ErrorElem = case Reason of
                    registration_required ->
                        ?ERR_REGISTRATION_REQUIRED;
                    bad_request ->
                        ?ERR_BAD_REQUEST;
                    not_exists ->
                        ?ERR_ITEM_NOT_FOUND;
                    not_allowed ->
                        ?ERR_NOT_ALLOWED;
                    _Other ->
                        ?ERRT_BAD_REQUEST(
                           <<"en">>, io_lib:format("~p", [Reason]))
                end,
    Reply = jlib:make_error_reply(OriginalPacket, ErrorElem),
    ejabberd_router:route(RoomJID, From, Reply).

%%====================================================================
%% Internal functions
%%====================================================================

-spec affiliations_to_items(affiliations()) -> [#xmlel{}].
affiliations_to_items(Affiliations) ->
    [ #xmlel{ name = <<"item">>,
              attrs = [{<<"affiliation">>, aff2b(Affiliation)},
                       {<<"jid">>, jlib:jid_to_binary(JID)}] }
      || {JID, Affiliation} <- Affiliations ].

-spec aff2b(affiliation()) -> binary().
aff2b(owner) -> <<"owner">>;
aff2b(member) -> <<"member">>;
aff2b(none) -> <<"none">>.

-spec items_to_affiliations([#xmlel{}]) -> affiliations().
items_to_affiliations(Items) ->
    lists:map(fun(Item) ->
                      BinJID = exml_query:path(Item, [{attr, <<"jid">>}]),
                      BinAff = exml_query:path(Item, [{attr, <<"affiliation">>}]),
                      {lower_nores(jlib:binary_to_jid(BinJID)), b2aff(BinAff)}
              end, Items).

-spec b2aff(binary()) -> affiliation().
b2aff(<<"owner">>) -> owner;
b2aff(<<"member">>) -> member;
b2aff(<<"none">>) -> none.

-spec make_affiliation_message(jid(), affiliations()) -> #xmlel{}.
make_affiliation_message(RoomJID, ChangedAffiliations) ->
    XElem = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT}],
                    children = affiliations_to_items(ChangedAffiliations) },
    BodyText = lists:map(
                 fun
                     ({{Username, _, _}, none}) ->
                         [<<"User ">>, Username,
                         <<" has been removed from member list.\n">>];
                     ({{Username, _, _}, member}) ->
                         [<<"User ">>, Username,
                          <<" is now a member.\n">>];
                     ({{Username, _, _}, owner}) ->
                         [<<"User ">>, Username,
                          <<" is now the owner of this room.\n">>]
                 end, ChangedAffiliations),
    Body = #xmlel{ name = <<"body">>,
                   children = [#xmlcdata{ content = BodyText }] },
    #xmlel{ name = <<"message">>,
            attrs = [{<<"from">>, jlib:jid_to_binary(nores(RoomJID))},
                     {<<"type">>, <<"groupchat">>}],
            children = [XElem, Body] }.

-spec rooms_user_jid(jid(), jid()) -> jid().
rooms_user_jid(RoomJID, From) ->
    jlib:jid_replace_resource(
      RoomJID, jlib:jid_to_binary(nores(From))).

-spec make_message_from_room(jid(), #xmlel{}) -> #xmlel{}.
make_message_from_room(FromJID, Packet) ->
    RoomBin = exml_query:path(Packet, [{attr, <<"to">>}]),
    UserBareBin = jlib:jid_to_binary(nores(FromJID)),
    NewFrom = <<RoomBin/binary, $/, UserBareBin/binary>>,
    NewAttrs = lists:keystore(<<"from">>, 1, Packet#xmlel.attrs,
                              {<<"from">>, NewFrom}),
    Packet#xmlel{ attrs = NewAttrs }.

%% Tiny helpers. I hate using these long-named functions from jlib. :)
-spec nores(jid()) -> jid().
nores(JID) -> jlib:jid_remove_resource(JID).

-spec lower_nores(jid()) -> {binary(), binary(), <<>>}.
lower_nores(JID) -> jlib:jid_remove_resource(jlib:jid_to_lower(JID)).
