%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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

-module(muc_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUC_HOST, <<"muc.localhost">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, disco},
     {group, room_management}].

groups() ->
    [{disco, [sequence], [disco_service,
                          disco_features,
                          disco_rooms]},
      {room_management, [sequence], [create_and_destroy_room]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

disco_service(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        escalus:assert(has_service, [?MUC_HOST], escalus:wait_for_stanza(Alice))
        end).

disco_features(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
          Server = escalus_client:server(Alice),
          escalus:send(Alice, stanza_get_features(Server)),
          has_features(escalus:wait_for_stanza(Alice))      
        end).

disco_rooms(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
          Server = escalus_client:server(Alice),
          escalus:send(Alice, stanza_get_rooms(Server)),
          count_rooms(escalus:wait_for_stanza(Alice), 0)
        end).

create_and_destroy_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
          Server = escalus_client:server(Alice),
          Room1 = stanza_create_room(<<"room1">>, <<"nick1">>), 
          escalus:send(Alice, Room1),
          %Alice gets topic message after creating the room
          [S, _S2] = escalus:wait_for_stanzas(Alice, 2),
          was_room_created(S),
          
          DestroyRoom1 = stanza_destroy_room(<<"room1">>),
          escalus:send(Alice, DestroyRoom1),
          [Presence, Iq] = escalus:wait_for_stanzas(Alice, 2),
          error_logger:info_msg("~p~n~p~n",[Presence, Iq]),
          was_room_destroyed(Iq),
          was_destroy_presented(Presence)
        end).

%%--------------------------------------------------------------------
%% Helpers (stanzas)
%%--------------------------------------------------------------------

stanza_destroy_room(Room) ->
  Payload = [ #xmlelement{name = <<"destroy">>, attrs = [], body = []} ],
  stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, Payload), Room).

stanza_create_room(Room, Nick) ->
  stanza_to_room(#xmlelement{name = <<"presence">>}, Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
  escalus_stanza:to(Stanza, <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>).

stanza_to_room(Stanza, Room) ->
  escalus_stanza:to(Stanza, <<Room/binary, "@", ?MUC_HOST/binary>>).

stanza_get_rooms(Config) ->
  %% <iq from='hag66@shakespeare.lit/pda'
  %%   id='zb8q41f4'
  %%   to='chat.shakespeare.lit'
  %%   type='get'>
  %% <query xmlns='http://jabber.org/protocol/disco#items'/>
  %% </iq>

  escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
                          ?MUC_HOST).

stanza_get_features(Config) ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='lx09df27'
    %%     to='chat.shakespeare.lit'
    %%     type='get'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_INFO, []), <<"to">>,
                             ?MUC_HOST).

stanza_get_services(Config) ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='h7ns81g'
    %%     to='shakespeare.lit'
    %%     type='get'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
                           escalus_config:get_config(ejabberd_domain, Config)).

%%--------------------------------------------------------------------
%% Helpers (assertions)
%%--------------------------------------------------------------------

was_destroy_presented(#xmlelement{body = [Items]} = Presence) ->
  #xmlelement{} = exml_query:subelement(Items, <<"destroy">>),
  <<"unavailable">> = exml_query:attr(Presence, <<"type">>).

was_room_destroyed(Query) ->
  <<"result">> = exml_query:attr(Query, <<"type">>).

was_room_created(#xmlelement{body = [ #xmlelement{body = Items} = Query ]}) ->
  #xmlelement{name = _, attrs = _, body = _} = Status = exml_query:subelement(Query, <<"status">>),
  <<"201">> = exml_query:attr(Status, <<"code">>),

  #xmlelement{name = _, attrs = _, body = _} = Item = exml_query:subelement(Query, <<"item">>),
  <<"owner">> = exml_query:attr(Item, <<"affiliation">>),
  <<"moderator">> = exml_query:attr(Item, <<"role">>).

has_room(JID, #xmlelement{body = [ #xmlelement{body = Rooms} ] = Query}) ->
  %% <iq from='chat.shakespeare.lit'
  %%   id='zb8q41f4'
  %%   to='hag66@shakespeare.lit/pda'
  %%   type='result'>
  %% <query xmlns='http://jabber.org/protocol/disco#items'>
  %%    <item jid='heath@chat.shakespeare.lit'
  %%         name='A Lonely Heath'/>
  %%    <item jid='coven@chat.shakespeare.lit'
  %%         name='A Dark Cave'/>
  %%    <item jid='forres@chat.shakespeare.lit'
  %%         name='The Palace'/>
  %%     <item jid='inverness@chat.shakespeare.lit'
  %%         name='Macbeth&apos;s Castle'/>
  %%   </query>
  %% </iq>
  
  RoomPred = fun(Item) ->
                exml_query:attr(Item, <<"jid">>) == JID
             end,
  true = lists:any(RoomPred, Rooms).

count_rooms(#xmlelement{body = [ #xmlelement{body = Rooms} ] = Query}, N) ->
  N = length(Rooms).

has_features(#xmlelement{body = [ Query ]}) ->
    %%<iq from='chat.shakespeare.lit'
    %%  id='lx09df27'
    %%  to='hag66@shakespeare.lit/pda'
    %%  type='result'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'>
    %%    <identity
    %%      category='conference'
    %%      name='Shakespearean Chat Service'
    %%      type='text'/>
    %%      <feature var='http://jabber.org/protocol/muc'/>
    %%  </query>
    %%</iq>

    Identity = exml_query:subelement(Query, <<"identity">>),
    <<"conference">> = exml_query:attr(Identity, <<"category">>),
    #xmlelement{name = _Name, attrs = _Attrs, body = _Body} = exml_query:subelement(Query, <<"feature">>).

has_muc(#xmlelement{body = [ #xmlelement{body = Services} = Query ]}) ->
    %% should be along the lines of (taken straight from the XEP):
    %% <iq from='shakespeare.lit'
    %%     id='h7ns81g'
    %%     to='hag66@shakespeare.lit/pda'
    %%     type='result'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'>
    %%     <item jid='chat.shakespeare.lit'
    %%           name='Chatroom Service'/>
    %%   </query>
    %% </iq>

    %% is like this:
    %% {xmlelement,<<"iq">>,
    %%     [{<<"from">>,<<"localhost">>},
    %%         {<<"to">>,<<"alice@localhost/res1">>},
    %%         {<<"id">>,<<"a5eb1dc70826598893b15f1936b18a34">>},
    %%         {<<"type">>,<<"result">>}],
    %%     [{xmlelement,<<"query">>,
    %%             [{<<"xmlns">>,
    %%                     <<"http://jabber.org/protocol/disco#items">>}],
    %%             [{xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"vjud.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"pubsub.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"muc.localhost">>}],
    %%                     []},
    %%                 {xmlelement,<<"item">>,
    %%                     [{<<"jid">>,<<"irc.localhost">>}],
    %%                     []}]}]}
    %% how to obtaing output like the above? simply put this in the test case:
    %% S = escalus:wait_for_stanza(Alice),
    %% error_logger:info_msg("~p~n", [S]),
    IsMUC = fun(Item) ->
                exml_query:attr(Item, <<"jid">>) == ?MUC_HOST
            end,
    lists:any(IsMUC, Services).
