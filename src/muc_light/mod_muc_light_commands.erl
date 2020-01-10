%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
%%
%% Author: Joseph Yiasemides <joseph.yiasemides@erlang-solutions.com>
%% Description: Administration commands for MUC Light
%%==============================================================================

-module(mod_muc_light_commands).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2, stop/1]).

-export([create_unique_room/4]).
-export([create_identifiable_room/5]).
-export([send_message/4]).
-export([invite_to_room/4]).
-export([change_affiliation/5]).
-export([delete_room/3]).
-export([change_room_config/5]).


-include("mod_muc_light.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").


%%--------------------------------------------------------------------
%% `gen_mod' callbacks
%%--------------------------------------------------------------------

start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).


%%--------------------------------------------------------------------
%% Interface descriptions
%%--------------------------------------------------------------------

commands() ->

    [
     [{name, create_muc_light_room},
      {category, <<"muc-lights">>},
      {desc, <<"Create a MUC Light room with unique username part in JID.">>},
      {module, ?MODULE},
      {function, create_unique_room},
      {action, create},
      {identifiers, [domain]},
      {args,
       [
        %% The parent `domain' under which MUC Light is
        %% configured.
        {domain, binary},
        {name, binary},
        {owner, binary},
        {subject, binary}
       ]},
      {result, {name, binary}}],

     [{name, create_identifiable_muc_light_room},
      {category, <<"muc-lights">>},
      {desc, <<"Create a MUC Light room with user-provided username part in JID">>},
      {module, ?MODULE},
      {function, create_identifiable_room},
      {action, update},
      {identifiers, [domain]},
      {args,
       [{domain, binary},
        {id, binary},
        {name, binary},
        {owner, binary},
        {subject, binary}
       ]},
      {result, {id, binary}}],

     [{name, change_muc_light_room_configuration},
      {category, <<"muc-lights">>},
      {subcategory, <<"config">>},
      {desc, <<"Change configuration of MUC Light room.">>},
      {module, ?MODULE},
      {function, change_room_config},
      {action, update},
      {identifiers, [domain]},
      {args,
       [
        {domain, binary},
        {id, binary},
        {name, binary},
        {user, binary},
        {subject, binary}
       ]},
      {result, ok}],

     [{name, invite_to_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"participants">>},
      {desc, <<"Invite to a MUC Light room.">>},
      {module, ?MODULE},
      {function, invite_to_room},
      {action, create},
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
        {name, binary},
        {sender, binary},
        {recipient, binary}
       ]},
      {result, ok}],

     [{name, send_message_to_muc_light_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"messages">>},
      {desc, <<"Send a message to a MUC Light room.">>},
      {module, ?MODULE},
      {function, send_message},
      {action, create},
      {identifiers, [domain, name]},
      {args,
       [{domain, binary},
        {name, binary},
        {from, binary},
        {body, binary}
       ]},
      {result, ok}],

     [{name, delete_room},
      {category, <<"muc-lights">>},
      {subcategory, <<"management">>},
      {desc, <<"Delete a MUC Light room.">>},
      {module, ?MODULE},
      {function, delete_room},
      {action, delete},
      {identifiers, [domain, name, owner]},
      {args,
       [{domain, binary},
        {name, binary},
        {owner, binary}]},
      {result, ok}]
    ].


%%--------------------------------------------------------------------
%% Internal procedures
%%--------------------------------------------------------------------

create_unique_room(Domain, RoomName, Creator, Subject) ->
    create_room(Domain, <<>>, RoomName, Creator, Subject).

create_identifiable_room(Domain, Identifier, RoomName, Creator, Subject) ->
    create_room(Domain, Identifier, RoomName, Creator, Subject).

invite_to_room(Domain, RoomName, Sender, Recipient0) ->
    Recipient1 = jid:binary_to_bare(Recipient0),
    {ok, R, _Aff} = muc_light_room_name_to_jid_and_aff(jid:from_binary(Sender), RoomName, Domain),
    S = jid:binary_to_bare(Sender),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                    [affiliate(jid:to_binary(Recipient1), <<"member">>)]),
    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                   <<"set">>, [Changes])).

change_affiliation(Domain, RoomID, Sender, Recipient0, Affiliation) ->
    Recipient1 = jid:binary_to_bare(Recipient0),
    MUCLightDomain = gen_mod:get_module_opt_subhost(Domain, mod_muc_light,
                                                    mod_muc_light:default_host()),
    R = jid:make(RoomID, MUCLightDomain, <<>>),
    S = jid:binary_to_bare(Sender),
    Changes = query(?NS_MUC_LIGHT_AFFILIATIONS,
                    [affiliate(jid:to_binary(Recipient1), Affiliation)]),
    ejabberd_router:route(S, R, iq(jid:to_binary(S), jid:to_binary(R),
                                   <<"set">>, [Changes])).

change_room_config(Domain, RoomID, RoomName, User, Subject) ->
    MUCLightDomain = gen_mod:get_module_opt_subhost(
                       Domain, mod_muc_light, mod_muc_light:default_host()),
    UserUS = jid:binary_to_bare(User),
    ConfigReq = #config{ raw_config =
                         [{<<"roomname">>, RoomName}, {<<"subject">>, Subject}]},
    case mod_muc_light:change_room_config(UserUS, RoomID, MUCLightDomain, ConfigReq) of
        {ok, _RoomJID, _}  ->
            ok;
        {error, _Reason} = E ->
            E
    end.

send_message(Domain, RoomName, Sender, Message) ->
    Body = #xmlel{name = <<"body">>,
                  children = [ #xmlcdata{ content = Message } ]
                 },
    Stanza = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"groupchat">>}],
                    children = [ Body ]
                   },
    S = jid:binary_to_bare(Sender),
    case get_user_rooms(jid:to_lus(S), Domain) of
        [] ->
            {error, given_user_does_not_occupy_any_room};
        RoomJIDs when is_list(RoomJIDs) ->
            FindFun = find_room_and_user_aff_by_room_name(RoomName, jid:to_lus(S)),
            {ok, {RU, RS}, _Aff} = lists:foldl(FindFun, none, RoomJIDs),
            true = is_subdomain(RS, Domain),
            R = jid:make(RU, RS, <<>>),
            ejabberd_router:route(S, R, Stanza)
    end.

-spec delete_room(DomainName :: binary(), RoomName :: binary(),
                  Owner :: binary()) ->
                         ok | {error, not_exists} | {error, not_allowed}.
delete_room(DomainName, RoomName, Owner) ->
    OwnerJID = jid:binary_to_bare(Owner),
    case muc_light_room_name_to_jid_and_aff(OwnerJID, RoomName, DomainName) of
        {error, _} = Error -> Error;
        {ok, RoomJID, owner} -> mod_muc_light:delete_room(jid:to_lus(RoomJID));
        {ok, _, _} -> {error, not_allowed}
    end.

%%--------------------------------------------------------------------
%% Ancillary
%%--------------------------------------------------------------------

create_room(Domain, Identifier, RoomName, Creator, Subject) ->
    C = jid:to_lus(jid:from_binary(Creator)),
    MUCLightDomain = gen_mod:get_module_opt_subhost(
                       Domain, mod_muc_light, mod_muc_light:default_host()),
    MUCService = jid:make(Identifier, MUCLightDomain, <<>>),
    Config = make_room_config(RoomName, Subject),
    case mod_muc_light:try_to_create_room(C, MUCService, Config) of
        {ok, RoomUS, _} ->
            jid:to_binary(RoomUS);
        {error, _Reason} = E ->
            E
    end.

make_room_config(Name, Subject) ->
    #create{raw_config = [{<<"roomname">>, Name},
                          {<<"subject">>, Subject}]
           }.

-spec muc_light_room_name_to_jid_and_aff(UserJID :: jid:jid(),
                                         RoomName :: binary(),
                                         Domain :: jid:lserver()) ->
    {ok, jid:jid(), aff()} | {error, given_user_does_not_occupy_any_room}.
muc_light_room_name_to_jid_and_aff(UserJID, RoomName, Domain) ->
    UserUS = jid:to_lus(UserJID),
    case get_user_rooms(UserUS, Domain) of
        [] ->
            {error, given_user_does_not_occupy_any_room};
        RoomUSs when is_list(RoomUSs) ->
            FindFun = find_room_and_user_aff_by_room_name(RoomName, UserUS),
            {ok, {RU, RS}, UserAff} = lists:foldl(FindFun, none, RoomUSs),
            true = is_subdomain(RS, Domain),
            {ok, jid:make(RU, RS, <<>>), UserAff}
    end.

-spec get_user_rooms(UserUS :: jid:simple_bare_jid(), Domain :: jid:lserver()) ->
    [jid:simple_bare_jid()].
get_user_rooms(UserUS, Domain) ->
    mod_muc_light_db_backend:get_user_rooms(UserUS, Domain).

-spec get_room_name_and_user_aff(RoomUS :: jid:simple_bare_jid(),
                                 UserUS :: jid:simple_bare_jid()) ->
    {ok, RoomName :: binary(), UserAff :: aff()} | {error, not_exists}.
get_room_name_and_user_aff(RoomUS, UserUS) ->
    case mod_muc_light_db_backend:get_info(RoomUS) of
        {ok, Cfg, Affs, _} ->
            {roomname, RoomName} = lists:keyfind(roomname, 1, Cfg),
            {_, UserAff} = lists:keyfind(UserUS, 1, Affs),
            {ok, RoomName, UserAff};
        Error ->
            Error
    end.

-type find_room_acc() :: {ok, RoomUS :: jid:simple_bare_jid(), UserAff :: aff()} | none.

-spec find_room_and_user_aff_by_room_name(RoomName :: binary(),
                                          UserUS :: jid:simple_bare_jid()) ->
    fun((RoomUS :: jid:simple_bare_jid(), find_room_acc()) -> find_room_acc()).
find_room_and_user_aff_by_room_name(RoomName, UserUS) ->
    fun (RoomUS, none) ->
            case get_room_name_and_user_aff(RoomUS, UserUS) of
                {ok, RoomName, UserAff} ->
                    {ok, RoomUS, UserAff};
                _ ->
                    none
            end;
        (_, Acc) when Acc =/= none ->
            Acc
    end.

is_subdomain(Child, Parent) ->
    %% Example input Child = <<"muclight.localhost">> and Parent =
    %% <<"localhost">>
    case binary:match(Child, Parent) of
        nomatch -> false;
        {_, _} -> true
    end.

iq(To, From, Type, Children) ->
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    #xmlel{name = <<"iq">>,
           attrs = [{<<"from">>, From},
                    {<<"to">>, To},
                    {<<"type">>, Type},
                    {<<"id">>, UUID}],
           children = Children
          }.

query(NS, Children) when is_binary(NS), is_list(Children) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, NS}],
           children = Children
          }.

affiliate(JID, Kind) when is_binary(JID), is_binary(Kind) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"affiliation">>, Kind}],
           children = [ #xmlcdata{ content = JID } ]
          }.

