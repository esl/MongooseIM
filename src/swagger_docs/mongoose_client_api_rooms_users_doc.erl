%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Rooms: /api/rooms/:id/users/[:user]
%%% @end
%%% ==================================================================

-module(mongoose_client_api_rooms_users_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_204_NO_CONTENT, 204).
-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_403_FORBIDDEN, 403).
-define(HTTP_404_NOT_FOUND, 404).
-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).

%%% ==================================================================
%%% Trails Callbacks
%%% ==================================================================

-behaviour(trails_handler).

-export([trails/0]).

trails() ->
  %% Definitions
  DefSetUserRoomReq = <<"req-body-client-api-room-set-users">>,

  %% Properties
  PropRoomParticipantReq = #{
    <<"user">> => #{
      type => <<"string">>,
      description => <<"This is the user's JID (Jabber ID) which consist of username and server parts. Example: alice@wonderland.com">>,
      default => <<"alice@wonderland.com">>
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefSetUserRoomReq, PropRoomParticipantReq}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestBodyParticipantRoom = #{
    in => body,
    name => <<"room">>,
    type => <<"object">>,
    description => <<"Room">>,
    required => true,
    schema => cowboy_swagger:schema(DefSetUserRoomReq)
  },

  %% Pushs
  RequestPushRoomId = #{
    in => path,
    name => <<"id">>,
    type => <<"string">>,
    description => <<"The ID of a room">>,
    default => <<"1575-564351-207767">>,
    required => true
  },

  RequestPushRoomUser = #{
    in => path,
    name => <<"user">>,
    type => <<"string">>,
    description => <<"The JID (ex: alice@wonderalnd.com) of user to remove">>,
    default => <<"alice@wonderalnd.com">>,
    required => true
  },

  %% Meta data
  MetadataClientApiRoomUsers = #{
    post => #{
      tags => ["Rooms"],
      description => <<"Adds a user to a room">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPushRoomId, RequestBodyParticipantRoom],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"User was successfully added to the room">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_403_FORBIDDEN => #{description => <<"When the authenticated user is not allowed to add users to the room">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"When there is no room with the given ID">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    delete => #{
      tags => ["Rooms"],
      description => <<"Removes a user from the room. The owner can remove any user. The occupant can also use this method, but can only remove themself">>,
      parameters => [RequestPushRoomId, RequestPushRoomUser],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"User was successfully added to the room">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_403_FORBIDDEN => #{description => <<"When the authenticated user is not allowed to add users to the room">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"When there is no room with the given ID">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiRoomsConfig = "/api/rooms/:id/users/[:user]",

  %% Options
  StoreOptions = [
    {PathClientApiRoomsConfig, #{path => PathClientApiRoomsConfig}, MetadataClientApiRoomUsers, mongoose_client_api_rooms_users}
  ],

  %% Trail all data
  [trails:trail(Path, Module, Options, Metadata) || {Path, Options, Metadata, Module} <- StoreOptions].
