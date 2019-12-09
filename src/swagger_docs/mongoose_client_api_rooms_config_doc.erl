%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Rooms: /api/rooms/[:id]/config
%%% @end
%%% ==================================================================

-module(mongoose_client_api_rooms_config_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_204_NO_CONTENT, 204).
-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).

%%% ==================================================================
%%% Trails Callbacks
%%% ==================================================================

-behaviour(trails_handler).

-export([trails/0]).

trails() ->
  %% Definitions
  DefConfigRoomReq = <<"req-body-client-api-room-config">>,

  %% Properties
  PropConfigRoomReq = #{
    <<"subject">> => #{
      type => <<"string">>,
      description => <<"The subject of the room">>,
      default => <<"Only important things">>
    },
    <<"name">> => #{
      type => <<"string">>,
      description => <<"The room's name">>,
      default => <<"Important room">>
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefConfigRoomReq, PropConfigRoomReq}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestBodyConfigRoom = #{
    in => body,
    name => <<"room">>,
    type => <<"object">>,
    description => <<"Room">>,
    required => true,
    schema => cowboy_swagger:schema(DefConfigRoomReq)
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

  %% Meta data
  MetadataClientApiRoomConfig = #{
    put => #{
      tags => ["Rooms"],
      description => <<"Changes room configuration">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPushRoomId, RequestBodyConfigRoom],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"Configuration was successfully changed">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiRoomsConfig = "/api/rooms/[:id]/config",

  %% Trail all data
  [trails:trail(PathClientApiRoomsConfig, mongoose_client_api_rooms_config, #{path => PathClientApiRoomsConfig}, MetadataClientApiRoomConfig)].
