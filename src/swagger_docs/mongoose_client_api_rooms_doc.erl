%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Rooms: /api/rooms/[:id]
%%% @end
%%% ==================================================================

-module(mongoose_client_api_rooms_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_200_OK, 200).
-define(HTTP_201_CREATED, 201).
-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_404_NOT_FOUND, 404).
-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).

%%% ==================================================================
%%% Trails Callbacks
%%% ==================================================================

-behaviour(trails_handler).

-export([trails/0]).

trails() ->
  %% Definitions
  DefCreateRoomReq = <<"req-body-client-api-create-room">>,
  DefSucsGetRoomsResp = <<"resp-success-body-client-api-get-rooms">>,
  DefSucsCreateRoomResp = <<"resp-success-body-client-api-create-room">>,
  DefSucsGetRoomWithIdResp = <<"resp-success-body-client-api-get-room-with-id">>,
  DefSucsGetRoomItemWithIdResp = <<"resp-success-body-client-api-get-room-item-with-id">>,

  %% Properties
  PropCreateRoomReq = #{
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

  PropGetRoomsResp = #{
    <<"id">> => #{
      type => <<"string">>,
      description => <<"Room's ID">>,
      default => <<"1575-564351-207767">>
    },
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

  PropCreateRoomResp = #{
    <<"id">> => #{
      type => <<"id">>,
      description => <<"Room's ID">>,
      default => <<"1575-564351-207767">>
    }
  },

  PropItemGetRoomWithIdResp = #{
    <<"user">> => #{
      type => <<"string">>,
      description => <<"This is the user's JID (Jabber ID) which consist of username and server parts. Example: alice@wonderland.com">>,
      default => <<"alice@wonderland.com">>
    },
    <<"role">> => #{
      type => <<"string">>,
      description => <<"The role of the user in a given room (can be 'owner' or 'member'). = ['owner', 'member']">>,
      default => <<"owner">>
    }
  },

  PropGetRoomWithIdResp = #{
    <<"subject">> => #{
      type => <<"string">>,
      description => <<"The subject of the room">>,
      default => <<"Only important things">>
    },
    <<"name">> => #{
      type => <<"string">>,
      description => <<"The room's name">>,
      default => <<"Important room">>
    },
    <<"participants">> => #{
      type => <<"array">>,
      description => <<"Participants">>,
      items => cowboy_swagger:schema(DefSucsGetRoomItemWithIdResp)
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefCreateRoomReq, PropCreateRoomReq},
    {DefSucsGetRoomsResp, PropGetRoomsResp},
    {DefSucsCreateRoomResp, PropCreateRoomResp},
    {DefSucsGetRoomWithIdResp, PropGetRoomWithIdResp},
    {DefSucsGetRoomItemWithIdResp, PropItemGetRoomWithIdResp}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestBodyCreateRoom = #{
    in => body,
    name => <<"room">>,
    type => <<"object">>,
    description => <<"Creates a room">>,
    required => true,
    schema => cowboy_swagger:schema(DefCreateRoomReq)
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

  %% Success responses
  SucsGetRooms = #{
    type => <<"array">>,
    items => #{
    type => <<"object">>,
    schema => cowboy_swagger:schema(DefSucsGetRoomsResp)
    }
  },

  %% Meta data
  MetadataClientApiRoom = #{
    get => #{
      tags => ["Rooms"],
      description => <<"Returns a list of rooms to which the user has subscribed.">>,
      produces => ["application/json"],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => SucsGetRooms},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    post => #{
      tags => ["Rooms"],
      description => <<"Creates a room.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestBodyCreateRoom],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefSucsCreateRoomResp)},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"Not found">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  MetadataClientApiRoomWithId = #{
    get => #{
      tags => ["Rooms"],
      description => <<"Returns room's details.">>,
      produces => ["application/json"],
      parameters => [RequestPushRoomId],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefSucsGetRoomWithIdResp)},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    put => #{
      tags => ["Rooms"],
      description => <<"Creates a room with the given ID.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPushRoomId, RequestBodyCreateRoom],
      responses => #{
        ?HTTP_201_CREATED => #{description => <<"Created">>, schema => cowboy_swagger:schema(DefSucsCreateRoomResp)},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"Not found">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiRooms = "/api/rooms",
  PathClientApiRoomsWithId = "/api/rooms/[:id]",

  %% Options
  StoreOptions = [
    {PathClientApiRooms, #{path => PathClientApiRooms}, MetadataClientApiRoom, mongoose_client_api_rooms},
    {PathClientApiRoomsWithId, #{path => PathClientApiRoomsWithId}, MetadataClientApiRoomWithId, mongoose_client_api_rooms}
  ],

  %% Trail all data
  [trails:trail(Path, Module, Options, Metadata) || {Path, Options, Metadata, Module} <- StoreOptions].
