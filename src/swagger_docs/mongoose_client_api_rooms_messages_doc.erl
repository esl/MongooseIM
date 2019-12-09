%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Rooms: /api/rooms/[:id]/messages
%%% @end
%%% ==================================================================

-module(mongoose_client_api_rooms_messages_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_200_OK, 200).
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
  DefSendMesgRoomReq = <<"req-body-client-api-room-send-message">>,
  DefMarkerRoomReq = <<"req-body-client-api-room-chat-marker">>,
  DefSendMesgRoomResp = <<"resp-body-client-api-room-send-message">>,
  DefGetMesgsRoomResp = <<"resp-body-client-api-room-get-messages">>,

  %% Properties
  PropRoomMsgReq = #{
    <<"body">> => #{
      type => <<"string">>,
      description => <<"Required if chat marker property is not set.">>,
      default => <<"Hello World!">>
    },
    <<"markable">> => #{
      type => <<"boolean">>,
      description => <<"May only be used when body is present as well">>,
      default => true
    },
    <<"chat_marker">> => #{
      type => <<"object">>,
      description => <<"May be used independently of body and markable properties">>,
      schema => cowboy_swagger:schema(DefMarkerRoomReq)
    }
  },

  PropRoomMarkerReq = #{
    <<"type">> => #{
      type => <<"string">>,
      description => <<"['received', 'displayed', 'acknowledged']">>,
      default => <<"received">>
    },
    <<"id">> => #{
      type => <<"string">>,
      description => <<"ID">>,
      default => <<"11-111-111">>
    }
  },

  PropRoomGetMsgsResp = #{
    <<"type">> => #{
      type => <<"string">>,
      description => <<"The type of a message, possible values are: 'message' for regular message 'affiliation' for affiliation changes. = ['message', 'affiliation']">>,
      default => <<"message">>
    },
    <<"from">> => #{
      type => <<"string">>,
      description => <<"The JID of message sender. In case of a regular message sent by room's participant the JID will be a full JID with the sender's bare JID as a resource part of room's JID">>,
      default => <<"alice@wonderland.com">>
    },
    <<"id">> => #{
      type => <<"string">>,
      description => <<"The ID of a message.">>,
      default => <<"56c331f6-23f0-48a5-8007-23a5353ecac4">>
    },
    <<"timestamp">> => #{
      type => <<"integer">>,
      description => <<"Unix timestamp in miliseconds">>,
      default => 1575632262307
    },
    <<"body">> => #{
      type => <<"string">>,
      description => <<"The message body. Present only if the type is 'message'">>,
      default => <<"Hello World!">>
    },
    <<"user">> => #{
      type => <<"string">>,
      description => <<"JID of a user, whom affiliation changed. Present only if the type is 'affiliation'">>,
      default => <<"alice@wonderland.com">>
    },
    <<"affiliation">> => #{
      type => <<"string">>,
      description => <<"The new affiliation of a user in the room. Present only if the type is 'affiliation'">>,
      default => <<"owner">>
    },
    <<"markable">> => #{
      type => <<"boolean">>,
      description => <<"The markable if body is present as well">>,
      default => true
    },
    <<"chat_marker">> => #{
      type => <<"object">>,
      description => <<"The chat_marker independently of body and markable properties">>,
      schema => cowboy_swagger:schema(DefMarkerRoomReq)
    }
  },

  PropRoomMarkerReq = #{
    <<"type">> => #{
      type => <<"string">>,
      description => <<"['received', 'displayed', 'acknowledged']">>,
      default => <<"received">>
    },
    <<"id">> => #{
      type => <<"string">>,
      description => <<"ID">>,
      default => <<"11-111-111">>
    }
  },

  PropRoomSendMsgsResp = #{
    <<"id">> => #{
      type => <<"string">>,
      description => <<"ID">>,
      default => <<"50009d9e-fa7f-4cf1-a291-b70b7decdc58">>
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefSendMesgRoomReq, PropRoomMsgReq},
    {DefMarkerRoomReq, PropRoomMarkerReq},
    {DefSendMesgRoomResp, PropRoomSendMsgsResp},
    {DefGetMesgsRoomResp, PropRoomGetMsgsResp}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestBodyMsgRoom = #{
    in => body,
    name => <<"message">>,
    type => <<"object">>,
    description => <<"Message">>,
    required => true,
    schema => cowboy_swagger:schema(DefSendMesgRoomReq)
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

  %% Querys
  RequestQueryRoomLimit = #{
    in => query,
    name => <<"limit">>,
    type => <<"integer">>,
    description => <<"Specifies the maximum number of messages to be returned. Default value is 50">>,
    default => 10
  },

  RequestQueryRoomBefore = #{
    in => query,
    name => <<"before">>,
    type => <<"integer">>,
    description => <<"The timestamp in milliseconds. If set, only messages before this date will be returned">>,
    default => 1575632262307
  },

  %% Meta data
  MetadataClientApiRoomUsers = #{
    post => #{
      tags => ["Rooms"],
      description => <<"Send a message to a room">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPushRoomId, RequestBodyMsgRoom],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefSendMesgRoomResp)},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_403_FORBIDDEN => #{description => <<"When the authenticated user is not allowed to add users to the room">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"When there is no room with the given ID">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    get => #{
      tags => ["Rooms"],
      description => <<"Gets room's messages from the archive">>,
      parameters => [RequestPushRoomId, RequestQueryRoomLimit, RequestQueryRoomBefore],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefGetMesgsRoomResp)},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_403_FORBIDDEN => #{description => <<"When the authenticated user is not allowed to add users to the room">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"When there is no room with the given ID">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiRoomsConfig = "/api/rooms/[:id]/messages",

  %% Options
  StoreOptions = [
    {PathClientApiRoomsConfig, #{path => PathClientApiRoomsConfig}, MetadataClientApiRoomUsers, mongoose_client_api_rooms_messages}
  ],

  %% Trail all data
  [trails:trail(Path, Module, Options, Metadata) || {Path, Options, Metadata, Module} <- StoreOptions].
