%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Messages: /api/messages/[:with]
%%% This module can be used as base template for other custom REST API documentation
%%% @end
%%% ==================================================================

-module(mongoose_client_api_messages_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_200_OK, 200).
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
  DefSendMsgsReq = <<"req-body-client-api-send-messages">>,
  DefSendMsgsResp = <<"resp-body-client-api-send-messages">>,
  DefSucsSendMsgsResp = <<"resp-success-body-client-api-send-messages">>,

  %% Properties
  PropSendMsgsReq = #{
    <<"to">> => #{
      type => <<"string">>,
      description => <<"This is user's JID (Jabber ID) which consist of username and server. Example: alice@wonderland.com">>,
      default => <<"alice@wonderland.lit">>
    },
    <<"body">> => #{
      type => <<"string">>,
      description => <<"Message body">>,
      default => <<"Hello Alice!">>
    }
  },

  PropSendMsgsResp = #{
    <<"id">> => #{
      type => <<"string">>,
      default => <<"76bcaa97-a114-4cb0-b783-ff929d8d8428">>
    }
  },

  PropSucsSendMsgsResp = #{
    <<"to">> => #{
      type => <<"string">>,
      description => <<"The message recipient's bare JID">>,
      default => <<"alice@wonderland.lit">>
    },
    <<"from">> => #{
      type => <<"string">>,
      description => <<"The message sender's bare JID">>,
      default => <<"rabbit@wonderland.lit">>
    },
    <<"timestamp">> => #{
      type => <<"integer">>,
      description => <<"Unix timestamp in milliseconds when the message was sent">>,
      default => 1478258324908
    },
    <<"id">> => #{
      type => <<"string">>,
      description => <<"ID of message">>,
      default => <<"76bcaa97-a114-4cb0-b783-ff929d8d8428">>
    },
    <<"body">> => #{
      type => <<"string">>,
      description => <<"Message content">>,
      default => <<"Hello Alice!">>
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefSendMsgsReq, PropSendMsgsReq},
    {DefSendMsgsResp, PropSendMsgsResp},
    {DefSucsSendMsgsResp, PropSucsSendMsgsResp}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestBodyMessages = #{
    name => <<"Request Body">>,
    in => body,
    required => true,
    description => <<"The message to be sent">>,
    schema => cowboy_swagger:schema(DefSendMsgsReq)
  },

  %% Querys
  RequestQueryGetLimitMessages = #{
    name => <<"limit">>,
    description => <<"Specifies the maximum number of messages to be returned. Default value is 50.">>,
    in => query,
    type => <<"integer">>,
    default => 10
  },

  RequestQueryGetBeforeMessages = #{
    name => <<"before">>,
    description => <<"The timestamp in milliseconds. If set, only messages before this date will be returned.">>,
    in => query,
    type => <<"integer">>,
    default => 1478258324908
  },

  %% Pushs
  RequestPushGetMessagesWith = #{
    in => path,
    name => <<"with">>,
    type => <<"string">>,
    description => <<"JID of the user with whom the messages were exchanged. Example: alice@wonderland.com.">>,
    default => <<"alice@wonderland.com">>,
    required => true
  },

  %% Success responses
  SucsGetMesgs = #{
    type => <<"array">>,
    items => #{
    type => <<"object">>,
    schema => cowboy_swagger:schema(DefSucsSendMsgsResp)
    }
  },

  %% Meta data
  MetadataClientApiMessages = #{
    get => #{
      tags => ["One-to-one messages"],
      description => <<"Gets all recent messages from all users from the archive.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestQueryGetLimitMessages, RequestQueryGetBeforeMessages],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => SucsGetMesgs},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    post => #{
      tags => ["One-to-one messages"],
      description => <<"Sends messages.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestBodyMessages],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefSendMsgsResp)},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  MetadataClientApiMessagesGetWith = #{
    get => #{
      tags => ["One-to-one messages"],
      description => <<"Gets all recent messages from specified user from the archive.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPushGetMessagesWith, RequestQueryGetLimitMessages, RequestQueryGetBeforeMessages],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => SucsGetMesgs},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiMessages = "/api/messages",
  PathClientApiMessagesWith = "/api/messages/[:with]",

  %% Options
  StoreOptions = [
    {PathClientApiMessages, #{path => PathClientApiMessages}, MetadataClientApiMessages, mongoose_client_api_messages},
    {PathClientApiMessagesWith, #{path => PathClientApiMessagesWith}, MetadataClientApiMessagesGetWith, mongoose_client_api_messages}
  ],

  %% Trail all data
  [trails:trail(Path, Module, Options, Metadata) || {Path, Options, Metadata, Module} <- StoreOptions].
