%%% ==================================================================
%%% @doc
%%% This module provided documentation of MongooseIM client REST API Contacts: /api/contacts/[:jid]
%%% @end
%%% ==================================================================

-module(mongoose_client_api_contacts_doc).

%%% ==================================================================
%%% Macros: HTTP Response Codes
%%% ==================================================================

-define(HTTP_200_OK, 200).
-define(HTTP_204_NO_CONTENT, 204).
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
  DefPostContsReq = <<"req-body-client-api-post-contacts">>,
  DefSucsGetAllConts = <<"resp-success-body-client-api-contacts-get-all">>,
  DefDelConts = <<"req-body-client-api-contacts-del-contacts">>,
  DefSucsDelConts = <<"resp-success-client-api-contacts-del-contacts">>,
  DefContactsAction = <<"req-action-client-api-contacts-put-contacts">>,

  %% Properties
  PropPostContsReq = #{
    <<"jid">> => #{
      type => <<"string">>,
      description => <<"This is user's JID (Jabber ID) which consist of username and server. Example: alice@wonderland.com">>,
      default => <<"alice@wonderland.lit">>
    }
  },

  PropSucsGetAllContsResp = #{
    <<"jid">> => #{
      type => <<"string">>,
      description => <<"JID of roster">>,
      default => <<"alice@wonderland.lit">>
    },
    <<"subscription">> => #{
      type => <<"string">>,
      description => <<"Subscription state of me vs contact. there are four possible state: 'none', 'both', 'to' - I receive updates about the contact's presence, 'from' - the contact receives updates about my presence">>,
      default => <<"none">>
    },
    <<"ask">> => #{
      type => <<"string">>,
      description => <<"Tells whether one of us has asked the other for subscription to presence info and is waiting for approval. Possible states: 'none', 'both', 'out' - I asked the contact and am waiting for his approval, 'in' - my contact asked me, it is up to me to decide">>,
      default => <<"none">>
    }
  },

  PropContactToDelete = #{
    <<"to_delete">> => #{
      type => <<"array">>,
      description => <<"Returns a list of not deleted users">>,
      items => #{
        type => <<"string">>,
        default => <<"alice@wonderland.lit">>
      }
    }
  },

  PropSucsContactsToDelete = #{
    <<"not_deleted">> => #{
      type => <<"array">>,
      description => <<"Returns a list of not deleted users">>,
      items => #{
        type => <<"string">>,
        default => <<"alice@wonderland.lit">>
      }
    }
  },

  PropPutContactUserAction = #{
    <<"action">> => #{
      type => <<"string">>,
      description => <<"Can be 'invite' or 'accept'">>,
      default => <<"invite">>
    }
  },

  %% Definitions and properties
  DefinitionsAndProperties = [
    {DefPostContsReq, PropPostContsReq},
    {DefSucsGetAllConts, PropSucsGetAllContsResp},
    {DefDelConts, PropContactToDelete},
    {DefSucsDelConts, PropSucsContactsToDelete},
    {DefContactsAction, PropPutContactUserAction}
  ],

  %% Add definitions
  lists:foreach(
    fun({Definition, DefinitionProperties}) ->
      cowboy_swagger:add_definition(Definition, DefinitionProperties)
    end, DefinitionsAndProperties),

  %% Request Body
  RequestPostBodyContact = #{
    name => <<"Request Body">>,
    in => body,
    required => true,
    description => <<"Contact body">>,
    schema => cowboy_swagger:schema(DefPostContsReq)
  },

  RequestBodyContactsDelete = #{
    name => <<"contacts">>,
    in => body,
    required => true,
    type => <<"object">>,
    schema => cowboy_swagger:schema(DefDelConts)
  },

  RequestBodyContactsUserAction = #{
    name => <<"action">>,
    in => body,
    required => true,
    type => <<"object">>,
    description => <<"Can be 'invite' or 'accept'">>,
    schema => cowboy_swagger:schema(DefContactsAction)
  },

  %% Pushs
  RequestPushContactWith = #{
    in => path,
    name => <<"jid">>,
    type => <<"string">>,
    description => <<"User JID. Example: alice@wonderland.com.">>,
    default => <<"alice@wonderland.lit">>,
    required => true
  },

  %% Success responses
  SucsGetAllConts = #{
    type => <<"array">>,
    items => #{
      type => <<"object">>,
      schema => cowboy_swagger:schema(DefSucsGetAllConts)
    }
  },

  %% Meta data
  MetadataClientApiContacts = #{
    post => #{
      tags => ["Contacts"],
      description => <<"Adds a user to a contact list.">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestPostBodyContact],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"No Content">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    get => #{
      tags => ["Contacts"],
      description => <<"Returns all contacts from the user's roster">>,
      produces => ["application/json"],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => SucsGetAllConts},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_400_BAD_REQUEST => #{description => <<"Bad Request">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    delete => #{
      tags => ["Contacts"],
      description => <<"Removes a list of users">>,
      consumes => ["application/json"],
      produces => ["application/json"],
      parameters => [RequestBodyContactsDelete],
      responses => #{
        ?HTTP_200_OK => #{description => <<"OK">>, schema => cowboy_swagger:schema(DefSucsDelConts)},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"Not Found">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Meta data
  MetadataClientApiWithJid = #{
    delete => #{
      tags => ["Contacts"],
      description => <<"Removes contact">>,
      parameters => [RequestPushContactWith],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"No Content">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"Not Found">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    },
    put => #{
      tags => ["Contacts"],
      description => <<"Manage subscription">>,
      produces => ["application/json"],
      parameters => [RequestBodyContactsUserAction, RequestPushContactWith],
      responses => #{
        ?HTTP_204_NO_CONTENT => #{description => <<"A subscription request was sent to the contact with value 'subscribe' or 'subscribed' (it may and may not change the 'subscription' and 'ask' states, depending what they were)">>},
        ?HTTP_401_UNAUTHORIZED => #{description => <<"Unauthorized">>},
        ?HTTP_404_NOT_FOUND => #{description => <<"The contact is not in the user's roster">>},
        ?HTTP_500_INTERNAL_SERVER_ERROR => #{description => <<"Internal Server Error">>}
      }
    }
  },

  %% Paths
  PathClientApiContacts = "/api/contacts",
  PathClientApiContactsWithJid = "/api/contacts/[:jid]",

  %% Options
  StoreOptions = [
    {PathClientApiContacts, #{path => PathClientApiContacts}, MetadataClientApiContacts, mongoose_client_api_contacts},
    {PathClientApiContactsWithJid, #{path => PathClientApiContactsWithJid}, MetadataClientApiWithJid, mongoose_client_api_contacts}
  ],

  %% Trail all data
  [trails:trail(Path, Module, Options, Metadata) || {Path, Options, Metadata, Module} <- StoreOptions].
