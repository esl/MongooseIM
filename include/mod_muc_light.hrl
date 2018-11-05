-define(NS_MUC_LIGHT, <<"urn:xmpp:muclight:0">>).
-define(NS_MUC_LIGHT_CONFIGURATION, <<"urn:xmpp:muclight:0#configuration">>).
-define(NS_MUC_LIGHT_AFFILIATIONS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_MUC_LIGHT_INFO, <<"urn:xmpp:muclight:0#info">>).
-define(NS_MUC_LIGHT_BLOCKING, <<"urn:xmpp:muclight:0#blocking">>).
-define(NS_MUC_LIGHT_CREATE, <<"urn:xmpp:muclight:0#create">>).
-define(NS_MUC_LIGHT_DESTROY, <<"urn:xmpp:muclight:0#destroy">>).

-define(DEFAULT_EQUAL_OCCUPANTS, false).
-define(DEFAULT_LEGACY_MODE, false).
-define(DEFAULT_ROOMS_PER_USER, infinity).
-define(DEFAULT_BLOCKING, true).
-define(DEFAULT_ALL_CAN_CONFIGURE, false).
-define(DEFAULT_ALL_CAN_INVITE, false).
-define(DEFAULT_MAX_OCCUPANTS, infinity).
-define(DEFAULT_ROOMS_PER_PAGE, 10).
-define(DEFAULT_ROOMS_IN_ROSTERS, false).

-type schema_value_type() :: binary | integer | float.
-type schema_item() :: {FormFieldName :: binary(), OptionName :: atom(),
                        ValueType :: schema_value_type()}.
-type config_schema() :: [schema_item()].

-type config_item() :: {Key :: atom(), Value :: term()}.
-type config() :: [config_item()].
-type raw_config() :: [{Key :: binary(), Value :: binary()}].

-type aff() :: owner | member | none.
-type aff_user() :: {jid:simple_bare_jid(), aff()}.
-type aff_users() :: [aff_user()].
-type validation_error() :: {error, {Key :: binary(), Reason :: atom()}}.

-type external_check_fun() :: fun((RoomUS :: jid:simple_bare_jid(),
                                   NewAffUsers :: aff_users()) ->
                                   ok | {error, any()}).

-type rooms_per_user() :: infinity | non_neg_integer().

-type blocking_what() :: user | room.
-type blocking_action() :: allow | deny.
-type blocking_who() :: jid:simple_bare_jid().
-type blocking_item() :: {
        What :: blocking_what(),
        Action :: blocking_action(),
        Who :: blocking_who()
       }.

-type disco_room_info() :: {RoomUS :: jid:simple_bare_jid(),
                            RoomName :: binary(),
                            RoomVersion :: binary()}.

-record(disco_info, {
          id = <<>> :: binary()
         }).

-type disco_info_req_props() :: #disco_info{}.

-record(disco_items, {
          id = <<>> :: binary(),
          rooms = [] :: [disco_room_info()],
          rsm = none :: none | jlib:rsm_in() | jlib:rsm_out()
         }).

-type disco_items_req_props() :: #disco_items{}.

-record(msg, {
          id = <<>> :: binary(),
          children = [] :: [jlib:xmlch()]
         }).

-record(config, {
          id = <<>> :: binary(),
          prev_version = <<>> :: binary(),
          version = <<>> :: binary(),
          raw_config = [] :: raw_config()
         }).

-type config_req_props() :: #config{}.

-record(affiliations, {
          id = <<>> :: binary(),
          prev_version = <<>> :: binary(),
          version = <<>> :: binary(),
          aff_users = [] :: aff_users()
         }).

-type affiliations_req_props() :: #affiliations{}.

-record(info, {
          id = <<>> :: binary(),
          prev_version = <<>> :: binary(),
          version = <<>> :: binary(),
          raw_config = [] :: raw_config(),
          aff_users = [] :: aff_users()
         }).

-record(blocking, {
          id = <<>> :: binary(),
          items = [] :: [blocking_item()]
         }).

-type blocking_req_props() :: #blocking{}.

-record(create, {
          id = <<>> :: binary(),
          version = <<>> :: binary(),
          raw_config = [] :: raw_config(),
          aff_users = [] :: aff_users()
         }).

-type create_req_props() :: #create{}.

-record(destroy, {
          id = <<>> :: binary()
         }).

-type muc_light_disco() :: {get, #disco_info{} | #disco_items{}}.

-type muc_light_packet() :: #msg{}
                          | {set | get, #config{} | #affiliations{} | #blocking{}}
                          | {get, #info{}}
                          | {set, #create{} | #destroy{}}.

-type muc_light_encode_request() :: {get, #config{} | #affiliations{} | #info{} | #blocking{}}
                                  | {set, #affiliations{}, OldAffUsers :: aff_users(),
                                     NewAffUsers :: aff_users()}
                                  | {set, #blocking{}}
                                  | {set, #create{}, UniqueRequested :: boolean()}
                                  | {set, #destroy{}, AffUsers :: aff_users()}
                                  | {set, #config{}, AffUsers :: aff_users()}
                                  | {error, bad_request}.

-type msg() :: #msg{}.

