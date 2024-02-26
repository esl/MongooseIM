-module(mam_iq).

-export([action/1]).
-export([action_type/1]).
-export([form_to_lookup_params/5]).
-export([lookup_params_with_archive_details/4]).

-import(mod_mam_utils,
        [maybe_microseconds/1,
         get_one_of_path/2]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-type action() :: 'mam_get_prefs'
                 | 'mam_lookup_messages'
                 | 'mam_set_prefs'
                 | 'mam_set_message_form'
                 | 'mam_get_message_form'
                 | 'mam_get_metadata'.

-type lookup_params() :: #{
        archive_id => mod_mam:archive_id(),
        owner_jid => jid:jid(),
        caller_jid => jid:jid(),
        rsm => jlib:rsm_in() | undefined,
        max_result_limit => non_neg_integer(),
        %% Contains page size value provided by client or enforced by server.
        %% It's a final value used by backend DB modules.
        page_size => non_neg_integer(),
        ordering_direction => backward | forward,
        %% unix_timestamp() is in microseconds
        now => mod_mam:unix_timestamp(),
        %% Filtering by date
        start_ts => mod_mam:unix_timestamp() | undefined,
        end_ts => mod_mam:unix_timestamp() | undefined,
        %% Filtering by contact
        with_jid => jid:jid() | undefined,
        %% Filtering by body text
        search_text => binary() | undefined,
        %% Filtering Result Set before/after specific message ids
        borders =>  mod_mam:borders() | undefined,
        %% Filtering Result Set based on specific message ids
        message_ids => [mod_mam:message_id()] | undefined,
        %% Affects 'policy-violation' for a case when:
        %% - user does not use forms to query archive
        %% - user does not provide "set" element
        %% see form_to_lookup_params for more info
        limit_passed => boolean(),
        %% Optimizations flags
        %% see form_to_lookup_params for more info
        is_simple => true | false,
        %% Contains information whether the client requested to get the results in reversed order
        flip_page => true | false,
        %% If the groupchat messages are stored in the user's archive,
        %% this parameter is used to decide whether to include them or not
        include_groupchat => true | false,
        %% Can have more fields, added in maybe_add_extra_lookup_params function
        %% in runtime
        atom() => _
      }.

-export_type([action/0, lookup_params/0]).

-callback extra_lookup_params(jlib:iq(), lookup_params()) -> lookup_params().

-spec action(jlib:iq()) -> action().
action(IQ = #iq{xmlns = ?NS_MAM_04}) ->
    action_v04plus(IQ);
action(IQ = #iq{xmlns = ?NS_MAM_06}) ->
    action_v04plus(IQ).

action_v04plus(#iq{type = Action, sub_el = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_get_message_form;
        {set, <<"query">>} -> mam_set_message_form;
        {get, <<"metadata">>} -> mam_get_metadata
    end.

-spec action_type(action()) -> 'get' | 'set'.
action_type(mam_get_prefs) -> get;
action_type(mam_set_prefs) -> set;
action_type(mam_lookup_messages) -> get;
action_type(mam_set_message_form) -> get;
action_type(mam_get_message_form) -> get;
action_type(mam_get_metadata) -> get.

%% @doc Convert id into internal format.
-spec fix_rsm('none' | jlib:rsm_in()) -> 'undefined' | jlib:rsm_in().
fix_rsm(none) ->
    undefined;
fix_rsm(RSM=#rsm_in{direction = aft, id = <<>>}) ->
    RSM#rsm_in{direction = undefined, id = undefined}; %% First page
fix_rsm(RSM=#rsm_in{direction = aft, id = undefined}) ->
    RSM#rsm_in{direction = undefined}; %% First page
fix_rsm(RSM=#rsm_in{id = undefined}) ->
    RSM;
fix_rsm(RSM=#rsm_in{id = <<>>}) ->
    RSM#rsm_in{id = undefined};
fix_rsm(RSM=#rsm_in{id = BExtMessID}) when is_binary(BExtMessID) ->
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    RSM#rsm_in{id = MessID}.

%% @doc This element's name is "limit". But it must be "max" according XEP-0313.
-spec elem_to_limit(any()) -> any().
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
                              [{element, <<"set">>}, {element, <<"max">>}, cdata],
                              [{element, <<"set">>}, {element, <<"limit">>}, cdata]
                             ]).


-spec form_to_start_microseconds(mongoose_data_forms:kv_map()) -> 'undefined' | non_neg_integer().
form_to_start_microseconds(#{<<"start">> := [V]}) ->
    maybe_microseconds(V);
form_to_start_microseconds(#{}) ->
    undefined.


-spec form_to_end_microseconds(mongoose_data_forms:kv_map()) -> 'undefined' | non_neg_integer().
form_to_end_microseconds(#{<<"end">> := [V]}) ->
    maybe_microseconds(V);
form_to_end_microseconds(#{}) ->
    undefined.

-spec form_to_with_jid(mongoose_data_forms:kv_map()) -> 'error' | 'undefined' | jid:jid().
form_to_with_jid(#{<<"with">> := [JID]}) ->
    jid:from_binary(JID);
form_to_with_jid(#{}) ->
    undefined.

-spec form_to_lookup_params(jlib:iq(), integer(), integer(), undefined | module(), boolean()) ->
    lookup_params().
form_to_lookup_params(#iq{sub_el = QueryEl} = IQ, MaxResultLimit, DefaultResultLimit, Module, EnforceSimple) ->
    Params0 = common_lookup_params(QueryEl, MaxResultLimit, DefaultResultLimit),
    KVs = query_to_map(QueryEl),
    Params = Params0#{
               %% Filtering by date.
               %% Start :: integer() | undefined
               start_ts => form_to_start_microseconds(KVs),
               end_ts => form_to_end_microseconds(KVs),
               %% Filtering by contact.
               with_jid => form_to_with_jid(KVs),
               %% Filtering by text
               search_text => mod_mam_utils:form_to_text(KVs),

               borders => mod_mam_utils:form_borders_decode(KVs),
               message_ids => form_to_msg_ids(KVs),
               %% Whether or not the client query included a <set/> element,
               %% the server MAY simply return its limited results.
               %% So, disable 'policy-violation'.
               limit_passed => true,
               %% `is_simple' can contain:
               %% - true - do not count records (useful during pagination, when we already
               %%          know how many messages we have from a previous query);
               %% - false - count messages (slow, according XEP-0313);
               is_simple => maybe_enforce_simple(KVs, EnforceSimple),
               include_groupchat => include_groupchat(KVs)},
    maybe_add_extra_lookup_params(Module, Params, IQ).

-spec query_to_map(exml:element()) -> mongoose_data_forms:kv_map().
query_to_map(QueryEl) ->
    case mongoose_data_forms:find_form(QueryEl) of
        undefined ->
            #{};
        Form ->
            #{kvs := KVs} = mongoose_data_forms:parse_form_fields(Form),
            KVs
    end.

-spec common_lookup_params(exml:element(), non_neg_integer(), non_neg_integer()) ->
    lookup_params().
common_lookup_params(QueryEl, MaxResultLimit, DefaultResultLimit) ->
    RSM = fix_rsm(jlib:rsm_decode(QueryEl)),
    Limit = elem_to_limit(QueryEl),
    #{now => erlang:system_time(microsecond),
      rsm => RSM,
      max_result_limit => MaxResultLimit,
      page_size => min(MaxResultLimit,
                       mod_mam_utils:maybe_integer(Limit, DefaultResultLimit)),
      limit_passed => Limit =/= <<>>,
      ordering_direction => ordering_direction(RSM),
      flip_page => mod_mam_utils:should_page_be_flipped(QueryEl)}.

-spec lookup_params_with_archive_details(lookup_params(), term(), jid:jid(), jid:jid()) ->
    lookup_params().
lookup_params_with_archive_details(Params, ArcID, ArcJID, CallerJID) ->
    Params#{archive_id => ArcID,
            owner_jid => ArcJID,
            caller_jid => CallerJID}.

ordering_direction(#rsm_in{direction = before}) -> backward;
ordering_direction(_) -> forward.

maybe_add_extra_lookup_params(undefined, Params, _) ->
    Params;
maybe_add_extra_lookup_params(Module, Params, IQ) ->
    Module:extra_lookup_params(IQ, Params).

maybe_enforce_simple(_, true) ->
    true;
maybe_enforce_simple(KVs, _) ->
    mod_mam_utils:form_decode_optimizations(KVs).

include_groupchat(#{<<"include-groupchat">> := [<<"false">>]}) ->
    false;
include_groupchat(_) ->
    undefined.

form_to_msg_ids(#{<<"ids">> := IDs}) ->
    [mod_mam_utils:external_binary_to_mess_id(ID) || ID <- IDs];
form_to_msg_ids(_) ->
    undefined.
