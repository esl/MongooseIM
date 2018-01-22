-module(mam_iq).

-export([action/1]).
-export([action_type/1]).

-export([fix_rsm/1]).
-export([elem_to_start_microseconds/1]).
-export([elem_to_end_microseconds/1]).
-export([elem_to_with_jid/1]).
-export([elem_to_limit/1]).
-export([query_to_lookup_params/4]).

-export([form_to_start_microseconds/1]).
-export([form_to_end_microseconds/1]).
-export([form_to_with_jid/1]).
-export([form_to_lookup_params/4]).

-export([lookup_params_with_archive_details/3]).

-import(mod_mam_utils,
        [maybe_microseconds/1,
         get_one_of_path/2,
         form_field_value_s/2]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-type action() :: 'mam_get_prefs'
                 | 'mam_lookup_messages'
                 | 'mam_purge_multiple_messages'
                 | 'mam_purge_single_message'
                 | 'mam_set_prefs'
                 | 'mam_set_message_form'
                 | 'mam_get_message_form'.

-export_type([action/0]).

-callback extra_lookup_params(jlib:iq(), map()) -> map().

-spec action(jlib:iq()) -> action().
action(IQ = #iq{xmlns = ?NS_MAM}) ->
    action_v02(IQ);
action(IQ = #iq{xmlns = ?NS_MAM_03}) ->
    action_v03(IQ);
action(IQ = #iq{xmlns = ?NS_MAM_04}) ->
    action_v03(IQ);
action(IQ = #iq{xmlns = ?NS_MAM_06}) ->
    action_v03(IQ).

action_v02(#iq{type = Action, sub_el = SubEl = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_lookup_messages;
        {set, <<"purge">>} ->
            case exml_query:attr(SubEl, <<"id">>, <<>>) of
                <<>> -> mam_purge_multiple_messages;
                _    -> mam_purge_single_message
            end
    end.

action_v03(#iq{type = Action, sub_el = #xmlel{name = Category}}) ->
    case {Action, Category} of
        {set, <<"prefs">>} -> mam_set_prefs;
        {get, <<"prefs">>} -> mam_get_prefs;
        {get, <<"query">>} -> mam_get_message_form;
        {set, <<"query">>} ->
            mam_set_message_form
            %% Purge is NOT official extention, it is not implemented for XEP-0313 v0.3.
            %% Use v0.2 namespace if you really want it.
    end.

-spec action_type(action()) -> 'get' | 'set'.
action_type(mam_get_prefs) -> get;
action_type(mam_set_prefs) -> set;
action_type(mam_lookup_messages) -> get;
action_type(mam_set_message_form) -> get;
action_type(mam_get_message_form) -> get;
action_type(mam_purge_single_message) -> set;
action_type(mam_purge_multiple_messages) -> set.

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


-spec elem_to_start_microseconds(exml:element()) -> 'undefined' | non_neg_integer().
elem_to_start_microseconds(El) ->
    maybe_microseconds(exml_query:path(El, [{element, <<"start">>}, cdata], <<>>)).


-spec elem_to_end_microseconds(exml:element()) -> 'undefined' | non_neg_integer().
elem_to_end_microseconds(El) ->
    maybe_microseconds(exml_query:path(El, [{element, <<"end">>}, cdata], <<>>)).


-spec elem_to_with_jid(exml:element()) -> 'error' | 'undefined' | jid:jid().
elem_to_with_jid(El) ->
    maybe_jid(exml_query:path(El, [{element, <<"with">>}, cdata], <<>>)).

%% @doc This element's name is "limit". But it must be "max" according XEP-0313.
-spec elem_to_limit(any()) -> any().
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
                              [{element, <<"set">>}, {element, <<"max">>}, cdata],
                              [{element, <<"set">>}, {element, <<"limit">>}, cdata]
                             ]).


-spec form_to_start_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_start_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"start">>)).


-spec form_to_end_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_end_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"end">>)).


-spec form_to_with_jid(exml:element()) -> 'error' | 'undefined' | jid:jid().
form_to_with_jid(El) ->
    maybe_jid(form_field_value_s(El, <<"with">>)).


-spec maybe_jid(binary()) -> 'error' | 'undefined' | jid:jid().
maybe_jid(<<>>) ->
    undefined;
maybe_jid(JID) when is_binary(JID) ->
    jid:from_binary(JID).

-spec query_to_lookup_params(jlib:iq(), integer(), integer(), undefined | module()) -> map().
query_to_lookup_params(#iq{sub_el = QueryEl} = IQ, MaxResultLimit, DefaultResultLimit, Module) ->
    Params0 = common_lookup_params(QueryEl, MaxResultLimit, DefaultResultLimit),
    Params = Params0#{
               %% Filtering by date.
               %% Start :: integer() | undefined
               start_ts => mam_iq:elem_to_start_microseconds(QueryEl),
               end_ts => mam_iq:elem_to_end_microseconds(QueryEl),
               %% Filtering by contact.
               search_text => undefined,
               with_jid => elem_to_with_jid(QueryEl),
               borders => mod_mam_utils:borders_decode(QueryEl),
               is_simple => mod_mam_utils:decode_optimizations(QueryEl)},

    maybe_add_extra_lookup_params(Module, Params, IQ).


-spec form_to_lookup_params(jlib:iq(), integer(), integer(), undefined | module()) -> map().
form_to_lookup_params(#iq{sub_el = QueryEl} = IQ, MaxResultLimit, DefaultResultLimit, Module) ->
    Params0 = common_lookup_params(QueryEl, MaxResultLimit, DefaultResultLimit),
    Params = Params0#{
               %% Filtering by date.
               %% Start :: integer() | undefined
               start_ts => mam_iq:form_to_start_microseconds(QueryEl),
               end_ts => mam_iq:form_to_end_microseconds(QueryEl),
               %% Filtering by contact.
               with_jid => mam_iq:form_to_with_jid(QueryEl),
               %% Filtering by text
               search_text => mod_mam_utils:form_to_text(QueryEl),

               borders => mod_mam_utils:form_borders_decode(QueryEl),
               %% Whether or not the client query included a <set/> element,
               %% the server MAY simply return its limited results.
               %% So, disable 'policy-violation'.
               limit_passed => true,
               %% `is_simple' can contain three values:
               %% - true - do not count records (useful during pagination, when we already
               %%          know how many messages we have from a previous query);
               %% - false - count messages (slow, according XEP-0313);
               %% - opt_count - count messages (same as false, fast for small result sets)
               %%
               %% The difference between false and opt_count is that with IsSimple=false we count
               %% messages first and then extract a messages on a page (if count is not zero).
               %% If IsSimple=opt_count we extract a page and then calculate messages (if required).
               %% `opt_count' can be passed inside an IQ.
               %% Same for mod_mam_muc.
               is_simple => mod_mam_utils:form_decode_optimizations(QueryEl)},
    maybe_add_extra_lookup_params(Module, Params, IQ).

common_lookup_params(QueryEl, MaxResultLimit, DefaultResultLimit) ->
    Limit = mam_iq:elem_to_limit(QueryEl),
    #{now => p1_time_compat:system_time(micro_seconds),
      rsm => mam_iq:fix_rsm(jlib:rsm_decode(QueryEl)),
      max_result_limit => MaxResultLimit,
      page_size => min(MaxResultLimit,
                       mod_mam_utils:maybe_integer(Limit, DefaultResultLimit)),
      limit_passed => Limit =/= <<>>}.

lookup_params_with_archive_details(Params, ArcID, ArcJID) ->
    Params#{archive_id => ArcID,
            owner_jid => ArcJID}.

maybe_add_extra_lookup_params(undefined, Params, _) ->
    Params;
maybe_add_extra_lookup_params(Module, Params, IQ) ->
    Module:extra_lookup_params(IQ, Params).

