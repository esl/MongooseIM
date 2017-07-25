-module(mod_mam_iq).

-export([fix_rsm/1]).
-export([elem_to_start_microseconds/1]).
-export([elem_to_end_microseconds/1]).
-export([elem_to_with_jid/1]).
-export([elem_to_limit/1]).
-export([query_to_lookup_params/1]).

-export([form_to_start_microseconds/1]).
-export([form_to_end_microseconds/1]).
-export([form_to_with_jid/1]).
-export([form_to_lookup_params/1]).

-export([lookup_params_with_archive_details/3]).

-import(mod_mam_utils,
        [maybe_microseconds/1,
         get_one_of_path/2,
         form_field_value_s/2]).

-include("jlib.hrl").

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


-spec elem_to_start_microseconds(jlib:xmlel()) -> 'undefined' | non_neg_integer().
elem_to_start_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"start">>}, cdata])).


-spec elem_to_end_microseconds(jlib:xmlel()) -> 'undefined' | non_neg_integer().
elem_to_end_microseconds(El) ->
    maybe_microseconds(xml:get_path_s(El, [{elem, <<"end">>}, cdata])).


-spec elem_to_with_jid(jlib:xmlel()) -> 'error' | 'undefined' | ejabberd:jid().
elem_to_with_jid(El) ->
    maybe_jid(xml:get_path_s(El, [{elem, <<"with">>}, cdata])).

%% @doc This element's name is "limit". But it must be "max" according XEP-0313.
-spec elem_to_limit(any()) -> any().
elem_to_limit(QueryEl) ->
    get_one_of_path(QueryEl, [
                              [{elem, <<"set">>}, {elem, <<"max">>}, cdata],
                              [{elem, <<"set">>}, {elem, <<"limit">>}, cdata]
                             ]).


-spec form_to_start_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_start_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"start">>)).


-spec form_to_end_microseconds(_) -> 'undefined' | non_neg_integer().
form_to_end_microseconds(El) ->
    maybe_microseconds(form_field_value_s(El, <<"end">>)).


-spec form_to_with_jid(jlib:xmlel()) -> 'error' | 'undefined' | ejabberd:jid().
form_to_with_jid(El) ->
    maybe_jid(form_field_value_s(El, <<"with">>)).


-spec maybe_jid(binary()) -> 'error' | 'undefined' | ejabberd:jid().
maybe_jid(<<>>) ->
    undefined;
maybe_jid(JID) when is_binary(JID) ->
    jid:from_binary(JID).

query_to_lookup_params(QueryEl) ->
    Now = p1_time_compat:system_time(micro_seconds),
    %% Filtering by date.
    %% Start :: integer() | undefined
    Start = mod_mam_iq:elem_to_start_microseconds(QueryEl),
    End   = mod_mam_iq:elem_to_end_microseconds(QueryEl),
    %% Filtering by contact.
    SearchText = undefined,
    With  = mod_mam_iq:elem_to_with_jid(QueryEl),
    RSM   = mod_mam_iq:fix_rsm(jlib:rsm_decode(QueryEl)),
    Borders = mod_mam_utils:borders_decode(QueryEl),
    Limit = mod_mam_iq:elem_to_limit(QueryEl),
    PageSize = min(mod_mam_params:max_result_limit(),
                   mod_mam_utils:maybe_integer(Limit, mod_mam_params:default_result_limit())),
    LimitPassed = Limit =/= <<>>,
    IsSimple = mod_mam_utils:decode_optimizations(QueryEl),

    #{with_jid => With,
      rsm => RSM,
      borders => Borders,
      start_ts => Start,
      end_ts => End,
      now => Now,
      search_text => SearchText,
      page_size => PageSize,
      limit_passed => LimitPassed,
      max_result_limit => mod_mam_params:max_result_limit(),
      is_simple => IsSimple}.

form_to_lookup_params(QueryEl) ->
    Limit = mod_mam_iq:elem_to_limit(QueryEl),
    #{now => p1_time_compat:system_time(micro_seconds),
      %% Filtering by date.
      %% Start :: integer() | undefined
      start_ts => mod_mam_iq:form_to_start_microseconds(QueryEl),
      end_ts => mod_mam_iq:form_to_end_microseconds(QueryEl),
      %% Filtering by contact.
      with_jid => mod_mam_iq:form_to_with_jid(QueryEl),
      %% Filtering by text
      search_text => mod_mam_utils:form_to_text(QueryEl),

      rsm => mod_mam_iq:fix_rsm(jlib:rsm_decode(QueryEl)),
      borders => mod_mam_utils:form_borders_decode(QueryEl),
      page_size => min(mod_mam_params:max_result_limit(),
                       mod_mam_utils:maybe_integer(Limit, mod_mam_params:default_result_limit())),
      %% Whether or not the client query included a <set/> element,
      %% the server MAY simply return its limited results.
      %% So, disable 'policy-violation'.
      limit_passed => true,
      max_result_limit => mod_mam_params:max_result_limit(),
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
      is_simple => mod_mam_utils:form_decode_optimizations(QueryEl)}.

lookup_params_with_archive_details(Params, ArcID, ArcJID) ->
    Params#{archive_id => ArcID,
            owner_jid => ArcJID}.




