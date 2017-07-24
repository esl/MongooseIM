-module(mod_mam_iq).

-export([fix_rsm/1]).
-export([elem_to_start_microseconds/1]).
-export([elem_to_end_microseconds/1]).
-export([elem_to_with_jid/1]).
-export([elem_to_limit/1]).

-export([form_to_start_microseconds/1]).
-export([form_to_end_microseconds/1]).
-export([form_to_with_jid/1]).

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


