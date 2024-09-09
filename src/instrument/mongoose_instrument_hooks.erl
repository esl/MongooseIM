-module(mongoose_instrument_hooks).

-export([set_up/2, tear_down/2, execute/2]).

-include("mongoose.hrl").

-spec set_up(gen_hook:hook_name(), gen_hook:hook_tag()) -> ok.
set_up(HookName, Tag) ->
    case is_instrumented(HookName) of
        true ->
            EventName = event_name(HookName),
            persistent_term:put({?MODULE, HookName}, EventName),
            try
                mongoose_instrument:set_up(EventName, labels(Tag), #{metrics => #{count => spiral}})
            catch
                error:#{what := inconsistent_labels} ->
                    log_tag_error(HookName, Tag)
            end;
        false ->
            ok
    end.

-spec log_tag_error(gen_hook:hook_name(), gen_hook:hook_tag()) -> ok.
log_tag_error(HookName, Tag) ->
    ?LOG_ERROR(#{what => inconsistent_hook_handler_tags,
                 text => <<"A single hook should not have both global and host-type handlers. "
                           "Instrumentation is not set up.">>,
                 hook_name => HookName, tag => Tag}).

-spec tear_down(gen_hook:hook_name(), gen_hook:hook_tag()) -> ok.
tear_down(HookName, Tag) ->
    case is_instrumented(HookName) of
        true ->
            EventName = event_name(HookName),
            mongoose_instrument:tear_down(EventName, labels(Tag)),
            persistent_term:erase({?MODULE, HookName});
        false ->
            ok
    end.

-spec execute(gen_hook:hook_name(), gen_hook:hook_tag()) -> ok.
execute(HookName, Tag) ->
    case is_instrumented(HookName) of
        true ->
            try persistent_term:get({?MODULE, HookName}) of
                EventName ->
                    mongoose_instrument:execute(EventName, labels(Tag), #{count => 1})
            catch error:badarg ->
                    ok % no handlers registered for this hook
            end;
        false ->
            ok
    end.

-spec event_name(gen_hook:hook_name()) -> mongoose_instrument:event_name().
event_name(HookName) ->
    list_to_atom("hook_" ++ atom_to_list(HookName)).

-spec labels(gen_hook:hook_tag()) -> mongoose_instrument:labels().
labels(global) -> #{};
labels(HostType) -> #{host_type => HostType}.

-spec is_instrumented(gen_hook:hook_name()) -> boolean().
is_instrumented(sm_register_connection) -> false;
is_instrumented(sm_remove_connection) -> false;
is_instrumented(auth_failed) -> false;
is_instrumented(user_send_packet) -> false;
is_instrumented(user_send_message) -> false;
is_instrumented(user_send_presence) -> false;
is_instrumented(user_send_iq) -> false;
is_instrumented(user_receive_packet) -> false;
is_instrumented(user_receive_message) -> false;
is_instrumented(user_receive_presence) -> false;
is_instrumented(user_receive_iq) -> false;
is_instrumented(xmpp_send_element) -> false;
is_instrumented(roster_get) -> false;
is_instrumented(roster_set) -> false;
is_instrumented(roster_push) -> false;
is_instrumented(register_user) -> false;
is_instrumented(remove_user) -> false;
is_instrumented(anonymous_purge) -> false;
is_instrumented(privacy_iq_get) -> false;
is_instrumented(privacy_iq_set) -> false;
is_instrumented(privacy_check_packet) -> false;
is_instrumented(mam_get_prefs) -> false;
is_instrumented(mam_set_prefs) -> false;
is_instrumented(mam_remove_archive) -> false;
is_instrumented(mam_archive_message) -> false;
is_instrumented(mam_muc_get_prefs) -> false;
is_instrumented(mam_muc_set_prefs) -> false;
is_instrumented(mam_muc_remove_archive) -> false;
is_instrumented(mam_muc_lookup_messages) -> false;
is_instrumented(mam_muc_archive_message) -> false;
is_instrumented(_) -> true.
