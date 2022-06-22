-module(mod_offline_api).

-export([delete_expired_messages/1, delete_old_messages/2]).

-spec delete_expired_messages(jid:lserver()) ->
    {ok | domain_not_found | server_error | module_not_loaded_error, iolist()}.
delete_expired_messages(Domain) ->
    call_for_loaded_module(Domain, fun remove_expired_messages/2, {Domain}).

-spec delete_old_messages(jid:lserver(), Days :: integer()) ->
    {ok | domain_not_found | server_error | module_not_loaded_error, iolist()}.
delete_old_messages(Domain, Days) ->
    call_for_loaded_module(Domain, fun remove_old_messages/2, {Domain, Days}).

call_for_loaded_module(Domain, Function, Args) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            case gen_mod:is_loaded(HostType, mod_offline) of
                true ->
                    Function(Args, HostType);
                false ->
                    {module_not_loaded_error, "mod_offline is not loaded for this host"}
            end;
        {error, not_found} ->
            {domain_not_found, "Unknown domain"}
    end.

remove_old_messages({Domain, Days}, HostType) ->
    case mod_offline:remove_old_messages(HostType, Domain, Days) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {server_error, io_lib:format("Can't remove old messages: ~n~p", [Reason])}
    end.

remove_expired_messages({Domain}, HostType) ->
    case mod_offline:remove_expired_messages(HostType, Domain) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {server_error, io_lib:format("Can't remove old messages: ~n~p", [Reason])}
    end.
