-module(mod_offline_api).

-export([delete_expired_messages/1, delete_old_messages/2]).

-type(api_result()) :: {ok | domain_not_found | server_error, iolist()}.

-spec delete_expired_messages(jid:lserver()) -> api_result().
delete_expired_messages(Domain) ->
    call_for_host_type(Domain, fun remove_expired_messages/2, [Domain]).

-spec delete_old_messages(jid:lserver(), Days :: pos_integer()) -> api_result().
delete_old_messages(Domain, Days) ->
    call_for_host_type(Domain, fun remove_old_messages/3, [Domain, Days]).

call_for_host_type(Domain, Function, Args) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            apply(Function, [HostType | Args]);
        {error, not_found} ->
            {domain_not_found, "Unknown domain"}
    end.

remove_old_messages(HostType, Domain, Days) ->
    case mod_offline:remove_old_messages(HostType, Domain, Days) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {server_error, io_lib:format("Can't remove old messages: ~n~p", [Reason])}
    end.

remove_expired_messages(HostType, Domain) ->
    case mod_offline:remove_expired_messages(HostType, Domain) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {server_error, io_lib:format("Can't remove old messages: ~n~p", [Reason])}
    end.
