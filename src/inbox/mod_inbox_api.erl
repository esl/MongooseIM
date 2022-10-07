%% @doc Provide an interface for frontends (like graphql or ctl) to manage inbox.
-module(mod_inbox_api).

-export([flush_user_bin/2, flush_domain_bin/2, flush_global_bin/2]).

-include("mongoose.hrl").
-include_lib("jid/include/jid.hrl").

-define(DOMAIN_NOT_FOUND_RESULT, {domain_not_found, <<"Domain not found">>}).

-define(USER_NOT_FOUND_RESULT(User, Server),
        {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Server])}).

-spec flush_user_bin(jid:jid(), Days :: integer()) ->
    {ok, integer()} | {domain_not_found | user_does_not_exist, iodata()}.
flush_user_bin(#jid{luser = LU, lserver = LS} = JID, Days) ->
    case mongoose_domain_api:get_host_type(LS) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(JID) of
                true ->
                    FromTS = days_to_timestamp(Days),
                    Count = mod_inbox_backend:empty_user_bin(HostType, LS, LU, FromTS),
                    {ok, Count};
                false ->
                    ?USER_NOT_FOUND_RESULT(LU, LS)
            end;
        {error, not_found} ->
            ?DOMAIN_NOT_FOUND_RESULT
    end.

-spec flush_domain_bin(jid:server(), Days :: integer()) ->
    {ok, integer()} | {domain_not_found, iodata()}.
flush_domain_bin(Domain, Days) ->
    LDomain = jid:nodeprep(Domain),
    case mongoose_domain_api:get_host_type(LDomain) of
        {ok, HostType} ->
            FromTS = days_to_timestamp(Days),
            Count = mod_inbox_backend:empty_domain_bin(HostType, Domain, FromTS),
            {ok, Count};
        {error, not_found} ->
            ?DOMAIN_NOT_FOUND_RESULT
    end.

-spec flush_global_bin(mongooseim:host_type(), Days :: integer()) ->
    {ok, integer()} | {host_type_not_found, binary()}.
flush_global_bin(HostType, Days) ->
    case validate_host_type(HostType) of
        ok ->
            FromTS = days_to_timestamp(Days),
            Count = mod_inbox_backend:empty_global_bin(HostType, FromTS),
            {ok, Count};
        {host_type_not_found, _} = Error ->
            Error
    end.

%% Internal

validate_host_type(HostType) ->
    case lists:member(HostType, ?ALL_HOST_TYPES) of
        true ->
            ok;
        false ->
            {host_type_not_found, <<"Host type not found">>}
    end.

days_to_timestamp(Days) ->
    Now = erlang:system_time(microsecond),
    mod_inbox_utils:calculate_ts_from(Now, Days).
