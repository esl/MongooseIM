%% Domain actions for mongooseimctl
-module(service_admin_extra_domain).
-export([commands/0,
         insert_domain/2,
         delete_domain/2,
         enable_domain/1,
         disable_domain/1]).

-ignore_xref([
    commands/0, insert_domain/2, delete_domain/2, enable_domain/1, disable_domain/1
]).

-include("ejabberd_commands.hrl").

-type cmd_result() :: {ok, string()} | {error, string()}.

-spec commands() -> [ejabberd_commands:cmd()].
commands() ->
    [
        #ejabberd_commands{name = insert_domain, tags = [domain],
                           desc = "Insert a domain",
                           module = ?MODULE, function = insert_domain,
                           args = [{domain, binary}, {host_type, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = delete_domain, tags = [domain],
                           desc = "Delete a domain",
                           module = ?MODULE, function = delete_domain,
                           args = [{domain, binary}, {host_type, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = enable_domain, tags = [domain],
                           desc = "Enable a domain",
                           module = ?MODULE, function = enable_domain,
                           args = [{domain, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = disable_domain, tags = [domain],
                           desc = "Disable a domain",
                           module = ?MODULE, function = disable_domain,
                           args = [{domain, binary}],
                           result = {res, restuple}}
    ].

-spec insert_domain(binary(), binary()) -> cmd_result().
insert_domain(Domain, HostType) ->
    SDomain = jid:nameprep(Domain),
    call_api(fun() -> mongoose_domain_api:insert_domain(SDomain, HostType) end, "Added").

-spec delete_domain(binary(), binary()) -> cmd_result().
delete_domain(Domain, HostType) ->
    SDomain = jid:nameprep(Domain),
    call_api(fun() -> mongoose_domain_api:delete_domain(SDomain, HostType) end, "Deleted").

-spec enable_domain(binary()) -> cmd_result().
enable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    call_api(fun() -> mongoose_domain_api:enable_domain(SDomain) end, "Enabled").

-spec disable_domain(binary()) -> cmd_result().
disable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    call_api(fun() -> mongoose_domain_api:disable_domain(SDomain) end, "Disabled").

call_api(F, OkMsg) ->
    case service_domain_db:enabled() of
        true ->
            case F() of
                {ok, _} -> {ok, OkMsg};
                {_Reason, ErrMsg} -> {error, ErrMsg}
            end;
        false ->
            {error, <<"Dynamic domains service is disabled">>}
    end.
