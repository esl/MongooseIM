%% Domain actions for mongooseimctl
-module(service_admin_extra_domain).
-export([commands/0,
         insert_domain/2,
         delete_domain/2,
         enable_domain/1,
         disable_domain/1]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").

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
    case mongoose_domain_api:insert_domain(SDomain, HostType) of
        ok -> {ok, "Added"};
        {error, duplicate} ->
            {error, "Domain already exists"};
        {error, static} ->
            {error, "Domain is static"};
        {error, {db_error, _}} ->
            {error, "Database error"};
        {error, service_disabled} ->
            {error, "Service disabled"};
        {error, unknown_host_type} ->
            {error, "Unknown host type"}
    end.

-spec delete_domain(binary(), binary()) -> cmd_result().
delete_domain(Domain, HostType) ->
    SDomain = jid:nameprep(Domain),
    case mongoose_domain_api:delete_domain(SDomain, HostType) of
        ok -> {ok, "Deleted"};
        {error, {db_error, _}} ->
            {error, "Database error"};
        {error, static} ->
            {error, "Domain is static"};
        {error, service_disabled} ->
            {error, "Service disabled"};
        {error, wrong_host_type} ->
            {error, "Wrong host type"};
        {error, unknown_host_type} ->
            {error, "Unknown host type"}
    end.

-spec enable_domain(binary()) -> cmd_result().
enable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    Res = mongoose_domain_api:enable_domain(SDomain),
    handle_enabled_result(Res, "enabled").

-spec disable_domain(binary()) -> cmd_result().
disable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    Res = mongoose_domain_api:disable_domain(SDomain),
    handle_enabled_result(Res, "disabled").

handle_enabled_result(Res, OkText) ->
    case Res of
        ok ->
            {ok, OkText};
        {error, not_found} ->
            {error, "Domain not found"};
        {error, static} ->
            {error, "Domain is static"};
        {error, service_disabled} ->
            {error, "Service disabled"};
        {error, {db_error, _}} ->
            {error, "Database error"}
    end.
