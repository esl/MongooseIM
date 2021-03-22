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

insert_domain(Domain, HostType) ->
    SDomain = jid:nameprep(Domain),
    case mongoose_domain_api:insert_domain(SDomain, HostType) of
        ok -> {ok, "Added"};
        {error, duplicate} ->
            {error, "domain already exists"};
        {error, {db_error, _}} ->
            {error, "database error"};
        {error, service_disabled} ->
            {error, "service disabled"};
        {error, unknown_host_type} ->
            {error, "unknown host type"}
    end.

delete_domain(Domain, HostType) ->
    SDomain = jid:nameprep(Domain),
    case mongoose_domain_api:delete_domain(SDomain, HostType) of
        ok -> {ok, "Deleted"};
        {error, {db_error, _}} ->
            {error, "database error"};
        {error, static} ->
            {error, "the domain is static"};
        {error, service_disabled} ->
            {error, "service disabled"};
        {error, wrong_host_type} ->
            {error, "wrong host type"};
        {error, unknown_host_type} ->
            {error, "unknown host type"}
    end.

enable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    Res = mongoose_domain_api:enable_domain(SDomain),
    handle_enabled_result(Res, "enabled").

disable_domain(Domain) ->
    SDomain = jid:nameprep(Domain),
    Res = mongoose_domain_api:disable_domain(SDomain),
    handle_enabled_result(Res, "disabled").

handle_enabled_result(Res, OkText) ->
    case Res of
        ok ->
            {ok, OkText};
        {error, not_found} ->
            {error, "domain not found"};
        {error, static} ->
            {error, "domain is static"};
        {error, service_disabled} ->
            {false, "service disabled"}
    end.
