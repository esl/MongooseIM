%% Domain actions for mongooseimctl
-module(service_admin_extra_domain).
-export([commands/0,
         insert_domain/2]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").

-spec commands() -> [ejabberd_commands:cmd()].
commands() ->
    [
        #ejabberd_commands{name = insert_domain, tags = [roster],
                           desc = "Insert a domain",
                           module = ?MODULE, function = insert_domain,
                           args = [{domain, binary}, {host_type, binary}],
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
            {error, "unknown host type"};
        {error, _} ->
            {error, "unknown error"}
    end.
