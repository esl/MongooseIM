-module(stats_api).

-export([incoming_s2s_number/0, outgoing_s2s_number/0, stats/1, stats/2]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").

-spec incoming_s2s_number() -> non_neg_integer().
incoming_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_in_sup)).

-spec outgoing_s2s_number() -> non_neg_integer().
outgoing_s2s_number() ->
    length(supervisor:which_children(ejabberd_s2s_out_sup)).

-spec stats(binary()) -> integer() | {error, string()}.
stats(Name) ->
    case Name of
        <<"uptimeseconds">> ->
            trunc(element(1, erlang:statistics(wall_clock))/1000);
        <<"registeredusers">> ->
            Domains = lists:flatmap(fun mongoose_domain_api:get_domains_by_host_type/1,
                                    ?ALL_HOST_TYPES),
            lists:sum([ejabberd_auth:get_vh_registered_users_number(Domain) || Domain <- Domains]);
        <<"onlineusersnode">> ->
            ejabberd_sm:get_node_sessions_number();
        <<"onlineusers">> ->
            ejabberd_sm:get_total_sessions_number();
        _ ->
            {error, "Wrong command name."}
    end.

-spec stats(binary(), jid:server()) -> integer() | {error, string()}.
stats(Name, Host) ->
    case Name of
        <<"registeredusers">> ->
            ejabberd_auth:get_vh_registered_users_number(Host);
        <<"onlineusers">> ->
            ejabberd_sm:get_vh_session_number(Host);
        _ ->
            {error, "Wrong command name."}
    end.
