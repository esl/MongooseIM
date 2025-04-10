-module(stats_api).

-export([incoming_s2s_number/0, outgoing_s2s_number/0, stats/1, stats/2]).

-include("mongoose.hrl").

-spec incoming_s2s_number() -> {ok, non_neg_integer()}.
incoming_s2s_number() ->
    Children = supervisor:which_children(mongoose_listener_sup),
    Listeners = [Ref || {Ref, _, _, [mongoose_s2s_listener | _]} <- Children],
    Number = lists:sum([maps:get(active_connections, ranch:info(Ref)) || Ref <- Listeners]),
    {ok, Number}.

-spec outgoing_s2s_number() -> {ok, non_neg_integer()}.
outgoing_s2s_number() ->
    {ok, length(supervisor:which_children(mongoose_s2s_out_sup))}.

-spec stats(binary()) -> {ok, integer()} | {not_found, string()}.
stats(<<"uptimeseconds">>) ->
    {ok, trunc(element(1, erlang:statistics(wall_clock)) / 1000)};
stats(<<"registeredusers">>) ->
    Domains = lists:flatmap(fun mongoose_domain_api:get_domains_by_host_type/1,
                            ?ALL_HOST_TYPES),
    {ok, lists:sum([ejabberd_auth:get_vh_registered_users_number(Domain) || Domain <- Domains])};
stats(<<"onlineusersnode">>) ->
    {ok, ejabberd_sm:get_node_sessions_number()};
stats(<<"onlineusers">>) ->
    {ok, ejabberd_sm:get_total_sessions_number()};
stats(_Name) ->
    {not_found, "Stats not found"}.

-spec stats(binary(), jid:server()) -> {ok, integer()} | {not_found, string()}.
stats(<<"registeredusers">>, Host) ->
    {ok, ejabberd_auth:get_vh_registered_users_number(Host)};
stats(<<"onlineusers">>, Host) ->
    {ok, ejabberd_sm:get_vh_session_number(Host)};
stats(_Name, _Host) ->
    {not_found, "Stats not found"}.
