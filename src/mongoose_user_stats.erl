-module(mongoose_user_stats).

-author('aleksander.lisiecki@erlang-solutions.com').

-export([report/0]).

% TODO replace tid to official ESL one
-define(BASE_URL, "https://www.google-analytics.com/collect?v=1&tid=UA-151110014-1&t=event").

report() ->
    ReportUrl = ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL),
    report_user_stats(ReportUrl).

% Functions are spawned and not linked, as MongooseIM should not care if they fail or not.
% Moreover the MongooseIM's start should not be blocked.
report_user_stats(disable) -> ok;
report_user_stats(ReportUrl) ->
    % Data used for more then one report
    Hosts = ejabberd_config:get_global_option(hosts),
    Reports = [
        fun() -> report_number_of_hosts(Hosts, ReportUrl) end,
        fun() -> report_used_modules(Hosts, ReportUrl) end
    ],
    [spawn(Fun) || Fun <- Reports].

report_number_of_hosts(Hosts, ReportUrl) ->
    Len = length(Hosts),
    report(ReportUrl, hosts_count, Len).

report_used_modules(Hosts, ReportUrl) ->
    ModulesWithOpts = lists:flatten(
        lists:map(fun gen_mod:loaded_modules_with_opts/1, Hosts)),
    lists:foreach(
        fun({Module, Opts}) ->
            Backend = proplists:get_value(backend, Opts, none),
            report(ReportUrl, modules, Module, Backend)
        end, ModulesWithOpts).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

report(ReportUrl, EventCategory, EventAction) ->
    report(ReportUrl, EventCategory, EventAction, empty).

report(ReportUrl, EventCategory, EventAction, EventLabel) ->
    MaybeLabel = maybe_event_label(EventLabel),
    LstClientId = term_to_string(client_id()),
    LstEventCategory = term_to_string(EventCategory),
    LstEventAction = term_to_string(EventAction),
    ListUrl = [
        ReportUrl,
        "&cid=", LstClientId,
        "&ec=", LstEventCategory,
        "&ea=", LstEventAction
        ] ++ MaybeLabel,
    URL = string:join(ListUrl, ""),
    lager:debug("~p reported = ~p", [?MODULE, URL]),
    httpc:request(URL).


maybe_event_label(empty) -> [];
maybe_event_label(EventLabel) ->
    LstEventLabel = term_to_string(EventLabel),
    ["&el=", LstEventLabel].

term_to_string(Term) ->
    R= io_lib:format("~p",[Term]),
    lists:flatten(R).

client_id() ->
    % TODO in the later implementation store client's ID in eg mnesia table and report stats with the same ID for the same client
    rand:uniform(1000 * 1000 * 1000 * 1000 * 1000).
