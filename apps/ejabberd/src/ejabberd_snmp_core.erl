-module(ejabberd_snmp_core).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").

-export([start/1,
         stop/0,
         is_started/0,
         increment_counter/1,
         decrement_counter/1,
         update_counter/2,
         set_counter/2,
         reset_counters/0,
         increment_window_counter/1,
         update_window_counter/2,
         window_change/0,
         counter_value/1,
         table_value/4]).

%%%' Helper defines (module local)

-define(STATS(Module), table_name(Module)).
-define(STATS_W(Module), table_name_w(Module)).
-define(COUNTERS_FOR_MODULE, [
    {core,         [sessionCount,
                    globalSessionCount,
                    globalUniqueSessionCount,
                    sessionSuccessfulLogins,
                    sessionAuthAnonymous,
                    sessionAuthFails,
                    sessionLogouts]},
    {c2s,          [xmppMessageSent,
                    xmppMessageReceived,
                    xmppMessageBounced,
                    xmppPresenceSent,
                    xmppPresenceReceived,
                    xmppIqSent,
                    xmppIqReceived,
                    xmppStanzaSent,
                    xmppStanzaReceived,
                    xmppStanzaDenied,
                    xmppStanzaDropped,
                    xmppStanzaCount,
                    xmppErrorTotal,
                    xmppErrorBadRequest,
                    xmppErrorIq,
                    xmppErrorMessage,
                    xmppErrorPresence,
                    xmppIqTimeouts]},
    {mod_roster,   [modRosterSets,
                    modRosterGets,
                    modPresenceSubscriptions,
                    modPresenceUnsubscriptions,
                    modRosterPush,
                    modRosterSize,
                    modRosterGroups]},
    {mod_register, [modRegisterCount,
                    modUnregisterCount,
                    modRegisterUserCount]},
    {mod_privacy,  [modPrivacySets,
                    modPrivacySetsActive,
                    modPrivacySetsDefault,
                    modPrivacyPush,
                    modPrivacyGets,
                    modPrivacyStanzaBlocked,
                    modPrivacyStanzaAll,
                    modPrivacyListLength]} ]).

-define(W_COUNTERS_FOR_MODULE, [
    {core,         [sessionSuccessfulLoginsW,
                    sessionAuthAnonymousW,
                    sessionAuthFailsW,
                    sessionLogoutsW]},
    {c2s,          [xmppMessageSentW,
                    xmppMessageReceivedW,
                    xmppPresenceSentW,
                    xmppPresenceReceivedW,
                    xmppIqSentW,
                    xmppIqReceivedW,
                    xmppStanzaSentW,
                    xmppStanzaReceivedW,
                    xmppErrorTotalW]},
    {mod_roster,   [modRosterSetsW,
                    modRosterGetsW,
                    modPresenceSubscriptionsW,
                    modPresenceUnsubscriptionsW,
                    modRosterPushW]},
    {mod_register, [modRegisterCountW,
                    modUnregisterCountW]},
    {mod_privacy,  [modPrivacySetsW,
                    modPrivacySetsActiveW,
                    modPrivacySetsDefaultW,
                    modPrivacyPushW,
                    modPrivacyGetsW]} ]).


-define(MODULE_FOR_COUNTERS, [
        {sessionCount,               core},
        {globalSessionCount,         core},
        {globalUniqueSessionCount,   core},
        {sessionSuccessfulLogins,    core},
        {sessionAuthAnonymous,       core},
        {sessionAuthFails,           core},
        {sessionLogouts,             core},
        {sessionSuccessfulLoginsW,   core},
        {sessionAuthAnonymousW,      core},
        {sessionAuthFailsW,          core},
        {sessionLogoutsW,            core},
        {xmppMessageSent,            c2s},
        {xmppMessageReceived,        c2s},
        {xmppMessageBounced,         c2s},
        {xmppPresenceSent,           c2s},
        {xmppPresenceReceived,       c2s},
        {xmppIqSent,                 c2s},
        {xmppIqReceived,             c2s},
        {xmppStanzaSent,             c2s},
        {xmppStanzaReceived,         c2s},
        {xmppStanzaDenied,           c2s},
        {xmppStanzaDropped,          c2s},
        {xmppStanzaCount,            c2s},
        {xmppErrorTotal,             c2s},
        {xmppErrorBadRequest,        c2s},
        {xmppErrorIq,                c2s},
        {xmppErrorMessage,           c2s},
        {xmppErrorPresence,          c2s},
        {xmppIqTimeouts,             c2s},
        {xmppMessageSentW,            c2s},
        {xmppMessageReceivedW,        c2s},
        {xmppPresenceSentW,           c2s},
        {xmppPresenceReceivedW,       c2s},
        {xmppIqSentW,                 c2s},
        {xmppIqReceivedW,             c2s},
        {xmppStanzaSentW,             c2s},
        {xmppStanzaReceivedW,         c2s},
        {xmppStanzaDeniedW,           c2s},
        {xmppErrorTotalW,             c2s},
        {modRosterSets,              mod_roster},
        {modRosterGets,              mod_roster},
        {modPresenceSubscriptions,   mod_roster},
        {modPresenceUnsubscriptions, mod_roster},
        {modRosterPush,              mod_roster},
        {modRosterSize,              mod_roster},
        {modRosterGroups,            mod_roster},
        {modRosterSetsW,             mod_roster},
        {modRosterGetsW,             mod_roster},
        {modPresenceSubscriptionsW,  mod_roster},
        {modPresenceUnsubscriptionsW,mod_roster},
        {modRosterPushW,              mod_roster},
        {modRegisterCount,           mod_register},
        {modUnregisterCount,         mod_register},
        {modRegisterCountW,          mod_register},
        {modUnregisterCountW,        mod_register},
        {modRegisterUserCount,       mod_register},
        {modPrivacySets,             mod_privacy},
        {modPrivacySetsActive,       mod_privacy},
        {modPrivacySetsDefault,      mod_privacy},
        {modPrivacyPush,             mod_privacy},
        {modPrivacyGets,             mod_privacy},
        {modPrivacyStanzaBlocked,    mod_privacy},
        {modPrivacyStanzaAll,        mod_privacy},
        {modPrivacyListLength,       mod_privacy},
        {modPrivacySetsW,            mod_privacy},
        {modPrivacySetsActiveW,      mod_privacy},
        {modPrivacySetsDefaultW,     mod_privacy},
        {modPrivacyPushW,            mod_privacy},
        {modPrivacyGetsW,            mod_privacy}]).

-type c_module() :: 'c2s'
                  | 'core'
                  | 'mod_privacy'
                  | 'mod_register'
                  | 'mod_roster'.
-type c_module_table() :: 'stats_c2s'
                        | 'stats_core'
                        | 'stats_mod_privacy'
                        | 'stats_mod_register'
                        | 'stats_mod_roster'.
-type c_module_table_w() :: 'stats_w_c2s'
                          | 'stats_w_core'
                          | 'stats_w_mod_privacy'
                          | 'stats_w_mod_register'
                          | 'stats_w_mod_roster'.
-type counter() :: integer().
-type col() :: {'noValue','noSuchInstance'} | {'value',non_neg_integer()}.
-type row() :: {col(), col()}.

%%%.

-spec start([c_module()]) -> 'ok'.
start(Modules) ->
    initialize_tables(Modules).


stop() ->
    destroy_tables(),
    ok.

%%%' Helper functions (module local)

-spec table_name(c_module()) -> c_module_table().
table_name(core)         -> stats_core;
table_name(c2s)          -> stats_c2s;
table_name(mod_privacy)  -> stats_mod_privacy;
table_name(mod_register) -> stats_mod_register;
table_name(mod_roster)   -> stats_mod_roster.


-spec table_name_w(c_module()) -> c_module_table_w().
table_name_w(core)         -> stats_w_core;
table_name_w(c2s)          -> stats_w_c2s;
table_name_w(mod_privacy)  -> stats_w_mod_privacy;
table_name_w(mod_register) -> stats_w_mod_register;
table_name_w(mod_roster)   -> stats_w_mod_roster.


%% @doc Get a list of counters defined for the given module
-spec counters_for(c_module()) -> [counter()].
counters_for(Module) ->
    {Module, Counters} = proplists:lookup(Module, ?COUNTERS_FOR_MODULE),
    {Module, WCounters} = proplists:lookup(Module, ?W_COUNTERS_FOR_MODULE),
    Counters ++ WCounters.


-spec w_counters_for(c_module()) -> [counter()].
w_counters_for(Module) ->
    {Module, Counters} = proplists:lookup(Module, ?W_COUNTERS_FOR_MODULE),
    Counters.


%% @doc Get the name of the module the given counter is defined for
-spec module_for(counter()) -> c_module().
module_for(Counter) ->
    {Counter, Module} = proplists:lookup(Counter, ?MODULE_FOR_COUNTERS),
    Module.

%%%.

initialize_tables([]) ->
    initialize_tables(get_all_modules());
initialize_tables(Modules) ->
    lists:foreach(fun initialize_table/1, Modules).


-spec initialize_table(c_module()) -> 'ok'.
initialize_table(Module) ->
    ets:new(?STATS(Module), [public, named_table]),
    ets:new(?STATS_W(Module), [public, named_table]),
    initialize_counters(Module).


-spec initialize_counters(c_module()) -> 'ok'.
initialize_counters(Module) ->
    Counters = counters_for(Module),
    lists:foreach(fun(C) -> ets:insert(?STATS(Module), {C, 0}) end,
                  Counters),
    lists:foreach(fun(C) -> ets:insert(?STATS_W(Module), {C, 0}) end,
                  w_counters_for(Module)).


%% @doc Reset all counters for initialized tables
-spec reset_counters() -> 'ok'.
reset_counters() ->
    lists:foreach(
        fun(Module) ->
            Tab = ?STATS(Module),
            case ets:info(Tab) of
            undefined ->
                ok;
            _ ->
                lists:foreach(fun(C) -> ets:insert(Tab, {C, 0}) end,
                              counters_for(Module))
            end
        end,
        get_all_modules()).


%% @doc Moves temporary window values to main counter tables
%% and resets temporary tables
-spec window_change() -> 'ok'.
window_change() ->
    lists:foreach(
        fun(Module) ->
            Tab = ?STATS_W(Module),
            case ets:info(Tab) of
            undefined ->
                ok;
            _ ->
                lists:foreach(fun(C) ->
                                      [{C, Value}] = ets:lookup(Tab, C),
                                      set_counter(C, Value),
                                      ets:insert(Tab, {C, 0})
                              end,
                              w_counters_for(Module))
            end
        end,
        get_all_modules()).


%% @doc Delete a table if it exists
-spec destroy_table(atom() | ets:tid()) -> 'ok' | 'true'.
destroy_table(Tab) ->
    case ets:info(Tab) of
    undefined ->
        ok;
    _ ->
        ets:delete(Tab)
    end.


-spec get_all_modules() -> [c_module()].
get_all_modules() ->
    proplists:get_keys(?COUNTERS_FOR_MODULE).


-spec get_all_tables() -> [c_module_table() | c_module_table_w()].
get_all_tables() ->
    [ ?STATS(Module) || Module <- get_all_modules() ]
    ++ [ ?STATS_W(Module) || Module <- get_all_modules() ].


%% @doc Delete all tables possibly used by this module
%% This operation won't error on tables which are not currently used.
-spec destroy_tables() -> 'ok'.
destroy_tables() ->
    lists:foreach(fun destroy_table/1, get_all_tables()).


-spec is_started() -> boolean().
is_started() ->
    lists:any(
        fun(Tab) ->
            case ets:info(Tab) of
                undefined -> false;
                _ -> true
            end
        end,
        get_all_tables()).


-spec increment_window_counter(_) -> 'ok' | [integer()] | integer().
increment_window_counter(Counter) ->
    update_window_counter(Counter, 1).


-spec update_window_counter(_,_) -> 'ok' | [integer()] | integer().
update_window_counter(Counter, How) ->
    Tab = ?STATS_W(module_for(Counter)),
    case ets:info(Tab) of
        undefined ->
            ok;
        _ ->
            ets:update_counter(Tab, Counter, How)
    end.


-spec increment_counter(integer()) -> 'false' | 'ok' | 'true' | integer().
increment_counter(Counter) ->
    update_counter(Counter, 1).


-spec decrement_counter(integer()) -> 'false' | 'ok' | 'true' | integer().
decrement_counter(Counter) ->
    update_counter(Counter, {2, -1, 0, 0}).


-spec set_counter(integer(),_) -> 'false' | 'ok' | 'true' | integer().
set_counter(Counter, Value) ->
    modify_counter(Counter, ets, update_element, [{2, Value}]).


-spec update_counter(integer(),_) -> 'false' | 'ok' | 'true' | integer().
update_counter(Counter, How) ->
    modify_counter(Counter, ets, update_counter, [How]).


-spec modify_counter(Counter :: counter(),
                     Mod :: 'ets',
                     Fun :: 'update_counter' | 'update_element',
                     Args :: [any(),...]) -> ok | integer() | boolean().
modify_counter(Counter, Mod, Fun, Args) ->
    Tab = ?STATS(module_for(Counter)),
    case ets:info(Tab) of
        undefined ->
            ok;
        _ ->
            ArgsNew = [Tab, Counter | Args],
            apply(Mod, Fun, ArgsNew)
    end.


-spec counter_value('generalNodeName' | 'generalUptime' | integer()) -> {'value', counter()}.
counter_value(generalUptime) ->
    {value, erlang:round(element(1, erlang:statistics(wall_clock))/1000)};
counter_value(generalNodeName) ->
    {value, atom_to_list(node())};
counter_value(Counter) ->
    Tab = ?STATS(module_for(Counter)),
    [{Counter, Value}] = ets:lookup(Tab, Counter),
    {value, Value}.


-spec table_value(atom(), list(), list(), atom()
                 ) -> 'ok' | maybe_improper_list() | {'genErr',_}.
table_value(get, RowInd, Cols, Table) ->
    Row = get_row(RowInd, Table),
    get_cols(Row, Cols, Table);
table_value(get_next,RowInd,Cols,Table) ->
    lists:foldl(fun(Col, Res) ->
                      case {Res, get_next_value(RowInd, Col, Table)} of
                          {{genErr, _}, _} ->
                              Res;
                          {_, {genErr, Err}} ->
                              {genErr, Err};
                          {_, Value} ->
                              [Value| Res]
                      end
              end,[], Cols);
table_value(_,_,_,_) ->
    ok.


%% @doc gets full row with specified index
-spec get_row([any(),...], 'routerRegisteredPathsTable') -> row().
get_row([RowInd], routerRegisteredPathsTable) ->
    Routes = mnesia:dirty_all_keys(route),
    case RowInd of
        RInd when RInd > 0, RInd =< length(Routes) ->
            HName = lists:nth(RowInd, Routes),
            H = case HName of
                    N when is_binary(N) -> binary_to_list(N);
                    _ ->
                        HName
                end,
            {{value, H}, {value, length(mnesia:dirty_read(route, HName))}};
        _ ->
            {{noValue, noSuchInstance}, {noValue, noSuchInstance}}
    end.


%% @doc gets column values from specified row row
-spec get_cols(Row :: row(), Cols :: [any()],
               Table :: 'routerRegisteredPathsTable') -> col().
get_cols(Row, Cols, Table) ->
    Mapping = column_mapping(Cols, get_column_map(Table)),
    lists:map(fun(Col) ->
                      try element(Col, Row) of
                          Res -> Res
                      catch
                          _:_ ->
                              {noValue, noSuchInstance}
                      end
              end, Mapping).


%% @doc gets value of specified cell in the table
-spec get_cell_value(R :: 'endOfTable' | number(),
                     C :: 1 | 2,
                     Table :: 'routerRegisteredPathsTable'
                     ) -> {'genErr' | ['endOfTable' | number(),...],_}.
get_cell_value(R, C, Table) ->
    Row = get_row([R], Table),
    [Col] = column_mapping([C], get_column_map(Table)),
    try element(Col, Row) of
        {value, Value} ->
            {[C, R], Value};
        _ ->
            {genErr, C}
    catch
        _:_ ->
            {genErr, Col}
    end.


%% @doc gets value of next element (table get_next)
-spec get_next_value(RowInd :: any(),
                     Col :: 0 | 1 | 2,
                     Table :: 'routerRegisteredPathsTable')
      -> 'endOfTable' | {'genErr' | ['endOfTable' | number(),...],_}.
get_next_value(RowInd, Col, Table) ->
    case next_indexes(RowInd, Col, Table) of
        {R, C} ->
            get_cell_value(R, C, Table);
        endOfTable ->
            endOfTable
    end.


%% @doc finds indexes of next element
-spec next_indexes(RowInd :: any(),
                   Col :: 0 | 1 | 2,
                   Table :: 'routerRegisteredPathsTable'
                   ) -> 'endOfTable' | {'endOfTable' | number(),1 | 2}.
next_indexes(RowInd, Col, Table) ->
    case {next_row(RowInd, Table),
          next_col(Col, get_column_map(Table))} of
        {{[], endOfTable}, _} ->
            endOfTable;
        {{last, _NextR}, {_C, endOfTable}} ->
            endOfTable;
        {{last, NextR}, {_C, NextC}} ->
            {NextR, NextC};
        {{_R, NextR}, {C, _NextC}} ->
            {NextR, C}
    end.


%% @doc finds next column
-spec next_col(Col :: 0 | 1 | 2,
               List :: [{1 | 2,1 | 2},...]) -> {1 | 2,'endOfTable' | 1 | 2}.
next_col(0, [H | _T]) ->
    {Col, _} = H,
    {Col, Col};
next_col(Col, [H | T]) ->
    case H of
        {Col, _} when length(T) > 0->
            [{Next, _} | _T1] = T,
            {Col, Next};
        {Col, _} ->
            {Col, endOfTable};
        _ ->
            next_col(Col, T)
    end.


%% @doc finds next row index
-spec next_row(RowInd :: any(),
               Table :: 'routerRegisteredPathsTable'
               ) -> {'last' | [] | number(),'endOfTable' | number()}.
next_row(RowInd, routerRegisteredPathsTable) ->
    case { RowInd, length(mnesia:dirty_all_keys(route))} of
        {_, L} when L == 0 ->
            {[], endOfTable};
        {[], _L} ->
            {[], 1};
        {[RInd] ,L} when L < RInd; RInd < 0 ->
            {[], 1};
        {[RInd], L} when L == RInd ->
            {last, 1};
        {[RInd], _} ->
            {RInd, RInd +1}
    end.


-spec column_mapping([any()],[{1 | 2,1 | 2},...]) -> [any()].
column_mapping(Cols, Map) ->
    lists:map(fun(Col) -> proplists:get_value(Col, Map) end, Cols).


-spec get_column_map('routerRegisteredPathsTable') -> [{1 | 2,1 | 2},...].
get_column_map(routerRegisteredPathsTable) ->
    [{?routeTo, 1},
     {?routeNum, 2}].


%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
