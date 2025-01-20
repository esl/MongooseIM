%% @doc This module contains debug utilities intended for use with Erlang Doctor.
%% Beware that this tool has the potential of seriously impacting
%% your system, leading to various issues including system crash or data loss.
%% Therefore, it is intended for use in development or QA environments,
%% and using it in a production environment is risky.
%%
%% Example: let's capture and list all stanzas exchanged between Alice and Bob.
%%
%% ```
%% tr:start().
%% tr:trace([mongoose_c2s_hooks]).
%%
%% %% Exchange stanzas between users
%%
%% tr:stop_tracing().
%% tr_util:c2s_elements_between_jids([<<"alice@localhost">>, <<"bob@localhost">>]).
%%
%% %% You will get a list of `c2s_element_info' maps with the exchanged stanzas.
%% '''
%%
%% @reference See <a href="https://hexdocs.pm/erlang_doctor/readme.html">Hex Docs</a>
%% for more information about Erlang Doctor.

-module(tr_util).

%% Debugging API for mongoose_c2s events and XMPP traffic
-export([c2s_elements_between_jids/1,
         c2s_hooks/0,
         c2s_elements/0]).

%% Selectors for use with `tr:call_stat' etc.
-export([tr_to_element_info/1,
         tr_to_hook_name_and_tag/1]).

%% Predicates for use with `tr:filter' etc.
-export([filter_c2s_hook/1]).

-include_lib("erlang_doctor/include/tr.hrl").
-include_lib("exml/include/exml.hrl").

-ignore_xref(?MODULE).

-type c2s_element_info() :: #{name := binary(),
                              contents := binary(),
                              ref := reference(),
                              hooks := [atom()],
                              jid := jid:literal_jid(),
                              from_jid := jid:literal_jid(),
                              to_jid := jid:literal_jid(),
                              id := binary(),
                              type := binary()}.

%% Complete utilities

%% @doc Get a list of XML elements (usually stanzas) exchanged between the listed JIDs.
%% The `to' and `from' attributes must match different JIDs from the list.
%% Matching starts from the beginning of the list. A bare JID matches any resource.
%%
%% Requires traces from modules: `[mongoose_c2s_hooks]'.
-spec c2s_elements_between_jids([jid:literal_jid()]) -> [c2s_element_info()].
c2s_elements_between_jids(TargetBinJids) ->
    Targets = lists:map(fun jid:from_binary_noprep/1, TargetBinJids),
    lists:filter(fun(#{from_jid := From, to_jid := To}) ->
                         case match_target_jids(From, Targets) of
                             [] -> false;
                             [H|_] -> match_target_jids(To, Targets -- [H]) =/= []
                         end
                 end, c2s_elements()).

%% @doc Get a list of all C2S hooks in the execution order, annotated by user JIDs,
%% for which they were executed.
%%
%% Requires traces from modules: `[mongoose_c2s_hooks]'.
-spec c2s_hooks() -> [{jid:literal_jid(), atom(), mongoose_acc:t()}].
c2s_hooks() ->
    [{jid:to_binary(mongoose_c2s:get_jid(Data)), Hook, Acc} ||
        #tr{mfa = {_, Hook, _}, data = [_HT, Acc, #{c2s_data := Data}]} <-
            tr:filter(fun filter_c2s_hook/1)
    ].

%% @doc Get information about XML elements, for which C2S hooks were executed.
%%
%% Requires traces from modules: `[mongoose_c2s_hooks]'.
-spec c2s_elements() -> [c2s_element_info()].
c2s_elements() ->
    join_hooks(c2s_element_hooks()).

%% Selectors for use with `tr:call_stat' etc.

-spec tr_to_hook_name_and_tag(tr:tr()) -> {gen_hook:hook_name(), gen_hook:hook_tag()}.
tr_to_hook_name_and_tag(#tr{mfa = {gen_hook, run_fold, _}, data = [HookName, Tag | _]}) ->
    {HookName, Tag}.

-spec tr_to_element_info(tr:tr()) -> c2s_element_info().
tr_to_element_info(#tr{mfa = {mongoose_c2s_hooks, Hook, _},
                       data = [_HT, #{stanza := ElementAcc}, #{c2s_data := Data}]}) ->
    element_info(Data, Hook, ElementAcc).

%% Predicates for use with `tr:filter' etc.

-spec filter_c2s_hook(tr:tr()) -> boolean().
filter_c2s_hook(#tr{mfa = {mongoose_c2s_hooks, _, _}}) -> true.

-spec filter_c2s_hook_with_element(tr:tr()) -> boolean().
filter_c2s_hook_with_element(#tr{mfa = {mongoose_c2s_hooks, _, _},
                                 data = [_HT, #{stanza := #{}}, _Data]}) ->
    true.

%% Internal helpers

-spec c2s_element_hooks() -> [c2s_element_info()].
c2s_element_hooks() ->
    lists:map(fun tr_to_element_info/1, tr:filter(fun filter_c2s_hook_with_element/1)).

-spec match_target_jids(jid:literal_jid(), [jid:jid()]) -> [jid:jid()].
match_target_jids(ActualBJid, Targets) ->
    Actual = jid:from_binary_noprep(ActualBJid),
    lists:filter(fun(Target) -> match_jid(Target, Actual) end, Targets).

-spec match_jid(jid:jid(), jid:jid()) -> boolean().
match_jid(Target, Actual) ->
    case jid:lresource(Target) of
        <<>> -> jid:are_bare_equal(Target, Actual);
        _ -> jid:are_equal(Target, Actual)
    end.

-spec join_hooks([c2s_element_info()]) -> [c2s_element_info()].
join_hooks([First | Rest]) ->
    lists:reverse(lists:foldl(fun join_hooks_step/2, [First], Rest));
join_hooks([]) ->
    [].

-spec join_hooks_step(c2s_element_info(), [c2s_element_info()]) -> [c2s_element_info()].
join_hooks_step(Cur, [Prev | Acc]) ->
    case {maps:take(hooks, Cur), maps:take(hooks, Prev)} of
        {{CurHooks, D}, {PrevHooks, D}} ->
            [D#{hooks => PrevHooks ++ CurHooks} | Acc];
        _ ->
            [Cur, Prev | Acc]
    end.

-spec element_info(mongoose_c2s:data(), gen_hook:hook_name(), mongoose_acc:stanza_metadata()) ->
          c2s_element_info().
element_info(Data, Hook, #{element := Element, ref := Ref, from_jid := From, to_jid := To}) ->
    Info = #{name => Element#xmlel.name,
             contents => exml:to_binary(Element#xmlel.children),
             ref => Ref,
             hooks => [Hook],
             jid => jid:to_binary(mongoose_c2s:get_jid(Data)),
             from_jid => jid:to_binary(From),
             to_jid => jid:to_binary(To)},
    maps:merge(Info, element_attr_info(Element#xmlel.attrs)).

-spec element_attr_info(exml:attrs()) -> #{atom() => binary()}.
element_attr_info(Attrs) ->
    AllowedAttrs = [<<"id">>, <<"type">>],
    #{binary_to_existing_atom(Key) => Value ||
         Key := Value <- maps:with(AllowedAttrs, Attrs)}.
