%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(shaper).
-author('konrad.zemek@erlang-solutions.com').

-export([new/1, update/2]).

-record(shaper, {
    max_rate :: undefined | pos_integer(),
    tokens = 0 :: non_neg_integer(),
    last_update = erlang:monotonic_time() :: integer()
}).

-type shaper() :: #shaper{} | none.

-export_type([shaper/0]).

-spec new(atom()) -> shaper().
new(Name) ->
    case ejabberd_config:get_global_option({shaper, Name, global}) of
        undefined -> none;
        none -> none;
        {maxrate, MaxRate} -> #shaper{max_rate = MaxRate, tokens = MaxRate}
    end.

%% @doc Update shaper.
%% `Delay' is how many milliseconds to wait.
-spec update(shaper(), Size :: pos_integer()) -> {shaper(), Delay :: non_neg_integer()}.
update(none, _Size) ->
    {none, 0};
update(Shaper, Size) ->
    Now = erlang:monotonic_time(),
    Second = erlang:convert_time_unit(1, seconds, native),

    SecondsSinceLastUpdate = (Now - Shaper#shaper.last_update) / Second,
    TokenGrowth = round(Shaper#shaper.max_rate * SecondsSinceLastUpdate),
    Tokens = min(Shaper#shaper.max_rate, Shaper#shaper.tokens + TokenGrowth),

    TokensLeft = max(0, Tokens - Size),
    AdditionalTokensNeeded = max(0, Size - Tokens),

    TimeNeededForTokensToGrow = round(AdditionalTokensNeeded / Shaper#shaper.max_rate * Second),
    Delay = erlang:convert_time_unit(TimeNeededForTokensToGrow, native, millisecond),
    LastUpdate = Now + TimeNeededForTokensToGrow,

    lager:debug("Tokens: ~p (+~p,-~p), delay: ~p ms", [TokensLeft, TokenGrowth, Size, Delay]),

    {Shaper#shaper{last_update = LastUpdate, tokens = TokensLeft}, Delay}.
