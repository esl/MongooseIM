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
    last_update = erlang:monotonic_time(millisecond) :: integer()
}).

-type shaper() :: #shaper{} | none.

-export_type([shaper/0]).

-spec new(atom()) -> shaper().
new(Name) ->
    case ejabberd_config:get_global_option({shaper, Name, global}) of
        {maxrate, MaxRatePerSecond} ->
            #shaper{max_rate = MaxRatePerSecond,
                    tokens = MaxRatePerSecond,
                    last_update = erlang:monotonic_time(millisecond)};
        _ -> none
    end.

%% @doc Update shaper.
%% `Delay' is how many milliseconds to wait.
-spec update(shaper(), Size :: pos_integer()) -> {shaper(), Delay :: non_neg_integer()}.
update(none, _Size) ->
    {none, 0};
update(#shaper{max_rate = MaxRatePerSecond,
               tokens = LastAvailableTokens,
               last_update = LastUpdate}, NowUsed) ->
    Now = erlang:monotonic_time(millisecond),
    % How much we might have recovered since last time, in milliseconds arithmetic
    GrowthPerMillisecond = MaxRatePerSecond / 1000,
    MilliSecondsSinceLastUpdate = (Now - LastUpdate),
    PossibleTokenGrowth = round(GrowthPerMillisecond * MilliSecondsSinceLastUpdate),
    % Available plus recovered cannot grow higher than the actual rate limit
    ExactlyAvailableNow = min(MaxRatePerSecond, LastAvailableTokens + PossibleTokenGrowth),
    TokensAvailable = max(0, ExactlyAvailableNow - NowUsed),
    TokensOverused = max(0, NowUsed - ExactlyAvailableNow),
    MaybeDelay = round(TokensOverused / GrowthPerMillisecond),
    {#shaper{max_rate = MaxRatePerSecond,
             tokens = TokensAvailable,
             last_update = Now + MaybeDelay},
     MaybeDelay}.
