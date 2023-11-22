%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_room_config.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Stateless utilities for room config processing
%%% Created : 15 Nov 2019 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_room_config).

%% API
-export([from_binary_kv_diff/2, from_binary_kv/2,
         to_binary_kv_diff/2, to_binary_kv/2]).

-include("mod_muc_light.hrl").

-export_type([binary_kv/0, kv/0, schema/0]).

%% Config primitives
-type key() :: atom().
-type value() :: binary() | integer() | float().
-type value_type() :: binary | integer | float.

%% Actual config
-type item() :: {key(), value()}.
-type kv() :: [item()].
-type binary_kv() :: [{Key :: binary(), Value :: binary()}].

%% User definition processing
-type schema_item() :: {FieldName :: binary(), DefaultValue :: value(), key(), value_type()}.
-type schema() :: [schema_item()]. % has to be sorted

%%====================================================================
%% API
%%====================================================================

%% Guarantees that config will have unique fields
-spec from_binary_kv_diff(RawConfig :: binary_kv(), ConfigSchema :: schema()) ->
    {ok, kv()} | validation_error().
from_binary_kv_diff(RawConfig, ConfigSchema) ->
    take_next(lists:ukeysort(1, RawConfig), ConfigSchema, true, fun take_next_kv/2, []).

-spec from_binary_kv(RawConfig :: binary_kv(), ConfigSchema :: schema()) ->
    {ok, kv()} | validation_error().
from_binary_kv(RawConfig, ConfigSchema) ->
    take_next(lists:ukeysort(1, RawConfig), ConfigSchema, false, fun take_next_kv/2, []).

-spec to_binary_kv_diff(RawConfig :: kv(), ConfigSchema :: schema()) ->
    {ok, binary_kv()} | validation_error().
to_binary_kv_diff(RawConfig, ConfigSchema) ->
    take_next(lists:ukeysort(1, RawConfig), ConfigSchema, true, fun take_next_binary_kv/2, []).

-spec to_binary_kv(Config :: kv(), ConfigSchema :: schema()) ->
    {ok, binary_kv()} | validation_error().
to_binary_kv(RawConfig, ConfigSchema) ->
    take_next(lists:ukeysort(1, RawConfig), ConfigSchema, false, fun take_next_binary_kv/2, []).

take_next([], [], _, _, Config) ->
    {ok, Config};
take_next(RawConfig, ConfigSchema, DropDefaults, TakeNext, Config) ->
    case {DropDefaults, TakeNext(RawConfig, ConfigSchema)} of
        {true, {default, RRawConfig, RConfigSchema, _}} ->
            % do not populate the diff with default values
            take_next(RRawConfig, RConfigSchema, DropDefaults, TakeNext, Config);
        {_, {_, RRawConfig, RConfigSchema, KV}} ->
            take_next(RRawConfig, RConfigSchema, DropDefaults, TakeNext, [KV | Config]);
        {_, {error, Reason}} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

take_next_kv([{KeyBin, ValBin} | RRawConfig], [{KeyBin, _Default, Key, Type} | RSchema]) ->
    try {value, RRawConfig, RSchema, {Key, b2value(ValBin, Type)}}
    catch _:_ -> {error, {KeyBin, type_error}}
    end;
take_next_kv(RawConfig, [{_KeyBin, Default, Key, _Type} | RSchema]) ->
    {default, RawConfig, RSchema, {Key, Default}};
take_next_kv([{KeyBin, _} | _], _) ->
    {error, {KeyBin, not_found}}.

take_next_binary_kv([{Key, ValBin} | RRawConfig], [{KeyBin, _Default, Key, Type} | RSchema]) ->
    try {value, RRawConfig, RSchema, {KeyBin, value2b(ValBin, Type)}}
    catch _:_ -> {error, {KeyBin, type_error}}
    end;
take_next_binary_kv(RawConfig, [{KeyBin, Default, _Key, _Type} | RSchema]) ->
    {default, RawConfig, RSchema, {KeyBin, Default}};
take_next_binary_kv([{KeyBin, _} | _], _) ->
    {error, {KeyBin, not_found}}.

-spec b2value(ValBin :: binary(), Type :: value_type()) -> Converted :: value().
b2value(ValBin, binary) when is_binary(ValBin) -> ValBin;
b2value(ValBin, integer) -> binary_to_integer(ValBin);
b2value(ValBin, float) -> binary_to_float(ValBin).

-spec value2b(Val :: value(), Type :: value_type()) -> Converted :: binary().
value2b(Val, binary) when is_binary(Val) -> Val;
value2b(Val, integer) -> integer_to_binary(Val);
value2b(Val, float) -> float_to_binary(Val).
