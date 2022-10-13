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
-export([from_binary_kv_diff/2, from_binary_kv/2, to_binary_kv/2]).

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
-type schema_item() :: {FieldName :: binary(), DefaultValue :: value(),
                                     key(), value_type()}.
-type schema() :: [schema_item()]. % has to be sorted

%%====================================================================
%% API
%%====================================================================

%% Guarantees that config will have unique fields
-spec from_binary_kv_diff(RawConfig :: binary_kv(), ConfigSchema :: schema()) ->
    {ok, kv()} | validation_error().
from_binary_kv_diff(RawConfig, ConfigSchema) ->
    from_binary_kv_diff(lists:ukeysort(1, RawConfig), ConfigSchema, []).

from_binary_kv_diff([], [], Config) ->
    {ok, Config};
from_binary_kv_diff(RawConfig, ConfigSchema, Config) ->
    case take_next_kv(RawConfig, ConfigSchema) of
        {error, Reason} ->
            {error, Reason};
        {value, RRawConfig, RConfigSchema, KV} ->
            from_binary_kv(RRawConfig, RConfigSchema, [KV | Config]);
        {default, _, _, _} ->
            % do not populate the diff with default values
            from_binary_kv(RawConfig, ConfigSchema, Config)
    end.

-spec from_binary_kv(RawConfig :: binary_kv(), ConfigSchema :: schema()) ->
          {ok, kv()} | validation_error().
from_binary_kv(RawConfig, ConfigSchema) ->
    from_binary_kv(lists:ukeysort(1, RawConfig), ConfigSchema, []).

from_binary_kv([], [], Config) ->
    {ok, Config};
from_binary_kv(RawConfig, ConfigSchema, Config) ->
    case take_next_kv(RawConfig, ConfigSchema) of
        {error, Reason} ->
            {error, Reason};
        {_, RRawConfig, RConfigSchema, KV} ->
            from_binary_kv(RRawConfig, RConfigSchema, [KV | Config])
    end.

take_next_kv([{KeyBin, ValBin} | RRawConfig], [{KeyBin, _Default, Key, Type} | RSchema]) ->
    try {value, RRawConfig, RSchema, {Key, b2value(ValBin, Type)}}
    catch _:_ -> {error, {KeyBin, type_error}}
    end;
take_next_kv(RawConfig, [{_KeyBin, Default, Key, _Type} | RSchema]) ->
    {default, RawConfig, RSchema, {Key, Default}};
take_next_kv([{KeyBin, _} | _], _) ->
    {error, {KeyBin, not_found}}.

-spec to_binary_kv(Config :: kv(), ConfigSchema :: schema()) -> binary_kv().
to_binary_kv(Config, ConfigSchema) ->
    ConfigWithSchema = lists:zip(lists:sort(Config), lists:keysort(3, ConfigSchema)),
    [{KeyBin, value2b(Val, Type)} || {{Key, Val}, {KeyBin, _Default, Key, Type}} <- ConfigWithSchema].

%%====================================================================
%% Internal functions
%%====================================================================

-spec b2value(ValBin :: binary(), Type :: value_type()) -> Converted :: value().
b2value(ValBin, binary) when is_binary(ValBin) -> ValBin;
b2value(ValBin, integer) -> binary_to_integer(ValBin);
b2value(ValBin, float) -> binary_to_float(ValBin).

-spec value2b(Val :: value(), Type :: value_type()) -> Converted :: binary().
value2b(Val, binary) when is_binary(Val) -> Val;
value2b(Val, integer) -> integer_to_binary(Val);
value2b(Val, float) -> float_to_binary(Val).
