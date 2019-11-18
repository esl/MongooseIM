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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_room_config).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([schema_from_definition/1, default_from_schema/1]).
-export([apply_binary_kv/3, to_binary_kv/2]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_muc_light.hrl").

-export_type([schema/0, binary_kv/0, kv/0, user_defined_schema/0]).

%% Config primitives
-type key() :: atom().
-type value() :: binary() | integer() | float().
-type value_type() :: binary | integer | float.
-type form_field_name() :: binary().

%% Schema
-type schema_item() :: {Default :: value(), key(), value_type()}.
-type schema() :: #{
        form_field_name() => schema_item(),
        key() => form_field_name()
       }.

%% Actual config
-type item() :: {key(), value()}.
-type kv() :: [item()].
-type binary_kv() :: [{Key :: binary(), Value :: binary()}].

%% User definition processing
-type user_defined_schema_item() :: {FieldName :: string(), DefaultValue :: string()}
                                    | {FieldName :: string(), DefaultValue :: value() | string(),
                                       key(), value_type()}.
-type user_defined_schema() :: [user_defined_schema_item()].

%%====================================================================
%% API
%%====================================================================

-spec schema_from_definition(UserDefinedSchema :: user_defined_schema()) -> schema().
schema_from_definition(UserDefinedSchema) ->
    lists:foldl(fun add_config_schema_field/2, #{}, UserDefinedSchema).

-spec default_from_schema(ConfigSchema :: schema()) -> kv().
default_from_schema(ConfigSchema) ->
    lists:foldl(fun({DefaultValue, Key, _}, Acc) -> [{Key, DefaultValue} | Acc];
                   (_, Acc) -> Acc
                end, [], maps:values(ConfigSchema)).

%% Guarantees that config will have unique fields
-spec apply_binary_kv(RawConfig :: binary_kv(), Config :: kv(), ConfigSchema :: schema()) ->
    {ok, kv()} | validation_error().
apply_binary_kv([], Config, _ConfigSchema) ->
    {ok, Config};
apply_binary_kv([{KeyBin, ValBin} | RRawConfig], Config, ConfigSchema) ->
    case from_kv_tuple(KeyBin, ValBin, ConfigSchema) of
        {ok, Key, Val} ->
            apply_binary_kv(RRawConfig, lists:keystore(Key, 1, Config, {Key, Val}), ConfigSchema);
        Error ->
            Error
    end.

-spec to_binary_kv(Config :: kv(), ConfigSchema :: schema()) -> binary_kv().
to_binary_kv([], _ConfigSchema) ->
    [];
to_binary_kv([{Key, Val} | RConfig], ConfigSchema) ->
    KeyBin = maps:get(Key, ConfigSchema),
    {_Def, _Key, ValType} = maps:get(KeyBin, ConfigSchema),
    [{KeyBin, value2b(Val, ValType)} | to_binary_kv(RConfig, ConfigSchema)].

%%====================================================================
%% Internal functions
%%====================================================================

-spec add_config_schema_field(UserDefinedSchemaItem :: user_defined_schema_item(),
                              SchemaAcc :: schema()) ->
    schema().
add_config_schema_field({FieldName, DefaultValue}, SchemaAcc) ->
    add_config_schema_field({FieldName, DefaultValue, list_to_atom(FieldName), binary}, SchemaAcc);
add_config_schema_field({FieldName, DefaultValue, Key, ValueType} = Definition, SchemaAcc) ->
    case is_list(FieldName) andalso
         is_valid_config_type(ValueType) andalso
         has_valid_type(DefaultValue, ValueType) andalso
         is_atom(Key) of
        true ->
            FieldNameBin = unicode:characters_to_binary(FieldName),
            SchemaAcc#{ FieldNameBin => {normalize_value(DefaultValue, ValueType), Key, ValueType},
                        Key => FieldNameBin };
        false ->
            error({invalid_schema_definition, Definition})
    end.

-spec from_kv_tuple(KeyBin :: binary(), ValBin :: binary(), ConfigSchema :: schema()) ->
    {ok, Key :: atom(), Val :: any()} | validation_error().
from_kv_tuple(KeyBin, ValBin, ConfigSchema) ->
    case ConfigSchema of
        #{ KeyBin := {_Def, Key, Type} } -> {ok, Key, b2value(ValBin, Type)};
        _ -> {error, {KeyBin, unknown}}
    end.

-spec is_valid_config_type(Type :: value_type() | term()) -> boolean().
is_valid_config_type(binary) -> true;
is_valid_config_type(integer) -> true;
is_valid_config_type(float) -> true;
is_valid_config_type(_) -> false.

-spec has_valid_type(Value :: value() | string() | term(), value_type()) -> boolean().
has_valid_type(Val, binary) -> is_binary(Val) or is_list(Val);
has_valid_type(Val, integer) -> is_integer(Val);
has_valid_type(Val, float) -> is_float(Val).

-spec normalize_value(Value :: value() | string(), value_type()) -> value().
normalize_value(Val, binary) when is_list(Val) -> unicode:characters_to_binary(Val);
normalize_value(Val, _) -> Val.

-spec b2value(ValBin :: binary(), Type :: value_type()) -> Converted :: value().
b2value(ValBin, binary) -> ValBin;
b2value(ValBin, integer) -> binary_to_integer(ValBin);
b2value(ValBin, float) -> binary_to_float(ValBin).

-spec value2b(Val :: value(), Type :: value_type()) -> Converted :: binary().
value2b(Val, binary) -> Val;
value2b(Val, integer) -> integer_to_binary(Val);
value2b(Val, float) -> float_to_binary(Val).

