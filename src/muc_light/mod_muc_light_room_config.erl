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
-export([schema_from_definition/1, default_from_definition/2]).
-export([apply_binary_kv/3, to_binary_kv/2]).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_muc_light.hrl").

-export_type([schema/0, binary_kv/0, kv/0]).

%% Config primitives
-type key() :: atom().
-type value() :: binary() | integer() | float().
-type value_type() :: binary | integer | float.
-type form_field_name() :: binary().

%% Schema
-type schema_item() :: {form_field_name(), key(), value_type()}.
-type schema() :: [schema_item()].

%% Actual config
-type item() :: {key(), value()}.
-type kv() :: [item()].
-type binary_kv() :: [{Key :: binary(), Value :: binary()}].

%% User definition processing
-type user_defined_schema_item() :: FieldName :: string()
                                    | {FieldName :: string(), FieldName :: value_type()}
                                    | schema_item().
-type user_defined_schema() :: [user_defined_schema_item()].

-type user_config_defaults_item() :: {FieldName :: string(), FieldValue :: term()}.
-type user_config_defaults() :: [user_config_defaults_item()].

%%====================================================================
%% API
%%====================================================================

-spec schema_from_definition(UserDefinedSchema :: user_defined_schema()) -> schema().
schema_from_definition(UserDefinedSchema) ->
    lists:map(fun expand_config_schema_field/1, UserDefinedSchema).

-spec default_from_definition(UserConfigDefaults :: user_config_defaults(),
                         ConfigSchema :: schema()) -> kv().
default_from_definition(UserConfigDefaults, ConfigSchema) ->
    DefaultConfigCandidate = lists:map(fun process_config_field/1, UserConfigDefaults),
    lists:foreach(fun({Key, Value}) ->
                          try
                              {_, _, ValueType} = lists:keyfind(Key, 2, ConfigSchema),
                              value2b(Value, ValueType)
                          catch
                              _:Error -> error({invalid_default_config, Key, Value, Error})
                          end
                  end, DefaultConfigCandidate),
    DefaultConfigCandidate.

%% Guarantees that config will have unique fields
-spec apply_binary_kv(RawConfig :: binary_kv(), Config :: kv(), ConfigSchema :: schema()) ->
    {ok, kv()} | validation_error().
apply_binary_kv([], Config, _ConfigSchema) ->
    {ok, Config};
apply_binary_kv([{KeyBin, ValBin} | RRawConfig], Config, ConfigSchema) ->
    case process_raw_config_opt(KeyBin, ValBin, ConfigSchema) of
        {ok, Key, Val} ->
            apply_binary_kv(RRawConfig, lists:keystore(Key, 1, Config, {Key, Val}), ConfigSchema);
        Error ->
            Error
    end.

-spec to_binary_kv(Config :: kv(), ConfigSchema :: schema()) -> binary_kv().
to_binary_kv([], _ConfigSchema) ->
    [];
to_binary_kv([{Key, Val} | RConfig], ConfigSchema) ->
    {KeyBin, _, ValType} = lists:keyfind(Key, 2, ConfigSchema),
    [{KeyBin, value2b(Val, ValType)} | to_binary_kv(RConfig, ConfigSchema)].

%%====================================================================
%% Internal functions
%%====================================================================

-spec expand_config_schema_field(UserDefinedSchemaItem :: user_defined_schema_item()) ->
    schema_item().
expand_config_schema_field({FieldName, Type}) ->
    {_, true} = {FieldName, is_valid_config_type(Type)},
    {list_to_binary(FieldName), list_to_atom(FieldName), Type};
expand_config_schema_field({FieldNameBin, FieldName, Type} = SchemaItem)
  when is_binary(FieldNameBin) andalso is_atom(FieldName) ->
    {_, true} = {FieldName, is_valid_config_type(Type)},
    SchemaItem;
expand_config_schema_field(Name) ->
    {list_to_binary(Name), list_to_atom(Name), binary}.

-spec process_config_field(UserConfigDefaultsItem :: user_config_defaults_item()) -> item().
process_config_field({Key, Value}) when is_list(Value) ->
    process_config_field({Key, list_to_binary(Value)});
process_config_field({Key, Value}) ->
    {list_to_atom(Key), Value}.

-spec process_raw_config_opt(
        KeyBin :: binary(), ValBin :: binary(), ConfigSchema :: schema()) ->
    {ok, Key :: atom(), Val :: any()} | validation_error().
process_raw_config_opt(KeyBin, ValBin, ConfigSchema) ->
    case lists:keyfind(KeyBin, 1, ConfigSchema) of
        {_, Key, Type} -> {ok, Key, b2value(ValBin, Type)};
        _ -> {error, {KeyBin, unknown}}
    end.

-spec is_valid_config_type(Type :: value_type() | term()) -> boolean().
is_valid_config_type(binary) -> true;
is_valid_config_type(integer) -> true;
is_valid_config_type(float) -> true;
is_valid_config_type(_) -> false.

-spec b2value(ValBin :: binary(), Type :: value_type()) -> Converted :: value().
b2value(ValBin, binary) -> ValBin;
b2value(ValBin, integer) -> binary_to_integer(ValBin);
b2value(ValBin, float) -> binary_to_float(ValBin).

-spec value2b(Val :: value(), Type :: value_type()) -> Converted :: binary().
value2b(Val, binary) -> Val;
value2b(Val, integer) -> integer_to_binary(Val);
value2b(Val, float) -> float_to_binary(Val).

