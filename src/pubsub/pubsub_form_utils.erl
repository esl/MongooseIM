%%%----------------------------------------------------------------------
%%% File    : pubsub_form_utils.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : mod_pubsub form processing utilities
%%% Created : 28 Nov 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>

%%% Portions created by ProcessOne and Brian Cully <bjc@kublai.com>
%%%----------------------------------------------------------------------

-module(pubsub_form_utils).

-author("piotr.nosek@erlang-solutions.com").

-export([make_sub_xform/1, parse_sub_xform/1]).

-include("mongoose_logger.hrl").
-include("mongoose_ns.hrl").
-include_lib("exml/include/exml.hrl").

-type convert_from_binary_fun() :: fun(([binary()]) -> any()).
-type convert_to_binary_fun() :: fun((any()) -> [binary()]).
-type convert_funs() :: #{ from_binaries := convert_from_binary_fun(),
                           to_binaries := convert_to_binary_fun() }.
-type field_data_type() :: boolean | integer | datetime | list | atom | {custom, convert_funs()}.

-type option_properties() :: #{
        form_type => binary(), % type reported as field attr in XML
        possible_choices => [{Value :: binary(), ValueDescription :: binary()}],
        data_type => field_data_type(),
        label => binary()
       }.

-type option_definition() :: {FormVar :: binary(),
                              InternalName :: atom(),
                              OptionProperties :: option_properties()}.

-type convert_from_binary_error() :: {error, {unknown_option, VarName :: binary}}.
-type parse_error() :: {error, invalid_form} | convert_from_binary_error().

%%====================================================================
%% API
%%====================================================================

%% Missing options won't have any <value/> elements
%% TODO: Right now
-spec make_sub_xform(Options :: mod_pubsub:subOptions()) -> {ok, exml:element()}.
make_sub_xform(Options) ->
    XFields = [make_field_xml(OptDefinition, Options) || OptDefinition <- sub_form_options()],
    {ok, make_sub_xform_xml(XFields)}.

%% The list of options returned by this function may be a subset of the options schema.
%% TODO: It is the behaviour of original code. Maybe it should be changed? To discuss.
-spec parse_sub_xform(exml:element() | undefined) -> {ok, mod_pubsub:subOptions()} | parse_error().
parse_sub_xform(undefined) ->
    {ok, []};
parse_sub_xform(XForm) ->
    case jlib:parse_xdata_submit(XForm) of
        invalid -> {error, invalid_form};
        XData -> convert_fields_from_binaries(XData, [], sub_form_options())
    end.

%%====================================================================
%% Form XML builders
%%====================================================================

-spec make_sub_xform_xml(XFields :: [exml:element()]) -> exml:element().
make_sub_xform_xml(XFields) ->
    FormTypeEl = #xmlel{name = <<"field">>,
                        attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
                        children = [#xmlel{name = <<"value">>, attrs = [],
                                           children = [{xmlcdata, ?NS_PUBSUB_SUB_OPTIONS}]}]},
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}], children = [FormTypeEl | XFields]}.

-spec make_field_xml(OptDefinition :: option_definition(),
                     Options :: mod_pubsub:subOptions()) -> exml:element().
make_field_xml({VarName, Key, #{ label := Label, form_type := FormType } = VarProps}, Options) ->
    ChoicesEls = make_choices_xml(VarProps),
    ValEls = make_values_xml(Key, Options, VarProps),

    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, VarName}, {<<"type">>, FormType}, {<<"label">>, Label}],
           children = ChoicesEls ++ ValEls}.

make_choices_xml(#{ possible_choices := PossibleChoices }) ->
    [ make_option_xml(Value, Label) || {Value, Label} <- PossibleChoices ];
make_choices_xml(#{}) ->
    [].

make_option_xml(Value, Label) ->
    #xmlel{name = <<"option">>, attrs = [{<<"label">>, Label}], children = [make_value_xml(Value)]}.

make_values_xml(Key, Options, #{ data_type := DataType }) ->
    case lists:keyfind(Key, 1, Options) of
        {_, Value} ->
            [make_value_xml(BinVal) || BinVal <- convert_value_to_binaries(Value, DataType)];
        false ->
            []
    end.

make_value_xml(Value) ->
    #xmlel{name = <<"value">>, attrs = [], children = [#xmlcdata{ content = Value }]}.

%%====================================================================
%% Form definitions & conversions
%%====================================================================

-spec sub_form_options() -> [option_definition()].
sub_form_options() ->
    [
     {<<"pubsub#deliver">>, deliver,
      #{ form_type => <<"boolean">>,
         data_type => boolean,
         label => <<"Whether an entity wants to receive or disable notifications">>
       }},

     {<<"pubsub#digest">>, digest,
      #{ form_type => <<"boolean">>,
         data_type => boolean,
         label => <<"Whether an entity wants to receive digests (aggregations)"
                    " of notifications or all notifications individually">>
       }},

     {<<"pubsub#digest_frequency">>, digest_frequency,
      #{ form_type => <<"text-single">>,
         data_type => integer,
         label => <<"The minimum number of milliseconds between sending"
                    " any two notification digests">>
       }},

     {<<"pubsub#expire">>, expire,
      #{ form_type => <<"text-single">>,
         data_type => datetime,
         label => <<"The DateTime at which a leased subscription will end or has ended">>
       }},

     {<<"pubsub#include_body">>, include_body,
      #{ form_type => <<"boolean">>,
         data_type => boolean,
         label =>  <<"Whether an entity wants to receive an XMPP message body"
                     " in addition to the payload format">>
       }},

     {<<"pubsub#show-values">>, show_values,
      #{ form_type => <<"list-multi">>,
         possible_choices => [{<<"away">>, <<"Away">>},
                             {<<"chat">>, <<"Chat">>},
                             {<<"dnd">>, <<"Do Not Disturb">>},
                             {<<"online">>, <<"Any online state">>},
                             {<<"xa">>, <<"Extended Away">>}],
         data_type => list,
         label => <<"The presence states for which an entity wants to receive notifications">>
       }},

     {<<"pubsub#subscription_type">>, subscription_type,
      #{ form_type => <<"list-single">>,
         possible_choices => [{<<"items">>, <<"Receive notification of new items only">>},
                             {<<"nodes">>, <<"Receive notification of new nodes only">>}],
         data_type => {custom, #{ from_binaries => fun convert_sub_type_from_binary/1,
                                  to_binaries => fun convert_sub_type_to_binary/1 }},
         label => <<"Type of notification to receive">>
       }},

     {<<"pubsub#subscription_depth">>, subscription_depth,
      #{ form_type => <<"list-single">>,
         possible_choices => [{<<"1">>, <<"Receive notification from direct child nodes only">>},
                             {<<"all">>, <<"Receive notification from all descendent nodes">>}],
         data_type => {custom, #{ from_binaries => fun convert_sub_depth_from_binary/1,
                                  to_binaries => fun convert_sub_depth_to_binary/1 }},
         label => <<"Depth from subscription for which to receive notifications">>
       }}
    ].

-spec convert_fields_from_binaries([{VarNameBin :: binary(), Values :: [binary()]}],
                                   Acc :: mod_pubsub:subOptions(),
                                   Schema :: [option_definition()]) ->
    {ok, mod_pubsub:subOptions()} | convert_from_binary_error().
convert_fields_from_binaries([], Result, _Schema) ->
    {ok, Result};
convert_fields_from_binaries([{<<"FORM_TYPE">>, _Values} | RData], Acc, Schema) ->
    convert_fields_from_binaries(RData, Acc, Schema);
convert_fields_from_binaries([{VarBin, Values} | RData], Acc, Schema) ->
    case lists:keyfind(VarBin, 1, Schema) of
        {_VBin, _Var, #{ data_type := DataType }} when Values == [] andalso DataType /= list ->
            convert_fields_from_binaries(RData, Acc, Schema);
        {_VBin, Var, #{ data_type := DataType }} ->
            try convert_value_from_binaries(Values, DataType) of
                Converted ->
                    NAcc = lists:keystore(Var, 1, Acc, {Var, Converted}),
                    convert_fields_from_binaries(RData, NAcc, Schema)
            catch
                C:R:S ->
                    {error, {conversion_failed, {Var, DataType, C, R, S}}}
            end;
        false ->
            {error, {unknown_option, VarBin}}
    end.

-spec convert_value_from_binaries(Bins :: [binary()], field_data_type()) -> any().
convert_value_from_binaries(Bins, {custom, #{ from_binaries := ConvertFromBinaryFun }}) ->
    ConvertFromBinaryFun(Bins);
convert_value_from_binaries([Bin], boolean) ->
    convert_bool_from_binary(Bin);
convert_value_from_binaries([Bin], integer) ->
    binary_to_integer(Bin);
convert_value_from_binaries([Bin], datetime) ->
    jlib:datetime_binary_to_timestamp(Bin);
convert_value_from_binaries(Bins, list) when is_list(Bins) ->
    Bins.

-spec convert_value_to_binaries(Value :: any(), field_data_type()) -> [binary()].
convert_value_to_binaries(Value, {custom, #{ to_binaries := ConvertToBinaryFun }}) ->
    ConvertToBinaryFun(Value);
convert_value_to_binaries(Value, boolean) ->
    [convert_bool_to_binary(Value)];
convert_value_to_binaries(Value, integer) ->
    [integer_to_binary(Value)];
convert_value_to_binaries(Value, datetime) ->
    jlib:now_to_utc_binary(Value);
convert_value_to_binaries(Value, list) when is_list(Value) ->
    Value.

convert_bool_from_binary(<<"0">>) -> false;
convert_bool_from_binary(<<"1">>) -> true;
convert_bool_from_binary(<<"false">>) -> false;
convert_bool_from_binary(<<"true">>) -> true.

convert_bool_to_binary(true) -> <<"true">>;
convert_bool_to_binary(false) -> <<"false">>.

convert_sub_depth_from_binary([<<"all">>]) -> all;
convert_sub_depth_from_binary([DepthBin]) -> binary_to_integer(DepthBin).

convert_sub_depth_to_binary(all) -> [<<"all">>];
convert_sub_depth_to_binary(Depth) -> [integer_to_binary(Depth)].

convert_sub_type_from_binary(<<"items">>) -> items;
convert_sub_type_from_binary(<<"nodes">>) -> nodes.

convert_sub_type_to_binary(items) -> <<"items">>;
convert_sub_type_to_binary(nodes) -> <<"nodes">>.
