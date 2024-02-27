-module(mongoose_data_forms).
-xep([{xep, 4}, {version, "2.13.1"}]).
-xep([{xep, 68}, {version, "1.3.0"}]).

%% Form processing
-export([find_and_parse_form/1, find_form/1, find_form/2,
         parse_form/1, parse_form_fields/1,
         is_form/1, is_form/2]).

%% Form construction
-export([form/1]).

-include_lib("exml/include/exml.hrl").
-include("mongoose_ns.hrl").

-type form() :: #{type => binary(), title => binary(), instructions => binary(), ns => binary(),
                  fields => [field()], reported => [field()], items => [[field()]]}.
-type field() :: #{var => binary(), type => binary(), label => binary(),
                   values => [binary()], options => [option()], validate => validate()}.
-type option() :: binary() | {binary(), binary()}.
-type validate() :: #{method => atom(), datatype => binary()}.

-type parsed_form() :: #{type => binary(), ns => binary(), kvs := kv_map()}.
-type kv_map() :: #{binary() => [binary()]}.

-export_type([form/0, field/0, option/0, validate/0, kv_map/0]).

-ignore_xref([is_form/1]). % exported for consistency, might be used later

%% Form processing

%% @doc Find a form in subelements, and then parse its fields
-spec find_and_parse_form(exml:element()) -> parsed_form() | {error, binary()}.
find_and_parse_form(Parent) ->
    case find_form(Parent) of
        undefined ->
            {error, <<"Form not found">>};
        Form ->
            parse_form_fields(Form)
    end.

-spec find_form(exml:element()) -> exml:element() | undefined.
find_form(Parent) ->
    exml_query:subelement_with_name_and_ns(Parent, <<"x">>, ?NS_XDATA).

-spec find_form(exml:element(), Default) -> exml:element() | Default.
find_form(Parent, Default) ->
    exml_query:subelement_with_name_and_ns(Parent, <<"x">>, ?NS_XDATA, Default).

%% @doc Check if the element is a form, and then parse its fields
-spec parse_form(exml:element()) -> parsed_form() | {error, binary()}.
parse_form(Elem) ->
    case is_form(Elem) of
        true ->
            parse_form_fields(Elem);
        false ->
            {error, <<"Invalid form element">>}
    end.

%% @doc Parse the form fields without checking that it is a form element
-spec parse_form_fields(exml:element()) -> parsed_form().
parse_form_fields(Elem) ->
    M = case form_type(Elem) of
            undefined -> #{};
            Type -> #{type => Type}
        end,
    KVs = form_fields_to_kvs(Elem#xmlel.children),
    case maps:take(<<"FORM_TYPE">>, KVs) of
        {[NS], FKVs} ->
            M#{ns => NS, kvs => FKVs};
        _ ->
            % Either zero or more than one value of FORM_TYPE.
            % According to XEP-0004 the form is still valid.
            M#{kvs => KVs}
    end.

-spec is_form(exml:element()) -> boolean().
is_form(#xmlel{name = Name} = Elem) ->
    Name =:= <<"x">> andalso exml_query:attr(Elem, <<"xmlns">>) =:= ?NS_XDATA.

-spec is_form(exml:element(), [binary()]) -> boolean().
is_form(Elem, Types) ->
    is_form(Elem) andalso lists:member(form_type(Elem), Types).

-spec form_type(exml:element()) -> binary() | undefined.
form_type(Form) ->
    exml_query:attr(Form, <<"type">>).

-spec form_fields_to_kvs([exml:element()]) -> kv_map().
form_fields_to_kvs(Fields) ->
    maps:from_list(lists:flatmap(fun form_field_to_kv/1, Fields)).

form_field_to_kv(FieldEl = #xmlel{name = <<"field">>}) ->
    case exml_query:attr(FieldEl, <<"var">>) of
        undefined -> [];
        Var -> [{Var, exml_query:paths(FieldEl, [{element, <<"value">>}, cdata])}]
    end;
form_field_to_kv(_) ->
    [].

%% Form construction

-spec form(form()) -> exml:element().
form(Spec) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, maps:get(type, Spec, <<"form">>)}],
           children = lists:flatmap(fun(Item) -> form_children(Item, Spec) end,
                                    [title, instructions, ns, fields, reported, items])
          }.

form_children(title, #{title := Title}) ->
    [form_title(Title)];
form_children(instructions, #{instructions := Instructions}) ->
    [form_instructions(Instructions)];
form_children(ns, #{ns := NS}) ->
    [form_type_field(NS)];
form_children(fields, #{fields := Fields}) ->
    [form_field(Field) || Field <- Fields];
form_children(reported, #{reported := ReportedFields}) ->
    [reported_element([form_field(Field) || Field <- ReportedFields])];
form_children(items, #{items := Items}) ->
    [item_element([form_field(Field) || Field <- ItemFields]) || ItemFields <- Items];
form_children(_, #{}) ->
    [].

-spec form_type_field(binary()) -> exml:element().
form_type_field(NS) when is_binary(NS) ->
    form_field(#{var => <<"FORM_TYPE">>, type => <<"hidden">>, values => [NS]}).

-spec form_field(field()) -> exml:element().
form_field(M) when is_map(M) ->
    Validate = form_field_validate(maps:get(validate, M, [])),
    Values = [form_field_value(Value) || Value <- maps:get(values, M, [])],
    Options = [form_field_option(Option) || Option <- maps:get(options, M, [])],
    Attrs = [{atom_to_binary(K), V}
             || {K, V} <- maps:to_list(M), K =/= values, K =/= options, K =/= validate],
    #xmlel{name = <<"field">>, attrs = Attrs, children = Values ++ Options ++ Validate}.

-spec form_title(binary()) -> exml:element().
form_title(Title) ->
    #xmlel{name = <<"title">>, attrs = [], children = [{xmlcdata, Title}]}.

-spec form_instructions(binary()) -> exml:element().
form_instructions(Instructions) ->
    #xmlel{name = <<"instructions">>, attrs = [], children = [{xmlcdata, Instructions}]}.

-spec reported_element([exml:element()]) -> exml:element().
reported_element(Fields) ->
    #xmlel{name = <<"reported">>, attrs = [], children = Fields}.

-spec item_element([exml:element()]) -> exml:element().
item_element(Fields) ->
    #xmlel{name = <<"item">>, attrs = [], children = Fields}.

-spec form_field_option(option()) -> exml:element().
form_field_option({Label, Value}) ->
    #xmlel{name = <<"option">>,
           attrs = [{<<"label">>, Label}],
           children = [form_field_value(Value)]};
form_field_option(Option) ->
    form_field_option({Option, Option}).

-spec form_field_value(binary()) -> exml:element().
form_field_value(Value) ->
    #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}.

-spec form_field_validate(validate()) -> [exml:element()].
form_field_validate(#{method := Method, datatype := Datatype}) ->
    [#xmlel{name = <<"validate">>,
            attrs = [{<<"xmlns">>, ?NS_DATA_VALIDATE}, {<<"datatype">>, Datatype}],
            children = form_field_validation_method(Method)}];
form_field_validate(_) -> [].

-spec form_field_validation_method(atom()) -> [exml:element()].
form_field_validation_method(open) ->
    [#xmlel{name = <<"open">>}].
