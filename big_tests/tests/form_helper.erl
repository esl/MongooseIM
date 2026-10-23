-module(form_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-type parsed_form() :: #{type => binary(), ns => binary(), kvs := kv_map()}.
-type kv_map() :: #{binary() => [binary()]}.

%% Form creation

form(Spec) ->
    #xmlel{name = <<"x">>,
           attrs = #{<<"xmlns">> => ?NS_DATA_FORMS,
                     <<"type">> => maps:get(type, Spec, <<"submit">>)},
           children = lists:flatmap(fun(Item) -> form_children(Item, Spec) end, [ns, fields])
          }.

form_children(ns, #{ns := NS}) ->
    [form_type_field(NS)];
form_children(fields, #{fields := Fields}) ->
    [form_field(Field) || Field <- Fields];
form_children(_, #{}) ->
    [].

form_type_field(NS) when is_binary(NS) ->
    form_field(#{var => <<"FORM_TYPE">>, type => <<"hidden">>, values => [NS]}).

form_field(M) when is_map(M) ->
    Values = [form_field_value(Value) || Value <- maps:get(values, M, [])],
    Attrs = #{atom_to_binary(K) => V || K := V <- M, K =/= values},
    #xmlel{name = <<"field">>, attrs = Attrs, children = Values}.

form_field_value(Value) ->
    #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}.

%% Form manipulation

remove_forms(El) ->
    modify_forms(El, fun(_) -> [] end).

remove_form_types(El) ->
    modify_forms(El, fun(Form) -> [remove_form_attr(Form, <<"type">>)] end).

remove_form_ns(El) ->
    modify_forms(El, fun(Form) -> [remove_form_attr(Form, <<"xmlns">>)] end).

remove_fields(El, Name) ->
    modify_forms(El, fun(Form) -> [remove_form_field(Form, Name)] end).

remove_form_attr(Form = #xmlel{attrs = Attrs}, AttrName) ->
    Form#xmlel{attrs = maps:remove(AttrName, Attrs)}.

remove_form_field(Form = #xmlel{children = Children}, FieldName) ->
    NewChildren = lists:filter(fun(#xmlel{attrs = #{<<"var">> := Name},
                                          name = <<"field">>}) ->
                                      FieldName =/= Name;
                                  (_) ->
                                      true
                               end, Children),
    Form#xmlel{children = NewChildren}.

%% Apply ModifyF to all nested data form elements
modify_forms(El = #xmlel{children = Children}, ModifyF) ->
    El#xmlel{children = lists:flatmap(fun(Child) ->
                                              case is_form(Child) of
                                                  true -> ModifyF(Child);
                                                  false -> [modify_forms(Child, ModifyF)]
                                              end
                                      end, Children)};
modify_forms(El, _ModifyF) ->
    El.

%% Form processing (copied from mongoose_data_forms)

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
    exml_query:subelement_with_name_and_ns(Parent, <<"x">>, ?NS_DATA_FORMS).

-spec find_form(exml:element(), Default) -> exml:element() | Default.
find_form(Parent, Default) ->
    exml_query:subelement_with_name_and_ns(Parent, <<"x">>, ?NS_DATA_FORMS, Default).

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
    Name =:= <<"x">> andalso exml_query:attr(Elem, <<"xmlns">>) =:= ?NS_DATA_FORMS.

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
