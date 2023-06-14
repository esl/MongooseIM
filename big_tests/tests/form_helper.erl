-module(form_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

%% Form creation

form(Spec) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_DATA_FORMS},
                    {<<"type">>, maps:get(type, Spec, <<"submit">>)}],
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
    Attrs = [{atom_to_binary(K), V} || {K, V} <- maps:to_list(M), K =/= values],
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
    Form#xmlel{attrs = proplists:delete(AttrName, Attrs)}.

remove_form_field(Form = #xmlel{children = Children}, FieldName) ->
    NewChildren = lists:filter(fun(#xmlel{name = <<"field">>, attrs = Attrs}) ->
                                       not lists:member({<<"var">>, FieldName}, Attrs);
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

is_form(#xmlel{name = <<"x">>} = El) ->
    exml_query:attr(El, <<"xmlns">>) =:= ?NS_DATA_FORMS;
is_form(_) ->
    false.
