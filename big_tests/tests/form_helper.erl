-module(form_helper).

-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

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
