%methods used in vcard graphQL API
-module(mod_vcard_api).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

-export([transform_from_graphql/1,
         set_vcard/2,
         to_gql_format/1,
         get_vcard/1,
         name_components_proccess/2,
         address_components_proccess/2,
         label_components_proccess/2,
         telephone_components_proccess/2,
         email_components_proccess/2,
         geo_components_proccess/2,
         org_components_proccess/2
        ]).

-ignore_xref([name_components_proccess/2,
              address_components_proccess/2,
              label_components_proccess/2,
              telephone_components_proccess/2,
              email_components_proccess/2,
              geo_components_proccess/2,
              transform_from_graphql/1,
              to_gql_format/1,
              org_components_proccess/2]).

set_vcard(#jid{luser = LUser, lserver = LServer} = UserJID, Vcard) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case set_vcard(HostType, UserJID, Vcard) of
                ok ->
                    case get_vcard(HostType, LUser, LServer) of
                        {ok, _} = Result ->
                            Result;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

get_vcard(#jid{luser = LUser, lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case get_vcard(HostType, LUser, LServer) of
                {ok, _} = Vcard ->
                    Vcard;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

set_vcard(HostType, UserJID, Vcard) ->
    mod_vcard:unsafe_set_vcard(HostType, UserJID, transform_from_graphql(Vcard)).

get_vcard(HostType, LUser, LServer) ->
    try mod_vcard_backend:get_vcard(HostType, LUser, LServer) of
        {ok, VCARD} ->
               [#xmlel{children = VcardData}] = VCARD,
               {ok, to_gql_format(VcardData)};
        {error, Reason} ->
               {error, Reason}
        catch E:R:Stack ->
            ?LOG_ERROR(#{what => vcard_sm_iq_get_failed,
                         class => E, reason => R, stacktrace => Stack}),
            {error, error}
        end.

transform_from_graphql(Vcard) ->
    #xmlel{name = <<"vCard">>,
           attrs = [{<<"xmlns">>, <<"vcard-temp">>}],
           children = lists:foldl(fun({Name, Value}, Acc) ->
                                      Acc ++ transform_field_and_value(Name, Value)
                                  end, [], maps:to_list(Vcard))}.

construct_xmlel(Name, Children) when is_list(Children)->
    [#xmlel{name = Name,
            attrs = [],
            children = Children}].

transform_field_and_value(_Name, null) ->
    [];
transform_field_and_value(Name, Value) when is_list(Value) ->
    lists:foldl(fun(Element, Acc) ->
                    Acc ++ transform_field_and_value(Name, Element)
                end, [], Value);
transform_field_and_value(Name, Value) when is_map(Value) ->
    construct_xmlel(from_gql_to_xml(Name), proccess_child_map(Value));
transform_field_and_value(Name, Value) ->
    construct_xmlel(from_gql_to_xml(Name), [{xmlcdata, Value}]).

transform_subfield_and_value(_Name, null) ->
    [];
transform_subfield_and_value(<<"tags">>, TagsList) ->
    lists:foldl(fun(Tag, Acc) ->
                    Acc ++ construct_xmlel(Tag, [])
                end, [], TagsList);
transform_subfield_and_value(Name, Value) when is_list(Value) ->
    lists:foldl(fun(Element, Acc) ->
                    Acc ++ construct_xmlel(from_gql_to_xml(Name), [{xmlcdata, Element}])
                end, [], Value);
transform_subfield_and_value(Name, Value) ->
    construct_xmlel(from_gql_to_xml(Name), [{xmlcdata, Value}]).

proccess_child_map(Value) ->
    lists:foldl(fun({Name, SubfieldValue}, Acc) ->
                    Acc ++ transform_subfield_and_value(Name, SubfieldValue)
                end, [], maps:to_list(Value)).

from_gql_to_xml(<<"formattedName">>) -> <<"FN">>;
from_gql_to_xml(<<"nameComponents">>) -> <<"N">>;
from_gql_to_xml(<<"birthday">>) -> <<"BDAY">>;
from_gql_to_xml(<<"address">>) -> <<"ADR">>;
from_gql_to_xml(<<"telephone">>) -> <<"TEL">>;
from_gql_to_xml(<<"timeZone">>) -> <<"TZ">>;
from_gql_to_xml(<<"sortString">>) -> <<"SOR">>;
from_gql_to_xml(<<"givenName">>) -> <<"GIVEN">>;
from_gql_to_xml(<<"middleName">>) -> <<"MIDDLE">>;
from_gql_to_xml(<<"credential">>) -> <<"CRED">>;
from_gql_to_xml(<<"country">>) -> <<"CTRY">>;
from_gql_to_xml(Name) -> list_to_binary(string:to_upper(binary_to_list(Name))).

to_gql_format(Vcard) ->
    lists:foldl(fun(#xmlel{name = Name, children = Value}, Acc) ->
        maps:merge(Acc, transform_from_xml(Name, Value, Acc))
    end, #{}, Vcard).

transform_from_xml(<<"FN">>, [{_, Value}], _) ->
    #{<<"formattedName">> => Value};
transform_from_xml(<<"N">>, Value, _) ->
    #{<<"nameComponents">> => lists:foldl(fun name_components_proccess/2, #{}, Value)};
transform_from_xml(<<"NICKNAME">>, Value, Acc) ->
    proccess_simple(<<"nickname">>, Value, Acc);
transform_from_xml(<<"PHOTO">>, Value, Acc) ->
    proccess_simple(<<"photo">>, Value, Acc);
transform_from_xml(<<"BDAY">>, Value, Acc) ->
    proccess_simple(<<"birthday">>, Value, Acc);
transform_from_xml(<<"ADR">>, Value, Acc) ->
    proccess_complex(<<"address">>, Value, Acc, fun address_components_proccess/2);
transform_from_xml(<<"LABEL">>, Value, Acc) ->
    proccess_complex(<<"label">>, Value, Acc, fun label_components_proccess/2);
transform_from_xml(<<"TEL">>, Value, Acc) ->
    proccess_complex(<<"telephone">>, Value, Acc, fun telephone_components_proccess/2);
transform_from_xml(<<"EMAIL">>, Value, Acc) ->
    proccess_complex(<<"email">>, Value, Acc, fun email_components_proccess/2);
transform_from_xml(<<"JABBERID">>, Value, Acc) ->
    proccess_simple(<<"jabberId">>, Value, Acc);
transform_from_xml(<<"MAILER">>, Value, Acc) ->
    proccess_simple(<<"mailer">>, Value, Acc);
transform_from_xml(<<"TZ">>, Value, Acc) ->
    proccess_simple(<<"timeZone">>, Value, Acc);
transform_from_xml(<<"GEO">>, Value, Acc) ->
    proccess_complex(<<"geo">>, Value, Acc, fun geo_components_proccess/2);
transform_from_xml(<<"TITLE">>, Value, Acc) ->
    proccess_simple(<<"title">>, Value, Acc);
transform_from_xml(<<"ROLE">>, Value, Acc) ->
    proccess_simple(<<"role">>, Value, Acc);
transform_from_xml(<<"LOGO">>, Value, Acc) ->
    proccess_simple(<<"logo">>, Value, Acc);
transform_from_xml(<<"AGENT">>, Value, Acc) ->
    proccess_simple(<<"agent">>, Value, Acc);
transform_from_xml(<<"ORG">>, Value, Acc) ->
    proccess_complex(<<"org">>, Value, Acc, fun org_components_proccess/2);
transform_from_xml(<<"CATEGORIES">>, Value, Acc) ->
    proccess_complex(<<"categories">>, Value, Acc, fun categories_components_proccess/2);
transform_from_xml(<<"NOTE">>, Value, Acc) ->
    proccess_simple(<<"note">>, Value, Acc);
transform_from_xml(<<"PRODID">>, Value, Acc) ->
    proccess_simple(<<"prodId">>, Value, Acc);
transform_from_xml(<<"REV">>, Value, Acc) ->
    proccess_simple(<<"rev">>, Value, Acc);
transform_from_xml(<<"SOR">>, Value, Acc) ->
    proccess_simple(<<"sortString">>, Value, Acc);
transform_from_xml(<<"SOUND">>, Value, Acc) ->
    proccess_simple(<<"sound">>, Value, Acc);
transform_from_xml(<<"UID">>, Value, Acc) ->
    proccess_simple(<<"uid">>, Value, Acc);
transform_from_xml(<<"URL">>, Value, Acc) ->
    proccess_simple(<<"url">>, Value, Acc);
transform_from_xml(<<"DESC">>, Value, Acc) ->
    proccess_simple(<<"desc">>, Value, Acc);
transform_from_xml(<<"CLASS">>, Value, Acc) ->
    proccess_complex(<<"class">>, Value, Acc, fun class_components_proccess/2);
transform_from_xml(<<"KEY">>, Value, Acc) ->
    proccess_complex(<<"key">>, Value, Acc, fun key_components_proccess/2).

proccess_value([{_, Value}]) ->
    Value;
proccess_value(_) ->
    null.

proccess_simple(Name, [{_, Value}], Acc) ->
    List = maps:get(Name, Acc, []),
    #{Name => List ++ [{ok, Value}]};
proccess_simple(_, _, _) ->
    #{}.

proccess_complex(Name, Value, Acc, Fun) ->
    List = maps:get(Name, Acc, []),
    #{Name => List ++ [{ok, lists:foldl(fun(Element, Accumulator) ->
                                            Fun(Element, Accumulator)
                                        end, #{}, Value)}]}.

name_components_proccess(#xmlel{name = <<"FAMILY">>, children = Value}, Acc) ->
    maps:put(<<"family">>, proccess_value(Value), Acc);
name_components_proccess(#xmlel{name = <<"GIVEN">>, children = Value}, Acc) ->
    maps:put(<<"givenName">>, proccess_value(Value), Acc);
name_components_proccess(#xmlel{name = <<"MIDDLE">>, children = Value}, Acc) ->
    maps:put(<<"middleName">>, proccess_value(Value), Acc);
name_components_proccess(#xmlel{name = <<"PREFIX">>, children = Value}, Acc) ->
    maps:put(<<"prefix">>, proccess_value(Value), Acc);
name_components_proccess(#xmlel{name = <<"SUFIX">>, children = Value}, Acc) ->
    maps:put(<<"sufix">>, proccess_value(Value), Acc).

address_components_proccess(#xmlel{name = <<"POBOX">>, children = Value}, Acc) ->
    maps:put(<<"pobox">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"EXTADD">>, children = Value}, Acc) ->
    maps:put(<<"extadd">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"STREET">>, children = Value}, Acc) ->
    maps:put(<<"street">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"LOCALITY">>, children = Value}, Acc) ->
    maps:put(<<"locality">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"REGION">>, children = Value}, Acc) ->
    maps:put(<<"region">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"PCODE">>, children = Value}, Acc) ->
    maps:put(<<"pcode">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = <<"CTRY">>, children = Value}, Acc) ->
    maps:put(<<"country">>, proccess_value(Value), Acc);
address_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

label_components_proccess(#xmlel{name = <<"LINE">>, children = Value}, Acc) ->
    List = maps:get(<<"line">>, Acc, []),
    maps:merge(Acc, #{<<"line">> => List ++ [{ok, proccess_value(Value)}]});
label_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

telephone_components_proccess(#xmlel{name = <<"NUMBER">>, children = Value}, Acc) ->
    maps:put(<<"number">>, proccess_value(Value), Acc);
telephone_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

email_components_proccess(#xmlel{name = <<"USERID">>, children = Value}, Acc) ->
    maps:put(<<"userId">>, proccess_value(Value), Acc);
email_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

geo_components_proccess(#xmlel{name = <<"LAT">>, children = Value}, Acc) ->
    maps:put(<<"lat">>, proccess_value(Value), Acc);
geo_components_proccess(#xmlel{name = <<"LON">>, children = Value}, Acc) ->
    maps:put(<<"lon">>, proccess_value(Value), Acc).

org_components_proccess(#xmlel{name = <<"ORGNAME">>, children = Value}, Acc) ->
    maps:put(<<"orgname">>, proccess_value(Value), Acc);
org_components_proccess(#xmlel{name = <<"ORGUNIT">>, children = Value}, Acc) ->
    List = maps:get(<<"orgunit">>, Acc, []),
    maps:merge(Acc, #{<<"orgunit">> => List ++ [{ok, proccess_value(Value)}]}).

categories_components_proccess(#xmlel{name = <<"KEYWORD">>, children = Value}, Acc) ->
    List = maps:get(<<"keyword">>, Acc, []),
    maps:merge(Acc, #{<<"keyword">> => List ++ [{ok, proccess_value(Value)}]}).

key_components_proccess(#xmlel{name = <<"CRED">>, children = Value}, Acc) ->
    maps:put(<<"credential">>, proccess_value(Value), Acc).

class_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).
