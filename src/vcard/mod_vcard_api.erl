%methods used in vcard graphQL API
-module(mod_vcard_api).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

-export([transform_from_graphql/1,
         set_vcard/4,
         to_gql_format/1,
         get_vcard/3,
         name_components_proccess/2,
         address_components_proccess/2,
         label_components_proccess/2,
         telephone_components_proccess/2,
         email_components_proccess/2,
         geo_components_proccess/2,
         org_components_proccess/2
        ]).

set_vcard(HostType, LUser, LServer, Vcard) ->
    mod_vcard:unsafe_set_vcard(HostType, #jid{user = LUser, lserver = LServer}, transform_from_graphql(Vcard)).

get_vcard(HostType, LUser, LServer) ->
    try mod_vcard_backend:get_vcard(HostType, LUser, LServer) of
        {ok, VCARD} ->
               [#xmlel{children = VcardData}] = VCARD,
               to_gql_format(VcardData);
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
    io:format("~p", [maps:to_list(Value)]),
    lists:foldl(fun({Name, SubfieldValue}, Acc) ->
                    Acc ++ transform_subfield_and_value(Name, SubfieldValue)
                end, [], maps:to_list(Value)).

from_gql_to_xml(<<"formattedName">>) -> <<"FN">>;
from_gql_to_xml(<<"nameComponents">>) -> <<"N">>;
from_gql_to_xml(<<"nickname">>) -> <<"NICKNAME">>;
from_gql_to_xml(<<"photo">>) -> <<"PHOTO">>;
from_gql_to_xml(<<"birthday">>) -> <<"BDAY">>;
from_gql_to_xml(<<"address">>) -> <<"ADR">>;
from_gql_to_xml(<<"label">>) -> <<"LABEL">>;
from_gql_to_xml(<<"telephone">>) -> <<"TEL">>;
from_gql_to_xml(<<"email">>) -> <<"EMAIL">>;
from_gql_to_xml(<<"jabberId">>) -> <<"JABBERID">>;
from_gql_to_xml(<<"mailer">>) -> <<"MAILER">>;
from_gql_to_xml(<<"timeZone">>) -> <<"TZ">>;
from_gql_to_xml(<<"geo">>) -> <<"GEO">>;
from_gql_to_xml(<<"title">>) -> <<"TITLE">>;
from_gql_to_xml(<<"role">>) -> <<"ROLE">>;
from_gql_to_xml(<<"logo">>) -> <<"LOGO">>;
from_gql_to_xml(<<"agent">>) -> <<"AGENT">>;
from_gql_to_xml(<<"org">>) -> <<"ORG">>;
from_gql_to_xml(<<"categories">>) -> <<"CATEGORIES">>;
from_gql_to_xml(<<"note">>) -> <<"NOTE">>;
from_gql_to_xml(<<"prodId">>) -> <<"PRODID">>;
from_gql_to_xml(<<"rev">>) -> <<"REV">>;
from_gql_to_xml(<<"sortString">>) -> <<"SOR">>;
from_gql_to_xml(<<"sound">>) -> <<"SOUND">>;
from_gql_to_xml(<<"uid">>) -> <<"UID">>;
from_gql_to_xml(<<"url">>) -> <<"URL">>;
from_gql_to_xml(<<"desc">>) -> <<"DESC">>;
from_gql_to_xml(<<"class">>) -> <<"CLASS">>;
from_gql_to_xml(<<"key">>) -> <<"KEY">>;
from_gql_to_xml(<<"lat">>) -> <<"LAT">>;
from_gql_to_xml(<<"lon">>) -> <<"LON">>;
from_gql_to_xml(<<"orgname">>) -> <<"ORGNAME">>;
from_gql_to_xml(<<"locality">>) -> <<"LOCALITY">>;
from_gql_to_xml(<<"orgunit">>) -> <<"ORGUNIT">>;
from_gql_to_xml(<<"givenName">>) -> <<"GIVEN">>;
from_gql_to_xml(<<"middleName">>) -> <<"MIDDLE">>;
from_gql_to_xml(<<"family">>) -> <<"FAMILY">>;
from_gql_to_xml(<<"prefix">>) -> <<"PREFIX">>;
from_gql_to_xml(<<"sufix">>) -> <<"SUFIX">>;
from_gql_to_xml(<<"pobox">>) -> <<"POBOX">>;
from_gql_to_xml(<<"extadd">>) -> <<"EXTADD">>;
from_gql_to_xml(<<"street">>) -> <<"STREET">>;
from_gql_to_xml(<<"region">>) -> <<"REGION">>;
from_gql_to_xml(<<"pcode">>) -> <<"PCODE">>;
from_gql_to_xml(<<"number">>) -> <<"NUMBER">>;
from_gql_to_xml(<<"line">>) -> <<"LINE">>;
from_gql_to_xml(<<"userId">>) -> <<"USERID">>;
from_gql_to_xml(<<"credential">>) -> <<"CRED">>;
from_gql_to_xml(<<"keyword">>) -> <<"KEYWORD">>;
from_gql_to_xml(<<"country">>) -> <<"CTRY">>.

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
    proccess_simple(<<"timezone">>, Value, Acc);
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
    proccess_complex(<<"geo">>, Value, Acc, fun org_components_proccess/2);
transform_from_xml(<<"CATEGORIES">>, Value, Acc) ->
    proccess_complex(<<"categories">>, Value, Acc, fun categories_components_proccess/2);
transform_from_xml(<<"NOTE">>, Value, Acc) ->
    proccess_simple(<<"note">>, Value, Acc);
transform_from_xml(<<"PRODID">>, Value, Acc) ->
    proccess_simple(<<"prodid">>, Value, Acc);
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
    Value; proccess_value(_) ->
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
    maps:merge(Acc, #{<<"family">> => proccess_value(Value)});
name_components_proccess(#xmlel{name = <<"GIVEN">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"givenName">> => proccess_value(Value)});
name_components_proccess(#xmlel{name = <<"MIDDLE">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"middleName">> => proccess_value(Value)});
name_components_proccess(#xmlel{name = <<"PREFIX">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"prefix">> => proccess_value(Value)});
name_components_proccess(#xmlel{name = <<"SUFIX">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"sufix">> => proccess_value(Value)}).

address_components_proccess(#xmlel{name = <<"POBOX">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"pobox">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"EXTADD">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"extadd">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"STREET">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"street">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"LOCALITY">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"locality">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"REGION">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"region">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"PCODE">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"pcode">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = <<"CTRY">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"country">> => proccess_value(Value)});
address_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

label_components_proccess(#xmlel{name = <<"LINE">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"line">> => proccess_value(Value)});
label_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

telephone_components_proccess(#xmlel{name = <<"NUMBER">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"number">> => proccess_value(Value)});
telephone_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

email_components_proccess(#xmlel{name = <<"USERID">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"userId">> => proccess_value(Value)});
email_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

geo_components_proccess(#xmlel{name = <<"LAT">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"lat">> => proccess_value(Value)});
geo_components_proccess(#xmlel{name = <<"LON">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"lon">> => proccess_value(Value)}).

org_components_proccess(#xmlel{name = <<"ORGNAME">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"orgname">> => proccess_value(Value)});
org_components_proccess(#xmlel{name = <<"ORGUNIT">>, children = Value}, Acc) ->
    List = maps:get(<<"orgunit">>, Acc, []),
    maps:merge(Acc, #{<<"orgunit">> => List ++ [{ok, proccess_value(Value)}]}).

categories_components_proccess(#xmlel{name = <<"KEYWORD">>, children = Value}, Acc) ->
    List = maps:get(<<"keyword">>, Acc, []),
    maps:merge(Acc, #{<<"keyword">> => List ++ [{ok, proccess_value(Value)}]}).

key_components_proccess(#xmlel{name = <<"CRED">>, children = Value}, Acc) ->
    maps:merge(Acc, #{<<"credential">> => proccess_value(Value)}).

class_components_proccess(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).
