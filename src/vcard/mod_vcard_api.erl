%% @doc Provide an interface for frontends (like graphql or ctl) to manage vcard.
-module(mod_vcard_api).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

-type vcard_map() :: #{binary() => vcard_subelement_binary() | vcard_subelement_map()}.
-type vcard_subelement_binary() :: binary() | [{ok, binary()}].
-type vcard_subelement_map() :: #{binary() => binary() | [{ok, binary()}]}.

-export([set_vcard/2,
         get_vcard/1]).

-spec set_vcard(jid:jid(), vcard_map()) ->
     {ok, vcard_map()} | {user_not_found, string()} | {internal, string()} | {vcard_not_found, string()}.
set_vcard(#jid{luser = LUser, lserver = LServer} = UserJID, Vcard) ->
    case check_user(UserJID) of
        {ok, HostType} ->
            case set_vcard(HostType, UserJID, Vcard) of
                ok ->
                    get_vcard_from_db(HostType, LUser, LServer);
                _ ->
                    {internal, "Internal server error"}
            end;
        Error ->
            Error
    end.

-spec get_vcard(jid:jid()) ->
     {ok, vcard_map()} | {user_not_found, string()} | {internal, string()} | {vcard_not_found, string()}
   | {vcard_not_configured_error, string()}.
get_vcard(#jid{luser = LUser, lserver = LServer} = UserJID) ->
    % check if mod_vcard is loaded is needed in user's get_vcard command, when user variable is not passed
    case check_user(UserJID) of
        {ok, HostType} ->
            case gen_mod:is_loaded(HostType, mod_vcard) of
                true ->
                    get_vcard_from_db(HostType, LUser, LServer);
                false ->
                    {vcard_not_configured_error, "Mod_vcard is not loaded for this host"}
            end;
        Error ->
            Error
    end.

set_vcard(HostType, UserJID, Vcard) ->
    mod_vcard:unsafe_set_vcard(HostType, UserJID, transform_from_map(Vcard)).

get_vcard_from_db(HostType, LUser, LServer) ->
    ItemNotFoundError = mongoose_xmpp_errors:item_not_found(),
    case mod_vcard_backend:get_vcard(HostType, LUser, LServer) of
        {ok, Result} ->
            [#xmlel{children = VcardData}] = Result,
            {ok, to_map_format(VcardData)};
        {error, ItemNotFoundError} ->
            {vcard_not_found, "Vcard for user not found"};
        _ ->
            {internal, "Internal server error"}
    end.

-spec check_user(jid:jid()) -> {ok, mongooseim:host_type()} | {user_not_found, binary()}.
check_user(JID = #jid{lserver = LServer}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case ejabberd_auth:does_user_exist(HostType, JID, stored) of
                true -> {ok, HostType};
                false -> {user_not_found, <<"Given user does not exist">>}
            end;
        {error, not_found} ->
            {user_not_found, <<"User's domain does not exist">>}
    end.

transform_from_map(Vcard) ->
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
    construct_xmlel(from_map_to_xml(Name), process_child_map(Value));
transform_field_and_value(Name, Value) ->
    construct_xmlel(from_map_to_xml(Name), [{xmlcdata, Value}]).

transform_subfield_and_value(_Name, null) ->
    [];
transform_subfield_and_value(<<"vcard">>, Value) ->
    [transform_from_map(Value)];
transform_subfield_and_value(<<"tags">>, TagsList) ->
    lists:foldl(fun(Tag, Acc) ->
                    Acc ++ construct_xmlel(Tag, [])
                end, [], TagsList);
transform_subfield_and_value(Name, Value) when is_list(Value) ->
    lists:foldl(fun(Element, Acc) ->
                    Acc ++ construct_xmlel(from_map_to_xml(Name), [{xmlcdata, Element}])
                end, [], Value);
transform_subfield_and_value(Name, Value) ->
    construct_xmlel(from_map_to_xml(Name), [{xmlcdata, Value}]).

process_child_map(Value) ->
    lists:foldl(fun({Name, SubfieldValue}, Acc) ->
                    Acc ++ transform_subfield_and_value(Name, SubfieldValue)
                end, [], maps:to_list(Value)).

from_map_to_xml(<<"formattedName">>) -> <<"FN">>;
from_map_to_xml(<<"nameComponents">>) -> <<"N">>;
from_map_to_xml(<<"birthday">>) -> <<"BDAY">>;
from_map_to_xml(<<"address">>) -> <<"ADR">>;
from_map_to_xml(<<"telephone">>) -> <<"TEL">>;
from_map_to_xml(<<"timeZone">>) -> <<"TZ">>;
from_map_to_xml(<<"sortString">>) -> <<"SOR">>;
from_map_to_xml(<<"givenName">>) -> <<"GIVEN">>;
from_map_to_xml(<<"middleName">>) -> <<"MIDDLE">>;
from_map_to_xml(<<"credential">>) -> <<"CRED">>;
from_map_to_xml(<<"country">>) -> <<"CTRY">>;
from_map_to_xml(<<"binValue">>) -> <<"BINVAL">>;
from_map_to_xml(<<"extValue">>) -> <<"EXTVAL">>;
from_map_to_xml(Name) -> list_to_binary(string:to_upper(binary_to_list(Name))).

to_map_format(Vcard) ->
    lists:foldl(fun(#xmlel{name = Name, children = Value}, Acc) ->
        maps:merge(Acc, transform_from_xml(Name, Value, Acc))
    end, #{}, Vcard).

transform_from_xml(<<"FN">>, [{_, Value}], _) ->
    #{<<"formattedName">> => Value};
transform_from_xml(<<"N">>, Value, _) ->
    #{<<"nameComponents">> => lists:foldl(fun name_components_process/2, #{}, Value)};
transform_from_xml(<<"NICKNAME">>, Value, Acc) ->
    simple_process(<<"nickname">>, Value, Acc);
transform_from_xml(<<"PHOTO">>, Value, Acc) ->
    complex_process(<<"photo">>, Value, Acc, fun image_components_process/2);
transform_from_xml(<<"BDAY">>, Value, Acc) ->
    simple_process(<<"birthday">>, Value, Acc);
transform_from_xml(<<"ADR">>, Value, Acc) ->
    complex_process(<<"address">>, Value, Acc, fun address_components_process/2);
transform_from_xml(<<"LABEL">>, Value, Acc) ->
    complex_process(<<"label">>, Value, Acc, fun label_components_process/2);
transform_from_xml(<<"TEL">>, Value, Acc) ->
    complex_process(<<"telephone">>, Value, Acc, fun telephone_components_process/2);
transform_from_xml(<<"EMAIL">>, Value, Acc) ->
    complex_process(<<"email">>, Value, Acc, fun email_components_process/2);
transform_from_xml(<<"JABBERID">>, Value, Acc) ->
    simple_process(<<"jabberId">>, Value, Acc);
transform_from_xml(<<"MAILER">>, Value, Acc) ->
    simple_process(<<"mailer">>, Value, Acc);
transform_from_xml(<<"TZ">>, Value, Acc) ->
    simple_process(<<"timeZone">>, Value, Acc);
transform_from_xml(<<"GEO">>, Value, Acc) ->
    complex_process(<<"geo">>, Value, Acc, fun geo_components_process/2);
transform_from_xml(<<"TITLE">>, Value, Acc) ->
    simple_process(<<"title">>, Value, Acc);
transform_from_xml(<<"ROLE">>, Value, Acc) ->
    simple_process(<<"role">>, Value, Acc);
transform_from_xml(<<"LOGO">>, Value, Acc) ->
    complex_process(<<"logo">>, Value, Acc, fun image_components_process/2);
transform_from_xml(<<"AGENT">>, Value, Acc) ->
    complex_process(<<"agent">>, Value, Acc, fun agent_components_process/2);
transform_from_xml(<<"ORG">>, Value, Acc) ->
    complex_process(<<"org">>, Value, Acc, fun org_components_process/2);
transform_from_xml(<<"CATEGORIES">>, Value, Acc) ->
    complex_process(<<"categories">>, Value, Acc, fun categories_components_process/2);
transform_from_xml(<<"NOTE">>, Value, Acc) ->
    simple_process(<<"note">>, Value, Acc);
transform_from_xml(<<"PRODID">>, Value, Acc) ->
    simple_process(<<"prodId">>, Value, Acc);
transform_from_xml(<<"REV">>, Value, Acc) ->
    simple_process(<<"rev">>, Value, Acc);
transform_from_xml(<<"SOR">>, Value, Acc) ->
    simple_process(<<"sortString">>, Value, Acc);
transform_from_xml(<<"SOUND">>, Value, Acc) ->
    complex_process(<<"sound">>, Value, Acc, fun sound_components_process/2);
transform_from_xml(<<"UID">>, Value, Acc) ->
    simple_process(<<"uid">>, Value, Acc);
transform_from_xml(<<"URL">>, Value, Acc) ->
    simple_process(<<"url">>, Value, Acc);
transform_from_xml(<<"DESC">>, Value, Acc) ->
    simple_process(<<"desc">>, Value, Acc);
transform_from_xml(<<"CLASS">>, Value, Acc) ->
    complex_process(<<"class">>, Value, Acc, fun class_components_process/2);
transform_from_xml(<<"KEY">>, Value, Acc) ->
    complex_process(<<"key">>, Value, Acc, fun key_components_process/2);
transform_from_xml(_, _, _) ->
    #{}.

process_value([{_, Value}]) ->
    Value;
process_value(_) ->
    null.

simple_process(Name, [{_, Value}], Acc) ->
    List = maps:get(Name, Acc, []),
    #{Name => List ++ [{ok, Value}]};
simple_process(_, _, _) ->
    #{}.

complex_process(Name, Value, Acc, Fun) ->
    List = maps:get(Name, Acc, []),
    #{Name => List ++ [{ok, lists:foldl(fun(Element, Accumulator) ->
                                            Fun(Element, Accumulator)
                                        end, #{}, Value)}]}.

name_components_process(#xmlel{name = <<"FAMILY">>, children = Value}, Acc) ->
    maps:put(<<"family">>, process_value(Value), Acc);
name_components_process(#xmlel{name = <<"GIVEN">>, children = Value}, Acc) ->
    maps:put(<<"givenName">>, process_value(Value), Acc);
name_components_process(#xmlel{name = <<"MIDDLE">>, children = Value}, Acc) ->
    maps:put(<<"middleName">>, process_value(Value), Acc);
name_components_process(#xmlel{name = <<"PREFIX">>, children = Value}, Acc) ->
    maps:put(<<"prefix">>, process_value(Value), Acc);
name_components_process(#xmlel{name = <<"SUFFIX">>, children = Value}, Acc) ->
    maps:put(<<"suffix">>, process_value(Value), Acc).

address_components_process(#xmlel{name = <<"POBOX">>, children = Value}, Acc) ->
    maps:put(<<"pobox">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"EXTADD">>, children = Value}, Acc) ->
    maps:put(<<"extadd">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"STREET">>, children = Value}, Acc) ->
    maps:put(<<"street">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"LOCALITY">>, children = Value}, Acc) ->
    maps:put(<<"locality">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"REGION">>, children = Value}, Acc) ->
    maps:put(<<"region">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"PCODE">>, children = Value}, Acc) ->
    maps:put(<<"pcode">>, process_value(Value), Acc);
address_components_process(#xmlel{name = <<"CTRY">>, children = Value}, Acc) ->
    maps:put(<<"country">>, process_value(Value), Acc);
address_components_process(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

label_components_process(#xmlel{name = <<"LINE">>, children = Value}, Acc) ->
    List = maps:get(<<"line">>, Acc, []),
    maps:merge(Acc, #{<<"line">> => List ++ [{ok, process_value(Value)}]});
label_components_process(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

telephone_components_process(#xmlel{name = <<"NUMBER">>, children = Value}, Acc) ->
    maps:put(<<"number">>, process_value(Value), Acc);
telephone_components_process(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

email_components_process(#xmlel{name = <<"USERID">>, children = Value}, Acc) ->
    maps:put(<<"userId">>, process_value(Value), Acc);
email_components_process(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

geo_components_process(#xmlel{name = <<"LAT">>, children = Value}, Acc) ->
    maps:put(<<"lat">>, process_value(Value), Acc);
geo_components_process(#xmlel{name = <<"LON">>, children = Value}, Acc) ->
    maps:put(<<"lon">>, process_value(Value), Acc).

org_components_process(#xmlel{name = <<"ORGNAME">>, children = Value}, Acc) ->
    maps:put(<<"orgname">>, process_value(Value), Acc);
org_components_process(#xmlel{name = <<"ORGUNIT">>, children = Value}, Acc) ->
    List = maps:get(<<"orgunit">>, Acc, []),
    maps:merge(Acc, #{<<"orgunit">> => List ++ [{ok, process_value(Value)}]}).

categories_components_process(#xmlel{name = <<"KEYWORD">>, children = Value}, Acc) ->
    List = maps:get(<<"keyword">>, Acc, []),
    maps:merge(Acc, #{<<"keyword">> => List ++ [{ok, process_value(Value)}]}).

key_components_process(#xmlel{name = <<"CRED">>, children = Value}, Acc) ->
    maps:put(<<"credential">>, process_value(Value), Acc);
key_components_process(#xmlel{name = <<"TYPE">>, children = Value}, Acc) ->
    maps:put(<<"type">>, process_value(Value), Acc).

class_components_process(#xmlel{name = Name, children = []}, Acc) ->
    List = maps:get(<<"tags">>, Acc, []),
    maps:merge(Acc, #{<<"tags">> => List ++ [{ok, Name}]}).

image_components_process(#xmlel{name = <<"TYPE">>, children = Value}, Acc) ->
    maps:put(<<"type">>, process_value(Value), Acc);
image_components_process(#xmlel{name = <<"BINVAL">>, children = Value}, Acc) ->
    maps:put(<<"binValue">>, process_value(Value), Acc);
image_components_process(#xmlel{name = <<"EXTVAL">>, children = Value}, Acc) ->
    maps:put(<<"extValue">>, process_value(Value), Acc).

sound_components_process(#xmlel{name = <<"PHONETIC">>, children = Value}, Acc) ->
    maps:put(<<"phonetic">>, process_value(Value), Acc);
sound_components_process(#xmlel{name = <<"BINVAL">>, children = Value}, Acc) ->
    maps:put(<<"binValue">>, process_value(Value), Acc);
sound_components_process(#xmlel{name = <<"EXTVAL">>, children = Value}, Acc) ->
    maps:put(<<"extValue">>, process_value(Value), Acc).

agent_components_process(#xmlel{name = <<"vCard">>, children = Value}, Acc) ->
    maps:put(<<"vcard">>, to_map_format(Value), Acc);
agent_components_process(#xmlel{name = <<"EXTVAL">>, children = Value}, Acc) ->
    maps:put(<<"extValue">>, process_value(Value), Acc).
