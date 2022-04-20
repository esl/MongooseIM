-module(mongoose_graphql_vcard_admin_mutation).
-behaviour(mongoose_graphql).

-include("mod_vcard.hrl").

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-define(UNKNOWN_DOMAIN_RESULT, {unknown_domain, "Domain not found"}).

execute(_Ctx, vcard, <<"setVcard">>,
        #{<<"user">> := #jid{luser = LUser, lserver = LServer} = _CallerJID,
          <<"vcard">> := Vcard}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            ok = unsafe_set_vcard(HostType, LUser, LServer, transform(Vcard)),
            {ok, #{<<"formattedName">> => <<"TestName">>,
                   <<"nameComponents">> => #{<<"family">> => <<"TestFamily">>,
                                             <<"givenName">> => <<"TestGiven">>,
                                             <<"middleName">> => <<"Test middle">>,
                                             <<"prefix">> => <<"Test prefix">>,
                                             <<"sufix">> => <<"Test sufix">>},
                   <<"nickname">> => [{ok, <<"Test nickname">>}],
                   <<"photo">> => [{ok, <<"TestPhoto">>}],
                   <<"birthday">> => [{ok, <<"TestBirthday">>}],
                   <<"address">> => [{ok, #{<<"tags">> => [],
                                            <<"pobox">> => <<"TestPobox">>,
                                            <<"extadd">> => <<"Test Extadd">>,
                                            <<"street">> => <<"TestStreet">>,
                                            <<"locality">> => <<"testLocality">>,
                                            <<"region">> => <<"TestRegion">>,
                                            <<"pcode">> => <<"TestPcode">>,
                                            <<"country">> => <<"TestCountry">>}}],
                   <<"label">> => [{ok, #{<<"tags">> => [], <<"line">> => []}}],
                   <<"telephone">> => [{ok, #{<<"tags">> => [], <<"number">> => "123456789"}}],
                   <<"email">> => [{ok, #{<<"tags">> => [], <<"userId">> => <<"UserId">>}}],
                   <<"jabberId">> => [{ok, <<"TestJabberId">>}],
                   <<"mailer">> => [{ok, <<"TestMailer">>}],
                   <<"timeZone">> => [{ok, <<"TestTimezone">>}],
                   <<"geo">> => [{ok, #{<<"lat">> => <<"latitude">>,
                                        <<"lon">> => <<"longtitude">>}}],
                   <<"title">> => [{ok, <<"testTitle">>}],
                   <<"role">> => [{ok, <<"testRole">>}],
                   <<"logo">> => [{ok, <<"testLogo">>}],
                   <<"agent">> => [{ok, <<"testAgent">>}],
                   <<"org">> => [{ok, #{<<"orgname">> => <<"testOrgname">>,
                                        <<"orgunit">> => [{ok, <<"testOrgunit">>}]}}],
                   <<"categories">> => [{ok, <<"testCategories">>}],
                   <<"note">> => [{ok, <<"testNote">>}],
                   <<"prodId">> => [{ok, <<"testProdId">>}],
                   <<"rev">> => [{ok, <<"testRev">>}],
                   <<"sortString">> => [{ok, <<"testSortString">>}],
                   <<"phonetic">> => [{ok, <<"testPhonetic">>}],
                   <<"uid">> => [{ok, <<"testUid">>}],
                   <<"url">> => [{ok, <<"testUrl">>}],
                   <<"desc">> => [{ok, <<"testDesc">>}],
                   <<"class">> => [{ok, <<"testClass">>}],
                   <<"key">> => [{ok, <<"testKey">>}]
            }}
       end.

transform(VCARD) ->
    #xmlel{name = <<"vCard">>,
    attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
    children = [#xmlel{name = <<"TITLE">>,
                       attrs = [],
                       children = [{xmlcdata, <<"title">>}]}]}.

unsafe_set_vcard(HostType, LUser, LServer, VCARD) ->
    case parse_vcard(LUser, LServer, VCARD) of
        {ok, VcardSearch} ->
            mod_vcard_backend:set_vcard(HostType, LUser, LServer, VCARD, VcardSearch);
        {error, Reason} ->
            {error, Reason}
    end.

parse_vcard(User, VHost, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"FAMILY">>}, cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"GIVEN">>}, cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, <<"NICKNAME">>}, cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, <<"BDAY">>}, cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"LOCALITY">>}, cdata]),
    EMail1   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>},
                                      {elem, <<"USERID">>}, cdata]),
    EMail2   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>}, cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGUNIT">>}, cdata]),
    EMail = case EMail1 of
                <<"">> -> EMail2;
                _ -> EMail1
            end,
    try
        LUser     = jid:nodeprep(User),
        LFN       = prepare_index(<<"FN">>, FN),
        LFamily   = prepare_index(<<"FAMILY">>, Family),
        LGiven    = prepare_index(<<"GIVEN">>, Given),
        LMiddle   = prepare_index(<<"MIDDLE">>, Middle),
        LNickname = prepare_index_allow_emoji(<<"NICKNAME">>, Nickname),
        LBDay     = prepare_index(<<"BDAY">>, BDay),
        LCTRY     = prepare_index(<<"CTRY">>, CTRY),
        LLocality = prepare_index(<<"LOCALITY">>, Locality),
        LEMail    = prepare_index(<<"EMAIL">>, EMail),
        LOrgName  = prepare_index(<<"ORGNAME">>, OrgName),
        LOrgUnit  = prepare_index(<<"ORGUNIT">>, OrgUnit),

        US = {LUser, VHost},

        {ok, #vcard_search{us        = US,
                           user      = {User, VHost},
                           luser     = LUser,
                           fn        = FN,       lfn        = LFN,
                           family    = Family,   lfamily    = LFamily,
                           given     = Given,    lgiven     = LGiven,
                           middle    = Middle,   lmiddle    = LMiddle,
                           nickname  = Nickname, lnickname  = LNickname,
                           bday      = BDay,     lbday      = LBDay,
                           ctry      = CTRY,     lctry      = LCTRY,
                           locality  = Locality, llocality  = LLocality,
                           email     = EMail,    lemail     = LEMail,
                           orgname   = OrgName,  lorgname   = LOrgName,
                           orgunit   = OrgUnit,  lorgunit   = LOrgUnit
                          }}
    catch
        throw:{invalid_input, Info} ->
            {error, {invalid_input, Info}}
    end.

prepare_index(FieldName, Value) ->
    case jid:str_tolower(Value) of
        error ->
            throw({invalid_input, {FieldName, Value}});
        LValue ->
            LValue
    end.

prepare_index_allow_emoji(FieldName, Value) ->
    {ok, Re} = re:compile(<<"[^[:alnum:][:space:][:punct:]]">>, [unicode, ucp]),
    Sanitized = re:replace(Value, Re, <<"">>, [global]),
    prepare_index(FieldName, Sanitized).
