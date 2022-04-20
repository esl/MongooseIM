-module(mongoose_graphql_vcard_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-define(UNKNOWN_DOMAIN_RESULT, {unknown_domain, "Domain not found"}).

execute(_Ctx, vcard, <<"getVcard">>, #{<<"user">> := #jid{luser = _LUser,
                                                          lserver = _LServer} = _CallerJID}) ->
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
           <<"geo">> => [{ok, #{<<"lat">> => <<"latitude">>, <<"lon">> => <<"longtitude">>}}],
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
    }}.
    %case mongoose_domain_api:get_domain_host_type(LServer) of
    %    {ok, HostType} ->
    %        {ok, VCARD} = mod_vcard_backend:get_vcard(HostType, LUser, LServer),
    %        io:format("~p", [VCARD]),
    %        {ok, #{<<"formattedName">> => ,
    %               <<"nameComponents">>,
    %               <<"nickname">>,
    %               <<"photo">>,
    %               <<"birthday">>,
    %               <<"address">>,
    %               <<"label">>,
    %               <<"telephone">>,
    %               <<"email">>,
    %               <<"jabberId">>,
    %               <<"mailer">>,
    %               <<"timeZone">>,
    %               <<"geo">>,
    %               <<"title">>,
    %               <<"role">>,
    %               <<"logo">>,
    %               <<"agent">>,
    %               <<"org">>,
    %               <<"categories">>,
    %               <<"note">>,
    %               <<"prodId">>,
    %               <<"rev">>,
    %               <<"sortString">>,
    %               <<"phonetic">>,
    %               <<"uid">>,
    %               <<"url">>,
    %               <<"desc">>,
    %               <<"class">>,
    %               <<"key">>,
    %        }};
    %    Error ->
    %        ?UNKNOWN_DOMAIN_RESULT
    %end.