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
            io:format("~p", [mod_vcard_api:transform_from_graphql(Vcard)]),
            mod_vcard_api:set_vcard(HostType, LUser, LServer, Vcard),
            VcardData = mod_vcard_api:get_vcard(HostType, LUser, LServer),
            io:format("\n---------------------------------------------\n"),
            io:format("~p", [VcardData]),
            {ok, mod_vcard_api:get_vcard(HostType, LUser, LServer)}
            %{ok, #{<<"formattedName">> => <<"TestName">>,
            %       <<"nameComponents">> => #{<<"family">> => <<"TestFamily">>,
            %                                 <<"givenName">> => <<"TestGiven">>,
            %                                 <<"middleName">> => <<"Test middle">>,
            %                                 <<"prefix">> => <<"Test prefix">>,
            %                                 <<"sufix">> => <<"Test sufix">>},
            %       <<"nickname">> => [{ok, <<"Test nickname">>}],
            %       <<"birthday">> => [{ok, <<"TestBirthday">>}],
            %       <<"address">> => [{ok, #{<<"tags">> => [<<"HOME">>],
            %                                <<"pobox">> => <<"TestPobox">>,
            %                                <<"extadd">> => <<"Test Extadd">>,
            %                                <<"street">> => <<"TestStreet">>,
            %                                <<"locality">> => <<"testLocality">>,
            %                                <<"region">> => <<"TestRegion">>,
            %                                <<"pcode">> => <<"TestPcode">>,
            %                                <<"country">> => <<"TestCountry">>}}],
            %       <<"label">> => [{ok, #{<<"tags">> => [], <<"line">> => []}}],
            %       <<"telephone">> => [{ok, #{<<"tags">> => [], <<"number">> => "123456789"}}],
            %       <<"email">> => [{ok, #{<<"tags">> => [], <<"userId">> => <<"UserId">>}}],
            %       <<"jabberId">> => [{ok, <<"TestJabberId">>}],
            %       <<"mailer">> => [{ok, <<"TestMailer">>}],
            %       <<"timeZone">> => [{ok, <<"TestTimezone">>}],
            %       <<"geo">> => [{ok, #{<<"lat">> => <<"latitude">>,
            %                            <<"lon">> => <<"longtitude">>}}],
            %       <<"title">> => [{ok, <<"testTitle">>}],
            %       <<"role">> => [{ok, <<"testRole">>}],
            %       <<"logo">> => [{ok, <<"testLogo">>}],
            %       <<"agent">> => [{ok, <<"testAgent">>}],
            %       <<"org">> => [{ok, #{<<"orgname">> => <<"testOrgname">>,
            %                            <<"orgunit">> => [{ok, <<"testOrgunit">>}]}}],
            %       <<"categories">> => [{ok, #{<<"keyword">> => [{ok, <<"testCategories">>}]}}],
            %       <<"note">> => [{ok, <<"testNote">>}],
            %       <<"prodId">> => [{ok, <<"testProdId">>}],
            %       <<"rev">> => [{ok, <<"testRev">>}],
            %       <<"sortString">> => [{ok, <<"testSortString">>}],
            %       <<"sound">> => [{ok, <<"testPhonetic">>}],
            %       <<"uid">> => [{ok, <<"testUid">>}],
            %       <<"url">> => [{ok, <<"testUrl">>}],
            %       <<"desc">> => [{ok, <<"testDesc">>}],
            %       <<"class">> => [{ok, #{<<"tags">> => [<<"PRIVATE">>]}}],
            %       <<"key">> => [{ok, #{<<"credential">> => <<"testKey">>}}]
            %}}
       end.
