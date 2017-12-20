-record(vcard_search,{us,
                      user, luser,
                      fn, lfn,
                      family, lfamily,
                      given, lgiven,
                      middle, lmiddle,
                      nickname, lnickname,
                      bday, lbday,
                      ctry, lctry,
                      locality, llocality,
                      email, lemail,
                      orgname, lorgname,
                      orgunit, lorgunit}).
-record(vcard, {us, vcard}).

-define(JUD_MATCHES, 30).

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

-define(LFIELD(Label, Var),
	#xmlel{name = <<"field">>,
               attrs = [{<<"label">>, translate:translate(Lang, Label)},
                             {<<"var">>, Var}]}).

-define(FIELD(Var, Val),
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children = [#xmlel{name = <<"value">>,
	                          children = [#xmlcdata{content = Val}]}]}).

-define(FORM(JID, SearchFields,Lang),
        [#xmlel{name = <<"instructions">>, attrs = [],
                children =
                [{xmlcdata,
                  translate:translate(Lang,
                                      <<"You need an x:data capable client to "
                                        "search">>)}]},
         #xmlel{name = <<"x">>,
                attrs =
                [{<<"xmlns">>, ?NS_XDATA},
                 {<<"type">>, <<"form">>}],
                children =
                [#xmlel{name = <<"title">>, attrs = [],
                        children =
                        [{xmlcdata,
                          <<(translate:translate(Lang,
                                                 <<"Search users in ">>))/binary,
                            (jid:to_binary(JID))/binary>>}]},
                 #xmlel{name = <<"instructions">>, attrs = [],
                        children =
                        [{xmlcdata,
                          translate:translate(Lang,
                                              <<"Fill in fields to search for any matching "
                                                "Jabber User">>)}]}]
                ++
                lists:map(fun ({X, Y}) ->
                                  ?TLFIELD(<<"text-single">>, X, Y)
                          end,
                          SearchFields)}]).


