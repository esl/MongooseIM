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
-type vcard_search() :: #vcard_search{}.
-export_type([vcard_search/0]).

-define(TLFIELD(Type, Label, Var),
        #{var => Var, type => Type, label => translate:translate(Lang, Label)}).

-define(FIELD(Var, Val),
        #{var => Var, values => [Val]}).
