%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : vCard support via ODBC
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_vcard_odbc).

-behaviour(mod_vcard).

%% mod_vcards callbacks
-export([init/2,remove_user/2, get_vcard/2, set_vcard/4, search/4, search_fields/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

%%--------------------------------------------------------------------
%% mod_vcards callbacks
%%--------------------------------------------------------------------

init(_VHost, _Options) ->
    ok.

remove_user(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_transaction(
      LServer,
      [["delete from vcard where username='", Username, "';"],
       ["delete from vcard_search where lusername='", Username, "';"]]).

get_vcard(LUser, LServer) ->
    U = ejabberd_odbc:escape(LUser),
    S = ejabberd_odbc:escape(LServer),
    case odbc_queries:get_vcard(S,U) of
        {selected, [<<"vcard">>], [{SVCARD}]} ->
            case xml_stream:parse_element(SVCARD) of
                {error, Reason} ->
                    ?WARNING_MSG("not sending bad vcard xml ~p~n~p",[Reason,SVCARD]),
                    {error, ?ERR_SERVICE_UNAVAILABLE};
                VCARD ->
                    {ok, [VCARD]}
            end;
        {selected, [<<"vcard">>],[]} ->
            {error, ?ERR_SERVICE_UNAVAILABLE}
    end.

set_vcard(User, VHost, VCard, VCardSearch) ->
    LUser = jlib:nodeprep(User),
    Username = ejabberd_odbc:escape(User),
    LUsername = ejabberd_odbc:escape(LUser),
    LServer = ejabberd_odbc:escape(VHost),
    SVCARD = ejabberd_odbc:escape(
               xml:element_to_binary(VCard)),

    SFN = ejabberd_odbc:escape(VCardSearch#vcard_search.fn),
    SLFN = ejabberd_odbc:escape(VCardSearch#vcard_search.lfn),
    SFamily = ejabberd_odbc:escape(VCardSearch#vcard_search.family),
    SLFamily = ejabberd_odbc:escape(VCardSearch#vcard_search.lfamily),
    SGiven = ejabberd_odbc:escape(VCardSearch#vcard_search.given),
    SLGiven = ejabberd_odbc:escape(VCardSearch#vcard_search.lgiven),
    SMiddle = ejabberd_odbc:escape(VCardSearch#vcard_search.middle),
    SLMiddle = ejabberd_odbc:escape(VCardSearch#vcard_search.lmiddle),
    SNickname = ejabberd_odbc:escape(VCardSearch#vcard_search.nickname),
    SLNickname = ejabberd_odbc:escape(VCardSearch#vcard_search.lnickname),
    SBDay = ejabberd_odbc:escape(VCardSearch#vcard_search.bday),
    SLBDay = ejabberd_odbc:escape(VCardSearch#vcard_search.lbday),
    SCTRY = ejabberd_odbc:escape(VCardSearch#vcard_search.ctry),
    SLCTRY = ejabberd_odbc:escape(VCardSearch#vcard_search.lctry),
    SLocality = ejabberd_odbc:escape(VCardSearch#vcard_search.locality),
    SLLocality = ejabberd_odbc:escape(VCardSearch#vcard_search.llocality),
    SEMail = ejabberd_odbc:escape(VCardSearch#vcard_search.email),
    SLEMail = ejabberd_odbc:escape(VCardSearch#vcard_search.lemail),
    SOrgName = ejabberd_odbc:escape(VCardSearch#vcard_search.orgname),
    SLOrgName = ejabberd_odbc:escape(VCardSearch#vcard_search.lorgname),
    SOrgUnit = ejabberd_odbc:escape(VCardSearch#vcard_search.orgunit),
    SLOrgUnit = ejabberd_odbc:escape(VCardSearch#vcard_search.lorgunit),

    odbc_queries:set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail,
                           SFN, SFamily, SGiven, SLBDay, SLCTRY,
                           SLEMail, SLFN, SLFamily, SLGiven,
                           SLLocality, SLMiddle, SLNickname,
                           SLOrgName, SLOrgUnit, SLocality,
                           SMiddle, SNickname, SOrgName,
                           SOrgUnit, SVCARD, Username),

    ejabberd_hooks:run(vcard_set, VHost, [LUser, VHost, VCard]),
    ok.

search(LServer, Data, _Lang, DefaultReportedFields) ->
    RestrictionSQL = make_restriction_sql(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE,
					    allow_return_all, false),
    R=if
	(RestrictionSQL == "") and (not AllowReturnAll) ->
	    [];
	true ->
	    Limit = case gen_mod:get_module_opt(LServer, ?MODULE,
						matches, ?JUD_MATCHES) of
			infinity ->
			    "";
			Val when is_integer(Val) and (Val > 0) ->
			    [" LIMIT ", integer_to_list(Val)];
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    [" LIMIT ", integer_to_list(?JUD_MATCHES)]
		    end,
	    case catch ejabberd_odbc:sql_query(
			 LServer,
			 ["select username, server, fn, family, given, middle, "
			  "       nickname, bday, ctry, locality, "
			  "       email, orgname, orgunit from vcard_search ",
			  RestrictionSQL, Limit, ";"]) of
		{selected, [<<"username">>, <<"server">>, <<"fn">>, <<"family">>, <<"given">>,
			    <<"middle">>, <<"nickname">>, <<"bday">>, <<"ctry">>, <<"locality">>,
			    <<"email">>, <<"orgname">>, <<"orgunit">>], Rs} when is_list(Rs) ->
		    Rs;
		Error ->
		    ?ERROR_MSG("~p", [Error]),
		    []
	    end
    end,
    Items = lists:map(fun(I) -> record_to_item(LServer,I) end, R),
    [DefaultReportedFields | Items].

search_fields(_VHost) ->
    [{<<"User">>, <<"user">>},
	 {<<"Full Name">>, <<"fn">>},
	 {<<"Given Name">>, <<"first">>},
	 {<<"Middle Name">>, <<"middle">>},
	 {<<"Family Name">>, <<"last">>},
	 {<<"Nickname">>, <<"nick">>},
	 {<<"Birthday">>, <<"bday">>},
	 {<<"Country">>, <<"ctry">>},
     {<<"City">>, <<"locality">>},
	 {<<"Email">>, <<"email">>},
	 {<<"Organization Name">>, <<"orgname">>},
	 {<<"Organization Unit">>, <<"orgunit">>}].

%%--------------------------------------------------------------------
%% internal
%%--------------------------------------------------------------------
make_restriction_sql(LServer, Data) ->
    filter_fields(Data, "", LServer).

filter_fields([], RestrictionSQL, _LServer) ->
    case RestrictionSQL of
	"" ->
	    "";
        <<>> ->
            <<>>;
	_ ->
	    [" where ", RestrictionSQL]
    end;
filter_fields([{SVar, [Val]} | Ds], RestrictionSQL, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = stringprep:tolower(Val),
    NewRestrictionSQL =
        case SVar of
            <<"user">>     -> make_val(RestrictionSQL, "lusername", LVal);
            <<"fn">>       -> make_val(RestrictionSQL, "lfn",       LVal);
            <<"last">>     -> make_val(RestrictionSQL, "lfamily",   LVal);
            <<"first">>    -> make_val(RestrictionSQL, "lgiven",    LVal);
            <<"middle">>   -> make_val(RestrictionSQL, "lmiddle",   LVal);
            <<"nick">>     -> make_val(RestrictionSQL, "lnickname", LVal);
            <<"bday">>     -> make_val(RestrictionSQL, "lbday",     LVal);
            <<"ctry">>     -> make_val(RestrictionSQL, "lctry",     LVal);
            <<"locality">> -> make_val(RestrictionSQL, "llocality", LVal);
            <<"email">>    -> make_val(RestrictionSQL, "lemail",    LVal);
            <<"orgname">>  -> make_val(RestrictionSQL, "lorgname",  LVal);
            <<"orgunit">>  -> make_val(RestrictionSQL, "lorgunit",  LVal);
            _              -> RestrictionSQL
        end,
    filter_fields(Ds, NewRestrictionSQL, LServer);
filter_fields([_ | Ds], RestrictionSQL, LServer) ->
    filter_fields(Ds,RestrictionSQL , LServer).

-spec make_val(RestrictionSQL, Field, Val) -> Result when
    RestrictionSQL :: iolist(),
    Field :: string(),
    Val :: binary(),
    Result :: iolist().
make_val(RestrictionSQL, Field, Val) ->
    Condition =
	case binary:last(Val) of
	    $* ->
		Val1 = binary:part(Val, 0, byte_size(Val)-1),
		SVal = ejabberd_odbc:escape_like(Val1),
		[Field, " LIKE '", SVal, "%'"];
	    _ ->
		SVal = ejabberd_odbc:escape(Val),
		[Field, " = '", SVal, "'"]
	end,
    case RestrictionSQL of
	"" ->
	    Condition;
	_ ->
	    [RestrictionSQL, " and ", Condition]
    end.

record_to_item(_CallerVHost, {Username, VCardVHost, FN, Family, Given, Middle,
             Nickname, BDay, CTRY, Locality,
             EMail, OrgName, OrgUnit}) ->
    #xmlel{name = "item",
           children = [
                        ?FIELD("jid", [Username, "@", VCardVHost]),
                        ?FIELD("fn", FN),
                        ?FIELD("last", Family),
                        ?FIELD("first", Given),
                        ?FIELD("middle", Middle),
                        ?FIELD("nick", Nickname),
                        ?FIELD("bday", BDay),
                        ?FIELD("ctry", CTRY),
                        ?FIELD("locality", Locality),
                        ?FIELD("email", EMail),
                        ?FIELD("orgname", OrgName),
                        ?FIELD("orgunit", OrgUnit)
                       ]}.


