-module(mod_vcard_mnesia).

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
    prepare_db(),
    ok.

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
            mnesia:delete({vcard, US}),
            mnesia:delete({vcard_search, US})
    end,
    mnesia:transaction(F).

get_vcard(LUser, LServer) ->
    US = {LUser, LServer},
    F  = fun() ->
                 mnesia:read({vcard, US})
         end,
    case mnesia:transaction(F) of
        {atomic, []} ->
            {error, ?ERR_SERVICE_UNAVAILABLE};
        {atomic, Rs} ->
            Els = lists:map(fun(R) ->
                                    R#vcard.vcard
                            end, Rs),
            {ok, Els};
        {aborted, Reason} ->
            ?ERROR_MSG("vCard lookup failed in process_sm_iq: ~p", [Reason]),
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

set_vcard(User, VHost, VCard, VCardSearch) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
                mnesia:write(#vcard{us ={LUser,VHost}, vcard = VCard}),
                mnesia:write(VCardSearch)
        end,
    {atomic, _} = mnesia:transaction(F),
    ejabberd_hooks:run(vcard_set, VHost,[LUser,VHost, VCard]),
    ok.

search(VHost, Data, _Lang, DefaultReportedFields) ->
    MatchHead = make_matchhead(VHost, Data),
    AllowReturnAll = gen_mod:get_module_opt(VHost, ?MODULE,
                                            allow_return_all, false),
    R=if
        (MatchHead == #vcard_search{_ = '_'}) and (not AllowReturnAll) ->
            [];
        true ->
            case catch mnesia:dirty_select(vcard_search,
                                           [{MatchHead, [], ['$_']}]) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("~p", [Reason]),
                    [];
                Rs ->
                    case gen_mod:get_module_opt(VHost, ?MODULE,
                                                matches, ?JUD_MATCHES) of
                        infinity ->
                            Rs;
                        Val when is_integer(Val) and (Val > 0) ->
                            lists:sublist(Rs, Val);
                        Val ->
                            ?ERROR_MSG("Illegal option value ~p. Default value ~p substituted.",
                                       [{matches, Val}, ?JUD_MATCHES]),
                            lists:sublist(Rs, ?JUD_MATCHES)
                    end
            end
    end,
    Items = lists:map(fun record_to_item/1,R),
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
prepare_db() ->
    create_tables(),
    update_tables(),
    set_indexes().

create_tables() ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
                                {attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, vcard_search)}]).

update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

set_indexes() ->
    mnesia:add_table_index(vcard_search, luser),
    mnesia:add_table_index(vcard_search, lfn),
    mnesia:add_table_index(vcard_search, lfamily),
    mnesia:add_table_index(vcard_search, lgiven),
    mnesia:add_table_index(vcard_search, lmiddle),
    mnesia:add_table_index(vcard_search, lnickname),
    mnesia:add_table_index(vcard_search, lbday),
    mnesia:add_table_index(vcard_search, lctry),
    mnesia:add_table_index(vcard_search, llocality),
    mnesia:add_table_index(vcard_search, lemail),
    mnesia:add_table_index(vcard_search, lorgname),
    mnesia:add_table_index(vcard_search, lorgunit).

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
        Fields ->
            ok;
        [user, vcard] ->
            ?INFO_MSG("Converting vcard table from "
                      "{user, vcard} format", []),
            Host = ?MYNAME,
            {atomic, ok} = mnesia:create_table(
                             mod_vcard_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, vcard},
                              {attributes, record_info(fields, vcard)}]),
            mnesia:transform_table(vcard, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_vcard_tmp_table),
                         mnesia:foldl(
                           fun(#vcard{us = U} = R, _) ->
                                   mnesia:dirty_write(
                                     mod_vcard_tmp_table,
                                     R#vcard{us = {U, Host}})
                           end, ok, vcard)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(vcard),
            F2 = fun() ->
                         mnesia:write_lock_table(vcard),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, mod_vcard_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_vcard_tmp_table);
        _ ->
            ?INFO_MSG("Recreating vcard table", []),
            mnesia:transform_table(vcard, ignore, Fields)
    end.


update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
        Fields ->
            ok;
        [user,     luser,
         fn,       lfn,
         family,   lfamily,
         given,    lgiven,
         middle,   lmiddle,
         nickname, lnickname,
         bday,	   lbday,
         ctry,	   lctry,
         locality, llocality,
         email,	   lemail,
         orgname,  lorgname,
         orgunit,  lorgunit] ->
            ?INFO_MSG("Converting vcard_search table from "
                      "{user, luser, fn, lfn, family, lfamily, given, lgiven, middle, lmiddle, nickname, lnickname, bday, lbday, ctry, lctry, locality, llocality, email, lemail, orgname, lorgname, orgunit, lorgunit} format", []),
            Host = ?MYNAME,
            {atomic, ok} = mnesia:create_table(
                             mod_vcard_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, vcard_search},
                              {attributes, record_info(fields, vcard_search)}]),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_vcard_tmp_table),
                         mnesia:foldl(
                           fun({vcard_search,
                                User,     LUser,     
                                FN,       LFN,       
                                Family,   LFamily,   
                                Given,    LGiven,    
                                Middle,   LMiddle,   
                                Nickname, LNickname, 
                                BDay,     LBDay,     
                                CTRY,     LCTRY,     
                                Locality, LLocality, 
                                EMail,    LEMail,    
                                OrgName,  LOrgName,  
                                OrgUnit,  LOrgUnit   
                               }, _) ->
                                   mnesia:dirty_write(
                                     mod_vcard_tmp_table,
                                     #vcard_search{
                                        us        = {LUser, Host},
                                        user      = {User, Host},
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
				      })
			   end, ok, vcard_search)
		 end,
	    mnesia:transaction(F1),
	    lists:foreach(fun(I) ->
				  mnesia:del_table_index(
				    vcard_search,
				    element(I, {vcard_search,
						user,     luser,
						fn,       lfn,
						family,   lfamily,
						given,    lgiven,
						middle,   lmiddle,
						nickname, lnickname,
						bday,	   lbday,
						ctry,	   lctry,
						locality, llocality,
						email,	   lemail,
						orgname,  lorgname,
						orgunit,  lorgunit}))
			  end, mnesia:table_info(vcard_search, index)),
	    mnesia:clear_table(vcard_search),
	    mnesia:transform_table(vcard_search, ignore, Fields),
	    F2 = fun() ->
	        	 mnesia:write_lock_table(vcard_search),
	        	 mnesia:foldl(
	        	   fun(R, _) ->
	        		   mnesia:dirty_write(R)
	        	   end, ok, mod_vcard_tmp_table)
	         end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard_search table", []),
	    mnesia:transform_table(vcard_search, ignore, Fields)
    end.

make_matchhead(VHost, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, VHost),
    Match.

filter_fields([], Match, _VHost) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, VHost)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = stringprep:tolower(Val),
    NewMatch =
        case SVar of
            <<"user">> ->
                case gen_mod:get_module_opt(VHost, ?MODULE,
                                            search_all_hosts, true) of
                    true ->
                        Match#vcard_search{luser = make_val(LVal)};
                    false ->
                        Host = find_my_host(VHost),
                        Match#vcard_search{us = {make_val(LVal), Host}}
                end;
            <<"fn">>       -> Match#vcard_search{lfn       = make_val(LVal)};
            <<"last">>     -> Match#vcard_search{lfamily   = make_val(LVal)};
            <<"first">>    -> Match#vcard_search{lgiven    = make_val(LVal)};
            <<"middle">>   -> Match#vcard_search{lmiddle   = make_val(LVal)};
            <<"nick">>     -> Match#vcard_search{lnickname = make_val(LVal)};
            <<"bday">>     -> Match#vcard_search{lbday     = make_val(LVal)};
            <<"ctry">>     -> Match#vcard_search{lctry     = make_val(LVal)};
            <<"locality">> -> Match#vcard_search{llocality = make_val(LVal)};
            <<"email">>    -> Match#vcard_search{lemail    = make_val(LVal)};
            <<"orgname">>  -> Match#vcard_search{lorgname  = make_val(LVal)};
            <<"orgunit">>  -> Match#vcard_search{lorgunit  = make_val(LVal)};
            _              -> Match
        end,
    filter_fields(Ds, NewMatch, VHost);
filter_fields([_ | Ds], Match, VHost) ->
    filter_fields(Ds, Match, VHost).

%% returns value as list to match substrings using match spec.
%% See vcard_search definition.
make_val(ValBin) ->
    Val = binary_to_list(ValBin),
    case lists:suffix("*", Val) of
    true ->
        lists:sublist(Val, length(Val) - 1) ++ '_';
    _ ->
        Val
    end.

find_my_host(VHost) ->
    Parts = binary:matches(VHost, <<".">>),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) ->
    ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_binstring(Parts),
    case lists:member(Domain, Hosts) of
    true ->
        Domain;
    false ->
        find_my_host(Tail, Hosts)
    end.

parts_to_binstring(Parts) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, $.] end, Parts)),
         right, $.).

record_to_item(R) ->
    {User, Server} = R#vcard_search.user,
    #xmlel{name = <<"item">>,
           children = [
                       ?FIELD(<<"jid">>, [User, <<"@">>, Server]),
                       ?FIELD(<<"fn">>, (R#vcard_search.fn)),
                       ?FIELD(<<"last">>, (R#vcard_search.family)),
                       ?FIELD(<<"first">>, (R#vcard_search.given)),
                       ?FIELD(<<"middle">>, (R#vcard_search.middle)),
                       ?FIELD(<<"nick">>, (R#vcard_search.nickname)),
                       ?FIELD(<<"bday">>, (R#vcard_search.bday)),
                       ?FIELD(<<"ctry">>, (R#vcard_search.ctry)),
                       ?FIELD(<<"locality">>, (R#vcard_search.locality)),
                       ?FIELD(<<"email">>, (R#vcard_search.email)),
                       ?FIELD(<<"orgname">>, (R#vcard_search.orgname)),
                       ?FIELD(<<"orgunit">>, (R#vcard_search.orgunit))
                      ]}.

