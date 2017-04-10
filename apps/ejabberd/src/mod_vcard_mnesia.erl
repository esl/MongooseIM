-module(mod_vcard_mnesia).

-behaviour(mod_vcard).

%% mod_vcards callbacks
-export([init/2,
         remove_user/2,
         get_vcard/2,
         set_vcard/4,
         search/2,
         search_fields/1,
         search_reported_fields/2]).

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
        %% TODO: the first clause to be out of place here - see
        %% http://xmpp.org/extensions/xep-0054.html examples 2, 3, 4:
        %%
        %%   If no vCard exists, the server MUST return a stanza error
        %%   (which SHOULD be <item-not-found/>) or an IQ-result containing
        %%   an empty <vCard/> element.
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
    LUser = jid:nodeprep(User),
    US = {LUser, VHost},
    Activated = is_vcard_activated(VCard),
    F = fun() ->
                mnesia:write(#vcard{us ={LUser, VHost}, vcard = VCard}),
                if
                    Activated ->
                        mnesia:write(VCardSearch);
                    true ->
                        mnesia:delete({vcard_search, US})
                end
        end,
    {atomic, _} = mnesia:transaction(F),
    ejabberd_hooks:run(vcard_set, VHost, [LUser, VHost, VCard]),
    ok.

is_vcard_activated(VCard) ->
    case exml_query:path(VCard, [{element, <<"DESC">>}, cdata]) of
        undefined ->
            false;
        Desc ->
            case (catch
                      lists:keyfind(<<"chatEnabled">>, 1,
                                    jsx:decode(Desc))) of
                {<<"chatEnabled">>, true} ->
                    true;
                Other ->
                    ?WARNING_MSG("VCard \"DESC\" JSON decoded to: ~p~n", [Other]),
                    false
            end
    end.

search(VHost, Data) ->
    MatchHead = make_matchhead(VHost, Data),
    R = do_search(VHost, MatchHead),
    lists:map(fun record_to_item/1, R).

do_search(VHost, MatchHeadTuple) ->
    MatchHead = tl(tuple_to_list(MatchHeadTuple)),
    {atomic, Rs} =
        mnesia:transaction(
          fun() ->
                  mnesia:foldl(
                    fun(RecTuple, AccIn) ->
                            Rec = tl(tuple_to_list(RecTuple)),
                            case match_compare(MatchHead, Rec) of
                                true ->
                                    [RecTuple | AccIn];
                                false ->
                                    AccIn
                            end
                    end, [], vcard_search)
          end),

    case mod_vcard:get_results_limit(VHost) of
        infinity ->
            Rs;
        Val ->
            lists:sublist(Rs, Val)
    end.

match_compare([], []) ->
    %% Everything matched
    true;
match_compare(['_' | MatchHead], [_ | Rec]) ->
    %% Ignore this field, continue
    match_compare(MatchHead, Rec);
match_compare([Pattern | MatchHead], [Field | Rec]) ->
    BinField = field_to_binary(Field),
    case binary:match(BinField, Pattern) of
        nomatch ->
            %% Mismatch, bail out
            false;
        {_, _} ->
            %% Match, continue
            match_compare(MatchHead, Rec)
    end.

field_to_binary(Field) when is_binary(Field) ->
    Field;

field_to_binary(Field) when is_list(Field) ->
    list_to_binary(Field);

field_to_binary({User, Domain}) when is_binary(User), is_binary(Domain) ->
    <<User/binary, "@", Domain/binary>>;
field_to_binary({User, Domain}) when is_list(User) ->
    field_to_binary({list_to_binary(User), Domain});
field_to_binary({User, Domain}) when is_list(Domain) ->
    field_to_binary({User, list_to_binary(Domain)});

field_to_binary(_) ->
    <<"">>.


search_fields(_VHost) ->
    mod_vcard:default_search_fields().

search_reported_fields(_VHost, Lang) ->
    mod_vcard:get_default_reported_fields(Lang).

%%--------------------------------------------------------------------
%% internal
%%--------------------------------------------------------------------

prepare_db() ->
    create_tables(),
    set_indexes(),
    add_table_copies().

create_tables() ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
                                {attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, vcard_search)}]).

add_table_copies() ->
    mnesia:add_table_copy(vcard, node(), disc_only_copies),
    mnesia:add_table_copy(vcard_search, node(), disc_copies).

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
         bday,     lbday,
         ctry,     lctry,
         locality, llocality,
         email,    lemail,
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
                                                bday,     lbday,
                                                ctry,     lctry,
                                                locality, llocality,
                                                email,    lemail,
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

%% Produces a Match that is no longer suitable for mnesia:dirty_select/2
make_matchhead(VHost, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, VHost),
    Match.

filter_fields([], Match, _VHost) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, VHost)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = binary:compile_pattern(stringprep:tolower(Val)),
    NewMatch =
        case SVar of
            <<"user">> -> Match#vcard_search{luser = LVal};
            <<"fn">>       -> Match#vcard_search{lfn       = LVal};
            <<"last">>     -> Match#vcard_search{lfamily   = LVal};
            <<"first">>    -> Match#vcard_search{lgiven    = LVal};
            <<"middle">>   -> Match#vcard_search{lmiddle   = LVal};
            <<"nick">>     -> Match#vcard_search{lnickname = LVal};
            <<"bday">>     -> Match#vcard_search{lbday     = LVal};
            <<"ctry">>     -> Match#vcard_search{lctry     = LVal};
            <<"locality">> -> Match#vcard_search{llocality = LVal};
            <<"email">>    -> Match#vcard_search{lemail    = LVal};
            <<"orgname">>  -> Match#vcard_search{lorgname  = LVal};
            <<"orgunit">>  -> Match#vcard_search{lorgunit  = LVal};
            _              -> Match
        end,
    filter_fields(Ds, NewMatch, VHost);
filter_fields([_ | Ds], Match, VHost) ->
    filter_fields(Ds, Match, VHost).

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
