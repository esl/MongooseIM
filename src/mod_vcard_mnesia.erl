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

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

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
            {error, mongoose_xmpp_errors:item_not_found()};
        {atomic, Rs} ->
            Els = lists:map(fun(R) ->
                                    R#vcard.vcard
                            end, Rs),
            {ok, Els};
        {aborted, Reason} ->
            ?ERROR_MSG("vCard lookup failed in process_sm_iq: ~p", [Reason]),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

set_vcard(User, VHost, VCard, VCardSearch) ->
    LUser = jid:nodeprep(User),
    F = fun() ->
                mnesia:write(#vcard{us ={LUser, VHost}, vcard = VCard}),
                mnesia:write(VCardSearch)
        end,
    {atomic, _} = mnesia:transaction(F),
    ejabberd_hooks:run(vcard_set, VHost, [LUser, VHost, VCard]),
    ok.

search(VHost, Data) ->
    MatchHead = make_matchhead(VHost, Data),
    R = do_search(VHost, MatchHead),
    lists:map(fun record_to_item/1, R).

do_search(_, #vcard_search{_ = '_'}) ->
    [];
do_search(VHost, MatchHeadIn) ->
    MatchHead = MatchHeadIn#vcard_search{us = {'_', VHost}},
    case catch mnesia:dirty_select(vcard_search,
        [{MatchHead, [], ['$_']}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]),
            [];
        Rs ->
            case mod_vcard:get_results_limit(VHost) of
                infinity ->
                    Rs;
                Val ->
                    lists:sublist(Rs, Val)
            end
    end.

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
            <<"user">> -> Match#vcard_search{luser = make_val(LVal)};
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
