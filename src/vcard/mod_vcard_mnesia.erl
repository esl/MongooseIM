-module(mod_vcard_mnesia).

-behaviour(mod_vcard_backend).

%% mod_vcards callbacks
-export([init/2,
         remove_user/3,
         get_vcard/3,
         set_vcard/5,
         search/3,
         search_fields/2,
         search_reported_fields/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

init(_HostType, _Options) ->
    prepare_db(),
    ok.

remove_user(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
            mnesia:delete({vcard, US}),
            mnesia:delete({vcard_search, US})
    end,
    mnesia:transaction(F).

get_vcard(_HostType, LUser, LServer) ->
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
            ?LOG_ERROR(#{what => process_sm_iq_vcard_lookup_failed,
                         reason => Reason, user => LUser, server => LServer}),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

set_vcard(HostType, User, LServer, VCard, VCardSearch) ->
    LUser = jid:nodeprep(User),
    VCardSearch2 = stringify_search_fields(VCardSearch),
    F = fun() ->
                mnesia:write(#vcard{us ={LUser, LServer}, vcard = VCard}),
                mnesia:write(VCardSearch2)
        end,
    {atomic, _} = mnesia:transaction(F),
    mongoose_hooks:vcard_set(HostType, LServer, LUser, VCard),
    ok.

-spec search(mongooseim:host_type(), jid:lserver(), term()) -> [[mongoose_data_forms:field()]].
search(HostType, LServer, Data) ->
    MatchHead = make_matchhead(LServer, Data),
    R = do_search(HostType, LServer, MatchHead),
    lists:map(fun record_to_item/1, R).

do_search(_, _, #vcard_search{_ = '_'}) ->
    [];
do_search(HostType, LServer, MatchHeadIn) ->
    MatchHead = MatchHeadIn#vcard_search{us = {'_', LServer}},
    case catch mnesia:dirty_select(vcard_search,
        [{MatchHead, [], ['$_']}]) of
        {'EXIT', Reason} ->
            ?LOG_ERROR(#{what => vcard_search_failed, server => LServer, host_type => HostType,
                         reason => Reason}),
            [];
        Rs ->
            case mod_vcard:get_results_limit(HostType) of
                infinity ->
                    Rs;
                Val ->
                    lists:sublist(Rs, Val)
            end
    end.

search_fields(_HostType, _LServer) ->
    mod_vcard:default_search_fields().

-spec search_reported_fields(mongooseim:host_type(), jid:lserver(), ejabberd:lang()) ->
          [mongoose_data_forms:field()].
search_reported_fields(_HostType, _LServer, Lang) ->
    mod_vcard:get_default_reported_fields(Lang).

%%--------------------------------------------------------------------
%% internal
%%--------------------------------------------------------------------

prepare_db() ->
    create_tables(),
    set_indexes(),
    add_table_copies().

create_tables() ->
    mongoose_mnesia:create_table(vcard,
        [{disc_only_copies, [node()]},
         {attributes, record_info(fields, vcard)}]),
    mongoose_mnesia:create_table(vcard_search,
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

make_matchhead(LServer, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer),
    Match.

filter_fields([], Match, _LServer) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = jid:str_tolower(Val),
    NewMatch =
        case SVar of
            <<"user">>     -> Match#vcard_search{luser     = make_val(LVal)};
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
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

%% Fulltext search is mnesia is something that is really-really wrong
stringify_search_fields(#vcard_search{} = S) ->
    S#vcard_search{
      lfn = binary_to_list(S#vcard_search.lfn),
      lfamily = binary_to_list(S#vcard_search.lfamily),
      luser = binary_to_list(S#vcard_search.luser),
      lgiven = binary_to_list(S#vcard_search.lgiven),
      lmiddle = binary_to_list(S#vcard_search.lmiddle),
      lnickname = binary_to_list(S#vcard_search.lnickname),
      lbday = binary_to_list(S#vcard_search.lbday),
      lctry = binary_to_list(S#vcard_search.lctry),
      llocality = binary_to_list(S#vcard_search.llocality),
      lemail = binary_to_list(S#vcard_search.lemail),
      lorgname = binary_to_list(S#vcard_search.lorgname),
      lorgunit = binary_to_list(S#vcard_search.lorgunit)
     }.

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
    [
     ?FIELD(<<"jid">>, <<User/binary, $@, Server/binary>>),
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
    ].
