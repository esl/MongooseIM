%%------------------------------------------------------------------
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc ElasticSearch backend for Message Archive Management for
%% one-to-one messages.
%%%------------------------------------------------------------------
-module(mod_mam_elasticsearch_arch).

-behaviour(gen_mod).
-behaviour(ejabberd_gen_mam_archive).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2]).
-export([stop/1]).

%% ejabberd_gen_mam_archive callbacks
-export([archive_message/9]).
-export([lookup_messages/3]).
-export([remove_archive/4]).
-export([archive_size/4]).

-export([get_mam_pm_gdpr_data/2]).

-include("mongoose.hrl").
-include("mongoose_rsm.hrl").
-include("mod_mam.hrl").
-include("jlib.hrl").

-define(INDEX_NAME, <<"messages">>).
-define(TYPE_NAME, <<"pm">>).

%%-------------------------------------------------------------------
%% gen_mod callbacks
%%-------------------------------------------------------------------

-spec start(jid:server(), list()) -> ok.
start(Host, _Opts) ->
    ejabberd_hooks:add(hooks(Host)),
    ok.

-spec stop(jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    ok.

-spec get_mam_pm_gdpr_data(ejabberd_gen_mam_archive:mam_pm_gdpr_data(), jid:jid()) ->
    ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(Acc, Owner) ->
    BinOwner = mod_mam_utils:bare_jid(Owner),
    Filter = #{term => #{owner => BinOwner}},
    Sorting = #{mam_id => #{order => asc}},
    SearchQuery = #{query => #{bool => #{filter => Filter}}, sort => Sorting},
    {ok, #{<<"hits">> := #{<<"hits">> := Hits}}}
        = mongoose_elasticsearch:search(?INDEX_NAME, ?TYPE_NAME, SearchQuery),
    Messages = lists:map(fun hit_to_gdpr_mam_message/1, Hits),
    Messages ++ Acc.

%%-------------------------------------------------------------------
%% ejabberd_gen_mam_archive callbacks
%%-------------------------------------------------------------------

archive_message(_Result, Host, MessageId, _UserId, LocalJid, RemoteJid, SourceJid, _Dir, Packet) ->
    Owner = mod_mam_utils:bare_jid(LocalJid),
    Remote = mod_mam_utils:bare_jid(RemoteJid),
    SourceBinJid = mod_mam_utils:full_jid(SourceJid),
    DocId = make_document_id(Owner, MessageId),
    Doc = make_document(MessageId, Owner, Remote, SourceBinJid, Packet),
    case mongoose_elasticsearch:insert_document(?INDEX_NAME, ?TYPE_NAME, DocId, Doc) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            ?ERROR_MSG("event=archive_message_failed server=~s user=~s remote=~s mess_id=~p reason=~1000p",
                       [Host, Owner, Remote, MessageId, Err]),
            ejabberd_hooks:run(mam_drop_message, Host, [Host]),
            Err
    end.

lookup_messages(Result, Host, #{rsm := #rsm_in{direction = before, id = ID} = RSM} = Params)
  when ID =/= undefined ->
    lookup_message_page(Result, Host, RSM, Params);
lookup_messages(Result, Host, #{rsm := #rsm_in{direction = aft, id = ID} = RSM} = Params)
  when ID =/= undefined ->
    lookup_message_page(Result, Host, RSM, Params);
lookup_messages(Result, Host, Params) ->
    do_lookup_messages(Result, Host, Params).

lookup_message_page(AccResult, Host, #rsm_in{id = ID} = RSM, Params) ->
    PageSize = maps:get(page_size, Params),
    case do_lookup_messages(AccResult, Host, Params#{page_size := 1 + PageSize}) of
        {error, _} = Err -> Err;
        {ok, LookupResult} ->
            mod_mam_utils:check_for_item_not_found(RSM, PageSize, LookupResult)
    end.

do_lookup_messages(_Result, Host, Params) ->
    SearchQuery0 = build_search_query(Params),
    Sorting = [#{mam_id => #{order => determine_sorting(Params)}}],
    ResultLimit = maps:get(page_size, Params),
    SearchQuery1 = SearchQuery0#{sort => Sorting,
                                 size => ResultLimit},
    SearchQuery2 = maybe_add_from_constraint(SearchQuery1, Params),
    case mongoose_elasticsearch:search(?INDEX_NAME, ?TYPE_NAME, SearchQuery2) of
        {ok, Result} ->
            {ok, search_result_to_mam_lookup_result(Result, Params)};
        {error, _} = Err ->
            ?ERROR_MSG("event=lookup_messages_failed server=~s reason=~1000p",
                       [Host, Err]),
            Err
    end.

-spec archive_size(Size :: integer(),
                   Host :: jid:server(),
                   _ArchiveId,
                   OwnerJid :: jid:jid()) -> non_neg_integer().
archive_size(_Size, _Host, _ArchiveId, OwnerJid) ->
    SearchQuery = build_search_query(#{owner_jid => OwnerJid}),
    archive_size(SearchQuery).

-spec remove_archive(Acc :: mongoose_acc:t(),
                     Host :: jid:server(),
                     ArchiveId :: mod_mam:archive_id(),
                     OwnerJid :: jid:jid()) -> Acc when Acc :: map().
remove_archive(Acc, Host, _ArchiveId, OwnerJid) ->
    remove_archive(Host, OwnerJid),
    Acc.

remove_archive(Host, OwnerJid) ->
    SearchQuery = build_search_query(#{owner_jid => OwnerJid}),
    case mongoose_elasticsearch:delete_by_query(?INDEX_NAME, ?TYPE_NAME, SearchQuery) of
        ok ->
            ok;
        {error, _} = Err ->
            ?ERROR_MSG("event=remove_archive_failed server=~s user=~s reason=~1000p",
                       [Host, jid:to_binary(OwnerJid), Err]),
            ok
    end.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

-spec hooks(jid:lserver()) -> [ejabberd_hooks:hook()].
hooks(Host) ->
    [{mam_archive_message, Host, ?MODULE, archive_message, 50},
     {mam_lookup_messages, Host, ?MODULE, lookup_messages, 50},
     {mam_archive_size, Host, ?MODULE, archive_size, 50},
     {mam_remove_archive, Host, ?MODULE, remove_archive, 50},
     {get_mam_pm_gdpr_data, Host, ?MODULE, get_mam_pm_gdpr_data, 50}].

-spec make_document_id(binary(), mod_mam:message_id()) -> binary().
make_document_id(Owner, MessageId) ->
    <<Owner/binary, $$, (integer_to_binary(MessageId))/binary>>.

-spec make_document(mod_mam:message_id(), binary(), binary(), binary(), exml:element()) ->
    map().
make_document(MessageId, Owner, Remote, SourceBinJid, Packet) ->
    #{mam_id     => MessageId,
      owner      => Owner,
      remote     => Remote,
      source_jid => SourceBinJid,
      message    => exml:to_binary(Packet),
      body       => exml_query:path(Packet, [{element, <<"body">>}, cdata])
     }.

-spec build_search_query(map()) -> mongoose_elasticsearch:query().
build_search_query(Params) ->
    Filters = build_filters(Params),
    TextSearchQuery = build_text_search_query(Params),
    #{query =>
      #{bool =>
        #{must => TextSearchQuery,
          filter => Filters}}}.

-spec build_filters(map()) -> [map()].
build_filters(Params) ->
    Builders = [fun owner_filter/1,
                fun with_jid_filter/1,
                fun range_filter/1],
    lists:flatmap(fun(F) -> F(Params) end, Builders).

-spec owner_filter(map()) -> [map()].
owner_filter(#{owner_jid := Owner}) ->
    BinOwner = mod_mam_utils:bare_jid(Owner),
    [#{term => #{owner => BinOwner}}].

-spec with_jid_filter(map()) -> [map()].
with_jid_filter(#{with_jid := #jid{} = WithJid}) ->
    [#{term => #{remote => mod_mam_utils:bare_jid(WithJid)}}];
with_jid_filter(_) ->
    [].

-spec range_filter(map()) -> [map()].
range_filter(#{end_ts := End, start_ts := Start, borders := Borders, rsm := RSM}) ->
    {StartId, EndId} = mod_mam_utils:calculate_msg_id_borders(RSM, Borders, Start, End),
    Range1 = maybe_add_end_filter(EndId, #{}),
    Range2 = maybe_add_start_filter(StartId, Range1),
    case maps:size(Range2) of
        0 ->
            [];
        _ ->
            [#{range => #{mam_id => Range2}}]
    end;
range_filter(_) ->
    [].

-spec maybe_add_end_filter(undefined | mod_mam:message_id(), map()) -> map().
maybe_add_end_filter(undefined, RangeMap) ->
    RangeMap;
maybe_add_end_filter(Value, RangeMap) ->
    RangeMap#{le => Value}.

-spec maybe_add_start_filter(undefined | mod_mam:message_id(), map()) -> map().
maybe_add_start_filter(undefined, RangeMap) ->
    RangeMap;
maybe_add_start_filter(Value, RangeMap) ->
    RangeMap#{ge => Value}.

-spec build_text_search_query(map()) -> map().
build_text_search_query(#{search_text := SearchText}) when is_binary(SearchText) ->
    #{simple_query_string => #{query => SearchText,
                               fields => [<<"body">>],
                               default_operator => <<"and">>}};
build_text_search_query(_) ->
    #{match_all => #{}}.

-spec determine_sorting(map()) -> asc | desc.
determine_sorting(#{rsm := #rsm_in{direction = before}}) ->
    desc;
determine_sorting(_) ->
    asc.

-spec maybe_add_from_constraint(mongoose_elasticsearch:query(), map()) ->
    mongoose_elasticsearch:query().
maybe_add_from_constraint(Query, #{rsm := #rsm_in{index = Offset}}) when is_integer(Offset) ->
    Query#{from => Offset};
maybe_add_from_constraint(Query, _) ->
    Query.

-spec search_result_to_mam_lookup_result(map(), map()) -> mod_mam:lookup_result().
search_result_to_mam_lookup_result(Result, Params) ->
    #{<<"hits">> :=
      #{<<"hits">> := Hits,
        <<"total">> := TotalCount}} = Result,

    Messages = lists:sort(
                 lists:map(fun hit_to_mam_message/1, Hits)),

    case maps:get(is_simple, Params) of
        true ->
            {undefined, undefined, Messages};
        _ ->
            CorrectedTotalCount = corrected_total_count(TotalCount, Params),
            Count = length(Messages),
            Offset = calculate_offset(TotalCount, Count, Params),
            {CorrectedTotalCount, Offset, Messages}
    end.

-spec hit_to_mam_message(map()) -> {mod_mam:message_id(), jid:jid(), exml:element()}.
hit_to_mam_message(#{<<"_source">> := JSON}) ->
    MessageId = maps:get(<<"mam_id">>, JSON),
    Packet = maps:get(<<"message">>, JSON),
    SourceBinJid = maps:get(<<"source_jid">>, JSON),

    {ok, Stanza} = exml:parse(Packet),
    {MessageId, jid:from_binary(SourceBinJid), Stanza}.

hit_to_gdpr_mam_message(#{<<"_source">> := JSON}) ->
    MessageId = maps:get(<<"mam_id">>, JSON),
    Packet = maps:get(<<"message">>, JSON),
    SourceBinJid = maps:get(<<"source_jid">>, JSON),
    {integer_to_binary(MessageId), SourceBinJid, Packet}.

%% Usage of RSM affects the `"total"' value returned by ElasticSearch. Per RSM spec, the count
%% returned by the query should represent the size of the whole result set, which in case of MAM
%% is bound only by the MAM filters.
%% The solution is to compute the archive size as if the RSM wasn't used. There is an obvious race
%% condition here, because a user may send a message between initial request to ElasticSearch and
%% the count request issued here.
-spec corrected_total_count(non_neg_integer(), mongoose_elasticsearch:query()) ->
    non_neg_integer().
corrected_total_count(_, #{rsm := #rsm_in{id = Id}} = Params) when is_integer(Id) ->
    Query = build_search_query(Params#{rsm := undefined}),
    archive_size(Query);
corrected_total_count(Count, _) ->
    Count.

-spec calculate_offset(non_neg_integer(), non_neg_integer(), map()) -> non_neg_integer().
calculate_offset(_, _, #{rsm := #rsm_in{direction = undefined, index = Index}}) when is_integer(Index) ->
    Index;
calculate_offset(TotalCount, Count, #{rsm := #rsm_in{direction = before}}) ->
    TotalCount - Count;
calculate_offset(_, _, #{rsm := #rsm_in{direction = aft, id = Id}} = Params0) when is_integer(Id) ->
    %% Not sure how this works..
    Params1 = update_borders(Params0#{rsm := undefined}, Id + 1),
    Query = build_search_query(Params1),
    archive_size(Query);
calculate_offset(_, _, _) ->
    0.

-spec update_borders(map(), non_neg_integer()) -> map().
update_borders(#{borders := Borders} = Params, EndId) ->
    Params#{borders := update_borders_to_id(Borders, EndId)}.

-spec update_borders_to_id(#mam_borders{} | undefined, non_neg_integer()) -> #mam_borders{}.
update_borders_to_id(undefined, EndId) ->
    #mam_borders{to_id = EndId};
update_borders_to_id(Borders, EndId) ->
    Borders#mam_borders{to_id = EndId}.

-spec archive_size(mod_mam_elasticsearch:query()) -> non_neg_integer().
archive_size(Query) ->
    case mongoose_elasticsearch:count(?INDEX_NAME, ?TYPE_NAME, Query) of
        {ok, Count} ->
            Count;
        {error, _} = Err ->
            ?ERROR_MSG("Failed to retrieve count of messages from ElasticSearch: ~p", [Err]),
            0
    end.
