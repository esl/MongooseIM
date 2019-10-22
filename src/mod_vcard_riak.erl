%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
%%==============================================================================
-module(mod_vcard_riak).
-behaviour(mod_vcard).

%% API
-export([init/2,
         remove_user/2,
         set_vcard/4,
         get_vcard/2,
         search/2,
         search_fields/1,
         search_reported_fields/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").
-include_lib("riakc/include/riakc.hrl").

-spec init(jid:lserver(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec remove_user(jid:luser(), jid:lserver()) -> ok.
remove_user(LUser, LServer) ->
    mongoose_riak:delete(bucket_type(LServer), LUser, [{dw, 2}]).

-spec set_vcard(jid:user(), jid:lserver(), exml:item(), term()) ->
    ok | {error, term()}.
set_vcard(User, VHost, VCard, _VCardSearch) ->
    BucketType = bucket_type(VHost),
    VCardEncoded = exml:to_binary(VCard),
    LUser = jid:nodeprep(User),
    Obj = riakc_obj:new(BucketType, LUser, VCardEncoded, "application/xml"),
    mongoose_riak:put(Obj).

-spec get_vcard(jid:luser(), jid:lserver()) ->
    {ok, term()} | {error, term()}.
get_vcard(LUser, LServer) ->
    BucketType = bucket_type(LServer),
    case mongoose_riak:get(BucketType, LUser) of
        {ok, Obj} ->
            XMLBin = riakc_obj:get_value(Obj),
            case exml:parse(XMLBin) of
                {ok, XMLEl} ->
                    {ok, [XMLEl]};
                {error, Reason} ->
                    ?WARNING_MSG("not sending bad vcard reason=~p, xml=~n~p", [Reason, XMLBin]),
                    {error, mongoose_xmpp_errors:service_unavailable()}
            end;
        {error, notfound} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        Other ->
            Other
    end.

-spec search(jid:lserver(), list()) -> list().
search(VHost, Data) ->
    YZQuery = make_yz_query(Data, []),
    do_search(YZQuery, VHost).

do_search([], _) ->
    [];
do_search(YZQueryIn, VHost) ->
    {_BucketType, BucketName} = bucket_type(VHost),
    YZQuery = [<<"_yz_rb:", BucketName/binary>> |  YZQueryIn],
    Limit = mod_vcard:get_results_limit(VHost),
    YZQueryBin = mongoose_bin:join(YZQuery, <<" AND ">>),
    case mongoose_riak:search(yz_vcard_index(VHost), YZQueryBin, [{rows, Limit}]) of
        {ok, #search_results{docs=R, num_found = _N}} ->
            lists:map(fun({_Index, Props}) -> doc2item(VHost, Props) end, R);
        Err ->
            ?ERROR_MSG("Error while search vCard, index=~s, query=~s, error=~p",
                [yz_vcard_index(VHost), YZQueryBin, Err]),
            []
    end.

-spec search_fields(jid:lserver()) -> list().
search_fields(_VHost) ->
    mod_vcard:default_search_fields().

search_reported_fields(_VHost, Lang) ->
    mod_vcard:get_default_reported_fields(Lang).

make_yz_query([], Acc) -> Acc;
make_yz_query([{Var, [Val]} | Rest], Acc) ->
    Part = [riak_search_mapping(Var), ":", make_val(Val)],
    make_yz_query(Rest, [erlang:iolist_to_binary(Part) | Acc]).

riak_search_mapping(<<"user">>) -> <<"_yz_rk">>;
riak_search_mapping(<<"fn">>) -> <<"vCard.FN">>;
riak_search_mapping(<<"first">>) -> <<"vCard.N.GIVEN">>;
riak_search_mapping(<<"middle">>) -> <<"vCard.N.MIDDLE">>;
riak_search_mapping(<<"last">>) -> <<"vCard.N.FAMILY">>;
riak_search_mapping(<<"nick">>) -> <<"vCard.NICKNAME">>;
riak_search_mapping(<<"bday">>) -> <<"vCard.BDAY">>;
riak_search_mapping(<<"ctry">>) -> <<"vCard.ADR.CTRY">>;
riak_search_mapping(<<"locality">>) -> <<"vCard.ADR.LOCALITY">>;
riak_search_mapping(<<"email">>) -> <<"vCard.EMAIL.USERID">>;
riak_search_mapping(<<"orgname">>) -> <<"vCard.ORG.ORGNAME">>;
riak_search_mapping(<<"orgunit">>) -> <<"vCard.ORG.ORGUNIT">>.

make_val(Val) ->
    LVal = stringprep:tolower(Val),
    case binary:match(LVal, <<" ">>) of
        nomatch ->
            LVal;
        _ ->
            [$", LVal, $"]
    end.

doc2item(VHost, Props) ->
    Vals = lists:map(pa:bind(fun extract_field/2, Props), search_fields(VHost)),
    #xmlel{name = <<"item">>,
           children = Vals}.

extract_field(Props, {_, <<"user">>}) ->
    {_, Username} = lists:keyfind(riak_search_mapping(<<"user">>), 1, Props),
    {_, Bucket} = lists:keyfind(<<"_yz_rb">>, 1, Props),
    [_, Host] = binary:split(Bucket, <<"_">>),
    ?FIELD(<<"jid">>, iolist_to_binary([Username, "@", Host]));
extract_field(Props, {_, Field}) ->
    V = case lists:keyfind(riak_search_mapping(Field), 1, Props) of
            {_, Val} ->
                Val;
            _ ->
                ""
        end,
    ?FIELD(Field, V).


bucket_type(Host) ->
    {gen_mod:get_module_opt(Host, mod_vcard, bucket_type, <<"vcard">>), <<"vcard_", Host/binary>>}.

yz_vcard_index(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard, search_index, <<"vcard">>).
