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
-behaviour(mod_vcard_backend).

%% API
-export([init/2,
         remove_user/3,
         set_vcard/5,
         get_vcard/3,
         search/3,
         search_fields/2,
         search_reported_fields/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").
-include_lib("riakc/include/riakc.hrl").

init(_HostType, _Opts) ->
    ok.

remove_user(HostType, LUser, LServer) ->
    mongoose_riak:delete(bucket_type(HostType, LServer), LUser, [{dw, 2}]).

set_vcard(HostType, User, LServer, VCard, _VCardSearch) ->
    BucketType = bucket_type(HostType, LServer),
    VCardEncoded = exml:to_binary(VCard),
    LUser = jid:nodeprep(User),
    Obj = riakc_obj:new(BucketType, LUser, VCardEncoded, "application/xml"),
    mongoose_riak:put(Obj).

get_vcard(HostType, LUser, LServer) ->
    BucketType = bucket_type(HostType, LServer),
    case mongoose_riak:get(BucketType, LUser) of
        {ok, Obj} ->
            XMLBin = riakc_obj:get_value(Obj),
            case exml:parse(XMLBin) of
                {ok, XMLEl} ->
                    {ok, [XMLEl]};
                {error, Reason} ->
                    ?LOG_WARNING(#{what => vcard_lookup_failed, reason => Reason,
                                   packet => XMLBin}),
                    {error, mongoose_xmpp_errors:service_unavailable()}
            end;
        {error, notfound} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        Other ->
            Other
    end.

search(HostType, LServer, Data) ->
    YZQuery = make_yz_query(Data, []),
    do_search(YZQuery, HostType, LServer).

do_search([], _HostType, _) ->
    [];
do_search(YZQueryIn, HostType, LServer) ->
    {_BucketType, BucketName} = bucket_type(HostType, LServer),
    YZQuery = [<<"_yz_rb:", BucketName/binary>> |  YZQueryIn],
    Limit = mod_vcard:get_results_limit(HostType),
    YZQueryBin = mongoose_bin:join(YZQuery, <<" AND ">>),
    case mongoose_riak:search(yz_vcard_index(HostType), YZQueryBin, [{rows, Limit}]) of
        {ok, #search_results{docs=R, num_found = _N}} ->
            lists:map(fun({_Index, Props}) -> doc2item(HostType, LServer, Props) end, R);
        Err ->
            ?LOG_ERROR(#{what => vcard_search_failed, index => yz_vcard_index(HostType),
                         riak_query => YZQueryBin, reason => Err}),
            []
    end.

search_fields(_HostType, _LServer) ->
    mod_vcard:default_search_fields().

search_reported_fields(_HostType, _LServer, Lang) ->
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
    LVal = jid:str_tolower(Val),
    case binary:match(LVal, <<" ">>) of
        nomatch ->
            LVal;
        _ ->
            [$", LVal, $"]
    end.

doc2item(HostType, LServer, Props) ->
    Vals = lists:map(pa:bind(fun extract_field/2, Props), search_fields(HostType, LServer)),
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


bucket_type(HostType, LServer) ->
    {gen_mod:get_module_opt(HostType, mod_vcard, [riak, bucket_type]), <<"vcard_", LServer/binary>>}.

yz_vcard_index(HostType) ->
    gen_mod:get_module_opt(HostType, mod_vcard, [riak, search_index]).
