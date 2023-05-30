%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : vCard support via RDBMS
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_vcard_rdbms).

-behaviour(mod_vcard_backend).

%% mod_vcards callbacks
-export([init/2,
         remove_user/3,
         remove_domain/2,
         get_vcard/3,
         set_vcard/5,
         search/3,
         search_fields/2,
         search_reported_fields/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

-type filter_type() :: equal | like.
-type limit_type() :: infinity | top | limit.
-type sql_column() :: binary().
-type sql_value() :: binary().
-type sql_filter() :: {filter_type(), sql_column(), sql_value()}.

init(HostType, _Options) ->
    mongoose_rdbms:prepare(vcard_remove, vcard, [username, server],
                           <<"DELETE FROM vcard WHERE username=? AND server=?">>),
    mongoose_rdbms:prepare(vcard_search_remove, vcard_search, [lusername, server],
                           <<"DELETE FROM vcard_search WHERE lusername=? AND server=?">>),
    mongoose_rdbms:prepare(vcard_remove_domain, vcard, [server],
                           <<"DELETE FROM vcard WHERE server=?">>),
    mongoose_rdbms:prepare(vcard_search_remove_domain, vcard_search, [server],
                           <<"DELETE FROM vcard_search WHERE server=?">>),
    mongoose_rdbms:prepare(vcard_select, vcard,
                           [username, server],
                           <<"SELECT vcard FROM vcard WHERE username=? AND server=?">>),
    rdbms_queries:prepare_upsert(HostType, vcard_upsert, vcard,
                                 [<<"username">>, <<"server">>, <<"vcard">>],
                                 [<<"vcard">>],
                                 [<<"username">>, <<"server">>]),
    SearchColumns = search_columns(),
    rdbms_queries:prepare_upsert(HostType, vcard_search_upsert, vcard_search,
                                 [<<"lusername">>, <<"server">>|SearchColumns],
                                 SearchColumns,
                                 [<<"lusername">>, <<"server">>]),
    ok.

%% Remove user callback
remove_user(HostType, LUser, LServer) ->
    F = fun() -> remove_user_t(HostType, LUser, LServer) end,
    mongoose_rdbms:sql_transaction(HostType, F).

remove_user_t(HostType, LUser, LServer) ->
    mongoose_rdbms:execute(HostType, vcard_remove, [LUser, LServer]),
    mongoose_rdbms:execute(HostType, vcard_search_remove, [LUser, LServer]).

%% Remove domain callback
-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    F = fun() -> remove_domain_t(HostType, Domain) end,
    mongoose_rdbms:sql_transaction(HostType, F),
    ok.

remove_domain_t(HostType, Domain) ->
    mongoose_rdbms:execute_successfully(HostType, vcard_remove_domain, [Domain]),
    mongoose_rdbms:execute_successfully(HostType, vcard_search_remove_domain, [Domain]).

%% Get a single vCard callback
get_vcard(HostType, LUser, LServer) ->
    Res = mongoose_rdbms:execute(HostType, vcard_select, [LUser, LServer]),
    case Res of
        {selected, [{SVCARD}]} ->
            case exml:parse(SVCARD) of
                {error, Reason} ->
                    ?LOG_WARNING(#{what => vcard_corrupted,
                                   text => <<"Not sending back bad vCard XML">>,
                                   reason => Reason, svcard => SVCARD,
                                   user => LUser, host => LServer}),
                    {error, mongoose_xmpp_errors:service_unavailable()};
                {ok, VCARD} ->
                    {ok, [VCARD]}
            end;
        {selected, []} ->
            {error, mongoose_xmpp_errors:item_not_found()}
    end.

%% Set a vCard callback
set_vcard(HostType, User, LServer, VCard, Search) ->
    LUser = jid:nodeprep(User),
    SearchArgs = assert_binaries(search_args(User, Search)),
    XML = exml:to_binary(VCard),
    F = fun() ->
            update_vcard_t(HostType, LUser, LServer, XML),
            update_vcard_search_t(HostType, LUser, LServer, SearchArgs),
            ok
        end,
    Result = handle_result(rdbms_queries:sql_transaction(HostType, F)),
    log_upsert_result(HostType, LServer, LUser, VCard, XML, Result),
    Result.

%% Do not pass unicode strings as a list of bytes into MySQL driver.
%% MySQL driver treats lists of integers as lists of codepoints.
%% So, it wouldn't be encoded properly.
%% Only binaries should be used to avoid confusion.
assert_binaries(Bins) ->
    case lists:all(fun is_binary/1, Bins) of
        true ->
            Bins;
        false ->
            error(#{what => assert_binaries_failed, binaries => Bins})
    end.

log_upsert_result(HostType, LServer, LUser, VCard, _XML, ok) ->
    mongoose_hooks:vcard_set(HostType, LServer, LUser, VCard);
log_upsert_result(HostType, LServer, LUser, _VCard, XML, {error, Reason}) ->
    ?LOG_WARNING(#{what => vcard_update_failed, reason => Reason,
                   host_type => HostType,
                   user => LUser, server => LServer, vcard_xml => XML}).

handle_result({atomic, ok})      -> ok;
handle_result({aborted, Reason}) -> {error, {aborted, Reason}};
handle_result({error, Reason})   -> {error, Reason}.

update_vcard_t(HostType, LUser, LServer, XML) ->
    InsertParams = [LUser, LServer, XML],
    UpdateParams = [XML],
    UniqueKeyValues = [LUser, LServer],
    rdbms_queries:execute_upsert(HostType, vcard_upsert, InsertParams, UpdateParams, UniqueKeyValues).

update_vcard_search_t(HostType, LUser, LServer, SearchArgs) ->
    InsertParams = [LUser, LServer|SearchArgs],
    UpdateParams = SearchArgs,
    UniqueKeyValues = [LUser, LServer],
    rdbms_queries:execute_upsert(HostType, vcard_search_upsert, InsertParams, UpdateParams, UniqueKeyValues).

%% Search vCards fields callback
search_fields(_HostType, _VHost) ->
    mod_vcard:default_search_fields().

%% Search vCards reported fields callback
-spec search_reported_fields(mongooseim:host_type(), jid:lserver(), ejabberd:lang()) ->
          [mongoose_data_forms:field()].
search_reported_fields(_HostType, _VHost, Lang) ->
    mod_vcard:get_default_reported_fields(Lang).

%% Search vCards callback
-spec search(mongooseim:host_type(), jid:lserver(), term()) -> [[mongoose_data_forms:field()]].
search(HostType, LServer, Data) ->
    Filters = make_filters(LServer, Data),
    case Filters of
        [] ->
            [];
        _ ->
            Limit = mod_vcard:get_results_limit(HostType),
            LimitType = limit_type(Limit),
            StmtName = filters_to_statement_name(Filters, LimitType),
            case mongoose_rdbms:prepared(StmtName) of
                false ->
                    %% Create a new type of a query
                    SQL = search_sql_binary(Filters, LimitType),
                    Columns = filters_to_columns(Filters, LimitType),
                    mongoose_rdbms:prepare(StmtName, vcard_search, Columns, SQL);
                true ->
                    ok
            end,
            Args = filters_to_args(Filters, LimitType, Limit),
            try mongoose_rdbms:execute(HostType, StmtName, Args) of
                {selected, Rs} when is_list(Rs) ->
                    record_to_items(Rs);
                Error ->
                    ?LOG_ERROR(#{what => vcard_db_search_failed, statement => StmtName,
                                 sql_query => search_sql_binary(Filters, LimitType),
                                 reason => Error, host => LServer}),
                    []
            catch Class:Error:Stacktrace ->
                    ?LOG_ERROR(#{what => vcard_db_search_failed, statement => StmtName,
                                 sql_query => search_sql_binary(Filters, LimitType),
                                 class => Class, stacktrace => Stacktrace,
                                 reason => Error, host => LServer}),
                    []
            end
    end.

-spec limit_type(infinity | non_neg_integer()) -> limit_type().
limit_type(infinity) ->
    infinity;
limit_type(_Limit) ->
    case mongoose_rdbms:db_type() of
        mssql -> top;
        _ -> limit
    end.

%% Encodes filter column names using short format
filters_to_statement_name(Filters, LimitType) ->
   Ids = [type_to_id(Type) ++ column_to_id(Col) || {Type, Col, _Val} <- Filters],
   LimitId = limit_type_to_id(LimitType),
   list_to_atom("vcard_search_" ++ LimitId ++ "_" ++ lists:append(Ids)).

filters_to_columns(Filters, LimitType) ->
   Columns = [Col || {_Type, Col, _Val} <- Filters],
   %% <<"limit">> is a pseudocolumn: does not exist in the schema,
   %% but mssql's driver code needs to know which type to use for the placeholder.
   case LimitType of
       infinity -> Columns;
       top      -> [<<"limit">>|Columns];
       limit    -> Columns ++ [<<"limit">>]
   end.

filters_to_args(Filters, LimitType, Limit) ->
   Args = [Val || {_Type, _Col, Val} <- Filters],
   case LimitType of
       infinity -> Args;
       top      -> [Limit|Args];
       limit    -> Args ++ [Limit]
   end.

search_sql_binary(Filters, LimitType) ->
    iolist_to_binary(search_sql(Filters, LimitType)).

search_sql(Filters, LimitType) ->
    {TopSQL, LimitSQL} = limit_type_to_sql(LimitType),
    RestrictionSQL = filters_to_sql(Filters),
    [<<"SELECT ">>, TopSQL,
     <<" username, server, fn, family, given, middle, "
       "nickname, bday, ctry, locality, "
       "email, orgname, orgunit "
       "FROM vcard_search ">>,
        RestrictionSQL, LimitSQL].

-spec limit_type_to_sql(limit_type()) -> {binary(), binary()}.
limit_type_to_sql(infinity) ->
    {<<>>, <<>>};
limit_type_to_sql(top) ->
    {<<" TOP (?) ">>, <<>>};
limit_type_to_sql(limit) ->
    {<<>>, <<" LIMIT ? ">>}.

filters_to_sql([Filter|Filters]) ->
    [" WHERE ", filter_to_sql(Filter)|
     [[" AND ", filter_to_sql(F)] || F <- Filters]].

filter_to_sql({equal, Col, _}) ->
     [Col, "=?"];
filter_to_sql({like, Col, _}) ->
     [Col, " LIKE ?"].

%% The result defines everything that is needed to prepare an SQL query.
-spec make_filters(jid:lserver(), list()) -> [sql_filter()].
make_filters(LServer, Data) ->
    Filters = only_tuples([filter_field(Var, Val) || {Var, [Val]} <- Data]),
    case Filters of
        [] ->
            [];
        _ ->
            HostFilter = {equal, <<"server">>, LServer},
            lists:sort([HostFilter|Filters])
    end.

only_tuples(List) ->
    [X || X <- List, is_tuple(X)].

filter_field(Var, Val) when is_binary(Val) and (Val /= <<"">>) ->
    case xmpp_field_to_column(Var) of
        false ->
            false;
        Field ->
            Type = value_type(Val),
            LVal = prepare_value(Val, Type),
            {Type, Field, LVal}
    end;
filter_field(_, _) ->
    false.

prepare_value(Val, like) ->
    LVal = without_last_byte(jid:str_tolower(Val)),
    <<LVal/binary, "%">>;
prepare_value(Val, equal) ->
    jid:str_tolower(Val).

value_type(Val) ->
    case binary:last(Val) of
        $* ->
            like;
        _ ->
            equal
    end.

limit_type_to_id(infinity) -> "inf";
limit_type_to_id(limit)    -> "lim";
limit_type_to_id(top)      -> "top".

type_to_id(like)  -> "l";
type_to_id(equal) -> "e".

xmpp_field_to_column(<<"user">>)     -> <<"lusername">>;
xmpp_field_to_column(<<"fn">>)       -> <<"lfn">>;
xmpp_field_to_column(<<"last">>)     -> <<"lfamily">>;
xmpp_field_to_column(<<"first">>)    -> <<"lgiven">>;
xmpp_field_to_column(<<"middle">>)   -> <<"lmiddle">>;
xmpp_field_to_column(<<"nick">>)     -> <<"lnickname">>;
xmpp_field_to_column(<<"bday">>)     -> <<"lbday">>;
xmpp_field_to_column(<<"ctry">>)     -> <<"lctry">>;
xmpp_field_to_column(<<"locality">>) -> <<"llocality">>;
xmpp_field_to_column(<<"email">>)    -> <<"lemail">>;
xmpp_field_to_column(<<"orgname">>)  -> <<"lorgname">>;
xmpp_field_to_column(<<"orgunit">>)  -> <<"lorgunit">>;
xmpp_field_to_column(_)              -> false.

column_to_id(<<"server">>)    -> "s";
column_to_id(<<"lusername">>) -> "u";
column_to_id(<<"lfn">>)       -> "f";
column_to_id(<<"lfamily">>)   -> "l";
column_to_id(<<"lgiven">>)    -> "f";
column_to_id(<<"lmiddle">>)   -> "m";
column_to_id(<<"lnickname">>) -> "n";
column_to_id(<<"lbday">>)     -> "b";
column_to_id(<<"lctry">>)     -> "c";
column_to_id(<<"llocality">>) -> "L";
column_to_id(<<"lemail">>)    -> "e";
column_to_id(<<"lorgname">>)  -> "N";
column_to_id(<<"lorgunit">>)  -> "U".

search_columns() ->
    [<<"username">>,
     <<"fn">>,       <<"lfn">>,
     <<"family">>,   <<"lfamily">>,
     <<"given">>,    <<"lgiven">>,
     <<"middle">>,   <<"lmiddle">>,
     <<"nickname">>, <<"lnickname">>,
     <<"bday">>,     <<"lbday">>,
     <<"ctry">>,     <<"lctry">>,
     <<"locality">>, <<"llocality">>,
     <<"email">>,    <<"lemail">>,
     <<"orgname">>,  <<"lorgname">>,
     <<"orgunit">>,  <<"lorgunit">>].

search_args(User, Search) ->
    [User,
     Search#vcard_search.fn,       Search#vcard_search.lfn,
     Search#vcard_search.family,   Search#vcard_search.lfamily,
     Search#vcard_search.given,    Search#vcard_search.lgiven,
     Search#vcard_search.middle,   Search#vcard_search.lmiddle,
     Search#vcard_search.nickname, Search#vcard_search.lnickname,
     Search#vcard_search.bday,     Search#vcard_search.lbday,
     Search#vcard_search.ctry,     Search#vcard_search.lctry,
     Search#vcard_search.locality, Search#vcard_search.llocality,
     Search#vcard_search.email,    Search#vcard_search.lemail,
     Search#vcard_search.orgname,  Search#vcard_search.lorgname,
     Search#vcard_search.orgunit,  Search#vcard_search.lorgunit].

without_last_byte(Bin) ->
    binary:part(Bin, 0, byte_size(Bin)-1).

record_to_items(Records) ->
    [record_to_item(Record) || Record <- Records].

record_to_item({Username, VCardVHost, FN, Family, Given, Middle,
             Nickname, BDay, CTRY, Locality,
             EMail, OrgName, OrgUnit}) ->
    [
     ?FIELD(<<"jid">>, <<Username/binary, "@", VCardVHost/binary>>),
     ?FIELD(<<"fn">>, FN),
     ?FIELD(<<"last">>, Family),
     ?FIELD(<<"first">>, Given),
     ?FIELD(<<"middle">>, Middle),
     ?FIELD(<<"nick">>, Nickname),
     ?FIELD(<<"bday">>, BDay),
     ?FIELD(<<"ctry">>, CTRY),
     ?FIELD(<<"locality">>, Locality),
     ?FIELD(<<"email">>, EMail),
     ?FIELD(<<"orgname">>, OrgName),
     ?FIELD(<<"orgunit">>, OrgUnit)
    ].
