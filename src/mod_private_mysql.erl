%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_mysql).
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-export([get_all_nss/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

init(_Host, _Opts) ->
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    Rows = [sql_row(NS, XML) || {NS, XML} <- NS2XML],
    replace_like_insert_result(
        rdbms_queries:multi_set_private_data(LServer, SLUser, Rows)).

replace_like_insert_result({updated, _})        -> ok;
replace_like_insert_result({error, Reason})     -> {error, Reason}.

sql_row(NS, XML) ->
    SNS = mongoose_rdbms:escape_string(NS),
    SData = mongoose_rdbms:escape_string(exml:to_binary(XML)),
    {SNS, SData}.

multi_get_data(LUser, LServer, NS2Def) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    SNSs = [mongoose_rdbms:escape_string(NS) || {NS, _Def} <- NS2Def],
    case rdbms_queries:multi_get_private_data(LServer, SLUser, SNSs) of
        {selected, Rows} ->
            RowsDict = dict:from_list(Rows),
            [select_value(NSDef, RowsDict) || NSDef <- NS2Def];
        _ ->
            [Def || {_NS, Def} <- NS2Def]
    end.

select_value({NS, Def}, RowsDict) ->
    case dict:find(NS, RowsDict) of
        {ok, SData} ->
            {ok, Elem} = exml:parse(SData),
            Elem;
        error ->
            Def
    end.

remove_user(LUser, LServer) ->
    SLUser = mongoose_rdbms:escape_string(LUser),
    rdbms_queries:del_user_private_storage(LServer, SLUser).

get_all_nss(LUser, LServer) ->
    EscLUser = mongoose_rdbms:escape_string(LUser),
    {selected, Res} = rdbms_queries:get_all_roster_namespaces(LServer, EscLUser),
    Keys = lists:map(fun({R}) -> R end, Res),
    Keys.
