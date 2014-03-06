%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_mysql).
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

init(_Host, _Opts) ->
    ok.

multi_set_data(LUser, LServer, NS2XML) ->
    SLUser = ejabberd_odbc:escape(LUser),
    Rows = [sql_row(NS, XML) || {NS, XML} <- NS2XML],
    replace_like_insert_result(
        odbc_queries:multi_set_private_data(LServer, SLUser, Rows)).

replace_like_insert_result({updated, _})        -> {atomic, ok};
replace_like_insert_result({error, Reason})     -> {error, Reason};
replace_like_insert_result({aborted, Reason})   -> {aborted, Reason}.

sql_row(NS, XML) ->
    SNS = ejabberd_odbc:escape(NS),
    SData = ejabberd_odbc:escape(xml:element_to_binary(XML)),
    {SNS, SData}.

multi_get_data(LUser, LServer, NS2Def) ->
    SLUser = ejabberd_odbc:escape(LUser),
    SNSs = [ejabberd_odbc:escape(NS) || {NS, _Def} <- NS2Def],
    case odbc_queries:multi_get_private_data(LServer, SLUser, SNSs) of
        {selected, [<<"namespace">>, <<"data">>], Rows} ->
            RowsDict = dict:from_list(Rows),
            [select_value(NSDef, RowsDict) || NSDef <- NS2Def];
        _ ->
            [Def || {_NS, Def} <- NS2Def]
    end.

select_value({NS, Def}, RowsDict) ->
    case dict:find(NS, RowsDict) of
        {ok, SData} ->
            #xmlel{} = xml_stream:parse_element(SData);
        error ->
            Def
    end.

remove_user(LUser, LServer) ->
    SLUser = ejabberd_odbc:escape(LUser),
    odbc_queries:del_user_private_storage(LServer, SLUser).
