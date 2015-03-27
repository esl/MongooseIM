%%%-------------------------------------------------------------------
%%% @author Baibossynov Valeryi
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Mar 2015 8:26 AM
%%%-------------------------------------------------------------------
-module(mod_offline_push).
-author("vol").
-include("jlib.hrl").

-include("ejabberd.hrl").

%% API
-export([stop/1, start/2, send_notice/3]).

% Mod private backend
-define(BACKEND, (mod_private_backend:backend())).

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_push", []),
    {api_key, ApiKey} = gen_mod:get_opt(gcm, Opts),
    gcm:start(offline_push, ApiKey),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.


stop(Host)->
    ?INFO_MSG("Stoping mod_offline_push", []),
    gcm:stop(offline_push),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.


send_notice(_From, To, Packet) ->
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    if (Body /= <<"">>) ->
        NS2Def = [{<<"notification:prefs">>,{xmlel,<<"notification">>,[{<<"xmlns">>,<<"notification:prefs">>}],[]}}],
        NotifPrefs = get_notif_prefs(To#jid.luser, To#jid.lserver, NS2Def),
        NotifFormat = [{<<"data">>, [{<<"message">>, Body}]}],
        gcm:push(offline_push, [NotifPrefs], NotifFormat);
        true ->
            ok
    end.


get_notif_prefs(LUser, LServer, NS2Def) ->
    parse_private_xml(?BACKEND:multi_get_data(LUser, LServer, NS2Def)).

parse_private_xml(PrivateData) ->
    parse_private_xml(#xmlel{children = PrivateData}, <<"notification">>).

parse_private_xml(OrigXML, Tag) ->
    case xml:get_subtag(OrigXML, Tag) of
        false ->
            false;
        NotifTag ->
            parse_private_xml(OrigXML, NotifTag, <<"token">>)
    end.

parse_private_xml(_OrigXML, NotifTag, TokenTagName) ->

    case xml:get_subtag(NotifTag, TokenTagName) of
        false ->
            false;
        TokenTag ->
            ?DEBUG("Token ~p", [xml:get_tag_cdata(TokenTag)]),
            xml:get_tag_cdata(TokenTag)
    end.



