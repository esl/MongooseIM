#!/usr/bin/env escript

-mode(compile).

-include_lib("xmerl/include/xmerl.hrl").

main([Type]) ->
    case os:getenv("DEBUG") of
        "1" ->
            {ok, DebugFile} = file:open("/tmp/script-debug", [write, append]),
            put(debug_file, DebugFile);
        _ ->
            ok
    end,
    try
        ok = io:setopts([binary]),
        loop(Type)
    catch
        C:R:S ->
            debug(C, R, S, #{ type => unrecoverable_error })
    after
        file:close(get(debug_file))
    end;
main(_) ->
    print_usage().

print_usage() ->
    MyName = filename:basename(escript:script_name()),
    io:format("Usage: ~s data-type~n", [MyName]),
    io:format("\tdata-type = eterm | xml~n"),
    io:format("\t\t eterm - if MIM stores messages in encoded format (default for MIM)~n"),
    io:format("\t\t xml - if MIM stores messages in plain XML~n"),
    io:format("\t\t Please check documentation to learn about IO format.~n~n").

loop("eterm") -> common_loop(fun jid_from_eterm/1);
loop("xml") -> common_loop(fun jid_from_xml/1);
loop(_) -> print_usage().

common_loop(ExtractionFun) ->
    case file:read_line(standard_io) of
        eof ->
            ok;
        {ok, InLenBin} ->
            % We skip trailing \n
            InLen = binary_to_integer(binary:part(InLenBin, 0, byte_size(InLenBin) - 1)),
            {ok, Data} = file:read(standard_io, InLen),
            safe_jid_extraction(ExtractionFun, Data),
            common_loop(ExtractionFun)
    end.

jid_from_eterm(ETerm) ->
    {xmlel, <<"message">>, MsgAttrs, MsgChildren} = binary_to_term(ETerm),
    case lists:keyfind(<<"type">>, 1, MsgAttrs) of
        {_, <<"groupchat">>} -> ok;
        _ -> throw(not_muc_message)
    end,
    case lists:keyfind([{<<"xmlns">>, <<"http://jabber.org/protocol/muc#user">>}], 3, MsgChildren) of
        {xmlel, <<"x">>, _, XChildren} ->
            {xmlel, _, ItemAttrs, _} = lists:keyfind(<<"item">>, 2, XChildren),
            {_, JID} = lists:keyfind(<<"jid">>, 1, ItemAttrs),
            JID;
        _ ->
            throw(not_muc_message)
    end.


jid_from_xml(XML) ->
    XmerlFriendlyXML = "<?xml version='1.0' encoding='utf-8'?>" ++ binary_to_list(XML),
    {Doc, _} = xmerl_scan:string(XmerlFriendlyXML),
    case xmerl_xpath:string("/message/@type", Doc) of
        [#xmlAttribute{ value = "groupchat" }] ->
            ok;
        _ ->
            throw(not_muc_message)
    end,
    Xs = xmerl_xpath:string("/message/x", Doc),
    IsMUC =
        lists:any(fun
            (#xmlElement{ namespace = #xmlNamespace{ default = 'http://jabber.org/protocol/muc#user' }}) ->
                true;
            (_Elem) ->
                false
        end, Xs),
    IsMUC orelse throw(not_muc_message),

    [#xmlAttribute{ value = JID }] = xmerl_xpath:string("/message/x/item/@jid", Doc),
    unicode:characters_to_binary(JID).

safe_jid_extraction(JIDExtractorFun, Data) ->
    try JIDExtractorFun(Data) of
        JID0 ->
            JID = bare_jid(JID0),
            OutLen = byte_size(JID),
            OutLenBin = integer_to_binary(OutLen),
            ok = file:write(standard_io, <<OutLenBin/binary, $\n, JID/binary>>)
    catch
        throw:R:S ->
            Extra = #{ type => invalid_message_type, data => Data },
            debug(throw, R, S, Extra),
            ok = io:put_chars("-2\n");
        C:R:S ->
            Extra = #{ type => cannot_extract_jid, data => Data },
            debug(C, R, S, Extra),
            ok = io:put_chars("-1\n")
    end.

bare_jid(JID) ->
    [BareJID | _] = string:split(string:lowercase(JID), "/"),
    BareJID.

debug(Class, Reason, StackTrace, Extra) ->
    case get(debug_file) of
        undefined ->
            ok;
        File ->
            ToWrite = #{ class => Class, reason => Reason, stack_trace => StackTrace },
            io:fwrite(File, "~p~n~n", [maps:merge(ToWrite, Extra)])
    end.

