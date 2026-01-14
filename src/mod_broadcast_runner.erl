-module(mod_broadcast_runner).

-export([run/2]).

-include("mongoose_logger.hrl").
-include("jlib.hrl").

run(HostType, JobId) ->
    process_flag(trap_exit, true),
    case mod_broadcast_rdbms:get_job(HostType, JobId) of
        {ok, Job} ->
            run_job(HostType, Job);
        not_found ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_broadcast_runner_get_job_failed, id => JobId, reason => Reason}),
            ok
    end.

run_job(HostType, Job = #{domain := Domain, sender_jid := SenderBin,
                          subject := Subject, body := Body,
                          rate_per_second := Rate}) ->
    FromJid = jid:from_binary(SenderBin),
    DelayMs = delay_ms(Rate),
    loop(HostType, Domain, FromJid, Subject, Body, Job#{delay_ms => DelayMs}).

loop(HostType, Domain, FromJid, Subject, Body, Job = #{id := JobId, delay_ms := DelayMs}) ->
    receive
        {'EXIT', _From, abort} ->
            ok;
        {'EXIT', _From, Reason} ->
            ok = mod_broadcast_rdbms:fail_job(HostType, JobId, aborted_errors, iolist_to_binary(io_lib:format("~p", [Reason]))),
            ok
    after 0 ->
        case mod_broadcast_rdbms:next_recipients(HostType, JobId, 200) of
            {ok, []} ->
                _ = mod_broadcast_rdbms:finish_job(HostType, JobId, success),
                ok;
            {ok, Users} ->
                case process_users(HostType, Domain, FromJid, Subject, Body, JobId, Users, DelayMs) of
                    ok -> loop(HostType, Domain, FromJid, Subject, Body, Job);
                    {error, ErrBin} ->
                        _ = mod_broadcast_rdbms:fail_job(HostType, JobId, aborted_errors, ErrBin),
                        ok
                end;
            {error, Reason} ->
                _ = mod_broadcast_rdbms:fail_job(HostType, JobId, aborted_errors,
                                                iolist_to_binary(io_lib:format("db_error:~p", [Reason]))),
                ok
        end
    end.

process_users(_HostType, _Domain, _FromJid, _Subject, _Body, _JobId, [], _DelayMs) ->
    ok;
process_users(HostType, Domain, FromJid, Subject, Body, JobId, [LUser | Rest], DelayMs) ->
    ToJid = jid:make_bare(LUser, Domain),
    Stanza = build_broadcast_message(FromJid, ToJid, Subject, Body),
    case mongoose_stanza_api:send_stanza(undefined, Stanza) of
        {ok, _} ->
            SentTS = erlang:system_time(second),
            _ = mod_broadcast_rdbms:mark_recipient_sent(HostType, JobId, LUser, SentTS),
            _ = mod_broadcast_rdbms:increment_progress(HostType, JobId, 1),
            timer:sleep(DelayMs),
            process_users(HostType, Domain, FromJid, Subject, Body, JobId, Rest, DelayMs);
        {ErrCode, Msg} ->
            ?LOG_WARNING(#{what => mod_broadcast_send_failed,
                           job_id => JobId,
                           to => jid:to_binary(ToJid),
                           error => ErrCode,
                           message => iolist_to_binary(Msg)}),
            {error, iolist_to_binary(Msg)}
    end.

build_broadcast_message(FromJid, ToJid, Subject, Body) ->
    FromBin = jid:to_binary(jid:to_bare(FromJid)),
    ToBin = jid:to_binary(jid:to_bare(ToJid)),
    Children = case Subject of
                   undefined -> [];
                   _ -> [#xmlel{name = <<"subject">>, children = [#xmlcdata{content = Subject}]}]
               end ++
               [#xmlel{name = <<"body">>, children = [#xmlcdata{content = Body}]}],
    #xmlel{name = <<"message">>,
           attrs = #{<<"type">> => <<"chat">>,
                     <<"from">> => FromBin,
                     <<"to">> => ToBin,
                     <<"id">> => mongoose_bin:gen_from_crypto()},
           children = Children}.

delay_ms(Rate) when is_integer(Rate), Rate > 0 ->
    erlang:max(1, 1000 div Rate);
delay_ms(_) ->
    100.
