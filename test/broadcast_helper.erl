%%%-------------------------------------------------------------------
%%% @doc Common helpers for broadcast test suites.
%%%-------------------------------------------------------------------

-module(broadcast_helper).
-author('piotr.nosek@erlang-solutions.com').

-export([host_type/0,
         domain/0,
         job_id/0,
         valid_job_spec/0,
         sample_broadcast_job/0,
         sample_broadcast_job/1]).

-include_lib("mod_broadcast.hrl").

host_type() ->
    <<"test_host_type">>.

domain() ->
    <<"test.domain">>.

job_id() ->
    4242.

valid_job_spec() ->
    #{name => <<"Test Broadcast">>,
      domain => domain(),
      sender => jid:make_noprep(<<"admin">>, domain(), <<>>),
      subject => <<"Test Subject">>,
      body => <<"Test message body">>,
      message_rate => 100,
      recipient_group => all_users_in_domain}.

sample_broadcast_job() ->
    sample_broadcast_job(1000).

sample_broadcast_job(MessageRate) ->
    #broadcast_job{id = job_id(),
                   name = <<"Test Broadcast">>,
                   host_type = host_type(),
                   domain = domain(),
                   sender = jid:make_noprep(<<"admin">>, domain(), <<>>),
                   subject = <<"Test Subject">>,
                   body = <<"Test message body">>,
                   message_rate = MessageRate,
                   recipient_group = all_users_in_domain,
                   owner_node = node(),
                   recipient_count = 100,
                   recipients_processed = 0,
                   execution_state = running,
                   abortion_reason = undefined,
                   created_at = {{2026, 2, 5}, {12, 0, 0}},
                   started_at = {{2026, 2, 5}, {12, 0, 1}},
                   stopped_at = undefined}.