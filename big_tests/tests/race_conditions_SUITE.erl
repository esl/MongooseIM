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
-module(race_conditions_SUITE).
-compile(export_all).

-export([handle_delayiq_iq/4]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").

-include("mam_helper.hrl"). %% mam? we need assert_equal_extra

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, main_group}].

groups() ->
    G = [{main_group, [], main_group_tests()}],
    ct_helper:repeat_all_until_all_ok(G).

main_group_tests() ->
    [confirm_c2s_queue_is_flushed_works,
     delayiq_handler_works,
     ignore_iq_result_from_old_session].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    mongoose_helper:inject_module(?MODULE),
    start_delayiq_handler(),
    Config.

end_per_group(_GroupName, Config) ->
    stop_delayiq_handler(),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%% Check, that confirm_c2s_queue_is_flushed works for any regular connection.
%% This function is used in more complex tests.
confirm_c2s_queue_is_flushed_works(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun(_FreshConfig, Alice) ->
            %% Should not crash
            confirm_c2s_queue_is_flushed(Alice)
        end).

%% Checks, that result IQ is delivered, when delayiq processing is resumed.
delayiq_handler_works(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun(_FreshConfig, Alice) ->
            RequestIQ = send_delayiq(Alice),
            {ok, DelayIqConfirmation} = receive_delayiq_confirmation(),
            %% HAVING waiting iq handler
            resume_delayiq(DelayIqConfirmation),
            %% HAVING result IQ stanza put into ejabberd_c2s process
            %% (if there is any)
            wait_for_delayiq_handler_to_finish(DelayIqConfirmation),
            receive_delayiq(Alice, RequestIQ),
            confirm_c2s_queue_is_flushed(Alice)
        end).

%% We reconnect Alice and check that new connection does not receive
%% IQ result from IQ, sent using the old connection.
%%
%% If old and new resources are different, that the stanza would not be routed
%% to the new connection in any case (with or without
%% check_incoming_accum_for_conflicts check in ejabberd_c2s).
ignore_iq_result_from_old_session(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun(FreshConfig, Alice) ->
            send_delayiq(Alice),
            {ok, DelayIqConfirmation} = receive_delayiq_confirmation(),
            %% HAVING waiting iq handler

            %% Stop the session.
            %% And wait for the session process to be dead
            %% and deregistered.
            mongoose_helper:logout_user(Config, Alice),

            %% Use the same resource for both old and new connection.
            Resource = escalus_client:resource(Alice),
            Alice2 = login_send_and_receive_presence(FreshConfig, alice, Resource),
            resume_delayiq(DelayIqConfirmation),
            %% HAVING result IQ stanza put into ejabberd_c2s process
            %% (if there is any)
            %% We expect, that Alice2 c2s process receives the route message,
            %% but would ignore it.
            wait_for_delayiq_handler_to_finish(DelayIqConfirmation),
            %% RESULT missing delayiq
            confirm_c2s_queue_is_flushed(Alice2)
        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_delayiq_handler() ->
    NS = delayiq_ns(),
    Domain = domain(),
    %% Register our module as an iq handler.
    %% It's important to use IQDisc=parallel, because
    %% this particular IQ should be executed in a separate process.
    Args = [ejabberd_sm, Domain, NS, ?MODULE, handle_delayiq_iq, parallel],
    mongoose_helper:successful_rpc(gen_iq_handler, add_iq_handler, Args).

stop_delayiq_handler() ->
    NS = delayiq_ns(),
    Domain = domain(),
    %% Register our module as an iq handler.
    %% It's important to use IQDisc=parallel, because
    %% this particular IQ should be executed in a separate process.
    Args = [ejabberd_sm, Domain, NS],
    mongoose_helper:successful_rpc(gen_iq_handler, remove_iq_handler, Args).

%% Brand new IQ namespace.
%% Send IQ get with this namespace, and MongooseIM would send you
%% IQ result, until you ask for it, using an RPC.
delayiq_ns() ->
    <<"urn:escalus:delayiq">>.

%% Make an XMPP stanza, that contains information
%% needed in handle_delayiq_iq/4 callback
delayiq_iq() ->
    BinCallerPid = encode_pid(self()),
    Payload = #xmlel{name = <<"data">>,
                     attrs = [{<<"caller_pid">>, BinCallerPid}]},
    escalus_stanza:iq_get(delayiq_ns(), [Payload]).

domain() ->
    ct:get_config({hosts, mim, domain}).

%% This function is executed by MongooseIM
handle_delayiq_iq(_From, _To, Acc, IQ) ->
    SubEl = mongoose_iq:iq_to_sub_el(IQ),
    BinCallerPid = exml_query:path(SubEl, [{element, <<"data">>},
                                           {attr, <<"caller_pid">>}]),
    CallerPid = decode_pid(BinCallerPid),
    CallerPid ! {handle_delayiq_iq_confirmation, self()},
    receive
        {handle_delayiq_iq_resume, CallerPid} ->
            ok
        after timer:seconds(60) ->
            %% provide just enough information for debugging
            erlang:error(#{reason => handle_delayiq_timeout, %% it's unexpected
                           caller_pid => CallerPid,
                           mongoose_pid => self(),
                           messages => erlang:process_info(self(), messages)})
    end,
    ResultIQ = mongoose_iq:empty_result_iq(IQ),
    {Acc, ResultIQ}.

%% Ensure, that MongooseIM started to process our IQ
receive_delayiq_confirmation() ->
    receive
        {handle_delayiq_iq_confirmation, MongoosePid} ->
            {ok, MongoosePid}
        after timer:seconds(5) ->
            {error, #{reason => receive_delayiq_confirmation_timeout,
                      caller_pid => self(),
                      messages => erlang:process_info(self(), messages)}}
    end.

%% Ask MongooseIM to return result IQ
resume_delayiq(MongoosePid) ->
    MongoosePid ! {handle_delayiq_iq_resume, self()}.

%% Send XMPP request stanza.
send_delayiq(Client) ->
    RequestIQ = delayiq_iq(),
    escalus:send(Client, RequestIQ),
    RequestIQ.

%% Receive XMPP result stanza.
receive_delayiq(Client, RequestIQ) ->
    ResultIQ = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq_result, [RequestIQ], ResultIQ),
    ResultIQ.

wait_for_delayiq_handler_to_finish(MongoosePid) ->
    MonRef = erlang:monitor(process, MongoosePid),
    receive
        {'DOWN', MonRef, process, _, _} ->
            ok
    after timer:seconds(5) ->
        error(#{reason => wait_for_delayiq_handler_to_finish_timeout,
                mongoose_pid => MongoosePid,
                caller_pid => self()})
    end.

%% The simplest way to confirm, that c2s queue is processed is to
%% send and receive a stanza from the process.
confirm_c2s_queue_is_flushed(Client) ->
    BadNS = <<"urn:escalus:this_iq_would_fail_but_its_expected">>,
    %% We can just send an unknown IQ and wait for error.
    %% Another way is to send a message, but we need two clients for this.
    RequestIQ = escalus_stanza:iq_get(BadNS, []),
    escalus:send(Client, RequestIQ),
    %% We will have a timeout here, if something goes wrong.
    ResultIQ = escalus_client:wait_for_stanza(Client),
    %% Expected result
    escalus:assert(is_iq_error, [], ResultIQ),
    %% Just check that it's our IQ
    QueryId = exml_query:attr(RequestIQ, <<"id">>),
    ResultId = exml_query:attr(ResultIQ, <<"id">>),
    ?assert_equal_extra(QueryId, ResultId,
                        #{request_iq => RequestIQ, result_iq => ResultIQ}).

login_send_presence(Config, User, Resource) ->
    {ok, Client} = escalus_client:start(Config, User, Resource),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.

login_send_and_receive_presence(Config, User, Resource) ->
    Client = login_send_presence(Config, User, Resource),
    P = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_presence, P),
    Client.

%% pid_to_list encoding would not work over erlang distribution.
encode_pid(Pid) ->
    base64:encode(erlang:term_to_binary(Pid)).

decode_pid(Pid) ->
    erlang:binary_to_term(base64:decode(Pid)).
