-module(mod_stream_management_stanzas).

-include("jlib.hrl").
-include("mongoose_ns.hrl").

-export([
         sm/0,
         stream_mgmt_enabled/0,
         stream_mgmt_enabled/1,
         stream_mgmt_resumed/2,
         stream_mgmt_failed/1,
         stream_mgmt_failed/2,
         stream_mgmt_ack/1,
         stream_mgmt_request/0,
         sm_handled_count_too_high_stanza/2
        ]).

-spec sm() -> exml:element().
sm() ->
    #xmlel{name = <<"sm">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec stream_mgmt_enabled() -> exml:element().
stream_mgmt_enabled() ->
    stream_mgmt_enabled([]).

-spec stream_mgmt_enabled([exml:attr()]) -> exml:element().
stream_mgmt_enabled(ExtraAttrs) ->
    #xmlel{name = <<"enabled">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}] ++ ExtraAttrs}.

-spec stream_mgmt_resumed(mod_stream_management:smid(), mod_stream_management:short()) ->
    exml:element().
stream_mgmt_resumed(SMID, Handled) ->
    #xmlel{name = <<"resumed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"previd">>, SMID},
                    {<<"h">>, integer_to_binary(Handled)}]}.

-spec stream_mgmt_failed(binary()) -> exml:element().
stream_mgmt_failed(Reason) ->
    stream_mgmt_failed(Reason, []).

-spec stream_mgmt_failed(binary(), [exml:attr()]) -> exml:element().
stream_mgmt_failed(Reason, Attrs) ->
    ReasonEl = #xmlel{name = Reason,
                      attrs = [{<<"xmlns">>, ?NS_STANZAS}]},
    #xmlel{name = <<"failed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3} | Attrs],
           children = [ReasonEl]}.

-spec stream_mgmt_ack(non_neg_integer()) -> exml:element().
stream_mgmt_ack(NIncoming) ->
    #xmlel{name = <<"a">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(NIncoming)}]}.

-spec stream_mgmt_request() -> exml:element().
stream_mgmt_request() ->
    #xmlel{name = <<"r">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec sm_handled_count_too_high_stanza(non_neg_integer(), non_neg_integer()) -> exml:element().
sm_handled_count_too_high_stanza(Handled, OldAcked) ->
    #xmlel{name = <<"handled-count-too-high">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(Handled)},
                    {<<"send-count">>, integer_to_binary(OldAcked)}]}.
