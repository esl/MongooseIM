-module(log_filter_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [format_otp_message].

init_per_suite(C) ->
    C.

end_per_suite(_C) ->
    ok.

format_otp_message(_C) ->
    #{msg := {report, Report = #{reason := kek}}} =
        mongoose_log_filter:format_otp_message(otp_message(), ok),
    false = maps:is_key(report, Report).

otp_message() ->
    #{level => error,
      meta => #{domain => [otp,sasl],
                error_logger => #{tag => error_report, type => supervisor_report},
                file => "supervisor.erl",
                gl => list_to_pid("<0.2346.0>"),
                line => 701,
                logger_formatter => #{title => "SUPERVISOR REPORT"},
                mfa => {supervisor,do_restart,3},
                pid => list_to_pid("<0.2515.0>"),
                report_cb => fun logger:format_otp_report/1,
                time => 1602577882170643},
      msg => {report,
              #{label => {supervisor,child_terminated},
                report =>
                    [{supervisor,{local,'mongoose_wpool$rdbms$global$default'}},
                     {errorContext,child_terminated},
                     {reason,kek},
                     {offender,
                         [{pid, list_to_pid("<0.20258.0>")},
                          {id,'wpool_pool-mongoose_wpool$rdbms$global$default-queue-manager'},
                          {mfargs,
                              {wpool_queue_manager,start_link,
                                  ['mongoose_wpool$rdbms$global$default',
                                   'wpool_pool-mongoose_wpool$rdbms$global$default-queue-manager',
                                   [{queue_type,fifo}]]}},
                          {restart_type,permanent},
                          {shutdown,brutal_kill},
                          {child_type,worker}]}]}}}.
