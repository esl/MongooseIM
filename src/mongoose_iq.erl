-module(mongoose_iq).
-export([try_to_handle_iq/4]).

-include("mongoose_logger.hrl").
-include("jlib.hrl").

%% @doc Generic error handling code for IQ.
%% Use this function, instead of `try ... catch _:_ -> ok end'.
-spec try_to_handle_iq(jid:jid(), jid:jid(), jlib:iq(), HandlerF) -> jlib:iq()
  when
    HandlerF :: fun((jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq()).
try_to_handle_iq(From, To, IQ = #iq{sub_el = SubEl}, HandlerF) ->
    try
        HandlerF(From, To, IQ)
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ?ERROR_MSG("event=handing_iq_failed "
                   "from=~ts to=~ts iq=~1000p "
                   "reason=~p:~p stacktrace=~1000p",
                   [jid:to_binary(From), jid:to_binary(To), IQ,
                    Class, Reason, Stacktrace]),
        IQ#iq{type = error,
              sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]}
    end.
