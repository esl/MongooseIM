%% @doc Functions to work with iq record.
%%
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
%%
-module(mongoose_iq).
-export([try_to_handle_iq/4,
         iq_to_sub_el/1,
         empty_result_iq/1]).

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

iq_to_sub_el(#iq{sub_el = SubEl}) ->
    SubEl.

empty_result_iq(IQ) ->
    IQ#iq{type = result, sub_el = []}.
