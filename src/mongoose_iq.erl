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
-export([update_acc_info/1]).
-export([info/1, xmlns/1, command/1]).

-include("mongoose_logger.hrl").
-include("jlib.hrl").

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------

%% @doc Generic error handling code for IQ.
%% Use this function, instead of `try ... catch _:_ -> ok end'.
-spec try_to_handle_iq(jid:jid(), jid:jid(), jlib:iq(), HandlerF) -> jlib:iq()
  when
    HandlerF :: fun((jid:jid(), jid:jid(), jlib:iq()) -> jlib:iq()).
try_to_handle_iq(From, To, IQ = #iq{sub_el = SubEl}, HandlerF) ->
    try
        HandlerF(From, To, IQ)
    catch Class:Reason:StackTrace ->
        ?ERROR_MSG("event=handing_iq_failed "
                   "from=~ts to=~ts iq=~1000p "
                   "reason=~p:~p stacktrace=~1000p",
                   [jid:to_binary(From), jid:to_binary(To), IQ,
                    Class, Reason, StackTrace]),
        IQ#iq{type = error,
              sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]}
    end.

iq_to_sub_el(#iq{sub_el = SubEl}) ->
    SubEl.

empty_result_iq(IQ) ->
    IQ#iq{type = result, sub_el = []}.

%% Fills mongoose_acc with useful IQ information
%% If the info cache is already present, then it is updated only if
%% IQ data ref() doesn't match stanza ref() inside acc.
-spec update_acc_info(mongoose_acc:t()) -> mongoose_acc:t().
update_acc_info(Acc0) ->
    IQRef = mongoose_acc:get(iq, ref, not_exists, Acc0),
    case mongoose_acc:stanza_ref(Acc0) of
        IQRef ->
            % Up to date
            Acc0;
        CurrentRef ->
            El = mongoose_acc:element(Acc0),
            IQ = jlib:iq_query_or_response_info(El),
            Acc1 = mongoose_acc:set(iq, record, IQ, Acc0),
            {XMLNS, Command} = case IQ of
                                   #iq{ xmlns = XMLNS0, sub_el = SubEl } ->
                                       {XMLNS0, sub_el_to_command(SubEl)};
                                   _ ->
                                       {undefined, undefined}
                               end,
            Acc2 = mongoose_acc:set(iq, xmlns, XMLNS, Acc1),
            Acc3 = mongoose_acc:set(iq, command, Command, Acc2),
            mongoose_acc:set(iq, ref, CurrentRef, Acc3)
    end.

%% update_and_get updates only when it is actually necessary
%% and adds IQ data if it's missing

-spec info(mongoose_acc:t()) -> {invalid | not_iq | jlib:iq(), UpdatedAcc :: mongoose_acc:t()}.
info(Acc) -> update_and_get(record, Acc).

-spec xmlns(mongoose_acc:t()) -> {binary() | undefined, UpdatedAcc :: mongoose_acc:t()}.
xmlns(Acc) -> update_and_get(xmlns, Acc).

-spec command(mongoose_acc:t()) -> {binary() | undefined, UpdatedAcc :: mongoose_acc:t()}.
command(Acc) -> update_and_get(command, Acc).

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------

-spec sub_el_to_command([exml:element()] | exml:element()) -> binary() | undefined.
sub_el_to_command([]) -> undefined;
sub_el_to_command([SubEl | _]) -> SubEl#xmlel.name;
sub_el_to_command(#xmlel{ name = Name }) -> Name.

-type iq_acc_field() :: record | xmlns | command.

-spec update_and_get(iq_acc_field(), mongoose_acc:t()) ->
    {FieldValue :: any(), mongoose_acc:t()}.
update_and_get(Field, Acc0) ->
    Acc1 = update_acc_info(Acc0),
    {mongoose_acc:get(iq, Field, Acc1), Acc1}.

