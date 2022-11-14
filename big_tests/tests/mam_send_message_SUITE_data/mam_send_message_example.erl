%% Adds some_hash element to each extracted message result.
%%
%% An example module for extending MAM lookup results.
%% Defines a callback for send_message callback.
%% Handles lookup messages hooks to extend message rows with extra info.
-module(mam_send_message_example).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-include_lib("exml/include/exml.hrl").

-export([start/2,
         stop/1,
         supported_features/0,
         lookup_messages/3,
         send_message/4]).


start(HostType, _Opts) ->
    gen_hook:add_handlers(hooks(HostType)).

stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)).

supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [{mam_lookup_messages, HostType, fun ?MODULE:lookup_messages/3, #{}, 60},
     {mam_muc_lookup_messages, HostType, fun ?MODULE:lookup_messages/3, #{}, 60}].

%% caller_jid could be used for privacy checking or per-user customization
lookup_messages({error, _Reason} = Result, _Params, _Extra) ->
    {ok, Result};
lookup_messages({ok, {TotalCount, Offset, MessageRows}},
                #{owner_jid := ArcJID, caller_jid := _CallerJID} = _Params,
                #{host_type := HostType}) ->
    MessageRows2 = [extend_message(HostType, ArcJID, Row) || Row <- MessageRows],
    {ok, {ok, {TotalCount, Offset, MessageRows2}}}.

extend_message(_HostType, _ArcJID, Row = #{}) ->
    %% Extend a message with a new field
    %% Usually extracted from a DB
    Row#{some_hash => erlang:phash2(Row, 32)}.

send_message(Row, From, To, Mess) ->
    Res = xml:get_subtag(Mess, <<"result">>),
    Res2 = xml:append_subtags(Res, [new_subelem(Row)]),
    Mess2 = xml:replace_subelement(Mess, Res2),
    mod_mam_utils:send_message(Row, From, To, Mess2).

new_subelem(#{some_hash := SomeHash}) ->
    #xmlel{name = <<"some_hash">>, attrs = [{<<"value">>, integer_to_binary(SomeHash)}]}.
