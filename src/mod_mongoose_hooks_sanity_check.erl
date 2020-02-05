-module(mod_mongoose_hooks_sanity_check).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose_logger.hrl").
-include("jid.hrl").
-include("jlib.hrl").

-export([start/2]).
-export([stop/1]).

-export([user_send_packet_in/4]).
-export([user_send_packet_out/4]).

-spec start(Host :: jid:server(), Opts :: list()) -> ok.
start(Host, _) ->
    ejabberd_hooks:add(hooks(Host)),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    ok.

hooks(Host) ->
    [
     {user_send_packet, Host, ?MODULE, user_send_packet_in, 0}, %% I want it to be called first
     {user_send_packet, Host, ?MODULE, user_send_packet_out, 1000} %% I want it to be called last
    ].


-spec user_send_packet_in(Acc :: mongoose_acc:t(), From :: jid:jid(),
                          To :: jid:jid(),
                          Packet :: exml:element()) -> map().
user_send_packet_in(Acc, From, To, Packet) ->
    Checks = [{fun is_acc/4, "Acc", Acc},
              {fun is_full_jid/4, "From", From},
              {fun is_full_jid/4, "To", To},
              {fun is_exml_element/4, "Packet", Packet}],
    verify_hook(user_send_packet, in, Checks, Acc).

-spec user_send_packet_out(Acc :: mongoose_acc:t(), From :: jid:jid(),
                           To :: jid:jid(),
                           Packet :: exml:element()) -> map().
user_send_packet_out(Acc, _From, _To, _Packet) ->
    verify_hook(user_send_packet, in, [{fun is_acc/4, "Acc", Acc}], Acc).

verify_hook(HookName, Direction, Checks, RetVal) ->
    case run_verifications(HookName, Direction, Checks, []) of
        [] ->
            RetVal;
        Results ->
            ?ERROR_MSG("event=hook_verification_failed, hook=~p, dir=~p, fail_ci_build=true, results=~p",
                       [HookName, Direction, Results]),
            stop
    end.

run_verifications(_, _, [], Result) ->
    Result;
run_verifications(HookName, Direction, [{Fun, ArgName, Arg} | Rest], Result) ->
    case Fun(HookName, Direction, ArgName, Arg) of
        true -> run_verifications(HookName, Direction, Rest, Result);
        Other -> run_verifications(HookName, Direction, Rest, [Other | Result])
    end.

is_acc(_HookName, _Dir, _AccName, #{mongoose_acc := true}) ->
    true;
is_acc(HookName, Dir, AccName, Acc) ->
    ?ERROR_MSG("event=hook_verification_failed, hook=~p, dir=~p, arg=~s, reason=not_an_acc, arg_value=~p",
               [HookName, Dir, AccName, Acc]),
    {not_a_map, AccName, Acc}.

is_full_jid(_HookName, _Direction, _ArgName, #jid{}) ->
    true;
is_full_jid(HookName, Direction, ArgName, ArgValue) ->
    ?ERROR_MSG("event=hook_verification_failed, hook=~p, dir=~p, arg=~s, reason=not_a_jid, arg_value=~p",
               [HookName, Direction, ArgName, ArgValue]),
    {not_a_jid, ArgName, ArgValue}.

is_exml_element(_HookName, _Direction, _ArgName, #xmlel{}) ->
    true;
is_exml_element(HookName, Direction, ArgName, ArgValue) ->
    ?ERROR_MSG("event=hook_verification_failed, hook=~p, dir=~p, arg=~s, reason=not_a_xml_element, arg_value=~p",
               [HookName, Direction, ArgName, ArgValue]),
    {not_a_xml_element, ArgName, ArgValue}.

