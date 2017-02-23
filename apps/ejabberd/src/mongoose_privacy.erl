%%%-------------------------------------------------------------------
%%% @author bartek <bartlomiej.gorny@erlang-solutions.com>
%%% @copyright (C) 2017, Erlang Solutions sp. z o.o.
%%% @doc Generic functions for checking packet against privacy rules. Works by
%%% calling 'privacy_check_packet' hooks to which various handlers might be attached.
%%%
%%% @end
%%% Created : 06. Feb 2017 11:37
%%%-------------------------------------------------------------------
-module(mongoose_privacy).
-author("bartek").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

%% API
-export([privacy_check_packet/6, privacy_check_packet/7]).

%%% API %%%

%% @doc abbreviated version - in most cases we have 'from' in Acc, but sometimes
%% the caller wants something different (e.g. mod_last which swaps addresses)
-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                           Server :: binary(),
                           User :: binary(),
                           PrivacyList :: userlist(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc, Server, User, PrivacyList, To, Dir) ->
    From = mongoose_acc:get(from_jid, Acc),
    privacy_check_packet(Acc, Server, User, PrivacyList, From, To, Dir).

%% @doc check packet, store result in accumulator, return acc and result for quick check
%% result may vary by recipient, because it may be broadcast to multiple jids
%% so many arguments because that's how hook handlers are implemented:
-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                           Server :: binary(),
                           User :: binary(),
                           PrivacyList :: userlist(),
                           From :: ejabberd:jid(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc, Server, User, PrivacyList, From, To, Dir) ->
    LJid = jid:to_lower(To),
    % check if it is there, if not run a hook and check again if it returned anything,
    % if not then store default value of 'allow'
    case mongoose_acc:retrieve(privacy_check, LJid, Acc) of
        undefined ->
            Packet = mongoose_acc:get(element, Acc),
            Acc1 = ejabberd_hooks:run_fold(privacy_check_packet,
                Server,
                Acc,
                [User,
                 Server,
                 PrivacyList,
                 {From, To, Packet},
                 Dir]),
            case mongoose_acc:retrieve(privacy_check, LJid, Acc1) of
                undefined ->
                    {mongoose_acc:store(privacy_check, LJid, allow, Acc1), allow};
                Res ->
                    {Acc1, Res}
            end;
        Res ->
            {Acc, Res}
    end.

