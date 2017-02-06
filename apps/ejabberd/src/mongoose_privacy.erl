%%%-------------------------------------------------------------------
%%% @author bartek <bartlomiej.gorny@erlang-solutions.com>
%%% @copyright (C) 2017, Erlang Solutions sp. z o.o.
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2017 11:37
%%%-------------------------------------------------------------------
-module(mongoose_privacy).
-author("bartek").

%% API
-export([privacy_check_packet/8]).

%%% API %%%

-spec privacy_check_packet(Acc :: mongoose_acc:t(),
    Server :: binary(),
    User :: binary(),
    PrivacyList :: list(),
    From :: ejabberd:jid(),
    To :: ejabberd:jid(),
    Packet :: jlib:xmlel() | mongoose_acc:t(),
    Dir :: 'in' | 'out') -> {mongoose_acc:t(), allow|deny|block}.
%% @doc check packet, store result in accumulator, return acc and result for quick check
%% result may vary by recipient, because it may be broadcast to multiple jids
%% so many arguments because that's how hook handlers are implemented:
privacy_check_packet(Acc, Server, User, PrivacyList, From, To, Packet, Dir) ->
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

