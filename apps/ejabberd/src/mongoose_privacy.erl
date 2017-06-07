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
-include_lib("jlib.hrl").
-include("mod_privacy.hrl").

%% API
-export([privacy_check_packet/6, privacy_check_packet/7]).

-type userlist() :: #userlist{}.
-export_type([userlist/0]).

%%% API %%%

%% @doc abbreviated version - in most cases we have 'from' in Acc, but sometimes
%% the caller wants something different (e.g. mod_last which swaps addresses)
-spec privacy_check_packet(Acc :: mongoose_acc:t() | {mongoose_acc:t(), jlib:xmlel()},
                           Server :: binary(),
                           User :: binary(),
                           PrivacyList :: userlist(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc, Server, User, PrivacyList, To, Dir) ->
    A = case Acc of
           {Ac, #xmlel{}} -> Ac;
            _ -> Acc
        end,
    From = mongoose_acc:get(from_jid, A),
    privacy_check_packet(Acc, Server, User, PrivacyList, From, To, Dir).

%% @doc check packet, store result in accumulator, return acc and result for quick check
-spec privacy_check_packet(Acc :: mongoose_acc:t() | {mongoose_acc:t(), jlib:xmlel()},
                           Server :: binary(),
                           User :: binary(),
                           PrivacyList :: userlist(),
                           From :: ejabberd:jid(),
                           To :: ejabberd:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), allow|deny|block}.
privacy_check_packet(Acc0, Server, User, PrivacyList, From, To, Dir) ->
    % see if we have just Acc or also stanza to check - may have different name/type
    {Acc, Name, Type} = case Acc0 of
                       {A, #xmlel{name = SName} = Stanza} ->
                           SType = exml_query:attr(Stanza, <<"type">>, undefined),
                           {A, SName, SType};
                       _ ->
                           {Acc0, mongoose_acc:get(name, Acc0), mongoose_acc:get(type, Acc0)}
                   end,
    % check if it is there, if not then set default and run a hook
    F = jid:to_binary(From),
    T = jid:to_binary(To),
    Key = {cached_privacy_check, Server, User, F, T, Name, Type, Dir},
    case mongoose_acc:get(Key, Acc, undefined) of
        undefined ->
%%            Packet = mongoose_acc:get(to_send, Acc),
            Acc1 = ejabberd_hooks:run_fold(privacy_check_packet,
                                           Server,
                                           mongoose_acc:put(privacy_check, allow, Acc),
                                           [User,
                                            Server,
                                            PrivacyList,
                                            {From, To, Name, Type},
                                            Dir]),
            Res = mongoose_acc:get(privacy_check, Acc1),
            Acc2 = mongoose_acc:remove(privacy_check, Acc1),
            {mongoose_acc:put(Key, Res, Acc2), Res};
        Res ->
            {Acc, Res}
    end.

