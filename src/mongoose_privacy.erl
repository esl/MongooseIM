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

-include("jlib.hrl").
-include("mod_privacy.hrl").

%% API
-export([privacy_check_packet/5, privacy_check_packet/6]).

-type userlist() :: #userlist{}.
-type decision() :: allow | deny | block.
-export_type([userlist/0]).
-export_type([decision/0]).

%%% API %%%

%% @doc abbreviated version - in most cases we have 'from' in Acc, but sometimes
%% the caller wants something different (e.g. mod_last which swaps addresses)
%% the first arg can be accumulator, if we are checking its element, or a tuple
%% {Acc, El} for cases where one acc triggers sending many messages which have to be checked
-spec privacy_check_packet(Acc :: mongoose_acc:t() | {mongoose_acc:t(), exml:element()},
                           JID :: jid:jid(),
                           PrivacyList :: userlist(),
                           To :: jid:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), decision()}.
privacy_check_packet(Acc0, JID, PrivacyList, To, Dir) ->
    Acc1 = case Acc0 of
           {Acc, #xmlel{}} -> Acc;
            _ -> Acc0
        end,
    From = mongoose_acc:from_jid(Acc1),
    privacy_check_packet(Acc0, JID, PrivacyList, From, To, Dir).

%% @doc check packet, store result in accumulator, return acc and result for quick check
%% Acc can be either a single argument (an Accumulator) or a tuple of {Acc, Stanza}
%% in the latter case name and type to check against privacy lists are taken from the Stanza
-spec privacy_check_packet(Acc :: mongoose_acc:t() | {mongoose_acc:t(), exml:element()},
                           JID :: jid:jid(),
                           PrivacyList :: userlist(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Dir :: 'in' | 'out') -> {mongoose_acc:t(), decision()}.
privacy_check_packet(Acc0, #jid{luser = LUser, lserver = LServer} = JID,
                     PrivacyList, From, To, Dir) ->
    % see if we have just Acc or also stanza to check - may have different name/type
    {Acc, Name, Type} = case Acc0 of
                       {A, #xmlel{name = SName} = Stanza} ->
                           SType = exml_query:attr(Stanza, <<"type">>, undefined),
                           {A, SName, SType};
                       _ ->
                           {Acc0, mongoose_acc:stanza_name(Acc0), mongoose_acc:stanza_type(Acc0)}
                   end,
    % check if it is there, if not then set default and run a hook
    Key = {cached_check, LServer, LUser, From, To, Name, Type, Dir},
    case mongoose_acc:get(privacy, Key, undefined, Acc) of
        undefined ->
            Acc1 = mongoose_hooks:privacy_check_packet(Acc, JID, PrivacyList,
                                                       {From, To, Name, Type}, Dir),
            Res = mongoose_acc:get(hook, result, Acc1),
            {mongoose_acc:set(privacy, Key, Res, Acc1), Res};
        Res ->
            {Acc, Res}
    end.
