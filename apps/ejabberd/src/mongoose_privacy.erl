%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Shared library of privacy-related functions
%%%
%%% @end
%%% Created : 05. Jan 2017 12:01
%%%-------------------------------------------------------------------
-module(mongoose_privacy).
-author("bartek").

-include_lib("ejabberd/include/jlib.hrl").

%% API
-export([privacy_check_packet/6, check_result/2]).

%% @doc A shared function to check privacy on a packet; check results are attached to the
%% stanza as `privacy_check` field, but they are also cached per recipient (a stanza can be)
%% checked multiple times against many recipients, if it is a broadcast)
-spec privacy_check_packet(
    User :: binary(),
    Server :: binary(),
    PrivList :: list(any()),
    Stanza :: map(),
    To :: ejabberd:jid(),
    Dir :: 'in' | 'out') -> map().
privacy_check_packet(User, Server, PrivList, Stanza, To, Dir) ->
    From = mongoose_stanza:get(from_jid, Stanza),
    Stanza1 = case check_result(To, Stanza) of
        undefined ->
            Packet = mongoose_stanza:get(element, Stanza),
            S1 = ejabberd_hooks:run_fold(
                                    privacy_check_packet, Server,
                                    Stanza,
                                    [User,
                                     Server,
                                     PrivList,
                                     {From, To, Packet},
                                     Dir]),
            Res = mongoose_stanza:get(privacy_check, S1, allow),
            Key = {To#jid.user, To#jid.server},
            mongoose_stanza:append(privacy_check_cache, {Key, Res}, S1);
        {ok, Res} ->
            mongoose_stanza:put(privacy_check, Res, Stanza)
    end,
    {ok, Stanza1, Res}.

check_result(To, Stanza) ->
    Key = {To#jid.user, To#jid.server},
    PCheck = mongoose_stanza:get(privacy_check_cache, Stanza, []),
    case proplists:lookup(Key, PCheck) of
        none -> undefined;
        {Key, Res} -> {ok, Res}
    end.
