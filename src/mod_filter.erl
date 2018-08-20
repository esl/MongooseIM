%%%----------------------------------------------------------------------
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : flexible filtering by server policy
%%% Created : 21 Sep 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%% Updated : 14 Jan 2016 by John Brodie <john@brodie.me>
%%% Updated : 25 Jul 2018 by Beisenbek Baisakov <beysenbek@gmail.com>
%%%----------------------------------------------------------------------

-module(mod_filter).
-behaviour(gen_mod).
-include("jlib.hrl").
-include("jid.hrl").
-include("mongoose.hrl").
-export([start/2,stop/1,
         filter_packet/1]).

start(Host, _Opts) ->
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90).

stop(Host) ->
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90).

-type fpacket() :: {From :: jid:jid(),
                    To :: jid:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: exml:element()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop) ->   
    drop;
filter_packet({From, To, Acc, Packet} = Input) ->
    X = case check_stanza(Input) of
        drop -> 
            drop;
        _ -> Input
    end,
    X.

check_stanza({From,To,Acc,#xmlel{name = StanzaType}} = Input) ->
    AccessRule = case StanzaType of
		     <<"presence">> -> mod_filter_presence;
		     <<"message">> -> mod_filter_message;
		     <<"iq">> -> mod_filter_iq
		 end,
    check_stanza_type(AccessRule, Input).

check_stanza_type(AccessRule, {From = #jid{lserver = HostFrom}, To = #jid{lserver = HostTo}, Acc, Packet} = Input) ->
    FromAccess = acl:match_rule(HostFrom, AccessRule, From),
    case FromAccess of
        allow -> check_access(Input);
        deny -> 
            drop;
        ToAccessRule -> 
            ToAccess = acl:match_rule(HostTo, ToAccessRule, To),
            case ToAccess of
                allow -> check_access(Input);
                deny -> drop
            end
    end.

check_access({From = #jid{lserver = HostFrom}, To = #jid{lserver = HostTo}, Acc, Packet} = Input) ->
    FromAccess = acl:match_rule(HostFrom, mod_filter, From),
    %% If the rule results in 'allow' or 'deny', treat that as the
    %% result.  Else it is a rule to be applied to the receiver.
    case FromAccess of
        allow -> Input;
        deny -> drop;
        ToAccessRule ->
            ToAccess = acl:match_rule(HostTo, ToAccessRule, To),
            case ToAccess of
                allow -> Input;
                deny -> drop
            end
    end.