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
-export([start/2, stop/1, filter_packet/1]).

start(Host, _Opts) ->
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_packet, 90).

stop(Host) ->
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_packet, 90).

filter_packet({_From, _To, _Acc, #xmlel{name = StanzaType}} = Input) ->
    AccessRule = case StanzaType of
		     <<"presence">> -> mod_filter_presence;
		     <<"message">> -> mod_filter_message;
		     <<"iq">> -> mod_filter_iq
		 end,
    check_stanza_type(<<"from">>, AccessRule, Input).

check_stanza_type(Direction, AccessRule, {From = #jid{lserver = HostFrom}, 
                                          To = #jid{lserver = HostTo},
                                          _Acc,
                                          _Packet} = Input) ->
    Access = case Direction of
                <<"from">> -> acl:match_rule(HostFrom, AccessRule, From);
                <<"to">> -> acl:match_rule(HostTo, AccessRule, To)
            end,
    check_access(Direction, AccessRule, Access, Input).

%% If the access results in 'allow' or 'deny', treat that as the
%% result. Else it is a rule to be applied to the receiver.
check_access(Direction, AccessRule, Access, Input) when Access =:= allow ->
    case AccessRule of 
        mod_filter -> Input;
        _ -> check_stanza_type(Direction, mod_filter, Input)   
    end;
check_access(_Direction, _AccessRule, Access, _Input) when Access =:= deny ->
    drop;
check_access(_Direction, AccessRule, Access, Input) ->
    case AccessRule of 
        mod_filter -> Input;
        _ -> check_stanza_type(<<"to">>, Access, Input)   
    end.