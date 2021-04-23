-module(mod_filter).
-behaviour(gen_mod).
-include("jlib.hrl").
-include("mongoose.hrl").
-export([start/2, stop/1, filter_global_packet/1, filter_local_packet/1]).

start(Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, Host, ?MODULE, filter_global_packet, 90),
    ejabberd_hooks:add(filter_local_packet, Host, ?MODULE, filter_local_packet, 90).

stop(Host) ->
    ejabberd_hooks:delete(filter_packet, Host, ?MODULE, filter_global_packet, 90),
    ejabberd_hooks:delete(filter_local_packet, Host, ?MODULE, filter_local_packet, 90).
    
filter_global_packet(Input) ->
   filter_packet(global, Input).

filter_local_packet({_From, _To = #jid{lserver = Host}, _Acc, _Packet} = Input) ->
   filter_packet(Host, Input).

filter_packet(Host, {From, To, _Acc, #xmlel{name = StanzaType}} = Input) ->
    case StanzaType of
		     <<"message">> -> 
                              Access = acl:match_rule(Host, mod_filter_message, From),
                              check_access(Host, To, Access, Input);
		     _ ->  Input
	end.
    
-spec check_access(Server :: binary() | global,
                   User :: binary(), 
                   Access :: allow | deny | term(), 
                   FilterInput :: binary()) ->
     drop | binary().
check_access(_Host, _To, deny, _Input) ->
    drop;
check_access(_Host, _To, allow, Input) ->
    Input;
check_access(Host, To, NestedAccessRule, Input) ->
    Access = acl:match_rule(Host, NestedAccessRule, To),
    check_access(Host, To, Access, Input).