%%%-------------------------------------------------------------------
%%% File    : mod_rest.erl
%%% Author  : Nolan Eakins <sneakin@semanticgap.com>
%%% Purpose : Provide an HTTP interface to POST stanzas into ejabberd
%%%
%%% Copyright (C) 2008 Nolan Eakins
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_rest).
-author('sneakin@semanticgap.com').

-behavior(gen_mod).

-export([start/2,stop/1]).



-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_ctl.hrl").

-define(DEFAULT_MAX_AGE, 1728000).  %% 20 days in seconds
-define(REST_LISTENER, mod_rest). 

%% ejabberd_cowboy API
-export ([cowboy_router_paths/1]).

%% response callbacks 
-export([init/3,
         allowed_methods/2,
         handle/2,
         terminate/3]).


%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
start(_Host, Opts) ->
    start_cowboy(Opts),
    ok.

stop(_Host) ->
    cowboy:stop_listener(?REST_LISTENER).


%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

cowboy_router_paths(BasePath)->
    [
     {BasePath, ?REST_LISTENER,[process2]}
    ].

start_cowboy(Opts) ->
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 10),
    IP = gen_mod:get_opt(ip, Opts, {0,0,0,0}),
    case gen_mod:get_opt(port, Opts, undefined) of
        undefined ->
            ok;
        Port ->
            Dispatch = cowboy_router:compile([{'_',cowboy_router_paths("/rest")}]),
            case cowboy:start_http(?REST_LISTENER, NumAcceptors,
                                   [{port, Port}, {ip, IP}],
                                   [{env, [{dispatch, Dispatch}]}]) of
                {error, {already_started, _Pid}} ->
                    ok;
                {ok, _Pid} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

terminate(_Reason, _Req, _State) ->
          ok.
%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
init(_Transport, Req, _Opts) ->
    {ok, Req, no_state}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

handle(Req, no_state) -> 
    {Host,R2} = cowboy_req:host(Req),
    {ClientIP,R3} = cowboy_req:peer(R2),
    {ok, Data, R4} = cowboy_req:body(R3), 
    {ClientAddress,_}=ClientIP,
    try
	    check_member_option(Host, ClientAddress, allowed_ips),
        {Code,Opts,Response} = maybe_post_request(Data,Host,ClientIP),
        {ok,Resp} = cowboy_req:reply(Code,Opts,Response,R4),
        {ok,Resp,no_state}
    catch 
        error:{badmatch, _ } = Error -> 
	        ?DEBUG("Error when processing REST request: ~nData: ~p~nError: ~p", [Data, Error]),
            {ok,R} = cowboy_req:reply(406, [], <<"Error: REST request is rejected by service.">>, R4),
            {ok,R,no_state}
    end.

%% If the first character of Data is <, it is considered a stanza to deliver.
%% Otherwise, it is considered an ejabberd command to execute.
maybe_post_request(<<"<",_/binary>> = Data, Host, ClientIp) ->
    try
	Stanza = xml_stream:parse_element(Data),
        From = jlib:binary_to_jid(xml:get_tag_attr_s(<<"from">>, Stanza)),
        To = jlib:binary_to_jid(xml:get_tag_attr_s(<<"to">>, Stanza)),
	allowed = check_stanza(Stanza, From, To, Host),
	?INFO_MSG("Got valid request from ~s~nwith IP ~p~nto ~s:~n~p",
		  [jlib:jid_to_binary(From),
		   ClientIp,
		   jlib:jid_to_binary(To),
		   Stanza]),
	post_request(Stanza, From, To)
    catch
	error:{badmatch, _} = Error ->
	    ?DEBUG("Error when processing REST request: ~nData: ~p~nError: ~p", [Data, Error]),
	    {406, [], "Error: REST request is rejected by service."};
	error:{Reason, _} = Error ->
	    ?DEBUG("Error when processing REST request: ~nData: ~p~nError: ~p", [Data, Error]),
	    {500, [], "Error: " ++ atom_to_list(Reason)};
	Error ->
	    ?DEBUG("Error when processing REST request: ~nData: ~p~nError: ~p", [Data, Error]),
	    {500, [], "Error"}
    end;    
maybe_post_request(Data, Host, _ClientIp) ->
    ?INFO_MSG("Data: ~p", [Data]),
    Args = split_line(binary_to_list(Data)),
    AccessCommands = get_option_access(Host),
    case ejabberd_ctl:process2(Args, AccessCommands) of
	{"", ?STATUS_SUCCESS} ->
	    {200, [], integer_to_list(?STATUS_SUCCESS)};
	{String, ?STATUS_SUCCESS} ->
	    {200, [], String};
	{"", Code} ->
	    {200, [], integer_to_list(Code)};
	{String, _Code} ->
	    {200, [], String}
    end.

%% This function throws an error if the module is not started in that VHost.
try_get_option(Host, OptionName, DefaultValue) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true -> ok;
	_ -> throw({module_must_be_started_in_vhost, ?MODULE, Host})
    end,
    gen_mod:get_module_opt(Host, ?MODULE, OptionName, DefaultValue).

get_option_access(Host) ->
    try_get_option(Host, access_commands, []).

%% This function crashes if the stanza does not satisfy configured restrictions
check_stanza(Stanza, _From, To, Host) ->
    check_member_option(Host, binary_to_list(jlib:jid_to_binary(To)), allowed_destinations),
    #xmlel{name = StanzaType} = Stanza,
    check_member_option(Host, binary_to_list(StanzaType), allowed_stanza_types),
    allowed.

check_member_option(Host, Element, Option) ->
    true = case try_get_option(Host, Option, all) of
	       all -> true;
	       AllowedValues -> lists:member(Element, AllowedValues)
	   end.

post_request(Stanza, From, To) ->
    case ejabberd_router:route(From, To, Stanza) of
	ok -> {200, [], "Ok"};
        _ -> {500, [], "Error"}
    end.

%% Split a line into args. Args are splitted by blankspaces. Args can be enclosed in "".
%%
%% Example call:
%% mod_rest:split_line("  a1 b2 \"c3 d4\"e5\" c6   d7 \\\"  e8\"f9   g0 \\\" h1  ").
%% ["a1","b2","c3 d4\"e5","c6","d7","  e8\"f9   g0 ","h1"]
%%
%% 32 is the integer that represents the blankspace
%% 34 is the integer that represents the double quotes: "
%% 92 is the integer that represents the backslash: \
split_line(Line) -> split(Line, "", []).
split("", "", Args) -> lists:reverse(Args);
split("", Arg, Args) -> split("", "", [lists:reverse(Arg) | Args]);
split([32 | Line], "", Args) -> split(Line, [], Args);
split([32 | Line], Arg, Args) -> split(Line, [], [lists:reverse(Arg) | Args]);
split([34 | Line], "", Args) -> {Line2, Arg2} = splitend(Line), split([32 | Line2], Arg2, Args);
split([92, 34 | Line], "", Args) -> {Line2, Arg2} = splitend(Line), split([32 | Line2], Arg2, Args);
split([Char | Line], Arg, Args) -> split(Line, [Char | Arg], Args).
splitend(Line) -> splitend(Line, []).
splitend([], Res) -> {"", Res};
splitend([34], Res) -> {"", Res};
splitend([92, 34], Res) -> {"", Res};
splitend([34, 32 | Line], Res) -> {Line, Res};
splitend([92, 34, 32 | Line], Res) -> {Line, Res};
splitend([Char | Line], Res) -> splitend(Line, [Char | Res]).
