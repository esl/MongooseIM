%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%%----------------------------------------------------------------------

-module(mod_privacy).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
     process_iq/3,
     process_iq_set/4,
     process_iq_get/5,
     get_user_list/3,
     check_packet/6,
     remove_user/2,
     updated_list/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-define(BACKEND, (mod_privacy_backend:backend())).

-type list_name() :: binary().
-type list_item() :: #listitem{}.

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host    :: binary(),
    Opts    :: list().

-callback remove_user(LUser, LServer) -> ok when
    LUser   :: binary(),
    LServer :: binary().

-callback get_list_names(LUser, LServer) ->
        {ok, {Default, Names}} | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Default :: list_name(),
    Names   :: list(list_name()),
    Reason  :: not_found | term().

-callback get_privacy_list(LUser, LServer, Name) ->
        {ok, Items} | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Name    :: list_name(),
    Items   :: list(list_item()),
    Reason  :: not_found | term().

-callback set_default_list(LUser, LServer, Name) -> ok | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Name    :: list_name(),
    Reason  :: not_found | term().

-callback forget_default_list(LUser, LServer) -> ok | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Reason  :: not_found | term().

-callback remove_privacy_list(LUser, LServer, Name) -> ok | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Name    :: list_name(),
    Reason  :: conflict | term().

-callback replace_privacy_list(LUser, LServer, Name, Items) ->
        ok | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Name    :: list_name(),
    Items   :: list(list_item()),
    Reason  :: conflict | term().

-callback get_default_list(LUser, LServer) ->
        {ok, {Default, Items}} | {error, Reason} when
    LUser   :: binary(),
    LServer :: binary(),
    Default :: list_name(),
    Items   :: list(list_item()),
    Reason  :: not_found | term().

%% gen_mod callbacks
%% ------------------------------------------------------------------

start(Host, Opts) ->
    start_backend_module(Opts),
    ?BACKEND:init(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(privacy_iq_get, Host,
               ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host,
               ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, Host,
               ?MODULE, get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, Host,
               ?MODULE, check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, Host,
               ?MODULE, updated_list, 50),
    ejabberd_hooks:add(remove_user, Host,
               ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY,
                  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host,
              ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host,
              ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, Host,
              ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, Host,
              ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, Host,
              ?MODULE, updated_list, 50),
    ejabberd_hooks:delete(remove_user, Host,
              ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY).


%% Dynamic modules
%% ------------------------------------------------------------------

start_backend_module(Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    {Mod, Code} = dynamic_compile:from_string(mod_privacy_backend(Backend)),
    code:load_binary(Mod, "mod_privacy_backend.erl", Code).

-spec mod_privacy_backend(atom()) -> string().
mod_privacy_backend(Backend) when is_atom(Backend) ->
    lists:flatten(
      ["-module(mod_privacy_backend).
        -export([backend/0]).  
        -spec backend() -> atom().
        backend() ->
        mod_privacy_",
           atom_to_list(Backend),
           ".\n"]).

%% Handlers
%% ------------------------------------------------------------------

process_iq(_From, _To, IQ) ->
    SubEl = IQ#iq.sub_el,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq_get(_,
        _From = #jid{luser = LUser, lserver = LServer},
        _To,
        #iq{sub_el = #xmlel{children = Els}},
        #userlist{name = Active}) ->
    case xml:remove_cdata(Els) of
        [] ->
            process_lists_get(LUser, LServer, Active);
        [#xmlel{name = Name, attrs = Attrs}] ->
            case Name of
                <<"list">> ->
                    ListName = xml:get_attr(<<"name">>, Attrs),
                    process_list_get(LUser, LServer, ListName);
                _ ->
                    {error, ?ERR_BAD_REQUEST}
            end;
        _ ->
            {error, ?ERR_BAD_REQUEST}
    end.

process_lists_get(LUser, LServer, Active) ->
    case ?BACKEND:get_list_names(LUser, LServer) of
        {ok, {Default, ListNames}} ->
            {result, [list_names_query(Active, Default, ListNames)]};
        {error, not_found} ->
            {result, [empty_list_names_query()]};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_list_get(LUser, LServer, {value, Name}) ->
    case ?BACKEND:get_privacy_list(LUser, LServer, Name) of
        {ok, List} ->
            LItems = lists:map(fun item_to_xml/1, List),
            {result, [list_query_result(Name, LItems)]};
        {error, not_found} ->
            {error, ?ERR_ITEM_NOT_FOUND};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
process_list_get(_LUser, _LServer, false) ->
    {error, ?ERR_BAD_REQUEST}.

process_iq_set(_, From, _To, #iq{sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = Name, attrs = Attrs, children = SubEls}] ->
            ListName = xml:get_attr(<<"name">>, Attrs),
            case Name of
                <<"list">> ->
                    process_list_set(LUser, LServer, ListName,
                             xml:remove_cdata(SubEls));
                <<"active">> ->
                    process_active_set(LUser, LServer, ListName);
                <<"default">> ->
                    process_default_set(LUser, LServer, ListName);
                _ ->
                    {error, ?ERR_BAD_REQUEST}
            end;
        _ ->
            {error, ?ERR_BAD_REQUEST}
    end.

process_default_set(LUser, LServer, {value, Name}) ->
    case ?BACKEND:set_default_list(LUser, LServer, Name) of
        ok ->
            {result, []};
        {error, not_found} ->
            {error, ?ERR_ITEM_NOT_FOUND};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
process_default_set(LUser, LServer, false) ->
    case ?BACKEND:forget_default_list(LUser, LServer) of
        ok ->
            {result, []};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_active_set(LUser, LServer, {value, Name}) ->
    case ?BACKEND:get_privacy_list(LUser, LServer, Name) of
        {ok, List} ->
            NeedDb = is_list_needdb(List),
            {result, [], #userlist{name = Name, list = List, needdb = NeedDb}};
        {error, not_found} ->
            {error, ?ERR_ITEM_NOT_FOUND};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}}.

process_list_set(LUser, LServer, {value, Name}, Els) ->
    case parse_items(Els) of
        false ->
            {error, ?ERR_BAD_REQUEST};
        remove ->
            remove_privacy_list(LUser, LServer, Name);
        List ->
            replace_privacy_list(LUser, LServer, Name, List)
    end;
process_list_set(_LUser, _LServer, false, _Els) ->
    {error, ?ERR_BAD_REQUEST}.

remove_privacy_list(LUser, LServer, Name) ->
    case ?BACKEND:remove_privacy_list(LUser, LServer, Name) of
        ok ->
            UserList = #userlist{name = Name, list = []},
            broadcast_privacy_list(LUser, LServer, Name, UserList),
            {result, []};
        %% TODO if Name == Active -> conflict
        {error, conflict} ->
            {error, ?ERR_CONFLICT};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

replace_privacy_list(LUser, LServer, Name, List) ->
    case ?BACKEND:replace_privacy_list(LUser, LServer, Name, List) of
        ok ->
            NeedDb = is_list_needdb(List),
            UserList = #userlist{name = Name, list = List, needdb = NeedDb},
            broadcast_privacy_list(LUser, LServer, Name, UserList),
            {result, []};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

is_list_needdb(Items) ->
    lists:any(fun is_item_needdb/1, Items).

is_item_needdb(#listitem{type = subscription}) -> true;
is_item_needdb(#listitem{type = group})        -> true;
is_item_needdb(_)                              -> false.

get_user_list(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case ?BACKEND:get_default_list(LUser, LServer) of
        {ok, {Default, List}} ->
            NeedDb = is_list_needdb(List),
            #userlist{name = Default, list = List, needdb = NeedDb};
        {error, _} ->
            #userlist{}
    end.

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(_, User, Server,
         #userlist{list = List, needdb = NeedDb},
         {From, To, Packet},
         Dir) ->
    case List of
        [] ->
            allow;
        _ ->
            PType = packet_directed_type(Dir, packet_type(Packet)),
            LJID = case Dir of
                   in -> jlib:jid_tolower(From);
                   out -> jlib:jid_tolower(To)
               end,
            {Subscription, Groups} =
            case NeedDb of
                true ->
                    Host = jlib:nameprep(Server),
                    roster_get_jid_info(Host, User, Server, LJID);
                false ->
                    {[], []}
            end,
            check_packet_aux(List, PType, LJID, Subscription, Groups)
    end.

check_packet_aux([], _PType, _JID, _Subscription, _Groups) ->
    allow;
check_packet_aux([Item | List], PType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case is_ptype_match(Item, PType) of
        true ->
            case Type of
                none ->
                    Action;
                _ ->
                    case is_type_match(Type, Value, JID, Subscription, Groups) of
                        true ->
                            Action;
                        false ->
                            check_packet_aux(
                                List, PType, JID, Subscription, Groups)
                    end
            end;
        false ->
            check_packet_aux(List, PType, JID, Subscription, Groups)
    end.

is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
        true ->
            true;
        false ->
            case PType of
                message ->
                    Item#listitem.match_message;
                iq ->
                    Item#listitem.match_iq;
                presence_in ->
                    Item#listitem.match_presence_in;
                presence_out ->
                    Item#listitem.match_presence_out;
                other ->
                    false
            end
    end.

is_type_match(Type, Value, JID, Subscription, Groups) ->
    case Type of
        jid ->
            case Value of
                {<<>>, Server, <<>>} ->
                    case JID of
                        {_, Server, _} ->
                            true;
                        _ ->
                            false
                    end;
                {User, Server, <<>>} ->
                    case JID of
                        {User, Server, _} ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    Value == JID
            end;
        subscription ->
            Value == Subscription;
        group ->
            lists:member(Value, Groups)
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    ?BACKEND:remove_user(LUser, LServer).


updated_list(_,
         #userlist{name = OldName} = Old,
         #userlist{name = NewName} = New) ->
    if
    OldName == NewName ->
        New;
    true ->
        Old
    end.


%% Deserialization
%% ------------------------------------------------------------------

packet_directed_type(Dir, Type) ->
    case {Type, Dir} of
         {message, in} -> message;
         {iq, in} -> iq;
         {presence, in} -> presence_in;
         {presence, out} -> presence_out;
         {_, _} -> other
    end.

packet_type(#xmlel{name = Name, attrs = Attrs}) ->
    case Name of
        <<"message">> -> message;
        <<"iq">> -> iq;
        <<"presence">> ->
            case xml:get_attr_s(<<"type">>, Attrs) of
                %% notification
                <<>> -> presence;
                <<"unavailable">> -> presence;
                %% subscribe, subscribed, unsubscribe,
                %% unsubscribed, error, probe, or other
                _ -> other
            end
    end.

parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    %% Sort the items by their 'order' attribute
    lists:keysort(#listitem.order, Res);
parse_items([#xmlel{name = <<"item">>, attrs = Attrs,
                    children = SubEls} | Els], Res) ->
    Type    = xml:get_attr_s(<<"type">>,   Attrs),
    Value   = xml:get_attr_s(<<"value">>,  Attrs),
    SAction = xml:get_attr_s(<<"action">>, Attrs),
    SOrder  = xml:get_attr_s(<<"order">>,  Attrs),
    Action  = parse_action(SAction),
    Order   = parse_order(SOrder),
    I1 = set_action_and_order(Action, Order),
    I2 = set_type_and_value(Type, Value, I1),
    I3 = set_matches(SubEls, I2),
    parse_items(Els, add_item(I3, Res));
parse_items(_, _Res) ->
    false.

parse_action(<<>>) ->
    false;
parse_action(Action) ->
    binary_to_action_s(Action).

parse_order(<<>>) ->
    false;
parse_order(Order) ->
    validate_order(binary_to_order_s(Order)).

validate_order(Order) when Order >= 0 ->
    Order;
validate_order(_) ->
    false.

set_action_and_order(false, _) ->
    false;
set_action_and_order(_, false) ->
    false;
set_action_and_order(Action, Order) when is_atom(Action), is_integer(Order) ->
    #listitem{action = Action, order = Order}.

set_type_and_value(_Type, _Value, false) ->
    false;
set_type_and_value(<<>>, _Value, Item) ->
    Item;
set_type_and_value(_Type, <<>>, _Item) ->
    false;
set_type_and_value(<<"jid">>, Value, Item) ->
    case jlib:binary_to_jid(Value) of
        error ->
            false;
        JID ->
            Item#listitem{type = jid, value = jlib:jid_tolower(JID)}
    end;
set_type_and_value(<<"group">>, Value, Item) ->
    Item#listitem{type = group, value = Value};
set_type_and_value(<<"subscription">>, Value, Item) ->
    case Value of
        <<"none">> ->
            Item#listitem{type = subscription, value = none};
        <<"both">> ->
            Item#listitem{type = subscription, value = both};
        <<"from">> ->
            Item#listitem{type = subscription, value = from};
        <<"to">> ->
            Item#listitem{type = subscription, value = to};
        _ ->
        false
    end.

set_matches(_SubEls, false) ->
    false;
set_matches(SubEls, Item) ->
    parse_matches(Item, xml:remove_cdata(SubEls)).

parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) ->
    parse_matches1(Item, Els).

parse_matches1(Item, []) ->
    Item;
parse_matches1(Item, [#xmlel{name = <<"message">>} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"iq">>} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"presence-in">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"presence-out">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [#xmlel{} | _Els]) ->
    false.

add_item(false, Items) ->
    Items;
add_item(Item, Items) ->
    [Item | Items].


%% Serialization
%% ------------------------------------------------------------------

empty_list_names_query() ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}]}.

list_names_query(Active, Default, ListNames) ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
        children = list_names(Active, Default, ListNames)}.

list_names(Active, Default, ListNames) ->
    [list_name(<<"active">>, Active) || Active =/= none] ++
    [list_name(<<"default">>, Default) || Default =/= none] ++
    [list_name(<<"list">>, ListName) || ListName <- ListNames].

list_name(Type, Name) ->
    #xmlel{name = Type, attrs = [{<<"name">>, Name}]}.

list_query_result(Name, LItems) ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
        children = [
            #xmlel{
                name = <<"list">>,
                attrs = [{<<"name">>, Name}],
                children = LItems}]}.

item_to_xml(Item) ->
    #xmlel{
        name = <<"item">>,
        attrs = item_to_xml_attrs(Item),
        children = item_to_xml_children(Item)}.

item_to_xml_attrs(Item=#listitem{type=none}) ->
    item_to_xml_attrs1(Item);
item_to_xml_attrs(Item=#listitem{type=Type, value=Value}) ->
    [{<<"type">>, type_to_binary(Type)},
     {<<"value">>, value_to_binary(Type, Value)}
     | item_to_xml_attrs1(Item)].

item_to_xml_attrs1(#listitem{action=Action, order=Order}) ->
    [{<<"action">>, action_to_binary(Action)},
     {<<"order">>, order_to_binary(Order)}].
    
item_to_xml_children(#listitem{match_all=true}) ->
    [];
item_to_xml_children(#listitem{match_all=false,
        match_iq=MatchIQ,
        match_message=MatchMessage,
        match_presence_in=MatchPresenceIn,
        match_presence_out=MatchPresenceOut}) ->
       [#xmlel{name = <<"message">>}        || MatchMessage]
    ++ [#xmlel{name = <<"presence-in">>}    || MatchPresenceIn]
    ++ [#xmlel{name = <<"presence-out">>}   || MatchPresenceOut]
    ++ [#xmlel{name = <<"iq">>}             || MatchIQ].

action_to_binary(Action) ->
    case Action of
    allow -> <<"allow">>;
    deny -> <<"deny">>
    end.

order_to_binary(Order) ->
    list_to_binary(integer_to_list(Order)).

binary_to_order(Binary) ->
    list_to_integer(binary_to_list(Binary)).

type_to_binary(Type) ->
    case Type of
    jid -> <<"jid">>;
    group -> <<"group">>;
    subscription -> <<"subscription">>
    end.

value_to_binary(Type, Val) ->
    case Type of
    jid -> jlib:jid_to_binary(Val);
    group -> Val;
    subscription ->
        case Val of
        both -> <<"both">>;
        to   -> <<"to">>;
        from -> <<"from">>;
        none -> <<"none">>
        end
    end.

binary_to_action(S) ->
    case S of
    <<"allow">> -> allow;
    <<"deny">> -> deny
    end.

binary_to_action_s(Action) ->
    try
        binary_to_action(Action)
    catch error:_ ->
        false
    end.

binary_to_order_s(Order) ->
    try
        binary_to_order(Order)
    catch error:_ ->
        false
    end.


%% Ejabberd
%% ------------------------------------------------------------------

broadcast_privacy_list(LUser, LServer, Name, UserList) ->
    UserJID = jlib:make_jid(LUser, LServer, <<>>),
    ejabberd_router:route(UserJID, UserJID, broadcast_privacy_list_packet(Name, UserList)).

%% TODO this is dirty
broadcast_privacy_list_packet(Name, UserList) ->
    #xmlel{
        name = <<"broadcast">>,
        children = [{privacy_list, UserList, Name}]}.

roster_get_jid_info(Host, User, Server, LJID) ->
    ejabberd_hooks:run_fold(
        roster_get_jid_info,
        Host,
        {none, []},
        [User, Server, LJID]).

