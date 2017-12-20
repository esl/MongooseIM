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
-xep([{xep, 16}, {version, "1.6"}]).
-xep([{xep, 126}, {version, "1.1"}]).
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_iq_set/4,
         process_iq_get/5,
         get_user_list/3,
         check_packet/6,
         remove_user/2,
         remove_user/3,
         updated_list/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-export_type([list_item/0]).

-type list_name() :: binary().
-type list_item() :: #listitem{}.

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host    :: binary(),
    Opts    :: list().

-callback remove_user(LUser, LServer) -> any() when
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
    gen_mod:start_backend_module(?MODULE, Opts, [get_privacy_list, get_list_names,
                                                 set_default_list, forget_default_list,
                                                 remove_privacy_list, replace_privacy_list,
                                                 get_default_list]),
    mod_privacy_backend:init(Host, Opts),
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
    ejabberd_hooks:add(anonymous_purge_hook, Host,
        ?MODULE, remove_user, 50).

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
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
        ?MODULE, remove_user, 50).

%% Handlers
%% ------------------------------------------------------------------

process_iq_get(Acc,
               _From = #jid{luser = LUser, lserver = LServer},
               _To,
               #iq{xmlns = ?NS_PRIVACY, sub_el = #xmlel{children = Els}},
               #userlist{name = Active}) ->
    Res = case xml:remove_cdata(Els) of
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
          end,
    mongoose_acc:put(iq_result, Res, Acc);
process_iq_get(Val, _, _, _, _) ->
    Val.

process_lists_get(LUser, LServer, Active) ->
    case mod_privacy_backend:get_list_names(LUser, LServer) of
        {ok, {Default, ListNames}} ->
            {result, [list_names_query(Active, Default, ListNames)]};
        {error, not_found} ->
            {result, [empty_list_names_query()]};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_list_get(LUser, LServer, {value, Name}) ->
    case mod_privacy_backend:get_privacy_list(LUser, LServer, Name) of
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

process_iq_set(Acc, From, _To, #iq{xmlns = ?NS_PRIVACY, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{children = Els} = SubEl,
    Res = case xml:remove_cdata(Els) of
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
          end,
    mongoose_acc:put(iq_result, Res, Acc);
process_iq_set(Val, _, _, _) ->
    Val.

process_default_set(LUser, LServer, {value, Name}) ->
    case mod_privacy_backend:set_default_list(LUser, LServer, Name) of
        ok ->
            {result, []};
        {error, not_found} ->
            {error, ?ERR_ITEM_NOT_FOUND};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
process_default_set(LUser, LServer, false) ->
    case mod_privacy_backend:forget_default_list(LUser, LServer) of
        ok ->
            {result, []};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_active_set(LUser, LServer, {value, Name}) ->
    case mod_privacy_backend:get_privacy_list(LUser, LServer, Name) of
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
    case mod_privacy_backend:remove_privacy_list(LUser, LServer, Name) of
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
    case mod_privacy_backend:replace_privacy_list(LUser, LServer, Name, List) of
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
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case mod_privacy_backend:get_default_list(LUser, LServer) of
        {ok, {Default, List}} ->
            NeedDb = is_list_needdb(List),
            #userlist{name = Default, list = List, needdb = NeedDb};
        {error, _} ->
            #userlist{}
    end.

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(Acc, User, Server,
         #userlist{list = List, needdb = NeedDb},
         {From, To, Name, Type},
         Dir) ->
    CheckResult = case List of
        [] ->
            allow;
        _ ->
            PType = packet_directed_type(Dir, packet_type(Name, Type)),
            LJID = case Dir of
                   in -> jid:to_lower(From);
                   out -> jid:to_lower(To)
                   end,
            {Subscription, Groups} =
            case NeedDb of
                true ->
                    Host = jid:nameprep(Server),
                    roster_get_jid_info(Host, User, Server, LJID);
                false ->
                    {[], []}
            end,
            check_packet_aux(List, PType, Type, LJID, Subscription, Groups)
    end,
    mongoose_acc:put(result, CheckResult, Acc).

%% allow error messages
check_packet_aux(_, message, <<"error">>, _JID, _Subscription, _Groups) ->
    allow;
%% if we run of of list items then it is allowed
check_packet_aux([], _PType, _MType, _JID, _Subscription, _Groups) ->
    allow;
%% check packet against next privacy list item
check_packet_aux([Item | List], PType, MType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    do_check_packet_aux(Type, Action, PType, Value, JID, MType, Subscription, Groups, Item, List).

%% list set by blocking commands (XEP-0191) block all communication, both in and out,
%% for a given JID
do_check_packet_aux(jid, block, message, JID, JID, _, _, _, _, _) ->
    block;
do_check_packet_aux(jid, block, message_out, JID, JID, _, _, _, _, _) ->
    block;
%% then we do more complicated checking
do_check_packet_aux(Type, Action, PType, Value, JID, MType, Subscription, Groups, Item, List) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case {is_ptype_match(Item, PType), Type} of
        {true, none} ->
            Action;
        {true, _} ->
            case is_type_match(Type, Value, JID, Subscription, Groups) of
                true ->
                    Action;
                false ->
                    check_packet_aux(List, PType, MType, JID, Subscription, Groups)
            end;
        {false, _} ->
            check_packet_aux(List, PType, MType, JID, Subscription, Groups)
    end.

is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
        true ->
            true;
        false ->
            case PType of
                message ->
                    Item#listitem.match_message;
                message_out ->
                    false; % according to xep-0016, privacy lists do not stop outgoing
                           % messages (so they say)
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

is_type_match(jid, Value, JID, _Subscription, _Groups) ->
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

is_type_match(subscription, Value, _JID, Subscription, _Groups) ->
    Value == Subscription;
is_type_match(group, Value, _JID, _Subscription, Groups) ->
    lists:member(Value, Groups).

remove_user(Acc, User, Server) ->
    R = remove_user(User, Server),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    mod_privacy_backend:remove_user(LUser, LServer).


updated_list(_, #userlist{name = SameName}, #userlist{name = SameName} = New) -> New;
updated_list(_, Old, _) -> Old.


%% Deserialization
%% ------------------------------------------------------------------


packet_directed_type(Dir, Type) ->
    case {Type, Dir} of
         {message, out} -> message_out;
         {message, in} -> message;
         {iq, in} -> iq;
         {presence, in} -> presence_in;
         {presence, out} -> presence_out;
         {_, _} -> other
    end.

packet_type(Name, Type) ->
    case Name of
        <<"message">> -> message;
        <<"iq">> -> iq;
        <<"presence">> ->
            case Type of
                %% notification
                undefined -> presence;
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
    case jid:from_binary(Value) of
        error ->
            false;
        JID ->
            Item#listitem{type = jid, value = jid:to_lower(JID)}
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
    jid -> jid:to_binary(Val);
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
    UserJID = jid:make(LUser, LServer, <<>>),
    ejabberd_sm:route(UserJID, UserJID, broadcast_privacy_list_packet(Name, UserList)).

%% TODO this is dirty
broadcast_privacy_list_packet(Name, UserList) ->
    {broadcast, {privacy_list, UserList, Name}}.

roster_get_jid_info(Host, User, Server, LJID) ->
    ejabberd_hooks:run_fold(
        roster_get_jid_info,
        Host,
        {none, []},
        [User, Server, LJID]).

