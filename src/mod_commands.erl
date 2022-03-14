-module(mod_commands).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/0, stop/0, supported_features/0,
         start/2, stop/1,
         register/3,
         unregister/2,
         registered_commands/0,
         registered_users/1,
         change_user_password/3,
         list_sessions/1,
         list_contacts/1,
         add_contact/2,
         add_contact/3,
         add_contact/4,
         delete_contacts/2,
         delete_contact/2,
         subscription/3,
         set_subscription/3,
         kick_session/3,
         get_recent_messages/3,
         get_recent_messages/4,
         send_message/3,
         send_stanza/1
        ]).

-ignore_xref([add_contact/2, add_contact/3, add_contact/4, change_user_password/3,
              delete_contact/2, delete_contacts/2, get_recent_messages/3,
              get_recent_messages/4, kick_session/3, list_contacts/1,
              list_sessions/1, register/3, registered_commands/0, registered_users/1,
              send_message/3, send_stanza/1, set_subscription/3, start/0, stop/0,
              subscription/3, unregister/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("session.hrl").

start() ->
    mongoose_commands:register(commands()).

stop() ->
    mongoose_commands:unregister(commands()).

start(_, _) -> start().
stop(_) -> stop().

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%%
%%% mongoose commands
%%%

commands() ->
    [
     [
      {name, list_methods},
      {category, <<"commands">>},
      {desc, <<"List commands">>},
      {module, ?MODULE},
      {function, registered_commands},
      {action, read},
      {args, []},
      {result, []}
     ],
     [
      {name, list_users},
      {category, <<"users">>},
      {desc, <<"List registered users on this host">>},
      {module, ?MODULE},
      {function, registered_users},
      {action, read},
      {args, [{host, binary}]},
      {result, []}
     ],
     [
      {name, register_user},
      {category, <<"users">>},
      {desc, <<"Register a user">>},
      {module, ?MODULE},
      {function, register},
      {action, create},
      {args, [{host, binary}, {username, binary}, {password, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, unregister_user},
      {category, <<"users">>},
      {desc, <<"UnRegister a user">>},
      {module, ?MODULE},
      {function, unregister},
      {action, delete},
      {args, [{host, binary}, {user, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, list_sessions},
      {category, <<"sessions">>},
      {desc, <<"Get session list">>},
      {module, ?MODULE},
      {function, list_sessions},
      {action, read},
      {args, [{host, binary}]},
      {result, []}
     ],
     [
      {name, kick_user},
      {category, <<"sessions">>},
      {desc, <<"Terminate user connection">>},
      {module, ?MODULE},
      {function, kick_session},
      {action, delete},
      {args, [{host, binary}, {user, binary}, {res, binary}]},
      {result, {msg, binary}}
     ],
     [
      {name, list_contacts},
      {category, <<"contacts">>},
      {desc, <<"Get roster">>},
      {module, ?MODULE},
      {function, list_contacts},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}]},
      {result, []}
     ],
     [
      {name, add_contact},
      {category, <<"contacts">>},
      {desc, <<"Add a contact to roster">>},
      {module, ?MODULE},
      {function, add_contact},
      {action, create},
      {security_policy, [user]},
      {args, [{caller, binary}, {jid, binary}]},
      {result, ok}
     ],
     [
      {name, subscription},
      {category, <<"contacts">>},
      {desc, <<"Send out a subscription request">>},
      {module, ?MODULE},
      {function, subscription},
      {action, update},
      {security_policy, [user]},
      {identifiers, [caller, jid]},
      % caller has to be in identifiers, otherwise it breaks admin rest api
      {args, [{caller, binary}, {jid, binary}, {action, binary}]},
      {result, ok}
     ],
     [
      {name, set_subscription},
      {category, <<"contacts">>},
      {subcategory, <<"manage">>},
      {desc, <<"Set / unset mutual subscription">>},
      {module, ?MODULE},
      {function, set_subscription},
      {action, update},
      {identifiers, [caller, jid]},
      {args, [{caller, binary}, {jid, binary}, {action, binary}]},
      {result, ok}
     ],
     [
      {name, delete_contact},
      {category, <<"contacts">>},
      {desc, <<"Remove a contact from roster">>},
      {module, ?MODULE},
      {function, delete_contact},
      {action, delete},
      {security_policy, [user]},
      {args, [{caller, binary}, {jid, binary}]},
      {result, ok}
     ],
     [
      {name, delete_contacts},
      {category, <<"contacts">>},
      {subcategory, <<"multiple">>},
      {desc, <<"Remove provided contacts from roster">>},
      {module, ?MODULE},
      {function, delete_contacts},
      {action, delete},
      {security_policy, [user]},
      {args, [{caller, binary}, {jids, [binary]}]},
      {result,  []}
     ],
     [
      {name, send_message},
      {category, <<"messages">>},
      {desc, <<"Send chat message from to">>},
      {module, ?MODULE},
      {function, send_message},
      {action, create},
      {security_policy, [user]},
      {args, [{caller, binary}, {to, binary}, {body, binary}]},
      {result, ok}
     ],
     [
      {name, send_stanza},
      {category, <<"stanzas">>},
      {desc, <<"Send an arbitrary stanza">>},
      {module, ?MODULE},
      {function, send_stanza},
      {action, create},
      {args, [{stanza, binary}]},
      {result, ok}
     ],
     [
      {name, get_last_messages_with_everybody},
      {category, <<"messages">>},
      {desc, <<"Get n last messages from archive, optionally before a certain date (unixtime)">>},
      {module, ?MODULE},
      {function, get_recent_messages},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}]},
      {optargs, [{before, integer, 0}, {limit, integer, 100}]},
      {result, []}
     ],
     [
      {name, get_last_messages},
      {category, <<"messages">>},
      {desc, <<"Get n last messages to/from given contact, with limit and date">>},
      {module, ?MODULE},
      {function, get_recent_messages},
      {action, read},
      {security_policy, [user]},
      {args, [{caller, binary}, {with, binary}]},
      {optargs, [{before, integer, 0}, {limit, integer, 100}]},
      {result, []}
     ],
     [
      {name, change_password},
      {category, <<"users">>},
      {desc, <<"Change user password">>},
      {module, ?MODULE},
      {function, change_user_password},
      {action, update},
      {security_policy, [user]},
      {identifiers, [host, user]},
      {args, [{host, binary}, {user, binary}, {newpass, binary}]},
      {result, ok}
     ]
    ].

kick_session(Host, User, Resource) ->
    case mongoose_session_api:kick_session(User, Host, Resource, <<"kicked">>) of
        {ok, Msg} -> Msg;
        {no_session, Msg} -> {error, not_found, Msg}
    end.

list_sessions(Host) ->
    mongoose_session_api:list_resources(Host).

registered_users(Host) ->
    mongoose_account_api:list_users(Host).

register(Host, User, Password) ->
    Res = mongoose_account_api:register_user(User, Host, Password),
    format_account_result(Res).

unregister(Host, User) ->
    Res = mongoose_account_api:unregister_user(User, Host),
    format_account_result(Res).

change_user_password(Host, User, Password) ->
    Res = mongoose_account_api:change_password(User, Host, Password),
    format_account_result(Res).

format_account_result({ok, Msg}) -> iolist_to_binary(Msg);
format_account_result({empty_password, Msg}) -> {error, bad_request, Msg};
format_account_result({invalid_jid, Msg}) -> {error, bad_request, Msg};
format_account_result({not_allowed, Msg}) -> {error, denied, Msg};
format_account_result({exists, Msg}) -> {error, denied, Msg};
format_account_result({cannot_register, Msg}) -> {error, internal, Msg}.

send_message(From, To, Body) ->
    case mongoose_stanza_helper:parse_from_to(From, To) of
        {ok, FromJID, ToJID} ->
            Packet = mongoose_stanza_helper:build_message(From, To, Body),
            do_send_packet(FromJID, ToJID, Packet);
        Error ->
            Error
    end.

send_stanza(BinStanza) ->
    case exml:parse(BinStanza) of
        {ok, Packet} ->
            From = exml_query:attr(Packet, <<"from">>),
            To = exml_query:attr(Packet, <<"to">>),
            case mongoose_stanza_helper:parse_from_to(From, To) of
                {ok, FromJID, ToJID} ->
                    do_send_packet(FromJID, ToJID, Packet);
                {error, missing} ->
                    {error, bad_request, "both from and to are required"};
                {error, type_error, E} ->
                    {error, type_error, E}
            end;
        {error, Reason} ->
            {error, bad_request, io_lib:format("Malformed stanza: ~p", [Reason])}
    end.

do_send_packet(From, To, Packet) ->
    case mongoose_domain_api:get_domain_host_type(From#jid.lserver) of
        {ok, HostType} ->
            Acc = mongoose_acc:new(#{location => ?LOCATION,
                                      host_type => HostType,
                                      lserver => From#jid.lserver,
                                      element => Packet}),
            Acc1 = mongoose_hooks:user_send_packet(Acc, From, To, Packet),
            ejabberd_router:route(From, To, Acc1),
            ok;
        {error, not_found} ->
            {error, unknown_domain}
    end.

list_contacts(Caller) ->
    case mod_roster_api:list_contacts(jid:from_binary(Caller)) of
        {ok, Rosters} ->
            [roster_info(mod_roster:item_to_map(R)) || R <- Rosters];
        Error ->
            skip_result_msg(Error)
    end.

roster_info(M) ->
    Jid = jid:to_binary(maps:get(jid, M)),
    #{subscription := Sub, ask := Ask} = M,
    #{jid => Jid, subscription => Sub, ask => Ask}.

add_contact(Caller, JabberID) ->
    add_contact(Caller, JabberID, <<"">>, []).

add_contact(Caller, JabberID, Name) ->
    add_contact(Caller, JabberID, Name, []).

add_contact(Caller, Other, Name, Groups) ->
    case mongoose_stanza_helper:parse_from_to(Caller, Other) of
        {ok, CallerJid, OtherJid} ->
            Res = mod_roster_api:add_contact(CallerJid, OtherJid, Name, Groups),
            skip_result_msg(Res);
        E ->
            E
    end.

delete_contacts(Caller, ToDelete) ->
    maybe_delete_contacts(Caller, ToDelete, []).

maybe_delete_contacts(_, [], NotDeleted) -> NotDeleted;
maybe_delete_contacts(Caller, [H | T], NotDeleted) ->
    case delete_contact(Caller, H) of
        ok ->
            maybe_delete_contacts(Caller, T, NotDeleted);
        _Error ->
            maybe_delete_contacts(Caller, T, NotDeleted ++ [H])
    end.

delete_contact(Caller, Other) ->
    case mongoose_stanza_helper:parse_from_to(Caller, Other) of
        {ok, CallerJID, OtherJID} ->
            Res = mod_roster_api:delete_contact(CallerJID, OtherJID),
            skip_result_msg(Res);
        E ->
            E
    end.

registered_commands() ->
    Items = collect_commands(),
    sort_commands(Items).

sort_commands(Items) ->
    WithKey = [{get_sorting_key(Item), Item} || Item <- Items],
    Sorted = lists:keysort(1, WithKey),
    [Item || {_Key, Item} <- Sorted].

get_sorting_key(Item) ->
    maps:get(path, Item).

collect_commands() ->
    [#{name => mongoose_commands:name(C),
       category => mongoose_commands:category(C),
       action => mongoose_commands:action(C),
       method => mongoose_api_common:action_to_method(mongoose_commands:action(C)),
       desc => mongoose_commands:desc(C),
       args => format_args(mongoose_commands:args(C)),
       path => mongoose_api_common:create_admin_url_path(C)
      } || C <- mongoose_commands:list(admin)].

format_args(Args) ->
    maps:from_list([{term_as_binary(Name), term_as_binary(rewrite_type(Type))}
                    || {Name, Type} <- Args]).

%% binary is useful internally, but could confuse a regular user
rewrite_type(binary) -> string;
rewrite_type(Type) -> Type.

term_as_binary(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

get_recent_messages(Caller, Before, Limit) ->
    get_recent_messages(Caller, undefined, Before, Limit).

get_recent_messages(Caller, With, Before, Limit) ->
    Before2 = maybe_seconds_to_microseconds(Before),
    Res = mongoose_stanza_api:lookup_recent_messages(Caller, With, Before2, Limit),
    lists:map(fun row_to_map/1, Res).

maybe_seconds_to_microseconds(X) when is_number(X) ->
    X * 1000000;
maybe_seconds_to_microseconds(X) ->
    X.

-spec row_to_map(mod_mam:message_row()) -> map().
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    Jbin = jid:to_binary(From),
    {Msec, _} = mod_mam_utils:decode_compact_uuid(Id),
    MsgId = case xml:get_tag_attr(<<"id">>, Msg) of
                {value, MId} -> MId;
                false -> <<"">>
            end,
    Body = exml_query:path(Msg, [{element, <<"body">>}, cdata]),
    #{sender => Jbin, timestamp => round(Msec / 1000000), message_id => MsgId,
      body => Body}.

subscription(Caller, Other, Action) ->
    case decode_action(Action) of
        error ->
            {error, bad_request, <<"invalid action">>};
        Act ->
            case mongoose_stanza_helper:parse_from_to(Caller, Other) of
                {ok, CallerJID, OtherJID} ->
                    Res = mod_roster_api:subscription(CallerJID, OtherJID, Act),
                    skip_result_msg(Res);
                E ->
                    E
            end
    end.

decode_action(<<"subscribe">>) -> subscribe;
decode_action(<<"subscribed">>) -> subscribed;
decode_action(_) -> error.

set_subscription(Caller, Other, Action) ->
    case mongoose_stanza_helper:parse_from_to(Caller, Other) of
        {ok, CallerJID, OtherJID} ->
            case decode_both_sub_action(Action) of
                error ->
                    {error, bad_request, <<"invalid action">>};
                ActionDecoded  ->
                    Res = mod_roster_api:set_mutual_subscription(CallerJID, OtherJID,
                                                                 ActionDecoded),
                    skip_result_msg(Res)
            end;
        E ->
            E
    end.

decode_both_sub_action(<<"connect">>) -> connect;
decode_both_sub_action(<<"disconnect">>) -> disconnect;
decode_both_sub_action(_) -> error.

skip_result_msg({ok, _Msg}) -> ok;
skip_result_msg({ErrCode, _Msg}) -> {error, ErrCode}.
