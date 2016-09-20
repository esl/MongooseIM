-module(muc_helper).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-type verify_fun() :: fun((Incoming :: #xmlel{}) -> any()).

-define(MUC_HOST, <<"muc.localhost">>).

-export_type([verify_fun/0]).

-spec foreach_occupant(
        Users :: [escalus:client()], Stanza :: #xmlel{}, VerifyFun :: verify_fun()) -> ok.
foreach_occupant(Users, Stanza, VerifyFun) ->
    lists:foreach(
      fun(Sender) ->
              escalus:send(Sender, Stanza),
              case exml_query:path(Stanza, [{attr, <<"type">>}]) of
                  <<"get">> ->
                      Incoming = escalus:wait_for_stanza(Sender),
                      escalus:assert(is_iq_result, Incoming),
                      VerifyFun(Incoming);
                  _ ->
                      foreach_recipient(Users, VerifyFun),
                      case Stanza of
                          #xmlel{ name = <<"iq">> } ->
                              escalus:assert(is_iq_result, escalus:wait_for_stanza(Sender));
                          _ ->
                              ok
                      end
              end
      end, Users).

-spec foreach_recipient(Users :: [escalus:client()], VerifyFun :: verify_fun()) -> ok.
foreach_recipient(Users, VerifyFun) ->
    lists:foreach(
      fun(Recipient) ->
              VerifyFun(escalus:wait_for_stanza(Recipient))
      end, Users).

load_muc(Host) ->
    dynamic_modules:start(<<"localhost">>, mod_muc,
        [{host, binary_to_list(Host)},
            {access, muc},
            {access_create, muc_create}]),
    dynamic_modules:start(<<"localhost">>, mod_muc_log,
        [{outdir, "/tmp/muclogs"},
            {access_log, muc}]).

unload_muc() ->
    dynamic_modules:stop(<<"localhost">>, mod_muc),
    dynamic_modules:stop(<<"localhost">>, mod_muc_log).

muc_host() ->
    ?MUC_HOST.

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    create_instant_room(<<"localhost">>, Room, From, Nick, Opts),
    [{nick, Nick}, {room, Room} | Config].

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    LUsername = escalus_utils:jid_to_lower(Username),
    LServer = escalus_utils:jid_to_lower(Server),
    {jid, Username, Server, <<"rpc">>, LUsername, LServer, <<"rpc">>}.

create_instant_room(Host, Room, From, Nick, Opts) ->
    Room1 = escalus_ejabberd:rpc(jid, nodeprep, [Room]),
    escalus_ejabberd:rpc(mod_muc, create_instant_room,
        [Host, Room1, From, Nick, Opts]).

destroy_room(Config) ->
    destroy_room(?MUC_HOST, ?config(room, Config)).

destroy_room(Host, Room) when is_binary(Host), is_binary(Room) ->
    Room1 = escalus_ejabberd:rpc(jid, nodeprep, [Room]),
    case escalus_ejabberd:rpc(
            ets, lookup, [muc_online_room, {Room1, Host}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_default_muc_room(Room, Nick) ->
    Form = escalus_stanza:x_data_form(<<"submit">>, []),
    Query = escalus_stanza:query_el(?NS_MUC_OWNER, [Form]),
    IQSet = escalus_stanza:iq(<<"set">>, [Query]),
    stanza_to_room(IQSet, Room, Nick).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.
