%%%----------------------------------------------------------------------
%%% File    : mod_invites.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Account and Roster Invitation (aka Great Invitations)
%%% Created : Fr Jul 12 2026 by Stefan Strigler <stefan@strigler.de>
%%%
%%% This is a backport of ejabberd's mod_invite.
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(mod_invites).

-author('stefan@strigler.de').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, deps/2]).

%% hooks and callbacks
-export([adhoc_commands/3,
         %c2s_unauthenticated_packet/2,
         remove_user/3,
         s2s_receive_packet/3, user_receive_packet/3, stream_feature_register/3]).

%% Service Discovery
-export([disco_local_identity/3, disco_local_features/3, disco_local_items/3]).

%% commands
-export([cleanup_expired/0, delete_invite_by_token/2, expire_invites/2, expire_invite_by_token/2, generate_invite/1,
         generate_invite/2, generate_reset_token/2, list_invites/1]).

%% helpers
-export([create_account_allowed/2, create_account_invite/4, format_invite/2,
         get_invite/2, get_invites_tree_t/2,
         get_max_invites/2, is_create_allowed/2, is_expired/1, is_reserved/3, is_token_valid/2,
         %roster_add/2,
         %send_presence/3,
         set_invitee/3, set_invitee/5, token_uri/1, transaction/2,
         xdata_field/3]).

-ifdef(TEST).
-export([create_roster_invite/2, create_reset_token/2, find_invites_tree_root_t/4, gen_invite/1,
         gen_invite/2, get_invites/2, get_invites_tree_as_root_t/2, is_token_valid/3]).
-endif.

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").
-include("mod_invites.hrl").

-type invite_token() :: #invite_token{}.
-export_type([invite_token/0]).

-callback cleanup_expired(Host :: binary()) -> non_neg_integer().
-callback create_invite_t(Host :: binary(), Invite :: invite_token()) -> invite_token().
-callback delete_invite_by_token(Server :: binary(), Token :: binary()) -> ok | {error, not_found}.
-callback expire_invite_by_token(Server :: binary(), Token :: binary()) -> ok | {error, not_found}.
-callback expire_tokens(User :: binary(), Server :: binary()) -> non_neg_integer().
-callback get_invite(Host :: binary(), Token :: binary()) ->
    invite_token() | {error, not_found}.
-callback get_invite_by_invitee_t(Host :: binary(), Invitee :: {User :: binary(), Host :: binary()}) ->
    invite_token() | {error, not_found}.
-callback get_invites_t(Host :: binary(), Inviter :: {User :: binary(), Host :: binary()}) ->
    [invite_token()].
-callback is_reserved(Host :: binary(), Token :: binary(), User :: binary()) -> boolean().
-callback is_token_valid(Host :: binary(), binary(), {binary(), binary()}) -> boolean().
-callback list_invites(Host :: binary()) -> [tuple()].
-callback remove_user(User :: binary(), Server :: binary()) -> any().
-callback set_invitee(Fun :: fun(() -> OkOrError),
                                Host :: binary(),
                                Token :: binary(),
                                Invitee :: binary(),
                                AccountName :: binary()) -> OkOrError | {error, conflict}
 when OkOrError :: ok | {error, term()}.
-callback transaction(Host:: binary(), fun(() -> T)) -> {atomic, T} | {aborted, any()}.

%%--------------------------------------------------------------------
%%| gen_mod callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"access_create_account">> => #option{type = atom,
                                                         validate = access_rule},
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_invites_db}},
                 <<"max_invites">> => #option{type = int_or_infinity,
                                              validate = positive},
                 <<"token_expire_seconds">> => #option{type = int_or_infinity,
                                                       validate = positive}
                },
       defaults = #{<<"access_create_account">> => none,
                    <<"backend">> => mnesia,
                    <<"max_invites">> => ?DEFAULT_MAX_INVITES,
                    <<"token_expire_seconds">> => ?DEFAULT_TOKEN_EXPIRE_SECONDS
                   }
      }.

deps(_Host, _Opts) ->
    %% TODO
   % [{mod_adhoc, #{}, soft}, {mod_register, #{}, soft}, {mod_roster, #{}, soft}].
    [].

-spec supported_features() -> [atom()].
supported_features() ->
    [].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {adhoc_local_commands, HostType, fun ?MODULE:adhoc_commands/3, #{}, 50},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 50},
     {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 50},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 50},
     {s2s_receive_packet, HostType, fun ?MODULE:s2s_receive_packet/3, #{}, 50},
     {user_receive_packet, HostType, fun ?MODULE:user_receive_packet/3, #{}, 50},
     {c2s_stream_features, HostType, fun ?MODULE:stream_feature_register/3, #{}, 50}%,
     %% note the sequence below is important
%     {c2s_unauthenticated_packet, HostType, fun ?MODULE:c2s_unauthenticated_packet/3, #{}, 10}
    ].

start(HostType, Opts) ->
    mod_invites_db_backend:start(HostType, Opts),
    ok.

stop(HostType) ->
    mod_invites_db_backend:stop(HostType),
    ok.

%%--------------------------------------------------------------------
%%| ejabberd command callbacks

cleanup_expired() ->
    lists:foldl(fun(Host, Count) ->
                   case gen_mod:is_loaded(Host, ?MODULE) of
                       true ->
                           Count + db_call(Host, cleanup_expired, [Host]);
                       false ->
                           Count
                   end
                end,
                0,
                ?MYHOSTS).

-spec delete_invite_by_token(binary(), binary()) -> ok | {error, not_found}.
delete_invite_by_token(Host, Token) ->
    pretty_format_command_result(try_db_call(Host, delete_invite_by_token, [Host, Token])).

-spec expire_invites(binary(), binary()) -> non_neg_integer().
expire_invites(User0, Server0) ->
    User = jid:nodeprep(User0),
    Server = jid:nameprep(Server0),
    pretty_format_command_result(try_db_call(Server, expire_tokens, [User, Server])).

-spec expire_invite_by_token(binary(), binary()) -> ok | {error, not_found}.
expire_invite_by_token(Host, Token) ->
    pretty_format_command_result(try_db_call(Host, expire_invite_by_token, [Host, Token])).

-spec generate_invite(binary()) -> {binary(), binary()} | {error, any()}.
generate_invite(Host) ->
    generate_invite(<<>>, Host).

-spec generate_invite(binary(), binary()) -> {binary(), binary()} | {error, any()}.
generate_invite(AccountName, Host0) ->
    Host = jid:nameprep(Host0),
    lift(create_account_invite(Host, {<<>>, Host}, AccountName, false)).

-ifdef(TEST).

-spec gen_invite(binary()) -> binary() | {error, any()}.
gen_invite(Host) ->
    gen_invite(<<>>, Host).

-endif.

-spec gen_invite(binary(), binary()) -> {binary(), binary()} | {error, any()}.
gen_invite(AccountName, Host0) ->
    Host = jid:nameprep(Host0),
    case create_account_invite(Host, {<<>>, Host}, AccountName, false) of
        {error, _Reason} = Error ->
            Error;
        Invite ->
            {token_uri(Invite), landing_page(Host, Invite)}
    end.

-spec generate_reset_token(binary(), binary()) -> {binary(), binary()} | {error, any()}.
generate_reset_token(User, Host) ->
    Res = case create_reset_token(User, Host) of
              {error, _Reason} = Error ->
                  Error;
              Invite ->
                  {token_uri(Invite), landing_page(Host, Invite)}
          end,
    pretty_format_command_result(Res).

list_invites(Host) ->
    try_db_call(Host, list_invites, [Host]).

format_invite(Host,
              #invite_token{token = TO,
                            inviter = {IU, IS},
                            invitee = IE,
                            created_at = CA,
                            expires = Exp,
                            type = TY,
                            account_name = AN} =
                  Invite) ->
    #{<<"token">> => TO,
      <<"valid">> => is_token_valid(Host, TO),
      <<"created_at">> => encode_datetime(CA),
      <<"expires">> => encode_datetime(Exp),
      <<"type">> => TY,
      <<"inviter">> => jid:to_binary(jid:make_bare(IU, IS)),
      <<"invitee">> => IE,
      <<"account_name">> => AN,
      <<"token_uri">> => token_uri(Invite),
      <<"landing_page">> => landing_page(Host, Invite)
     }.

%%--------------------------------------------------------------------
%%| hooks and callbacks

remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    case try_db_call(HostType, remove_user, [LUser, LServer]) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => muc_remove_user_failed,
                         reason => Reason, acc => Acc}),
            {ok, Acc};
        _ ->
            {ok, Acc}
    end.

%% ---

-spec adhoc_commands(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mod_adhoc:command_hook_acc(),
      Params :: #{adhoc_request := adhoc:request()},
      Extra :: gen_hook:extra().
adhoc_commands(empty,
               #{adhoc_request := #adhoc_request{node = ?NS_INVITE_INVITE = Node,
                                                 action = <<"execute">>,
                                                 session_id = SID,
                                                 lang = Lang},
                 from := #jid{luser = LUser, lserver = LServer}},
               _) ->
    Invite = create_roster_invite(LServer, {LUser, LServer}),
    Form = mongoose_data_forms:form(
              #{type => <<"result">>,
                title => trans(Lang, <<"New Invite Token Created">>),
                fields =>
                    maybe_add_landing_url(LServer,
                                          Invite,
                                          Lang,
                                          [#{var => <<"uri">>,
                                             label => trans(Lang, <<"Invite URI">>),
                                             type => <<"text-single">>,
                                             values => [token_uri(Invite)]},
                                           #{var => <<"expire">>,
                                             label =>
                                                 trans(Lang,
                                                       <<"Invite token valid until">>),
                                             type => <<"text-single">>,
                                             values =>
                                                 [encode_datetime(Invite#invite_token.expires)]}
                                          ])}),
    Response = adhoc:produce_response(
                 #adhoc_response{status = completed,
                                 node = Node,
                                 elements = [Form],
                                 lang = Lang,
                                 session_id = SID}),
    {ok, Response};
adhoc_commands(empty,
               #{adhoc_request := #adhoc_request{node = ?NS_INVITE_CREATE_ACCOUNT = Node,
                                                 action = <<"execute">>,
                                                 session_id = SID,
                                                 xdata = false,
                                                 lang = Lang},
                 from := From,
                 to := #jid{lserver = LServer}},
               _) ->
    check(fun create_account_allowed/2,
          [LServer, From],
          fun() ->
                  Form =
                      mongoose_data_forms:form(
                        #{type => <<"form">>,
                          title => trans(Lang, <<"Account Creation Invite">>),
                          fields =>
                              [#{var => <<"username">>,
                                 label => trans(Lang, <<"Username">>),
                                 type => <<"text-single">>},
                               #{var => <<"roster-subscription">>,
                            label => trans(Lang, <<"Roster Subscription">>),
                            type => <<"boolean">>}
                         ]}),
                  Response = adhoc:produce_response(
                               #adhoc_response{status = executing,
                                               node = Node,
                                               default_action = <<"complete">>,
                                               actions = [<<"complete">>],
                                               elements = [Form],
                                               lang = Lang,
                                               session_id = maybe_gen_sid(SID)}),
                  {ok, Response}
          end,
          fun(Reason) -> {error, to_stanza_error(Lang, Reason)} end);
adhoc_commands(empty,
               #{adhoc_request := #adhoc_request{node = ?NS_INVITE_CREATE_ACCOUNT = Node,
                                                 session_id = SID,
                                                 xdata = XData,
                                                 lang = Lang},
                 from := #jid{luser = LUser, lserver = LServer} = From,
                 to := #jid{lserver = LServer}},
               _) when XData /= false ->
    case mongoose_data_forms:parse_form(XData) of
        #{type := <<"submit">>, kvs := KVs} ->
            check(fun create_account_allowed/2,
                  [LServer, From],
                  fun() ->
                          AccountName = hd(maps:get(<<"username">>, KVs, [<<>>])),
                          Invite =
                              create_account_invite(LServer,
                                                    {LUser, LServer},
                                                    AccountName,
                                                    to_boolean(hd(maps:get(<<"roster-subscription">>, KVs, false)))),
                          case Invite of
                              {error, Reason} ->
                                  {ok, {error, to_stanza_error(Lang, Reason)}};
                              _Invite ->
                                  ResultFields =
                                      maybe_add_landing_url(LServer,
                                                            Invite,
                                                            Lang,
                                                            [#{var => <<"uri">>,
                                                               label => trans(Lang, <<"Invite URI">>),
                                                               type => <<"text-single">>,
                                                               values => [token_uri(Invite)]},
                                                             #{var => <<"expire">>,
                                                               label => trans(Lang, <<"Invite token valid until">>),
                                                               type => <<"text-single">>,
                                                               values =>
                                                                   [encode_datetime(Invite#invite_token.expires)]}]),
                                  ResultXData = mongoose_data_forms:form(#{type => <<"result">>,
                                                                           fields => ResultFields}),
                                  Response = adhoc:produce_response(
                                               #adhoc_response{status = completed,
                                                               node = Node,
                                                               lang = Lang,
                                                               session_id = SID,
                                                               elements = [ResultXData]}),
                                  {ok, Response}
                          end
                  end,
                  fun(Reason) -> {ok, {error, to_stanza_error(Lang, Reason)}} end);
        _ ->
            {ok, {error, mongoose_xmpp_errors:bad_request()}}
    end;
adhoc_commands(Acc, _, _) ->
    {ok, Acc}.

-spec s2s_receive_packet(Acc, map(), any()) -> {ok|stop, Acc} when Acc :: mongoose_acc:t().
s2s_receive_packet(Acc, Params, Extras) ->
    user_receive_packet(Acc, Params, Extras).

-spec user_receive_packet(Acc, map(), any()) -> {ok|stop, Acc} when Acc :: mongoose_acc:t().
user_receive_packet(Acc, _Params, _Extras) ->
    case maybe_handle_pre_auth_token(Acc) of
        true ->
            {stop, Acc};
        false ->
            {ok, Acc}
    end.

maybe_handle_pre_auth_token(Acc) ->
    case get_preauth_token(Acc) of
        undefined ->
            ?DEBUG("no preauth token", []),
            false;
        Token ->
            ?DEBUG("got preauth token: ~p", [Token]),
            #jid{luser = LUser, lserver = LServer} = To = jid:to_bare(mongoose_acc:to_jid(Acc)),
            case is_token_valid(LServer, Token, {LUser, LServer}) of
                true ->
                    ?DEBUG("got valid token! ~p", [Token]),
                    From = jid:to_bare(mongoose_acc:from_jid(Acc)),
                    ok = roster_add(LServer, To, From),
                    _Acc1 = send_presence(LServer, To, From, <<"subscribed">>),
                    _Acc2 = send_presence(LServer, To, From, <<"subscribe">>),
                    set_invitee(LServer, Token, From),
                    true;
                false ->
                    ?INFO_MSG("Got invalid preauth token from ~s: ~p",
                              [jid:to_binary(mongoose_acc:from_jid(Acc)), Token]),
                    false
            end
    end.

get_preauth_token(Acc) ->
    case {mongoose_acc:stanza_name(Acc), mongoose_acc:stanza_type(Acc)} of
        {<<"presence">>, <<"subscribe">>} ->
            Presence = mongoose_acc:element(Acc),
            ?DEBUG("got presence: ~p", [Presence]),
            exml_query:path(Presence, [{element_with_ns, <<"preauth">>, ?NS_PARS}, {attr, <<"token">>}]);
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%%| Service Disco

-define(INFO_IDENTITY(Category, Type, Name),
        #{category => Category,
          type => Type,
          name => Name}).
-define(INFO_COMMAND(Name),
        ?INFO_IDENTITY(<<"automation">>, <<"command-node">>, Name)).

%-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
-spec disco_local_identity(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:identity_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
disco_local_identity(Acc = #{node := ?NS_INVITE_CREATE_ACCOUNT}, _, _) ->
    {ok, mongoose_disco:add_identities([?INFO_COMMAND("Create Account")], Acc)};
disco_local_identity(Acc = #{node := ?NS_INVITE_INVITE}, _, _) ->
    {ok, mongoose_disco:add_identities([?INFO_COMMAND("Invite User")], Acc)};
disco_local_identity(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec disco_local_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_features(Acc = #{node := Ns, from_jid := From, to_jid := #jid{lserver = LServer}}, _, _) ->
    maybe
        allow ?=
            case Ns of
                ?NS_INVITE_CREATE_ACCOUNT ->
                    Access = gen_mod:get_module_opt(LServer, ?MODULE, access_create_account),
                    acl:match_rule(LServer, Access, From);
                ?NS_INVITE_INVITE ->
                    allow;
                _ ->
                    false
            end,
        {ok, mongoose_disco:add_features([?NS_COMMANDS], Acc)}
    else
        false ->
            {ok, Acc};
        deny ->
            %% FIXME
            {error, "Access denied by service policy"}
    end;
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:item_acc(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
disco_local_items(Acc = #{from_jid := From, to_jid := #jid{lserver = LServer}, node := ?NS_COMMANDS}, _, _) ->
    InviteUser =
        #{jid => LServer,
          node => ?NS_INVITE_INVITE,
          name => <<"Invite User">>},
    CreateAccount =
        #{jid => LServer,
          node => ?NS_INVITE_CREATE_ACCOUNT,
          name => <<"Create Account">>},
    Items =
        case create_account_allowed(LServer, From) of
             ok ->
                 [InviteUser, CreateAccount];
            {error, not_allowed} ->
                [InviteUser]
        end,
    ResAcc = mongoose_disco:add_items(Items, Acc),
    {ok, ResAcc};
disco_local_items(Acc, _Params, _Extra) ->
    {ok, Acc}.

%% ---

%%--------------------------------------------------------------------
%%| ibr hooks
stream_feature_register(Acc, #{lserver := Host}, _) ->
    {ok, mod_invites_register:stream_feature_register(Acc, Host)}.

c2s_unauthenticated_packet(State, IQ) ->
    mod_invites_register:c2s_unauthenticated_packet(State, IQ).


%%--------------------------------------------------------------------
%%| helpers
get_invite(Host, Token) ->
    db_call(Host, get_invite, [Host, Token]).

-ifdef(TEST).

get_invites(Host, Inviter) ->
    transaction(Host, fun() -> get_invites_t(Host, Inviter) end).

-endif.

get_invites_t(Host, Inviter) ->
    db_call(Host, get_invites_t, [Host, Inviter]).

is_expired(#invite_token{expires = Expires}) ->
    Now = erlang:timestamp(),
    calendar:datetime_to_gregorian_seconds(Expires)
    < calendar:datetime_to_gregorian_seconds(
          calendar:now_to_universal_time(Now)).

is_reserved(Host, Token, User) ->
    db_call(Host, is_reserved, [Host, Token, User]).

-spec is_token_valid(binary(), binary()) -> boolean().
is_token_valid(Host, Token) ->
    is_token_valid(Host, Token, {<<>>, Host}).

-spec is_token_valid(binary(), binary(), {binary(), binary()}) -> boolean().
is_token_valid(Host, Token, Inviter) ->
    db_call(Host, is_token_valid, [Host, Token, Inviter]).

%-spec set_invitee(binary(), binary(), jid() | binary()) -> ok.
set_invitee(Host, Token, #jid{} = InviteeJid) ->
    set_invitee(Host,
                Token,
                jid:to_bare_binary(InviteeJid),
                <<>>);
set_invitee(Host, Token, Invitee) ->
    set_invitee(Host, Token, Invitee, <<>>).

set_invitee(Host, Token, Invitee, AccountName) ->
    set_invitee(fun() -> ok end, Host, Token, Invitee, AccountName).

-spec set_invitee(binary(), binary(), binary(), binary()) -> ok.
set_invitee(F, Host, Token, Invitee, AccountName) ->
    %% This invalidates the invite token if Invitee isn't empty
    db_call(Host, set_invitee, [F, Host, Token, Invitee, AccountName]).

create_roster_invite(Host, Inviter) ->
    create_invite(roster_only, Host, Inviter, <<>>).

create_account_invite(Host, Inviter, AccountName, _Subscribe = true) ->
    create_invite(account_subscription, Host, Inviter, AccountName);
create_account_invite(Host, Inviter, AccountName, _Subcribe = false) ->
    create_invite(account_only, Host, Inviter, AccountName).

create_invite(Type, Host, Inviter, AccountName) ->
    F = fun() -> create_invite_t(Type, Host, Inviter, AccountName) end,
    transaction(Host, F).

create_invite_t(Type, Host, Inviter, AccountName) ->
    try invite_token_t(Type, Host, Inviter, AccountName) of
        Invite ->
            db_call(Host, create_invite_t, [Host, Invite])
    catch
        _:({error, _Reason} = Error) ->
            Error;
        _:Error ->
            {error, Error}
    end.

check_account_name(<<>>, _) ->
    <<>>;
check_account_name(error, _) ->
    {error, account_name_invalid};
check_account_name(_, error) ->
    {error, hostname_invalid};
check_account_name(AccountName, Host) ->
    case lists:member(Host, ?MYHOSTS) of
        false ->
            {error, host_unknown};
        true ->
            case ejabberd_auth:does_user_exist(jid:make_bare(AccountName, Host)) of
                true ->
                    {error, user_exists};
                false ->
                    case is_reserved(Host, <<>>, AccountName) of
                        true ->
                            {error, reserved};
                        false ->
                            AccountName
                    end
            end
    end.

check_max_invites_t(roster_only, _) ->
    ok;
check_max_invites_t(_Type, {User, Host}) ->
    case is_create_allowed_t(User, Host) of
        true ->
            ok;
        false ->
            {error, num_invites_exceeded}
    end.

is_create_allowed(User, Host) ->
    transaction(Host, fun() -> is_create_allowed_t(User, Host) end).

is_create_allowed_t(User, Host) ->
    case get_max_invites(User, Host) of
        infinity ->
            true;
        MaxInvites ->
            Invites = get_invites_t(Host, {User, Host}),
            NumCreated =
                lists:foldl(fun (#invite_token{type = roster_only, account_name = <<>>}, Num) ->
                                    Num;
                                (#invite_token{type = roster_only}, Num) ->
                                    %% We make sure to set account_name to the registered name when
                                    %% creating the account. This field is not used in roster_only
                                    %% scenario otherwise.
                                    Num + 1;
                                (#invite_token{invitee = <<>>} = Invite, Num) ->
                                    %% account create tokens count unless they haven't been used and
                                    %% are expired
                                    case is_expired(Invite) of
                                        true ->
                                            Num;
                                        false ->
                                            Num + 1
                                    end;
                                (_, Num) ->
                                    %% account create token where invitee is not empty
                                    Num + 1
                            end,
                            0,
                            Invites),
            NumCreated < MaxInvites
    end.

get_max_invites(<<>>, _Server) ->
    infinity;
get_max_invites(User, Server) ->
    case {gen_mod:get_module_opt(Server, ?MODULE, max_invites),
          acl:match_rule(Server, admin, jid:make_bare(User, Server))}
    of
        {infinity, _} ->
            infinity;
        {_, allow} ->
            infinity;
        {MaxInvites, deny} ->
            MaxInvites
    end.

check_overuse_t(roster_only, {User, Host}) ->
    NumInvites = length(get_invites_t(Host, {User, Host})),
    case NumInvites >= ?OVERUSE_LIMIT of
        true ->
            {error, num_invites_exceeded};
        false ->
            ok
    end;
check_overuse_t(_Type, {User, Host}) ->
    NumInvites = length(get_invites_tree_t(Host, {User, Host})),
    case NumInvites >= ?OVERUSE_LIMIT of
        true ->
            {error, num_invites_exceeded};
        false ->
            ok
    end.

get_invites_tree_t(Host, Inviter) ->
    Now = calendar:datetime_to_gregorian_seconds(
              calendar:now_to_datetime(
                  erlang:timestamp())),
    Root = find_invites_tree_root_t(Now, Host, Inviter, 0),
    get_invites_tree_as_root_t(Host, Root).

find_invites_tree_root_t(Now, Host, Invitee, Lvl) ->
    case get_invite_by_invitee_t(Host, Invitee) of
        #invite_token{inviter = Inviter, created_at = CreatedAt} ->
            maybe_block_speedy_goat(Now, CreatedAt, Lvl),
            find_invites_tree_root_t(Now, Host, Inviter, Lvl + 1);
        {error, not_found} ->
            Invitee
    end.

-spec get_invite_by_invitee_t(binary(), {binary(), binary()}) ->
                                 invite_token() | {error, not_found}.
get_invite_by_invitee_t(_Host, {<<>>, _Server}) ->
    {error, not_found};
get_invite_by_invitee_t(Host, {User, Server}) ->
    db_call(Host, get_invite_by_invitee_t, [Host, {User, Server}]).

maybe_block_speedy_goat(Now, CreatedAt, Lvl) when Lvl == ?SPEEDY_GOAT_LEVELS ->
    Then = calendar:datetime_to_gregorian_seconds(CreatedAt),
    if Now - Then < ?SPEEDY_GOAT_SECONDS ->
           throw(speedy_goat);
       true ->
           ok
    end;
maybe_block_speedy_goat(_, _, _) ->
    ok.

-spec get_invites_tree_as_root_t(binary(), {binary(), binary()}) -> [invite_token()].
get_invites_tree_as_root_t(Host, Inviter) ->
    Invites = get_invites_t(Host, Inviter),
    get_invites_tree_as_root_t(Host, Inviter, Invites, []).

get_invites_tree_as_root_t(_Host, _Inviter, [], Acc) ->
    Acc;
get_invites_tree_as_root_t(Host,
                           Inviter,
                           [#invite_token{type = roster_only, account_name = <<>>} | Invites],
                           Acc) ->
    get_invites_tree_as_root_t(Host, Inviter, Invites, Acc);
get_invites_tree_as_root_t(Host,
                           Inviter,
                           [#invite_token{invitee = <<>>} = Invite | Invites],
                           Acc) ->
    get_invites_tree_as_root_t(Host, Inviter, Invites, [Invite | Acc]);
get_invites_tree_as_root_t(Host,
                           Inviter,
                           [#invite_token{invitee = InviteeJID} = Invite | Invites],
                           Acc) ->
    case jid:decode(InviteeJID) of
        #jid{luser = Invitee, lserver = Host} ->
            get_invites_tree_as_root_t(Host,
                                       Inviter,
                                       Invites,
                                       [Invite | Acc]
                                       ++ get_invites_tree_as_root_t(Host, {Invitee, Host}));
        _Nomatch ->
            get_invites_tree_as_root_t(Host, Inviter, Invites, [Invite | Acc])
    end.

maybe_throw({error, _} = Error) ->
    throw(Error);
maybe_throw(Good) ->
    Good.

invite_token_t(Type, Host, Inviter, AccountName0) ->
    maybe_throw(check_max_invites_t(Type, Inviter)),
    maybe_throw(check_overuse_t(Type, Inviter)),
    Token = p1_rand:get_alphanum_string(?DEFAULT_TOKEN_LENGTH),
    AccountName = maybe_throw(check_account_name(jid:nodeprep(AccountName0), Host)),
    ExpireSeconds = gen_mod:get_module_opt(Host, ?MODULE, token_expire_seconds),
    set_token_expires(#invite_token{token = Token,
                                    inviter = Inviter,
                                    type = Type,
                                    account_name = AccountName},
                      ExpireSeconds).

-spec create_reset_token(binary(), binary()) -> invite_token() | {error, any()}.
create_reset_token(User, Host) ->
    maybe
        (#invite_token{} = ResetToken) ?= reset_token(User, Host),
        F = fun() -> db_call(Host, create_invite_t, [ResetToken]) end,
        transaction(Host, F)
    end.

reset_token(User, Host) ->
    maybe
        true ?= lists:member(Host, ?MYHOSTS) orelse {error, host_unknown},
        true ?= ejabberd_auth:user_exists(User, Host) orelse {error, user_not_exists},
        set_token_expires(#invite_token{token =
                                            p1_rand:get_alphanum_string(?DEFAULT_TOKEN_LENGTH),
                                        inviter = {<<>>, Host},
                                        type = reset_token,
                                        account_name = User},
                          gen_mod:get_module_opt(Host, ?MODULE, token_expire_seconds))
    end.

token_uri(#invite_token{type = roster_only,
                        token = Token,
                        inviter = {User, Host}}) ->
    IBR = maybe_add_ibr_allowed(User, Host),
    Inviter =
        jid:to_binary(
            jid:make_bare(User, Host)),
    <<"xmpp:", Inviter/binary, "?roster;preauth=", Token/binary, IBR/binary>>;
token_uri(#invite_token{token = Token,
                        account_name = AccountName,
                        inviter = {_User, Host}}) ->
    Invitee =
        case AccountName of
            <<>> ->
                Host;
            _ ->
                <<AccountName/binary, "@", Host/binary>>
        end,
    <<"xmpp:", Invitee/binary, "?register;preauth=", Token/binary>>.

maybe_add_ibr_allowed(User, Host) ->
    case create_account_allowed(Host, jid:make_bare(User, Host)) of
        ok ->
            <<";ibr=y">>;
        {error, not_allowed} ->
            <<>>
    end.

landing_page(_Host, _Invite) ->
    %%mod_invites_http:landing_page(Host, Invite).
    <<"TBD">>.

-spec db_call(binary(), atom(), [any()]) -> any().
db_call(Host, Fun, Args) ->
    mongoose_backend:call(Host, mod_invites_db, Fun, Args).

%% father forgive me
lift({error, _R} = E) ->
    E;
lift({ok, _V} = R) ->
    R;
lift(Res) ->
    {ok, Res}.

-spec try_db_call(Host :: binary(), Fun :: atom(), Args :: [any()]) ->
                     {ok, any()} | {error, any()}.
try_db_call(Host, Fun, Args) ->
    try
        lift(db_call(Host, Fun, Args))
    catch
        _:({error, _Reason} = Error) ->
            Error;
        error:Error ->
            {error, Error}
    end.

transaction(Host, F) ->
    try db_call(Host, transaction, [Host, F]) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    catch
        _:Error ->
            Error
    end.

-spec trans(binary(), binary()) -> binary().
trans(_Lang, Msg) ->
    %translate:translate(Lang, Msg).
    Msg.

-spec encode_datetime(calendar:datetime()) -> binary().
encode_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                 [Year, Month, Day, Hour, Minute, Second])).

set_token_expires(#invite_token{created_at = CreatedAt} = Invite, ExpireSecs) ->
    Invite#invite_token{expires =
                            calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(CreatedAt)
                                                                   + ExpireSecs)}.

xdata_field(_Field, [], Default) ->
    Default;
xdata_field(Field, [El | Fields], Default) ->
    case exml_query:paths(El, [{element_with_attr, <<"var">>, Field}, {element, <<"value">>}, cdata]) of
        [<<>> | _] -> Default;
        [Value | _] ->
            Value;
        [] ->
            xdata_field(Field, Fields, Default)
    end.

maybe_add_landing_url(Host, Invite, Lang, Fields) ->
    case landing_page(Host, Invite) of
        <<>> ->
            Fields;
        LandingPage ->
            [#{var => <<"landing-url">>,
               values => [LandingPage],
               label => trans(Lang, <<"Invite Landing Page URL">>),
               type => <<"text-single">>}
            | Fields]
    end.

check(Check, Args, Fun, Else) ->
    case erlang:apply(Check, Args) of
        ok ->
            Fun();
        {error, Reason} ->
            Else(Reason)
    end.

create_account_allowed(Host, User) ->
    case gen_mod:get_module_opt(Host, ?MODULE, access_create_account) of
        none ->
            {error, not_allowed};
        Access ->
            case acl:match_rule(Host, Access, User) of
                deny ->
                    {error, not_allowed};
                allow ->
                    ok
            end
    end.

to_boolean(<<>>) ->
    false;
to_boolean(Boolean) when is_boolean(Boolean) ->
    Boolean;
to_boolean(True) when True == <<"1">>; True == <<"true">> ->
    true;
to_boolean(False) when False == <<"0">>; False == <<"false">> ->
    false.

to_stanza_error(Lang, not_allowed) ->
    Text = trans(Lang, <<"Access forbidden">>),
    mongoose_xmpp_errors:forbidden(Text);
to_stanza_error(Lang, Reason) ->
    Text = trans(Lang, reason_to_text(Reason)),
    mongoose_xmpp_errors:bad_request(Text).

reason_to_text(account_name_invalid) ->
    ?BIN("Username invalid");
reason_to_text(host_unknown) ->
    ?BIN("Host unknown");
reason_to_text(hostname_invalid) ->
    ?BIN("Hostname invalid");
reason_to_text(num_invites_exceeded) ->
    ?BIN("Maximum number of invites reached");
reason_to_text(reserved) ->
    ?BIN("Username is reserved");
reason_to_text(user_exists) ->
    ?BIN("User already exists").

maybe_gen_sid(<<>>) ->
    p1_rand:get_alphanum_string(?DEFAULT_TOKEN_LENGTH);
maybe_gen_sid(SID) ->
    SID.

roster_add(Host, UserJID, RosterItemJID) ->
    mod_roster:set_roster_entry(Host, UserJID, RosterItemJID, #{subscription => from, ask => subscribe}).

send_presence(HostType, FromJid, ToJid, Type) ->
    #jid{lserver =FromS} = FromJid,
    Presence = #xmlel{name = <<"presence">>,
                      attrs = #{<<"from">> => jid:to_binary(FromJid),
                                <<"to">> => jid:to_binary(ToJid),
                                <<"type">> => Type}},
    AccParams = #{host_type => HostType, lserver => FromS, location => ?LOCATION,
                  element => Presence, from_jid => FromJid, to_jid => ToJid},
    Acc = mongoose_acc:new(AccParams),
    mongoose_router:route(Acc).

pretty_format_command_result({error, {module_not_loaded, ?MODULE, Host}}) ->
    {error,
     lists:flatten(
         io_lib:format("Virtual host not known: ~s", [binary_to_list(Host)]))};
pretty_format_command_result({error, host_unknown}) ->
    {error, "Virtual host not known"};
pretty_format_command_result({error, user_exists}) ->
    {error, "Username already taken"};
pretty_format_command_result({error, user_not_exists}) ->
    {error, "User does not exist"};
pretty_format_command_result({ok, Result}) ->
    Result;
pretty_format_command_result(Result) ->
    Result.
