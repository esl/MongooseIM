%%%----------------------------------------------------------------------
%%% File    : mod_invites.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Account and Roster Invitation (aka Great Invitations)
%%% Created : Fr Jul 12 2026 by Stefan Strigler <stefan@strigler.de>
%%%
%%% This is a backport of ejabberd's mod_invites. Allows to create two
%%% types of invites, roster invites and account creation invites.
%%%
%%% Furthermore it allows to hand out reset tokens that so people can
%%% change their account password if they've lost their current one
%%% without the need to set a temporary password for them.
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
-xep([{xep, 379}, {version, "0.3.3"}]).
-xep([{xep, 401}, {version, "0.6.0"}]).
-xep([{xep, 445}, {version, "0.2.0"}]).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, deps/2]).

%% hooks and callbacks
-export([adhoc_commands/3, user_send_xmlel/3, remove_user/3,
         s2s_receive_packet/3, user_receive_packet/3, stream_feature_register/3]).

%% Service Discovery
-export([disco_local_identity/3, disco_local_features/3, disco_local_items/3]).

%% commands
-export([cleanup_expired/0,
         delete_invite_by_token/2,
         expire_invite_by_token/2,
         expire_invites/2,
         generate_invite/2,
         generate_reset_token/2,
         list_invites/1]).

%% helpers
-export([create_account_allowed/2,
         create_account_invite/4,
         format_invite/2,
         get_invite/3,
         get_invites_tree_t/2,
         is_create_allowed/2,
         is_expired/1,
         is_reserved/4,
         is_token_valid/3,
         pretty_format_command_result/1,
         roster_add/4,
         set_invitee/6,
         token_uri/2,
         transaction/2]).

%% exported for testing
-export([create_reset_token/3,
         create_roster_invite/2,
         get_invites/2,
         get_max_invites/2,
         set_invitee/4
        ]).

-ifdef(TEST).
-export([find_invites_tree_root_t/5,
         get_invites_t/2,
         get_invites_tree_as_root_t/3]).
-endif.

-ignore_xref([create_reset_token/3,
              create_roster_invite/2,
              get_invites/2,
              get_max_invites/2,
              set_invitee/4
             ]).
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").
-include("mod_roster.hrl").
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
-callback is_reserved_t(Host :: binary(), Token :: binary(), User :: binary()) -> boolean().
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
                 <<"landing_page">> => #option{type = binary},
                 <<"max_invites">> => #option{type = int_or_infinity,
                                              validate = positive},
                 <<"site_name">> => #option{type = binary},
                 <<"template_dir">> => #option{type = binary},
                 <<"token_expire_seconds">> => #option{type = int_or_infinity,
                                                       validate = positive},
                 <<"webchat_url">> => #option{type = binary}
                },
       defaults = #{<<"access_create_account">> => none,
                    <<"backend">> => mnesia,
                    <<"landing_page">> => <<"none">>,
                    <<"max_invites">> => ?DEFAULT_MAX_INVITES,
                    <<"template_dir">> => filename:join([code:priv_dir(mongooseim), ?MODULE, <<>>]),
                    <<"token_expire_seconds">> => ?DEFAULT_TOKEN_EXPIRE_SECONDS,
                    <<"webchat_url">> => <<"none">>
                   }
      }.

deps(_Host, _Opts) ->
    [{mod_adhoc, #{}, soft}, {mod_register, #{}, soft}, {mod_roster, #{}, soft}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {adhoc_local_commands, HostType, fun ?MODULE:adhoc_commands/3, #{}, 50},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 50},
     {disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 50},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 50},
     {s2s_receive_packet, HostType, fun ?MODULE:s2s_receive_packet/3, #{}, 50},
     {user_receive_packet, HostType, fun ?MODULE:user_receive_packet/3, #{}, 50},
     {c2s_stream_features, HostType, fun ?MODULE:stream_feature_register/3, #{}, 50},
     %% note the sequence below is important
     {user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 10}
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
                        {ok, HostType} = mongoose_domain_api:get_host_type(Host),
                        case gen_mod:is_loaded(HostType, ?MODULE) of
                            true ->
                                Count + db_call(HostType, cleanup_expired, [Host]);
                            false ->
                                Count
                        end
                end,
                0,
                ?ALL_HOST_TYPES).

-spec delete_invite_by_token(binary(), binary()) -> ok | {error, iodata()}.
delete_invite_by_token(Host, Token) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    pretty_format_command_result(try_db_call(HostType, delete_invite_by_token, [Host, Token])).

-spec expire_invites(binary(), binary()) -> non_neg_integer().
expire_invites(Host, User) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    pretty_format_command_result(try_db_call(HostType, expire_tokens, [User, Host])).

-spec expire_invite_by_token(binary(), binary()) -> ok | {error, iodata()}.
expire_invite_by_token(Host, Token) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    pretty_format_command_result(try_db_call(HostType, expire_invite_by_token, [Host, Token])).

-spec generate_invite(binary(), binary()) -> invite_token() | {error, any()}.
generate_invite(Host, User) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    pretty_format_command_result(create_account_invite(HostType, {<<>>, Host}, User, false)).

-spec generate_reset_token(binary(), binary()) -> invite_token() | {error, any()}.
generate_reset_token(Host, User) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    pretty_format_command_result(create_reset_token(HostType, User, Host)).

list_invites(Host) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    try_db_call(HostType, list_invites, [Host]).

format_invite(Host,
              #invite_token{token = Token,
                            inviter = Inviter = {InviteUser, InviteServer},
                            invitee = Invitee,
                            created_at = CreatedAt,
                            expires = Expires,
                            type = Type,
                            account_name = AccountName} =
                  Invite) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(Host),
    #{<<"token">> => Token,
      <<"valid">> => is_token_valid(HostType, Token, Inviter),
      <<"created_at">> => encode_datetime(CreatedAt),
      <<"expires">> => encode_datetime(Expires),
      <<"type">> => Type,
      <<"inviter">> => jid:to_binary(jid:make_bare(InviteUser, InviteServer)),
      <<"invitee">> => Invitee,
      <<"account_name">> => AccountName,
      <<"token_uri">> => token_uri(HostType, Invite),
      <<"landing_page">> => landing_page(HostType, Host, Invite)
     }.

%%--------------------------------------------------------------------
%%| hooks and callbacks

remove_user(Acc, #{jid := #jid{luser = LUser, lserver = LServer} = Jid}, #{host_type := HostType}) ->
    case try_db_call(HostType, remove_user, [LUser, LServer]) of
        {error, Reason} ->
            ?LOG_ERROR(#{what => mod_invites_remove_user_failed,
                         reason => Reason, acc => Acc, jid => Jid}),
            {ok, Acc};
        _ ->
            ?LOG_INFO(#{what => mod_invites_remove_user, jid => Jid}),
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
               #{host_type := HostType}) ->
    Invite = create_roster_invite(HostType, {LUser, LServer}),
    Form = mongoose_data_forms:form(
              #{type => <<"result">>,
                title => trans(Lang, <<"New Invite Token Created">>),
                fields =>
                    maybe_add_landing_url(HostType,
                                          LServer,
                                          Invite,
                                          Lang,
                                          [#{var => <<"FORM_TYPE">>,
                                             type => <<"hidden">>,
                                             values => [?NS_INVITE_INVITATION]},
                                           #{var => <<"uri">>,
                                             label => trans(Lang, <<"Invite URI">>),
                                             type => <<"text-single">>,
                                             values => [token_uri(HostType, Invite)]},
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
                 from := From},
               #{host_type := HostType}) ->
    check(fun create_account_allowed/2,
          [HostType, From],
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
          fun(Reason) -> {ok, {error, to_stanza_error(Lang, Reason)}} end);
adhoc_commands(empty,
               #{adhoc_request := #adhoc_request{node = ?NS_INVITE_CREATE_ACCOUNT = Node,
                                                 session_id = SID,
                                                 xdata = #xmlel{} = XData,
                                                 lang = Lang},
                 from := #jid{luser = LUser, lserver = LServer} = From,
                 to := #jid{lserver = LServer}},
               #{host_type := HostType}) ->
    case mongoose_data_forms:parse_form(XData) of
        #{type := <<"submit">>, kvs := KVs} ->
            check(fun create_account_allowed/2,
                  [HostType, From],
                  fun() ->
                          AccountName = hd(maps:get(<<"username">>, KVs, [<<>>])),
                          case
                              create_account_invite(HostType,
                                                    {LUser, LServer},
                                                    AccountName,
                                                    to_boolean(hd(maps:get(<<"roster-subscription">>, KVs, false))))
                          of
                              {error, Reason} ->
                                  {ok, {error, to_stanza_error(Lang, Reason)}};
                              Invite ->
                                  ResultFields =
                                      maybe_add_landing_url(HostType,
                                                            LServer,
                                                            Invite,
                                                            Lang,
                                                            [#{var => <<"FORM_TYPE">>,
                                                               type => <<"hidden">>,
                                                               values => [?NS_INVITE_INVITATION]},
                                                             #{var => <<"uri">>,
                                                               label => trans(Lang, <<"Invite URI">>),
                                                               type => <<"text-single">>,
                                                               values => [token_uri(HostType, Invite)]},
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
        {true, NewAcc} ->
            {stop, NewAcc};
        false ->
            {ok, Acc}
    end.

maybe_handle_pre_auth_token(Acc) ->
    case get_preauth_token(Acc) of
        undefined ->
            false;
        Token ->
            ?LOG_DEBUG("got preauth token: ~p", [Token]),
            #jid{luser = LUser, lserver = LServer} = To = jid:to_bare(mongoose_acc:to_jid(Acc)),
            HostType = mongoose_acc:host_type(Acc),
            case is_token_valid(HostType, Token, {LUser, LServer}) of
                true ->
                    ?LOG_DEBUG("got valid token! ~p", [Token]),
                    From = jid:to_bare(mongoose_acc:from_jid(Acc)),
                    ok = roster_add(mongoose_acc:host_type(Acc), To, From, #{subscription => from, ask => out}),
                    Acc1 = send_presence(Acc, To, From, <<"subscribed">>),
                    Acc2 = send_presence(Acc1, To, From, <<"subscribe">>),
                    set_invitee(HostType, LServer, Token, From),
                    {true, Acc2};
                false ->
                    ?LOG_INFO(#{what => maybe_handle_preauth_token_failed,
                                reason => token_invalid,
                                token => Token,
                                from => jid:to_binary(mongoose_acc:from_jid(Acc))}),
                    false
            end
    end.

get_preauth_token(Acc) ->
    case {mongoose_acc:stanza_name(Acc), mongoose_acc:stanza_type(Acc)} of
        {<<"presence">>, <<"subscribe">>} ->
            Presence = mongoose_acc:element(Acc),
            get_pars_token(Presence);
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

-spec disco_local_identity(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:identity_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
disco_local_identity(Acc = #{node := ?NS_INVITE_CREATE_ACCOUNT}, _, _) ->
    {ok, mongoose_disco:add_identities([?INFO_COMMAND(<<"Create Account">>)], Acc)};
disco_local_identity(Acc = #{node := ?NS_INVITE_INVITE}, _, _) ->
    {ok, mongoose_disco:add_identities([?INFO_COMMAND(<<"Invite User">>)], Acc)};
disco_local_identity(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec disco_local_features(Acc, Params, Extra) -> {ok, Acc} | {stop, exml:element()} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_features(Acc = #{node := Ns, from_jid := From}, _, #{host_type := HostType}) ->
    maybe
        allow ?=
            case Ns of
                ?NS_INVITE_CREATE_ACCOUNT ->
                    Access = gen_mod:get_module_opt(HostType, ?MODULE, access_create_account),
                    acl:match_rule(HostType, Access, From);
                ?NS_INVITE_INVITE ->
                    allow;
                _ ->
                    deny
            end,
        {ok, mongoose_disco:add_features([?NS_COMMANDS], Acc)}
    else
        deny ->
            {ok, Acc}
    end;
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_disco:item_acc(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
disco_local_items(Acc = #{from_jid := From, to_jid := #jid{lserver = LServer}, node := ?NS_COMMANDS},
                  _,
                  #{host_type := HostType}) ->
    InviteUser =
        #{jid => LServer,
          node => ?NS_INVITE_INVITE,
          name => <<"Invite User">>},
    CreateAccount =
        #{jid => LServer,
          node => ?NS_INVITE_CREATE_ACCOUNT,
          name => <<"Create Account">>},
    Items =
        case create_account_allowed(HostType, From) of
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
-spec stream_feature_register(Acc, map(), gen_hook:extra()) -> {ok, Acc} when Acc :: [exml:element()].
stream_feature_register(Acc, _, #{host_type := HostType}) ->
    {ok, mod_invites_register:stream_feature_register(Acc, HostType)}.

user_send_xmlel(Acc, Params, Extras) ->
    mod_invites_register:user_send_xmlel(Acc, Params, Extras).


%%--------------------------------------------------------------------
%%| helpers
get_invite(HostType, Host, Token) ->
    db_call(HostType, get_invite, [Host, Token]).

get_invites(HostType, Inviter) ->
    transaction(HostType, fun() -> get_invites_t(HostType, Inviter) end).

get_invites_t(HostType, {_User, Host} = Inviter) ->
    db_call(HostType, get_invites_t, [Host, Inviter]).

is_expired(#invite_token{expires = Expires}) ->
    Now = erlang:timestamp(),
    calendar:datetime_to_gregorian_seconds(Expires)
    < calendar:datetime_to_gregorian_seconds(
          calendar:now_to_universal_time(Now)).

is_reserved(HostType, Host, Token, AccountName) ->
    transaction(HostType, fun() -> is_reserved_t(HostType, Host, Token, AccountName) end).

is_reserved_t(HostType, Host, Token, AccountName) ->
    db_call(HostType, is_reserved_t, [Host, Token, AccountName]).

-spec is_token_valid(binary(), binary(), {binary(), binary()} | binary()) -> boolean().
is_token_valid(HostType, Token, Host) when is_binary(Host) ->
    is_token_valid(HostType, Token, {<<>>, Host});
is_token_valid(HostType, Token, {_User, Host} = Inviter) ->
    db_call(HostType, is_token_valid, [Host, Token, Inviter]).

set_invitee(HostType, Host, Token, #jid{} = InviteeJid) ->
    set_invitee(HostType,
                Host,
                Token,
                jid:to_bare_binary(InviteeJid),
                <<>>);
set_invitee(HostType, Host, Token, Invitee) ->
    set_invitee(HostType, Host, Token, Invitee, <<>>).

set_invitee(HostType, Host, Token, Invitee, AccountName) ->
    set_invitee(HostType, fun() -> ok end, Host, Token, Invitee, AccountName).

-spec set_invitee(binary(), fun(() -> ok | {error, any()}), binary(), binary(), binary(), binary()) ->
          ok | {error, term()}.
set_invitee(HostType, F, Host, Token, Invitee, AccountName) ->
    %% This invalidates the invite token if Invitee isn't empty
    db_call(HostType, set_invitee, [F, Host, Token, Invitee, AccountName]).

create_roster_invite(HostType, Inviter) ->
    create_invite(HostType, roster_only, Inviter, <<>>).

create_account_invite(HostType, Inviter, AccountName, _Subscribe = true) ->
    create_invite(HostType, account_subscription, Inviter, AccountName);
create_account_invite(HostType, Inviter, AccountName, _Subcribe = false) ->
    create_invite(HostType, account_only, Inviter, AccountName).

create_invite(HostType, Type, Inviter, AccountName) ->
    F = fun() -> create_invite_t(HostType, Type, Inviter, AccountName) end,
    transaction(HostType, F).

create_invite_t(HostType, Type, {_User, Host} = Inviter, AccountName) ->
    try invite_token_t(HostType, Type, Inviter, AccountName) of
        Invite ->
            db_call(HostType, create_invite_t, [Host, Invite])
    catch
        _:({error, _Reason} = Error) ->
            Error;
        _:Error ->
            {error, Error}
    end.

check_account_name_t(_HostType, <<>>, _) ->
    <<>>;
check_account_name_t(_HostType, error, _) ->
    {error, account_name_invalid};
check_account_name_t(_HostType, _, error) ->
    {error, hostname_invalid};
check_account_name_t(HostType, AccountName, Host) ->
    case lists:member(Host, mongoose_domain_api:get_domains_by_host_type(HostType)) of
        false ->
            {error, host_unknown};
        true ->
            case ejabberd_auth:does_user_exist(jid:make_bare(AccountName, Host)) of
                true ->
                    {error, user_exists};
                false ->
                    case is_reserved_t(HostType, Host, <<>>, AccountName) of
                        true ->
                            {error, reserved};
                        false ->
                            AccountName
                    end
            end
    end.

check_max_invites_t(_HostType, roster_only, _) ->
    ok;
check_max_invites_t(HostType, _Type, Inviter) ->
    case is_create_allowed_t(HostType, Inviter) of
        true ->
            ok;
        false ->
            {error, num_invites_exceeded}
    end.

is_create_allowed(HostType, Inviter) ->
    transaction(HostType, fun() -> is_create_allowed_t(HostType, Inviter) end).

is_create_allowed_t(HostType, Inviter) ->
    case get_max_invites(HostType, Inviter) of
        infinity ->
            true;
        MaxInvites ->
            Invites = get_invites_t(HostType, Inviter),
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

get_max_invites(_, {<<>>, _Server}) ->
    infinity;
get_max_invites(HostType, {User, Server}) ->
    case {gen_mod:get_module_opt(HostType, ?MODULE, max_invites),
          acl:match_rule(HostType, admin, jid:make_bare(User, Server))}
    of
        {infinity, _} ->
            infinity;
        {_, allow} ->
            infinity;
        {MaxInvites, deny} ->
            MaxInvites
    end.

check_overuse_t(_HostType, _Type, {<<>>, _Host}) ->
    ok;
check_overuse_t(HostType, Type, Inviter) ->
    case over_overuse_limit_t(HostType, Type, Inviter) of
        true ->
            {error, num_invites_exceeded};
        false ->
            ok
    end.

over_overuse_limit_t(HostType, Type, Inviter) ->
    case get_max_invites(HostType, Inviter) of
        infinity ->
            false;
        _ ->
            get_num_invites_t(Type, HostType, Inviter) >= ?OVERUSE_LIMIT
    end.

get_num_invites_t(roster_only, HostType, Inviter) ->
    length(get_invites_t(HostType, Inviter));
get_num_invites_t(_Type, HostType, Inviter) ->
    length(get_invites_tree_t(HostType, Inviter)).

get_invites_tree_t(HostType, {_User, Host} = Inviter) ->
    Now = calendar:datetime_to_gregorian_seconds(
              calendar:now_to_datetime(
                  erlang:timestamp())),
    Root = find_invites_tree_root_t(HostType, Now, Host, Inviter, 0),
    get_invites_tree_as_root_t(HostType, Host, Root).

find_invites_tree_root_t(HostType, Now, Host, Invitee, Lvl) ->
    case get_invite_by_invitee_t(HostType, Host, Invitee) of
        #invite_token{inviter = {<<>>, _}} ->
            Invitee;
        #invite_token{inviter = Inviter, created_at = CreatedAt} ->
            maybe_block_speedy_goat(Now, CreatedAt, Lvl),
            find_invites_tree_root_t(HostType, Now, Host, Inviter, Lvl + 1);
        {error, not_found} ->
            Invitee
    end.

-spec get_invite_by_invitee_t(binary(), binary(), {binary(), binary()}) ->
                                 invite_token() | {error, not_found}.
get_invite_by_invitee_t(_HostType, _Host, {<<>>, _Server}) ->
    {error, not_found};
get_invite_by_invitee_t(HostType, Host, {User, Server}) ->
    db_call(HostType, get_invite_by_invitee_t, [Host, {User, Server}]).

maybe_block_speedy_goat(Now, CreatedAt, Lvl) when Lvl == ?SPEEDY_GOAT_LEVELS ->
    Then = calendar:datetime_to_gregorian_seconds(CreatedAt),
    if Now - Then < ?SPEEDY_GOAT_SECONDS ->
           throw(speedy_goat);
       true ->
           ok
    end;
maybe_block_speedy_goat(_, _, _) ->
    ok.

-spec get_invites_tree_as_root_t(binary(), binary(), {binary(), binary()}) -> [invite_token()].
get_invites_tree_as_root_t(HostType, Host, Inviter) ->
    Invites = get_invites_t(HostType, Inviter),
    get_invites_tree_as_root_t(HostType, Host, Inviter, Invites, []).

get_invites_tree_as_root_t(_HostType,_Host, _Inviter, [], Acc) ->
    Acc;
get_invites_tree_as_root_t(HostType,
                           Host,
                           Inviter,
                           [#invite_token{type = roster_only, account_name = <<>>} | Invites],
                           Acc) ->
    get_invites_tree_as_root_t(HostType, Host, Inviter, Invites, Acc);
get_invites_tree_as_root_t(HostType,
                           Host,
                           Inviter,
                           [#invite_token{invitee = <<>>} = Invite | Invites],
                           Acc) ->
    get_invites_tree_as_root_t(HostType, Host, Inviter, Invites, [Invite | Acc]);
get_invites_tree_as_root_t(HostType,
                           Host,
                           Inviter,
                           [#invite_token{invitee = InviteeJID} = Invite | Invites],
                           Acc) ->
    case jid:from_binary(InviteeJID) of
        #jid{luser = Invitee, lserver = Host} ->
            get_invites_tree_as_root_t(HostType,
                                       Host,
                                       Inviter,
                                       Invites,
                                       [Invite | Acc]
                                       ++ get_invites_tree_as_root_t(HostType, Host, {Invitee, Host}));
        _Nomatch ->
            get_invites_tree_as_root_t(HostType, Host, Inviter, Invites, [Invite | Acc])
    end.

maybe_throw({error, _} = Error) ->
    throw(Error);
maybe_throw(Good) ->
    Good.

invite_token_t(HostType, Type, {_User, Host} = Inviter, AccountName0) ->
    maybe_throw(check_max_invites_t(HostType, Type, Inviter)),
    maybe_throw(check_overuse_t(HostType, Type, Inviter)),
    Token = p1_rand:get_alphanum_string(?DEFAULT_TOKEN_LENGTH),
    AccountName = maybe_throw(check_account_name_t(HostType, jid:nodeprep(AccountName0), Host)),
    ExpireSeconds = gen_mod:get_module_opt(HostType, ?MODULE, token_expire_seconds),
    set_token_expires(#invite_token{token = Token,
                                    inviter = Inviter,
                                    type = Type,
                                    account_name = AccountName},
                      ExpireSeconds).

-spec create_reset_token(binary(), binary(), binary()) -> invite_token() | {error, any()}.
create_reset_token(HostType, User, Host) ->
    maybe
        (#invite_token{} = ResetToken) ?= reset_token(HostType, User, Host),
        F = fun() -> db_call(HostType, create_invite_t, [Host, ResetToken]) end,
        transaction(Host, F)
    end.

reset_token(HostType, User, Host) ->
    maybe
        true ?= lists:member(
                  Host,
                  mongoose_domain_api:get_domains_by_host_type(HostType))
            orelse {error, host_unknown},
        true ?= ejabberd_auth:does_stored_user_exist(Host, jid:make_bare(User, Host)) orelse {error, user_not_exists},
        set_token_expires(#invite_token{token =
                                            p1_rand:get_alphanum_string(?DEFAULT_TOKEN_LENGTH),
                                        inviter = {<<>>, Host},
                                        type = reset_token,
                                        account_name = User},
                          gen_mod:get_module_opt(HostType, ?MODULE, token_expire_seconds))
    end.

token_uri(HostType,
          #invite_token{type = roster_only,
                        token = Token,
                        inviter = {User, Host}}) ->
    Jid = jid:make_bare(User, Host),
    IBR = maybe_add_ibr_allowed(HostType, Jid),
    Inviter =
        jid:to_binary(
            Jid),
    <<"xmpp:", Inviter/binary, "?roster;preauth=", Token/binary, IBR/binary>>;
token_uri(_HostType,
          #invite_token{token = Token,
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

maybe_add_ibr_allowed(HostType, Jid) ->
    case create_account_allowed(HostType, Jid) of
        ok ->
            <<";ibr=y">>;
        {error, not_allowed} ->
            <<>>
    end.

landing_page(HostType, Host, Invite) ->
    mod_invites_http:landing_page(HostType, Host, Invite).

-spec db_call(binary(), atom(), [any()]) -> any().
db_call(HostType, Fun, Args) ->
    try
        mongoose_backend:call(HostType, mod_invites_db, Fun, Args)
    catch
        _:badarg ->
            throw({error, host_unknown})
    end.

%% father forgive me
lift({error, _R} = E) ->
    E;
lift({ok, _V} = R) ->
    R;
lift(Res) ->
    {ok, Res}.

-spec try_db_call(HostType :: binary(), Fun :: atom(), Args :: [any()]) ->
                     {ok, any()} | {error, any()}.
try_db_call(HostType, Fun, Args) ->
    try
        lift(db_call(HostType, Fun, Args))
    catch
        _:({error, _Reason} = Error) ->
            Error;
        error:Error ->
            {error, Error}
    end.

transaction(HostType, F) ->
    try db_call(HostType, transaction, [HostType, F]) of
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

maybe_add_landing_url(HostType, Host, Invite, Lang, Fields) ->
    case landing_page(HostType, Host, Invite) of
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

-spec create_account_allowed(HostType :: binary(), User :: jid:jid()) -> ok | {error, not_allowed}.
create_account_allowed(HostType, User) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, access_create_account) of
        none ->
            {error, not_allowed};
        Access ->
            case acl:match_rule(HostType, Access, User) of
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

-spec roster_add(mongooseim:host_type(), jid:jid(), jid:jid(), map()) -> ok | {error, any()}.
roster_add(HostType, UserJID, RosterItemJID, Params) ->
    UpdateF = update_item_from_params_f(Params),
    mod_roster:set_roster_item(HostType, RosterItemJID, UserJID, UserJID, UpdateF).

update_item_from_params_f(Params) ->
    fun(Item) ->
            maps:fold(fun maybe_update/3, Item, Params)
    end.

maybe_update(name, Name, I) ->
    I#roster{name = Name};
maybe_update(group, Groups, I) ->
    I#roster{groups = Groups};
maybe_update(subscription, Subscription, I) ->
    I#roster{subscription = Subscription};
maybe_update(ask, Ask, I) ->
    I#roster{ask = Ask};
maybe_update(_, _, I) ->
    I.

send_presence(Acc, FromJid, ToJid, Type) ->
    Presence = #xmlel{name = <<"presence">>,
                      attrs = #{<<"to">> => jid:to_binary(ToJid),
                                <<"type">> => Type}},
    Acc1 = mongoose_acc:update(FromJid, ToJid, Presence, Acc),
    mongoose_router:route(Acc1).

get_pars_token(Xmlel) ->
    exml_query:path(Xmlel, [{element_with_ns, <<"preauth">>, ?NS_PARS}, {attr, <<"token">>}], undefined).

-spec pretty_format_command_result({error, atom()} | {ok, term() | term()}) -> {error, iodata()} | term().
pretty_format_command_result({error, Error}) ->
    {error, pretty_format_command_error(Error)};
pretty_format_command_result({ok, Result}) ->
    Result;
pretty_format_command_result(Result) ->
    Result.

pretty_format_command_error({module_not_loaded, ?MODULE, Host}) ->
     lists:flatten(
         io_lib:format("Virtual host not known: ~s", [binary_to_list(Host)]));
pretty_format_command_error(host_unknown) ->
    "Virtual host not known";
pretty_format_command_error(not_found) ->
    "Token not found";
pretty_format_command_error(user_exists) ->
    "Username already taken";
pretty_format_command_error(user_not_exists) ->
    "User does not exist";
pretty_format_command_error(reserved) ->
    "Username is reserved";
pretty_format_command_error(account_name_invalid) ->
    "Username is invalid".
