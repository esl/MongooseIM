%%%----------------------------------------------------------------------
%%% File    : mod_invites_register.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Provide web page(s) to sign up using an invite token.
%%% Created : Fri Oct 31 2025 by Stefan Strigler <stefan@strigler.de>
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
-module(mod_invites_register).

-author('stefan@strigler.de').

-export([user_send_xmlel/3, stream_feature_register/2]).
-export([try_register_or_reset/7]).

-include("mongoose.hrl").
-include("mod_invites.hrl").
-include("jlib.hrl").

-spec stream_feature_register([#xmlel{}], binary()) -> [#xmlel{}].
stream_feature_register(Acc, HostType) ->
    case gen_mod:get_module_opt(HostType, mod_invites, access_create_account) of
        none ->
            Acc;
        _ ->
            [#xmlel{name = <<"register">>, attrs = #{<<"xmlns">> => ?NS_FEATURE_IBR_TOKEN}} | Acc]
    end.

-spec user_send_xmlel(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_xmlel(Acc, Params, Extra) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"iq">> ->
            {Iq, Acc1} = mongoose_iq:info(Acc),
            handle_unauthenticated_iq(Acc1, Params, Extra, Iq);
        _ -> {ok, Acc}
    end.

handle_unauthenticated_iq(Acc,
                          #{c2s_data := StateData},
                          #{host_type := _HostType},
                          #iq{type = set, xmlns=?NS_PARS} = IQ) ->
    Token = exml_query:path(mongoose_iq:iq_to_sub_el(IQ), [{attr, <<"token">>}], <<>>),
    LServer = mongoose_c2s:get_lserver(StateData),
    %% invite is stored in state (ResAcc) so we have access at next step
    {ResAcc, ResIQ} = process_token(Acc, LServer, Token, IQ),
    Res = make_iq_response_acc(ResIQ, ResAcc, jid:make_noprep(<<>>, LServer, <<>>)),
    {stop, Res};
handle_unauthenticated_iq(Acc,
                          #{c2s_data := StateData},
                          #{host_type := HostType},
                          #iq{type = set, xmlns=?NS_REGISTER, lang = Lang} = IQ) ->
    LServer = mongoose_c2s:get_lserver(StateData),
    FromServer = jid:make_noprep(<<>>, LServer, <<>>),
    case mongoose_c2s:get_mod_state(StateData, mod_invites) of
        {ok, Invite} ->
            case check_form(mongoose_iq:iq_to_sub_el(IQ)) of
                {ok, {Username, Password}} ->
                    {Address, _} = mongoose_c2s:get_ip(StateData),
                    case try_register_or_reset(Acc, Invite, Username, LServer, Password, Address, Lang) of
                        {ok, NewAcc0, UpdatedInvite} ->
                            NewAcc = mongoose_c2s_acc:to_acc(NewAcc0, state_mod, {mod_invites, UpdatedInvite}),
                            {stop, make_iq_response_acc(mongoose_iq:empty_result_iq(IQ), NewAcc, FromServer)};
                        {error, Err} ->
                            ResIQ = error_response(IQ, Err),
                            {stop, make_iq_response_acc(ResIQ, Acc, FromServer)}
                    end;
                {error, BadRes} ->
                    ?LOG_INFO(#{what => invites_iq_set_register_check_form, host => HostType, value => BadRes}),
                    ResIQ = error_response(IQ, mongoose_xmpp_errors:bad_request()),
                    {stop, make_iq_response_acc(ResIQ, Acc, FromServer)}
            end;
        _ ->
            %% This is to protect regular IBR (w/0 token, if enabled) from taking a reserved name
            case check_form(mongoose_iq:iq_to_sub_el(IQ)) of
                {ok, {Username, _Password}} ->
                    case mod_invites:is_reserved(HostType, LServer, <<>>, Username) of
                        true ->
                            ResIQ = error_response(IQ, mongoose_xmpp_errors:not_allowed()),
                            {stop, make_iq_response_acc(ResIQ, Acc, FromServer)};
                        false ->
                            {ok, Acc}
                    end;
                _ ->
                    {ok, Acc}
            end
    end;
handle_unauthenticated_iq(Acc, _Params, _Extra, _IQ) ->
    {ok, Acc}.

make_iq_response_acc(IQ, Acc, From) ->
    make_iq_response_acc(IQ, Acc, From, #jid{}).

make_iq_response_acc(IQ, Acc, From, To) ->
    Response = set_sender(jlib:iq_to_xml(IQ), From),
    AccParams = #{from_jid => From, to_jid => To, element => Response},
    ResponseAcc = mongoose_acc:update_stanza(AccParams, Acc),
    mongoose_c2s_acc:to_acc(Acc, route, ResponseAcc).


set_sender(#xmlel{attrs = A} = Stanza, #jid{} = From) ->
    Stanza#xmlel{attrs = A#{<<"from">> => jid:to_binary(From)}}.

maybe_create_mutual_subscription(Acc, #invite_token{inviter = {User, _Server}, type = Type})
    when User == <<>>; % server token
         Type /= account_subscription ->
    Acc;
maybe_create_mutual_subscription(Acc, #invite_token{inviter = {User, Server},
                                                    invitee = Invitee}) ->
    InviterJID = jid:make_bare(User, Server),
    InviteeJID = jid:from_binary(Invitee),
    HostType = mongoose_acc:host_type(Acc),
    mod_invites:roster_add(HostType, InviterJID, InviteeJID, #{subscription => both}),
    mod_invites:roster_add(HostType, InviteeJID, InviterJID, #{subscription => both}),
    Acc.

process_token(Acc, Host, Token, #iq{lang = Lang} = IQ) ->
    case can_create_account_or_change_pw(mongoose_acc:host_type(Acc), Host, Token) of
        {true, Invite} ->
            NewAcc = mongoose_c2s_acc:to_acc(Acc, state_mod, {mod_invites, Invite}),
            {NewAcc, mongoose_iq:empty_result_iq(IQ)};
        false ->
            {Acc, preauth_invalid(IQ, Lang)}
    end.

can_create_account_or_change_pw(HostType, Host, Token) ->
    try mod_invites:is_token_valid(HostType, Token, {<<>>, Host}) of
        true ->
            case mod_invites:get_invite(HostType, Host, Token) of
                #invite_token{type = reset_token} = Invite ->
                    {true, Invite};
                #invite_token{type = roster_only, account_name = AccountName}
                    when AccountName /= <<>> ->
                    false;
                Invite ->
                    maybe
                        true ?= create_account_allowed(HostType, Invite),
                        {true, Invite}
                    end
            end;
        false ->
            false
    catch
        _:not_found ->
            false
    end.

create_account_allowed(HostType, #invite_token{type = roster_only} = Invite) ->
    #invite_token{inviter = Inviter} = Invite,
    case mod_invites:is_create_allowed(HostType, Inviter) of
        true ->
            NumInvites =
                length(mod_invites:transaction(HostType,
                                               fun() ->
                                                  mod_invites:get_invites_tree_t(HostType, Inviter)
                                               end)),
            NumInvites < ?OVERUSE_LIMIT;
        false ->
            false
    end;
create_account_allowed(_, #invite_token{inviter = {<<>>, _Host}}) ->
    true;
create_account_allowed(HostType, #invite_token{inviter = {User, Host}}) ->
    mod_invites:create_account_allowed(HostType, jid:make_bare(User, Host)) == ok.

preauth_invalid(IQ, _Lan2g) ->
    Text = ?BIN("The token provided is either invalid or expired."),
    error_response(IQ, mongoose_xmpp_errors:item_not_found(Text)).

-spec try_register_or_reset(mongoose_acc:t(),
                            mod_invites:invite_token(),
                            binary(),
                            binary(),
                            binary(),
                            tuple(),
                            binary()) ->
          {ok, mongoose_acc:t(),  mod_invites:invite_token()} | {error, exml:element()}.
try_register_or_reset(Acc,
                      #invite_token{type = reset_token} = Invite,
                      User,
                      Server,
                      Password,
                      _Source,
                      Lang) ->
    case Invite#invite_token.account_name == User of
        true ->
            HostType = mongoose_acc:host_type(Acc),
            ChPwF = fun() -> mod_register:try_set_password(HostType, jid:make_bare(User, Server), Password) end,
            NewInvite =
                #invite_token{invitee = Invitee} =
                    maybe_set_invitee(Invite, jid:make_bare(User, Server)),
            case mod_invites:set_invitee(HostType, ChPwF, Server, Invite#invite_token.token, Invitee, User) of
                ok ->
                    {ok, Acc, NewInvite};
                {error, #xmlel{} = XmlEl} ->
                    {error, XmlEl}
            end;
        false ->
            {error, to_xmpp_error(not_allowed, Lang)}
    end;
try_register_or_reset(Acc, Invite, User, Server, Password, Source, Lang) ->
    #invite_token{token = Token} = Invite,
    HostType = mongoose_acc:host_type(Acc),
    case {jid:nodeprep(User), not mod_invites:is_reserved(HostType, Server, Token, User)} of
        {error, _} ->
            {error, to_xmpp_error(invalid_jid, Lang)};
        {_, false} ->
            {error, to_xmpp_error(not_allowed, Lang)};
        {_, true} ->
            UserJid = jid:make_bare(User, Server),
            RegF =
                fun() ->
                        mod_register:verify_password_and_register(
                          HostType, UserJid, Password, Source)
                end,
            NewInvite =
                #invite_token{invitee = Invitee, account_name = AccountName} =
                    maybe_set_account_name(
                      maybe_set_invitee(Invite, UserJid),
                      User),
            case mod_invites:set_invitee(HostType, RegF, Server, Token, Invitee, AccountName) of
                ok ->
                    Acc0 = maybe_create_mutual_subscription(Acc, NewInvite),
                    {ok, Acc0, NewInvite};
                {error, conflict} ->
                    ?LOG_WARNING(#{what => try_register_or_reset_failed,
                                   reason => conflict,
                                   invite => NewInvite}),
                    {error, to_xmpp_error(conflict, Lang)};
                {error, #xmlel{} = XmlEl} ->
                    {error, XmlEl}
            end
    end.

to_xmpp_error(Why, _Lang) when Why == not_allowed; Why == invalid_password ->
    mongoose_xmpp_errors:not_allowed();
to_xmpp_error(invalid_jid = _Why, _Lang) ->
    mongoose_xmpp_errors:jid_malformed();
to_xmpp_error(conflict, _Lang) ->
    mongoose_xmpp_errors:conflict();
to_xmpp_error(_Unexpected, _Lang) ->
    mongoose_xmpp_errors:internal_server_error().

check_form(XmlEl) ->
    case
        {
         exml_query:path(XmlEl, [{element, <<"username">>}, cdata]),
         exml_query:path(XmlEl, [{element, <<"password">>}, cdata])
        }
    of
        {Username, Password} when is_binary(Username),
                                  is_binary(Password) ->
            {ok, {Username, Password}};
        BadRes ->
            {error, {bad_form, BadRes}}
    end.

maybe_set_invitee(#invite_token{type = roster_only} = Invite, _Invitee) ->
    Invite;
maybe_set_invitee(Invite, Invitee) ->
    Invite#invite_token{invitee = jid:to_binary(Invitee)}.

maybe_set_account_name(#invite_token{type = roster_only} = Invite, AccountName) ->
    Invite#invite_token{account_name = AccountName};
maybe_set_account_name(Invite, _AccountName) ->
    Invite.

error_response(Request, Reason) ->
    Request#iq{type = error, sub_el = Reason}.
