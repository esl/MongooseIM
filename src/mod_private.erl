%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_private).
-author('alexey@process-one.net').

-behaviour(gen_mod).
-behaviour(gdpr).

-export([start/2,
         stop/1,
         process_sm_iq/4,
         remove_user/3,
         remove_user/2]).

-export([get_personal_data/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-xep([{xep, 49}, {version, "1.2"}]).

%% ------------------------------------------------------------------
%% Backend callbacks

-callback init(Host, Opts) -> ok when
    Host    :: binary(),
    Opts    :: list().

-callback multi_set_data(LUser, LServer, NS2XML) -> Result when
    LUser   :: binary(),
    LServer :: binary(),
    NS2XML  :: [{NS, XML}],
    NS      :: binary(),
    XML     :: #xmlel{},
    Reason  :: term(),
    Result  :: ok | {aborted, Reason} | {error, Reason}.

-callback multi_get_data(LUser, LServer, NS2Def) -> [XML | Default] when
    LUser   :: binary(),
    LServer :: binary(),
    NS2Def  :: [{NS, Default}],
    NS      :: binary(),
    Default :: term(),
    XML     :: #xmlel{}.

-callback remove_user(LUser, LServer) -> any() when
    LUser   :: binary(),
    LServer :: binary().

-callback get_all_nss(LUser, LServer) -> NSs when
    LUser   :: binary(),
    LServer :: binary(),
    NSs      :: [binary()].

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(jid:user(), jid:server()) ->
    [{gdpr:data_group(), gdpr:schema(), gdpr:entries()}].
get_personal_data(Username, Server) ->
    LUser = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    Schema = ["ns", "xml"],
    Entries =
    lists:flatmap(fun(B) ->
                          get_personal_data_from_backend(B, LUser, LServer)
                  end, mongoose_lib:find_behaviour_implementations(mod_private)),
    [{private, Schema, Entries}].

get_personal_data_from_backend(Backend, LUser, LServer) ->
    try
        NSs = Backend:get_all_nss(LUser, LServer),
        lists:map(
          fun(NS) ->
                  { NS, exml:to_binary(Backend:multi_get_data(LUser, LServer, [{NS, default}])) }
          end, NSs)
    catch
        C:R ->
            ?WARNING_MSG("event=cannot_retrieve_personal_data,"
                         "backend=~p,class=~p,reason=~p,stacktrace=~p",
                         [Backend, C, R, erlang:get_stacktrace()]),
            []
    end.

%% ------------------------------------------------------------------
%% gen_mod callbacks

start(Host, Opts) ->
    gen_mod:start_backend_module(?MODULE, Opts, [multi_get_data, multi_set_data]),
    mod_private_backend:init(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
                                  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE).


%% ------------------------------------------------------------------
%% Handlers

remove_user(Acc, User, Server) ->
    R = remove_user(User, Server),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    mod_private_backend:remove_user(LUser, LServer).

process_sm_iq(
        From = #jid{luser = LUser, lserver = LServer},
        To   = #jid{},
        Acc,
        IQ   = #iq{type = Type, sub_el = SubElem = #xmlel{children = Elems}}) ->
    IsKnown = lists:member(LServer, ?MYHOSTS),
    IsEqual = compare_bare_jids(From, To),
    Strategy = choose_strategy(IsKnown, IsEqual, Type),
    Res = case Strategy of
        get ->
            NS2XML = to_map(Elems),
            XMLs = mod_private_backend:multi_get_data(LUser, LServer, NS2XML),
            IQ#iq{type = result, sub_el = [SubElem#xmlel{children = XMLs}]};
        set ->
            NS2XML = to_map(Elems),
            Result = mod_private_backend:multi_set_data(LUser, LServer, NS2XML),
            case Result of
                ok ->
                    IQ#iq{type = result, sub_el = [SubElem]};
                {error, Reason} ->
                    ?ERROR_MSG("~p:multi_set_data failed ~p for ~ts@~ts.",
                               [mod_private_backend, Reason, LUser, LServer]),
                    error_iq(IQ, mongoose_xmpp_errors:internal_server_error());
                {aborted, Reason} ->
                    ?ERROR_MSG("~p:multi_set_data aborted ~p for ~ts@~ts.",
                               [mod_private_backend, Reason, LUser, LServer]),
                    error_iq(IQ, mongoose_xmpp_errors:internal_server_error())
            end;
        not_allowed ->
            error_iq(IQ, mongoose_xmpp_errors:not_allowed());
        forbidden ->
            error_iq(IQ, mongoose_xmpp_errors:forbidden())
    end,
    {Acc, Res}.

%% ------------------------------------------------------------------
%% Helpers

choose_strategy(true,  true, get) -> get;
choose_strategy(true,  true, set) -> set;
choose_strategy(false, _,    _  ) -> not_allowed;
choose_strategy(_,     _,    _  ) -> forbidden.

compare_bare_jids(#jid{luser = LUser, lserver = LServer},
                  #jid{luser = LUser, lserver = LServer}) -> true;
compare_bare_jids(_, _) -> false.

element_to_namespace(#xmlel{attrs = Attrs}) ->
    xml:get_attr_s(<<"xmlns">>, Attrs);
element_to_namespace(_) ->
    <<>>.

%% Skip invalid elements.
to_map(Elems) ->
    [{NS, Elem} || Elem <- Elems, is_valid_namespace(NS = element_to_namespace(Elem))].

is_valid_namespace(Namespace) -> Namespace =/= <<>>.

error_iq(IQ=#iq{sub_el=SubElem}, ErrorStanza) ->
    IQ#iq{type = error, sub_el = [SubElem, ErrorStanza]}.
