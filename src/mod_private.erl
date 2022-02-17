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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_private).
-author('alexey@process-one.net').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2,
         stop/1,
         supported_features/0,
         config_spec/0,
         process_iq/5,
         remove_user/3,
         remove_domain/3]).

-export([get_personal_data/3]).

-export([config_metrics/1]).

-ignore_xref([
    behaviour_info/1, get_personal_data/3, remove_user/3, remove_domain/3
]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-xep([{xep, 49}, {version, "1.2"}]).

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(gdpr:personal_data(), mongooseim:host_type(), jid:jid()) -> gdpr:personal_data().
get_personal_data(Acc, HostType, #jid{ luser = LUser, lserver = LServer }) ->
    Schema = ["ns", "xml"],
    NSs = mod_private_backend:get_all_nss(HostType, LUser, LServer),
    Entries = lists:map(
                fun(NS) ->
                        Data = mod_private_backend:multi_get_data(
                                 HostType, LUser, LServer, [{NS, default}]),
                        { NS, exml:to_binary(Data) }
                end, NSs),
    [{private, Schema, Entries} | Acc].

%% ------------------------------------------------------------------
%% gen_mod callbacks

-spec start(HostType :: mongooseim:host_type(), Opts :: gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mod_private_backend:init(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_PRIVATE, ejabberd_sm,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc).

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_PRIVATE, ejabberd_sm),
    ok.

supported_features() -> [dynamic_domains].

hooks(HostType) ->
    [{remove_user, HostType, ?MODULE, remove_user, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {anonymous_purge_hook, HostType, ?MODULE, remove_user, 50},
     {get_personal_data, HostType, ?MODULE, get_personal_data, 50}].

config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_private}},
                 <<"riak">> => riak_config_spec()},
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"backend">> => rdbms},
       format_items = map
      }.

riak_config_spec() ->
    #section{
       items = #{<<"bucket_type">> => #option{type = binary,
                                              validate = non_empty}
                },
       defaults = #{<<"bucket_type">> => <<"private">>},
       format_items = map
      }.

%% ------------------------------------------------------------------
%% Handlers

remove_user(Acc, User, Server) ->
    HostType = mongoose_acc:host_type(Acc),
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    R = mod_private_backend:remove_user(HostType, LUser, LServer),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mod_private_backend:remove_domain(HostType, Domain),
    Acc.

process_iq(Acc,
           From = #jid{lserver = LServer, luser = LUser},
           To = #jid{lserver = LServer, luser = LUser},
           IQ = #iq{type = Type, sub_el = SubElem = #xmlel{children = Elems}},
           _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    IsEqual = compare_bare_jids(From, To),
    Strategy = choose_strategy(IsEqual, Type),
    Res = case Strategy of
        get ->
            NS2XML = to_map(Elems),
            XMLs = mod_private_backend:multi_get_data(HostType, LUser, LServer, NS2XML),
            IQ#iq{type = result, sub_el = [SubElem#xmlel{children = XMLs}]};
        set ->
            NS2XML = to_map(Elems),
            Result = mod_private_backend:multi_set_data(HostType, LUser, LServer, NS2XML),
            case Result of
                ok ->
                    IQ#iq{type = result, sub_el = [SubElem]};
                {error, Reason} ->
                    ?LOG_ERROR(#{what => multi_set_data_failed, reason => Reason,
                                 user => LUser, server => LServer}),
                    error_iq(IQ, mongoose_xmpp_errors:internal_server_error());
                {aborted, Reason} ->
                    ?LOG_ERROR(#{what => multi_set_data_aborted, reason => Reason,
                                 user => LUser, server => LServer}),
                    error_iq(IQ, mongoose_xmpp_errors:internal_server_error())
            end;
        forbidden ->
            error_iq(IQ, mongoose_xmpp_errors:forbidden())
    end,
    {Acc, Res};
process_iq(Acc, _From, _To, IQ, _Extra) ->
    Txt = <<"Only requests from/to your JID are allowed">>,
    Err = mongoose_xmpp_errors:forbidden(<<"en">>, Txt),
    Res = error_iq(IQ, Err),
    {Acc, Res}.

%% ------------------------------------------------------------------
%% Helpers

choose_strategy(true, get) -> get;
choose_strategy(true, set) -> set;
choose_strategy(_,    _  ) -> forbidden.

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

config_metrics(Host) ->
    OptsToReport = [{backend, mnesia}], %list of tuples {option, defualt_value}
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).
