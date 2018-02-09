%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_vcard.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_admin_extra_vcard).
-author('badlop@process-one.net').

-export([
    commands/0,

    get_vcard/3,
    get_vcard/4,
    set_vcard/4,
    set_vcard/5
]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
    Vcard1FieldsString = "Some vcard field names in get/set_vcard are:\n"
                         " FN - Full Name\n"
                         " NICKNAME - Nickname\n"
                         " BDAY - Birthday\n"
                         " TITLE - Work: Position\n"
                         " ROLE - Work: Role",

    Vcard2FieldsString = "Some vcard field names and subnames in get/set_vcard2 are:\n"
                         " N FAMILY - Family name\n"
                         " N GIVEN - Given name\n"
                         " N MIDDLE - Middle name\n"
                         " ADR CTRY - Address: Country\n"
                         " ADR LOCALITY - Address: City\n"
                         " EMAIL USERID - E-Mail Address\n"
                         " ORG ORGNAME - Work: Company\n"
                         " ORG ORGUNIT - Work: Department",

    VcardXEP = "For a full list of vCard fields check XEP-0054: vcard-temp at "
               "http://www.xmpp.org/extensions/xep-0054.html",

    [
        #ejabberd_commands{name = get_vcard, tags = [vcard],
                           desc = "Get content from a vCard field",
                           longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = get_vcard,
                           args = [{user, binary}, {host, binary}, {name, binary}],
                           result = {content, binary}},
        #ejabberd_commands{name = get_vcard2, tags = [vcard],
                           desc = "Get content from a vCard field",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = get_vcard,
                           args = [{user, binary}, {host, binary},
                                   {name, binary}, {subname, binary}],
                           result = {content, binary}},
        #ejabberd_commands{name = get_vcard2_multi, tags = [vcard],
                           desc = "Get multiple contents from a vCard field",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = get_vcard,
                           args = [{user, binary}, {host, binary},
                                   {name, binary}, {subname, binary}],
                           result = {contents, {list, {value, binary}}}},
        #ejabberd_commands{name = set_vcard, tags = [vcard],
                           desc = "Set content in a vCard field",
                           longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, binary}, {host, binary},
                                   {name, binary}, {content, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = set_vcard2, tags = [vcard],
                           desc = "Set content in a vCard subfield",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, binary}, {host, binary}, {name, binary},
                                   {subname, binary}, {content, binary}],
                           result = {res, restuple}},
        #ejabberd_commands{name = set_vcard2_multi, tags = [vcard],
                           desc = "Set multiple contents in a vCard subfield",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n"
                                      ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, binary}, {host, binary}, {name, binary},
                                   {subname, binary}, {contents, {list, binary}}],
                           result = {res, restuple}}
    ].

%%%
%%% Vcard
%%%
-spec get_vcard(jid:user(), jid:server(), any())
               -> {error, string()} | [binary()].
get_vcard(User, Host, Name) ->
    Acc = mongoose_acc:new(),
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            get_vcard_content(Acc, User, Host, [Name]);
        false ->
            {error, io_lib:format("User ~s@~s does not exist", [User, Host])}
    end.

-spec get_vcard(jid:user(), jid:server(), any(), any())
               -> {error, string()} | [binary()].
get_vcard(User, Host, Name, Subname) ->
    Acc = mongoose_acc:new(),
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            get_vcard_content(Acc, User, Host, [Name, Subname]);
        false ->
            {error, io_lib:format("User ~s@~s does not exist", [User, Host])}
    end.

-spec set_vcard(jid:user(), jid:server(), [binary()],
                binary() | [binary()]) -> {ok, string()} | {user_does_not_exist, string()}.
set_vcard(User, Host, Name, SomeContent) ->
    Acc = mongoose_acc:new(),
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            set_vcard_content(Acc, User, Host, [Name], SomeContent);
        false ->
            {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Host])}
    end.

-spec set_vcard(jid:user(), jid:server(), [binary()], [binary()],
                binary() | [binary()]) -> {ok, string()} | {user_does_not_exist, string()}.
set_vcard(User, Host, Name, Subname, SomeContent) ->
    Acc = mongoose_acc:new(),
    case ejabberd_auth:is_user_exists(User, Host) of
        true ->
            set_vcard_content(Acc, User, Host, [Name, Subname], SomeContent);
        false ->
            {user_does_not_exist, io_lib:format("User ~s@~s does not exist", [User, Host])}
    end.


%%
%% Internal vcard

-spec get_module_resource(jid:server()) -> string().
get_module_resource(Server) ->
    case gen_mod:get_module_opt(Server, ?MODULE, module_resource, none) of
        none -> atom_to_list(?MODULE);
        R when is_list(R) -> R
    end.


-spec get_vcard_content(mongoose_acc:t(), jid:user(), jid:server(), any())
                       -> {error, string()} | list(binary()).
get_vcard_content(Acc, User, Server, Data) ->
    JID = jid:make(User, Server, list_to_binary(get_module_resource(Server))),
    IQ = #iq{type = get, xmlns = ?NS_VCARD, sub_el = []},
    {_, IQr} = mod_vcard:process_sm_iq(JID, JID, Acc, IQ),
    case IQr#iq.sub_el of
        [#xmlel{} = A1] ->
            case get_vcard(Data, A1) of
                [] ->
                    {error, "Value not found in vcard"};
                ElemList ->
                    [exml_query:cdata(Elem) || Elem <- ElemList]
            end;
        _ ->
            {error, "Vcard not found"}
    end.


-spec get_vcard([binary()], exml:element()) -> [exml:element()].
get_vcard([Data1, Data2], A1) ->
    A2List = exml_query:subelements(A1, Data1),
    lists:flatten([get_vcard([Data2], A2) || A2 <- A2List]);
get_vcard([Data], A1) ->
    exml_query:subelements(A1, Data).

-spec set_vcard_content(mongoose_acc:t(), jid:user(), jid:server(), Data :: [binary()],
                        ContentList :: binary() | [binary()]) -> {ok, string()}.
set_vcard_content(Acc, U, S, D, SomeContent) when is_binary(SomeContent) ->
    set_vcard_content(Acc, U, S, D, [SomeContent]);
set_vcard_content(Acc, User, Server, Data, ContentList) ->
    JID = jid:make(User, Server, <<>>),
    IQ = #iq{type = get, xmlns = ?NS_VCARD, sub_el = []},
    {Acc1, IQr} = mod_vcard:process_sm_iq(JID, JID, Acc, IQ),

    %% Get old vcard
    A4 = case IQr#iq.sub_el of
             [A1] ->
                 {_, _, _, A2} = A1,
                 update_vcard_els(Data, ContentList, A2);
             _ ->
                 update_vcard_els(Data, ContentList, [])
         end,

    %% Build new vcard
    SubEl = #xmlel{name = <<"vCard">>, attrs = [{<<"xmlns">>, <<"vcard-temp">>}], children = A4},
    IQ2 = #iq{type = set, sub_el = SubEl},
    mod_vcard:process_sm_iq(JID, JID, Acc1, IQ2),
    {ok, ""}.

-spec update_vcard_els(Data :: [binary(), ...],
                       ContentList :: [binary() | string()],
                       Els :: [jlib:xmlcdata() | exml:element()]
                      ) -> [jlib:xmlcdata() | exml:element()].
update_vcard_els(Data, ContentList, Els1) ->
    Els2 = lists:keysort(2, Els1),
    [Data1 | Data2] = Data,
    NewEls = case Data2 of
                 [] ->
                     [#xmlel{ name = Data1, children = [#xmlcdata{content = Content}] }
                      || Content <- ContentList];
                 [D2] ->
                     OldEl = case lists:keysearch(Data1, 2, Els2) of
                                 {value, A} -> A;
                                 false -> #xmlel{ name = Data1 }
                             end,
                     ContentOld1 = OldEl#xmlel.children,
                     Content2 = [#xmlel{ name = D2, children = [#xmlcdata{content=Content}]}
                                 || Content <- ContentList],
                     ContentOld2 = [A || {_, X, _, _} = A <- ContentOld1, X/=D2],
                     ContentOld3 = lists:keysort(2, ContentOld2),
                     ContentNew = lists:keymerge(2, Content2, ContentOld3),
                     [#xmlel{ name = Data1, children = ContentNew}]
             end,
    Els3 = lists:keydelete(Data1, 2, Els2),
    lists:keymerge(2, NewEls, Els3).
