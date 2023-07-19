%%%----------------------------------------------------------------------
%%% File    : adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Provide helper functions for ad-hoc commands (XEP-0050)
%%% Created : 31 Oct 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
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

-module(adhoc).
-author('henoch@dtek.chalmers.se').
-xep([{xep, 50}, {version, "1.3.0"}]).
-export([parse_request/1,
         produce_response/2,
         produce_response/4,
         produce_response/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-type request() :: #adhoc_request{}.
-type response() :: #adhoc_response{}.

-export_type([request/0, response/0]).

%% @doc Parse an ad-hoc request.  Return either an adhoc_request record or
%% an {error, ErrorType} tuple.
-spec parse_request(jlib:iq()) -> request() | {error, exml:element()}.
parse_request(#iq{type = set, lang = Lang, sub_el = SubEl, xmlns = ?NS_COMMANDS}) ->
    ?LOG_DEBUG(#{what => adhoc_parse_request,
                 text => <<"entering parse_request...">>,
                 sub_el => SubEl}),
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    SessionID = xml:get_tag_attr_s(<<"sessionid">>, SubEl),
    Action = xml:get_tag_attr_s(<<"action">>, SubEl),
    XData = mongoose_data_forms:find_form(SubEl, false),
    #xmlel{children = AllEls} = SubEl,
    Others = case XData of
                 false ->
                     AllEls;
                 _ ->
                     lists:delete(XData, AllEls)
             end,

    #adhoc_request{lang = Lang,
                   node = Node,
                   session_id = SessionID,
                   action = Action,
                   xdata = XData,
                   others = Others};
parse_request(_) ->
    {error, mongoose_xmpp_errors:bad_request()}.

%% @doc Produce a <command/> node to use as response from an adhoc_response
%% record, filling in values for language, node and session id from
%% the request.
-spec produce_response(request(), response() | atom()) -> #xmlel{}.
produce_response(Request, Status) when is_atom(Status) ->
    produce_response(Request, Status, <<>>, []);
produce_response(#adhoc_request{lang = Lang,
                                node = Node,
                                session_id = SessionID},
                 Response) ->
    produce_response(Response#adhoc_response{lang = Lang,
                                             node = Node,
                                             session_id = SessionID}).

%% @doc Produce a <command/> node to use as response from an adhoc_response
%% record, filling in values for language, node and session id from
%% the request.
-spec produce_response(request(), Status :: atom(), DefaultAction :: binary(),
                       Elements :: [exml:element()]) -> exml:element().
produce_response(Request, Status, DefaultAction, Elements) ->
    #adhoc_request{lang = Lang, node = Node, session_id = SessionID} = Request,
    produce_response(#adhoc_response{lang = Lang, node = Node, session_id = SessionID,
                                     status = Status, default_action = DefaultAction,
                                     elements = Elements}).


%% @doc Produce a <command/> node to use as response from an adhoc_response
%% record.
-spec produce_response(response()) -> exml:element().
produce_response(#adhoc_response{lang = _Lang,
                                 node = Node,
                                 session_id = ProvidedSessionID,
                                 status = Status,
                                 default_action = DefaultAction,
                                 actions = Actions,
                                 notes = Notes,
                                 elements = Elements}) ->
    SessionID = ensure_correct_session_id(ProvidedSessionID),
    ActionsEls = maybe_actions_element(Actions, DefaultAction),
    NotesEls = lists:map(fun note_to_xmlel/1, Notes),
    #xmlel{name = <<"command">>,
           attrs = [{<<"xmlns">>, ?NS_COMMANDS},
                    {<<"sessionid">>, SessionID},
                    {<<"node">>, Node},
                    {<<"status">>, list_to_binary(atom_to_list(Status))}],
           children = ActionsEls ++ NotesEls ++ Elements}.

-spec ensure_correct_session_id(binary()) -> binary().
ensure_correct_session_id(SessionID) when is_binary(SessionID), SessionID /= <<>> ->
    SessionID;
ensure_correct_session_id(_) -> 
    USec = os:system_time(microsecond),
    TS = calendar:system_time_to_rfc3339(USec, [{offset, "Z"}, {unit, microsecond}]),
    list_to_binary(TS).

-spec maybe_actions_element([binary()], binary()) -> [exml:element()].
maybe_actions_element([], _DefaultAction) ->
    [];
maybe_actions_element(Actions, <<>>) ->
    % If the "execute" attribute is absent, it defaults to "next".
    AllActions = ensure_default_action_present(Actions, <<"next">>),
    [#xmlel{
        name = <<"actions">>,
        children = [#xmlel{name = Action} || Action <- AllActions]
    }];
maybe_actions_element(Actions, DefaultAction) ->
    % A form which has an <actions/> element and an "execute" attribute
    % which evaluates to an action which is not allowed is invalid.
    AllActions = ensure_default_action_present(Actions, DefaultAction),
    [#xmlel{
        name = <<"actions">>,
        attrs = [{<<"execute">>, DefaultAction}],
        children = [#xmlel{name = Action} || Action <- AllActions]
    }].

-spec ensure_default_action_present([binary()], binary()) -> [binary()].
ensure_default_action_present(Actions, DefaultAction) ->
    case lists:member(DefaultAction, Actions) of
        true -> Actions;
        false -> [DefaultAction | Actions]
    end.

-spec note_to_xmlel({binary(), iodata()}) -> exml:element().
note_to_xmlel({Type, Text}) ->
    #xmlel{
        name = <<"note">>,
        attrs = [{<<"type">>, Type}],
        children = [#xmlcdata{content = Text}]
    }.
