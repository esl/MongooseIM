%% @doc This suite tests API of adhoc module which implements XEP-0050: Ad-Hoc Commands
-module(adhoc_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        {group, basic}
    ].

groups() ->
    [
        {basic, [parallel], [
            parse_correct_request_with_form,
            parse_correct_request_without_form,
            parse_incorrect_request_wrong_type,
            parse_incorrect_request_wrong_namespace,
            produce_response_full,
            produce_response_no_session_id,
            produce_response_no_actions,
            produce_response_no_default_action,
            produce_response_default_action_not_present
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_correct_request_with_form(_C) ->
    parse_correct_request(sample_form()).

parse_correct_request_without_form(_C) ->
    parse_correct_request(false).

parse_correct_request(Form) ->
    % given
    IqRequest =
        case Form of
            false -> sample_request();
            _ -> sample_request([Form])
        end,
    % when
    #adhoc_request{
        lang = Lang,
        node = Node,
        session_id = SessionID,
        action = Action,
        xdata = XData,
        others = Others
    } = adhoc:parse_request(IqRequest),
    % then
    ?assertEqual(Lang, <<"en-us">>),
    ?assertEqual(Node, <<"node_name">>),
    ?assertEqual(SessionID, <<"1">>),
    ?assertEqual(Action, <<"execute">>),
    ?assertEqual(XData, Form),
    ?assertEqual(Others, [#xmlel{name = <<"test">>}]).

parse_incorrect_request_wrong_type(_C) ->
    % given
    IqRequest = (sample_request())#iq{type = get},
    % when
    {error, Error} = adhoc:parse_request(IqRequest),
    % then
    ?assert(is_bad_request(Error)).

parse_incorrect_request_wrong_namespace(_C) ->
    % given
    IqRequest = (sample_request())#iq{xmlns = <<"wrong_namespace">>},
    % when
    {error, Error} = adhoc:parse_request(IqRequest),
    % then
    ?assert(is_bad_request(Error)).

produce_response_full(_C) ->
    % given
    Actions = [<<"next">>, <<"complete">>],
    Notes = [{<<"info">>, <<"Information message.">>}],
    AdditionalElements = [sample_form()],
    AdhocResponse = #adhoc_response{
        node = <<"node_name">>,
        session_id = <<"1234">>,
        status = executing,
        default_action = <<"next">>,
        actions = Actions,
        notes = Notes,
        elements = AdditionalElements
    },
    ExpectedActionsEls = [#xmlel{
        name = <<"actions">>,
        attrs = #{<<"execute">> => <<"next">>},
        children = [#xmlel{name = Action} || Action <- Actions]
    }],
    ExpectedNotesEls = [
        #xmlel{
            name = <<"note">>,
            attrs = #{<<"type">> => Type},
            children = [#xmlcdata{content = Text}]
        }
     || {Type, Text} <- Notes
    ],
    ExpectedChildren = ExpectedActionsEls ++ ExpectedNotesEls ++ AdditionalElements,
    % when
    #xmlel{ name = <<"command">>, children = Children } =
        Resp = adhoc:produce_response(AdhocResponse),
    % then
    ?assertEqual(<<"1234">>, exml_query:attr(Resp, <<"sessionid">>)),
    ?assertEqual(<<"node_name">>, exml_query:attr(Resp, <<"node">>)),
    ?assertEqual(<<"executing">>, exml_query:attr(Resp, <<"status">>)),
    ?assertEqual(lists:sort(ExpectedChildren), lists:sort(Children)).

produce_response_no_session_id(_C) ->
    % given
    AdhocResponse = #adhoc_response{
        session_id = <<>>
    },
    % when
    #xmlel{
        name = <<"command">>,
        attrs = Attrs
    } = adhoc:produce_response(AdhocResponse),
    % then
    SessionID = maps:get(<<"sessionid">>, Attrs, undefined),
    ?assert(is_binary(SessionID)),
    ?assertNotEqual(<<>>, SessionID).

produce_response_no_actions(_C) ->
    % given
    AdhocResponse = #adhoc_response{
        actions = []
    },
    % when
    #xmlel{
        name = <<"command">>,
        children = Children
    } = adhoc:produce_response(AdhocResponse),
    % then
    ?assertEqual([], Children).

produce_response_no_default_action(_C) ->
    % given
    Actions = [<<"complete">>],
    AdhocResponse = #adhoc_response{
        actions = Actions
    },
    ExpectedActionsEls = [#xmlel{
        name = <<"actions">>,
        children = [#xmlel{name = Action} || Action <- [<<"next">> | Actions]]
    }],
    % when
    #xmlel{
        name = <<"command">>,
        children = Children
    } = adhoc:produce_response(AdhocResponse),
    % then
    ?assertEqual(lists:sort(ExpectedActionsEls), lists:sort(Children)).

produce_response_default_action_not_present(_C) ->
    % given
    Actions = [<<"complete">>],
    AdhocResponse = #adhoc_response{
        default_action = <<"prev">>,
        actions = Actions
    },
    ExpectedActionsEls = [#xmlel{
        name = <<"actions">>,
        attrs = #{<<"execute">> => <<"prev">>},
        children = [#xmlel{name = Action} || Action <- [<<"prev">> | Actions]]
    }],
    % when
    #xmlel{
        name = <<"command">>,
        children = Children
    } = adhoc:produce_response(AdhocResponse),
    % then
    ?assertEqual(lists:sort(ExpectedActionsEls), lists:sort(Children)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sample_form() ->
    #xmlel{
        name = <<"x">>,
        attrs = #{
            <<"xmlns">> => <<"jabber:x:data">>,
            <<"type">> => <<"form">>
        },
        children = []
    }.

sample_request() ->
    sample_request([]).

sample_request(MaybeForm) ->
    #iq{
        type = set,
        lang = <<"en-us">>,
        sub_el = #xmlel{
            attrs = #{
                <<"node">> => <<"node_name">>,
                <<"sessionid">> => <<"1">>,
                <<"action">> => <<"execute">>
            },
            children = [#xmlel{name = <<"test">>}] ++ MaybeForm
        },
        xmlns = ?NS_COMMANDS
    }.

is_bad_request(#xmlel{
    name = <<"error">>,
    attrs = #{<<"code">> := <<"400">>, <<"type">> := <<"modify">>},
    children = [#xmlel{name = <<"bad-request">>}]
}) ->
    true;
is_bad_request(_) ->
    false.
