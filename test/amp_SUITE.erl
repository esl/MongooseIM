-module(amp_SUITE).
-compile([export_all]).

%% @doc Tests for XEP-0079 <rule> -related functions
-include("amp.hrl").
-include_lib("exml/include/exml.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(prop_helper, [prop/2, readable_bitstring/0]).

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [{group, conversion},
     {group, mutating},
     {group, classification}
    ].

groups() ->
    [{conversion, [parallel],
      [amp_binaries_to_rule_test,
       valid_rules_property,
       invalid_rules_property]},
     {mutating, [parallel],
      [strip_amp_el_test,
       strip_amp_el_noop_test]},
     {classification, [parallel],
      [extract_non_amp_test,
       extract_simple_request_test,
       extract_complex_request_test,
       extract_amp_response_test,
       extract_amp_error_response_test,
       extract_not_acceptable_rules_test,
       extract_incomplete_amp_test]}
    ].

amp_binaries_to_rule_test(_) ->
    ?ae(#amp_rule{condition = 'deliver', value = 'none', action = 'notify'},
        amp:binaries_to_rule(<<"deliver">>, <<"none">>, <<"notify">>)),
    ?ae(#amp_rule{condition = 'match-resource', value = 'any', action = 'notify'},
        amp:binaries_to_rule(<<"match-resource">>, <<"any">>, <<"notify">>)),
    ?ae(#amp_rule{condition = 'expire-at', value = <<"2014-04-04T12:12:12Z">>, action = 'notify'},
        amp:binaries_to_rule(<<"expire-at">>, <<"2014-04-04T12:12:12Z">>, <<"notify">>)),
    ?ae(#amp_invalid_rule{condition = <<"Zeus">>, value = <<"Hades">>, action = <<"Poseidon">>},
        amp:binaries_to_rule(<<"Zeus">>, <<"Hades">>, <<"Poseidon">>)).

valid_rules_property(_) ->
    prop(valid_rules_property,
             ?FORALL({C,V,A}, amp_gen:valid_cva_binaries(),
                     is_valid_rule(amp:binaries_to_rule(C,V,A)))).

invalid_rules_property(_) ->
    prop(invalid_rules_property,
             ?FORALL({C,V,A}, amp_gen:invalid_cva_binaries(),
                     not is_valid_rule(amp:binaries_to_rule(C,V,A)))).

extract_non_amp_test(_) ->
    {ok, M} = exml:parse(<<"
<message
    from='bernardo@shakespeare.lit/pda'
    to='francisco@shakespeare.lit'
    type='chat'>
  <body>Who's there?</body>
</message>">>),
    ?assertEqual(none,
                 amp:extract_requested_rules(M)).

extract_simple_request_test(_) ->
    {ok, M} = exml:parse(<<"
<message
    from='northumberland@shakespeare.lit'
    id='richard2-4.1.247'
    to='kingrichard@royalty.england.lit'>
  <body>My lord, dispatch; read o'er these articles.</body>
  <amp xmlns='http://jabber.org/protocol/amp'>
    <rule condition='deliver' action='notify' value='direct'/>
  </amp>
</message> ">>),
    ?assertEqual({rules, [#amp_rule{condition = 'deliver',
                                    value     = 'direct',
                                    action    = 'notify'}]},
                  amp:extract_requested_rules(M)).

extract_complex_request_test(_) ->
    {ok, M} = exml:parse(<<"
<message to='francisco@hamlet.lit/pda'
         from='bernardo@hamlet.lit/elsinore'
         id='ibb1'>
  <body>Hello, fellow human</body>
  <amp xmlns='http://jabber.org/protocol/amp' per-hop='true'>
    <rule action='notify' condition='deliver' value='direct'/>
    <rule action='error' condition='deliver' value='none'/>
    <rule action='notify' condition='deliver' value='forward'/>
  </amp>
</message>">>),
    ?assertEqual({rules, [#amp_rule{condition = 'deliver',
                                    value     = 'direct',
                                    action    = 'notify'},
                          #amp_rule{condition = 'deliver',
                                    value     = 'none',
                                    action    = 'error'},
                          #amp_rule{condition = 'deliver',
                                    value     = 'forward',
                                    action    = 'notify'}
                         ]},
       amp:extract_requested_rules(M)).

extract_amp_response_test(_) ->
    {ok, M} = exml:parse(<<"
  <message from='hamlet.lit'
           to='bernardo@hamlet.lit/elsinore'
           id='chatty2'>
    <amp xmlns='http://jabber.org/protocol/amp'
         status='notify'
         to='bernardo@hamlet.lit/elsinore'
         from='francisco@hamlet.lit'>
      <rule action='notify' condition='deliver' value='stored'/>
    </amp>
  </message>">>),
    ?assertEqual(none,
                 amp:extract_requested_rules(M)).

extract_amp_error_response_test(_) ->
    {ok, M} = exml:parse(<<"
<message
    from='shakespeare.lit'
    id='richard2-4.1.247'
    to='northumberland@shakespeare.lit'
    type='error'>
  <amp xmlns='http://jabber.org/protocol/amp'>
    <rule action='drop' condition='expire-at' value='2004-01-01T00:00:00Z'/>
  </amp>
  <error type='modify' code='400'>
    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    <unsupported-actions xmlns='http://jabber.org/protocol/amp'>
      <rule condition='expire-at'
            action='drop'
            value='2004-01-01T00:00:00Z'/>
    </unsupported-actions>
  </error>
</message>">>),
    ?assertEqual(none,
                 amp:extract_requested_rules(M)).


extract_not_acceptable_rules_test(_) ->
    {ok, M} = exml:parse(<<"
<message
    from='northumberland@shakespeare.lit'
    id='richard2-4.1.247'
    to='kingrichard@royalty.england.lit'>
  <body>My lord, dispatch; read o'er these articles.</body>
  <amp xmlns='http://jabber.org/protocol/amp'>
    <rule condition='hello' action='mike' value='!'/>
  </amp>
</message> ">>),
    ?assertEqual({errors, [
                           {'not-acceptable',
                            #amp_invalid_rule{action = <<"mike">>,
                                              condition = <<"hello">>,
                                              value = <<"!">>}}]},
                amp:extract_requested_rules(M)).

extract_incomplete_amp_test(_) ->
    {ok, M} = exml:parse(<<"
<message
    from='northumberland@shakespeare.lit'
    id='richard2-4.1.247'
    to='kingrichard@royalty.england.lit'>
  <body>My lord, dispatch; read o'er these articles.</body>
  <amp xmlns='http://jabber.org/protocol/amp'>
    <incompatible-rule condition='hello' action='mike' value='!'/>
  </amp>
</message> ">>),
    ?assertEqual(none,
                 amp:extract_requested_rules(M)).

strip_amp_el_test(_) ->
    AmpEl = #xmlel{name = <<"amp">>,
                   attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/amp">>},
                           {<<"status">>,<<"notify">>},
                           {<<"to">>,<<"bernardo@hamlet.lit/elsinore">>},
                           {<<"from">>,<<"francisco@hamlet.lit">>}],
                   children = [
                               #xmlel{name = <<"rule">>,
                                       attrs = [{<<"action">>,<<"notify">>},
                                               {<<"condition">>,<<"deliver">>},
                                               {<<"value">>,<<"stored">>}]}]},
    BodyEl = #xmlel{name = <<"body">>,
                    attrs = [],
                    children = [#xmlcdata{content = <<"Hello there!">>}]},
    M = #xmlel{name = <<"message">>,
               attrs =
                   [{<<"from">>,<<"hamlet.lit">>},
                    {<<"to">>,<<"bernardo@hamlet.lit/elsinore">>},
                    {<<"id">>,<<"chatty2">>}],
               children = [AmpEl,BodyEl]},
    ?assertEqual(
       #xmlel{name = <<"message">>,
              attrs = [{<<"from">>, <<"hamlet.lit">>},
                      {<<"to">>, <<"bernardo@hamlet.lit/elsinore">>},
                      {<<"id">>, <<"chatty2">>}],
              children = [BodyEl]
             },
       amp:strip_amp_el(M)).

strip_amp_el_noop_test(_) ->
    BodyEl = #xmlel{name = <<"body">>,
                    attrs = [],
                    children = [#xmlcdata{content = <<"Hello there!">>}]},
    M = #xmlel{name = <<"message">>,
               attrs =
                   [{<<"from">>,<<"hamlet.lit">>},
                    {<<"to">>,<<"bernardo@hamlet.lit/elsinore">>},
                    {<<"id">>,<<"chatty2">>}],
               children = [BodyEl]},
    ?assertEqual(M, amp:strip_amp_el(M)).


is_valid_rule(#amp_rule{}) -> true;
is_valid_rule(#amp_invalid_rule{}) -> false.


