%% request XML
Req =
<<"<request xmlns='urn:xmpp:foreign_event:http:0'
  type='http'
  url='http://localhost:8080'
  method='get'>
  <header name='content-type'>application-json</header>
  <header name='token'>token</header>
  <payload>
  '{\"key\":\"value\"}'
  </payload>
  </request>">>.

%% pasing the request
{ok, Parsed}, exml:parse(Req).
exml_query:paths(Parsed, [{element, <<"header">>}]).

%% Recomile file in the shell
compile:file("../../lib/ejabberd/src/foreign_event/mod_foreign.erl", [{i, "../../lib/ejabberd/include"}]).

%% Trace pubsub node creation/publishing
recon_trace:calls([{mod_pubsub,publish_item, fun(_) -> return_trace() end},
                   {mod_pubsub,create_node, fun(_) -> return_trace() end}],
                  100,
                  [{scope, local}]).

% Trace paring of Foreign Event
recon_trace:calls({mod_foreign, parse_foreign_event, fun(_) -> return_trace() end},100,[{scope, local}]).

% Trace encoding the foregin event HTTP response
recon_trace:calls({mod_foreign_http, encode, fun(_) -> return_trace() end},100,[{scope, local}]).

