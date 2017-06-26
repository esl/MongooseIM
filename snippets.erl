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

%% Event XML
Event =
<<"
  <message from='pubsub.localhost' to='alice48.158796@localhost/res1' type='headline'>
  <event xmlns='http://jabber.org/protocol/pubsub#event'>
  <items node='foreign-event_tSMwDj8='>
  <item id='5DA9B4461888'>
  <request xmlns='urn:xmpp:foreign_event:http:0' type='http' url='http://localhost:8080' method='get'>
  <header name='content-type'>application-json</header>
  <header name='token'>token</header>
<payload>
  '{\"key\":\"value\"}'
</payload>
  </request>
  </item>
  </items>
  </event>
</message>
">>.

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
recon_trace:calls({mod_foreign, publish_to_pubsub, fun(_) -> return_trace() end}, 100, [{scope, local}]).

% Trace paring of Foreign Event
recon_trace:calls({mod_foreign, parse_foreign_event, fun(_) -> return_trace() end},100,[{scope, local}]).

% Trace encoding the foregin event HTTP response
recon_trace:calls({mod_foreign_http, encode, fun(_) -> return_trace() end},100,[{scope, local}]).


%% mod_foreign global metrics

mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign, failed_requests]).
mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign, successful_requests]).
mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign, response_time]).

%% mod_foreign_http metrics
mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign_http, failed_http_requests]).
mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign_http, successful_http_requests]).
mongoose_metrics:get_metric_value([<<"localhost">>, mod_foreign_http, http_request_time]).
