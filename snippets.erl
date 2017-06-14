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



