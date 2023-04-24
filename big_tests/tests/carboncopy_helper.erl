-module(carboncopy_helper).
-export([wait_for_carbon_with_body/3]).

wait_for_carbon_with_body(Client, Body, #{from := From, to := To}) ->
    escalus:assert(
      is_forwarded_received_message,
      [escalus_client:full_jid(From), escalus_client:full_jid(To), Body],
      escalus_client:wait_for_stanza(Client)).
