%% Defineds record to store SIP session information
%% Type is in mod_jingle_sip_session:session()
-record(jingle_sip_session, {
        %% SIP CallID
        sid,
        dialog,
        state,
        direction,
        request,
        node,
        owner,
        from,
        to,
        now,
        meta}).
