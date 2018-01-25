-ifndef(MONGOOSEIM_JID_HRL).
-define(MONGOOSEIM_JID_HRL, true).
-record(jid, {user = <<>>      :: jid:user(),
    server = <<>>    :: jid:server(),
    resource = <<>>  :: jid:resource(),
    luser = <<>>     :: jid:luser(),
    lserver = <<>>   :: jid:lserver(),
    lresource = <<>> :: jid:lresource()
}).
-endif.