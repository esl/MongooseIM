-record (token, { type              :: mod_auth_token:token_type(),
                  expiry_datetime   :: calendar:datetime(),
                  user_jid          :: ejabberd:jid(),
                  sequence_no       :: mod_auth_token:sequence_no() | undefined,
                  mac_signature     :: binary() | undefined,
                  token_body        :: binary() | undefined }).
