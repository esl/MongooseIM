-record (token, { type :: mod_auth_token:token_type(),
                  expiry_datetime :: calendar:datetime(),
                  user_jid = <<>> :: binary(),
                  sequence_no = <<>> :: integer(),
                  mac_signature = <<>> :: binary(),
                  token_body = <<>> :: binary() }).
