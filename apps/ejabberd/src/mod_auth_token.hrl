-type token_type() :: access | refresh | provision.

-record (auth_token, { type :: token_type(),
                       expiry_datetime :: calendar:datetime(),
                       user_jid = <<>> :: binary(),
                       sequence_no = <<>> :: integer(),
                       mac_signature = <<>> :: binary(),
                       token_body = <<>> :: binary() }).
