# Fields

- reason, class, stacktrace - standard error catching fields


| Name          | Type    | Usage       | Description                                         | Examples                            | Notes                              |
|---------------|---------|-------------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| what          | atom    | required    | Event (or issue) name                               | remove_user_failed                  |                                    |
| text          | binary  | recommened  | Human readable description                          | `<<"MAM failed to contact MySQL">>` |                                    |
| path          | binary  |             | HTTP path                                           | `<<"/api/add_user">>`               |                                    |
| code          | integer |             | HTTP code                                           | 200                                 |                                    |
| user          | binary  |             | Local Username                                      | `<<"alice">>`                       | Use `#jid.luser` when available    |
| server        | binary  |             | Local Server (host) name                            | `<<"localhost">>`                   | Use `#jid.lserver` when available  |
| sub_host      | binary  |             | Subhost when MUC or pubsub are used                 | `<<"muc.localhost">>`               | It's not the same as `server`      |
| remote_user   | binary  |             | Remote Username (usually who makes IQ requests)     | `<<"alice">>`                       | Use `#jid.luser` when available    |
| remote_server | binary  |             | Remote Server (usually who makes IQ requests)       | `<<"otherhost">>`                   | Use `#jid.lserver` when available  |
| acc           | map     |             | Accumulator, that would be used by formatter        | `#{...}`                            |                                    |
| c2s_state     | record  |             | C2S process state, that would be used by formatter  | `#state{}`                          |                                    |
| from_jid      | binary  | auto        | Accumulator's from_jid                              | `<<"alice@localhost">>`             |                                    |
| to_jid        | binary  | auto        | Accumulator's to_jid                                | `<<"to@localhost">>`                |                                    |
| packet        | binary  | auto        | Accumulator's element                               | `<<"<message>...">>`                | Encoded as XML, not erlang records |
| exml_packet   | record  |             | Same as packet, but in `#xmlel{}` format            | `#xmlel{}`                          | Record, formatted in formatter     |
| reason        | term    |             | `catch Class:Reason:Stacktrace`                     | `http_timeout`                      |                                    |
| class         | enum    |             | `catch Class:Reason:Stacktrace`                     | `error`                             |                                    |
| stacktrace    | term    |             | `catch Class:Reason:Stacktrace`                     | `[...]`                             | Formatted by formatter             |
