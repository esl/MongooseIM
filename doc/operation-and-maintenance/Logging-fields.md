# Fields

- reason, class, stacktrace - standard error catching fields.
- module, function, line, timestamp, node, when, pid - reserved fields (could be used by logger itself).

When logging IQs, adding `acc` field should be enough. If `acc` not available, `iq` can be used.
If `iq` is not available, `sub_el` could be logged as last option.

| Name          | Type    | Usage       | Description                                         | Examples                            | Notes                              |
|---------------|---------|-------------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| what          | atom    | required    | Event (or issue) name                               | remove_user_failed                  | Prefer `_failed` suffix            |
| text          | binary  | recommened  | Human readable description                          | `<<"MAM failed to contact MySQL">>` |                                    |
| path          | binary  |             | HTTP path                                           | `<<"/api/add_user">>`               |                                    |
| code          | integer |             | HTTP code                                           | 200                                 |                                    |
| user          | binary  | recommened  | Local Username                                      | `<<"alice">>`                       | Use `#jid.luser` when available    |
| server        | binary  |             | Local Server (host) name                            | `<<"localhost">>`                   | Use `#jid.lserver` when available  |
| sub_host      | binary  |             | Subhost when MUC or pubsub are used                 | `<<"muc.localhost">>`               | It's not the same as `server`      |
| remote_user   | binary  |             | Remote Username (usually who makes IQ requests)     | `<<"alice">>`                       | Use `#jid.luser` when available    |
| remote_server | binary  |             | Remote Server (usually who makes IQ requests)       | `<<"otherhost">>`                   | Use `#jid.lserver` when available  |
| acc           | map     |             | Accumulator, that would be used by formatter        | `#{...}`                            |                                    |
| req           | map     |             | Cowboy request                                      |                                     | Provide when available             |
| iq            | record  |             | MongooseIM IQ record                                | `#iq{}`                             | Provide when available (but it could be acc instead) |
| sub_el        | record  |             | IQ sub element                                      | `#xmlel{}`                          | Provide ONLY if `iq` not available |
| c2s_state     | record  |             | C2S process state, that would be used by formatter  | `#state{}`                          |                                    |
| from_jid      | binary  | auto        | Accumulator's from_jid                              | `<<"alice@localhost">>`             |                                    |
| to_jid        | binary  | auto        | Accumulator's to_jid                                | `<<"to@localhost">>`                |                                    |
| packet        | binary  | auto        | Accumulator's element                               | `<<"<message>...">>`                | Encoded as XML, not erlang records |
| exml_packet   | record  |             | Same as packet, but in `#xmlel{}` format            | `#xmlel{}`                          | Record, formatted in formatter     |
| reason        | term    |             | `catch Class:Reason:Stacktrace`                     | `http_timeout`                      |                                    |
| class         | enum    |             | `catch Class:Reason:Stacktrace`                     | `error`                             |                                    |
| stacktrace    | term    |             | `catch Class:Reason:Stacktrace`                     | `[...]`                             | Formatted by formatter             |
| duration      | integer |             | Duration of some operation in milliseconds          | 5000                                | Don't use it for microseconds      |
| state_name    | atom    |             | State name in `gen_fsm`                             | `wait_for_stream`                   |                                    |
| state         | term    |             | `gen_server` state                                  | `#state{}`                          | Consider adding a formatter        |
| call_from     | tuple   |             | From argument in gen_server's handle_call           | `{Pid, Tag}`                        |                                    |
| ip            | tuple   |             | IP address                                          | inet:ip_address()                   |                                    |
| port          | integer |             | TCP/UDP port number                                 | 5222                                |                                    |
| peer          | tuple   |             | `peer() :: {inet:ip_address(), inet:port_number()}` | `{{127,0,0,1},5222}`                |                                    |
