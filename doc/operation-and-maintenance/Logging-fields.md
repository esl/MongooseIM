# Fields

* `reason`, `class`, `stacktrace`: standard error catching fields.
* `module`, `function`, `line`, `timestamp`, `node`, `when`, `pid`: reserved fields (could be used by logger itself).
* When logging IQs, adding the `acc` field should be enough. If `acc` not available, `iq` can be used.
  If `iq` is not available, `sub_el` could be logged as a last option.
* `what`: why we are logging. We often use the function name as the `what` field.
    * *Suffixes*: If something goes wrong, use a `_failed` suffix (instead of `unable_to` and `_error`).
      The most common suffixes are `_starting`, `_started`, `_stopping`, `_stopped`, and `_result`.
    * *Prefixes*: We sometimes add prefixes to `what` to signal where we are logging from.
      Such prefixes should be short. Please, don't prefix with the complete module name.
      Some examples for prefixes are: `mam_`, `sm_`, `muc_`, `auth_`, `s2s_`, `pool_`.

When checking the final event name, remove duplicates from it.

| Bad event names                    | Good event names         | Why                               |
|------------------------------------|--------------------------|-----------------------------------|
| `s2s_dns_error`                    | `s2s_dns_lookup_failed`  | Not `_failed` prefix              |
| `s2s_dns_error`                    | `s2s_dns_lookup_timeout` | More specific failure reason      |
| `mod_mam_starting`                 | `mam_starting`           | Use `mam_` prefix for MAM modules |
| `mongoose_wpool_mgr_pool_starting` | `pool_starting`          | Too long and repetitive           |

## Logger defaults

Timestamp should be ordered first when possible, so that sorting is automatic.

| Name          | Type    | Description                                         | Examples                  |
|---------------|---------|-----------------------------------------------------|---------------------------|
| timestamp     | atom    | The timestamp (with timezone information)           | 2018-07-11T13:41:10+00:00 |
| at            | string  | Where in code the call or log line was emitted      | module:function:line      |
| level         | enum    | log level according to RFC 5424                     | warning                   |

## Generally required

| Name          | Type    | Description                                         | Examples                            | Notes                              |
|---------------|---------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| what          | atom    | Event (or issue) name                               | remove_user_failed                  |                                    |
| text          | binary  | Human readable description                          | `<<"MAM failed to contact MySQL">>` |                                    |
| result        | binary  | Explanation of the `what` key                       | `failed`                            | Optional                           |
| tags          | [atom]  | The subcomponent taking action and logging data.    | [c2s, presence], [mam, rdbms]       | This category should be chosen based on filtering needs, and may represent the domain of concern for some operations |

## HTTP requests

| Name          | Type    | Description                                         | Examples                            | Notes                              |
|---------------|---------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| path          | binary  | HTTP path                                           | `<<"/api/add_user">>`               |                                    |
| code          | integer | HTTP code                                           | 200                                 |                                    |
| ip            | tuple   | IP address                                          | inet:ip_address()                   |                                    |
| port          | integer | TCP/UDP port number                                 | 5222                                |                                    |
| peer          | tuple   | `peer() :: {inet:ip_address(), inet:port_number()}` | `{{127,0,0,1},5222}`                |                                    |
| req           | map     | Cowboy request                                      |                                     | Provide when available             |
| reply_body    | binary  | Body reply                                          | <<"ok">>                            |                                    |

## XMPP

| Name          | Type    | Description                                         | Examples                            | Notes                              |
|---------------|---------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| acc           | map     | mongoose_acc, used to extract fields                | `#{...}`                            |                                    |
| user          | binary  | Local Username                                      | `<<"alice">>`                       | Use `#jid.luser` when available    |
| server        | binary  | Local Server (host) name                            | `<<"localhost">>`                   | Use `#jid.lserver` when available  |
| sub_host      | binary  | Subhost when MUC or pubsub are used                 | `<<"muc.localhost">>`               | It's not the same as `server`      |
| remote_user   | binary  | Remote Username (usually who makes IQ requests)     | `<<"alice">>`                       | Use `#jid.luser` when available    |
| remote_server | binary  | Remote Server (usually who makes IQ requests)       | `<<"otherhost">>`                   | Use `#jid.lserver` when available  |
| iq            | record  | MongooseIM IQ record                                | `#iq{}`                             | Provide when available (but it could be acc instead) |
| sub_el        | record  | IQ sub element                                      | `#xmlel{}`                          | Provide ONLY if `iq` not available |
| c2s_state     | record  | C2S process state, that would be used by formatter  | `#state{}`                          |                                    |
| from_jid      | binary  | Accumulator's from_jid                              | `<<"alice@localhost">>`             |                                    |
| to_jid        | binary  | Accumulator's to_jid                                | `<<"to@localhost">>`                |                                    |
| packet        | binary  | Accumulator's element                               | `<<"<message>...">>`                | Encoded as XML, not erlang records |
| exml_packet   | record  | Same as packet, but in `#xmlel{}` format            | `#xmlel{}`                          | Record, formatted in formatter     |

## Other requests

| Name          | Type    | Description                                         | Examples                            | Notes                              |
|---------------|---------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| duration      | integer | Duration of some operation in milliseconds          | 5000                                | Don't use it for microseconds      |
| state_name    | atom    | State name in `gen_fsm`                             | `wait_for_stream`                   |                                    |
| state         | term    | `gen_server` state                                  | `#state{}`                          | Consider adding a formatter        |
| call_from     | tuple   | From argument in gen_server's handle_call           | `{Pid, Tag}`                        |                                    |

## When logging exceptions
`what` key should contain en `_exception` suffix. Following keys should be present:

| Name          | Type    | Description                                         | Examples                            | Notes                              |
|---------------|---------|-----------------------------------------------------|-------------------------------------|------------------------------------|
| class         | enum    | `catch Class:Reason:Stacktrace`                     | `error`                             |                                    |
| reason        | term    | `catch Class:Reason:Stacktrace`                     | `http_timeout`                      |                                    |
| stacktrace    | term    | `catch Class:Reason:Stacktrace`                     | `[...]`                             | Formatted by formatter             |


## Macros for logging unexpected requests

`gen_server` processes sometimes receive messages they couldn't process.
We use macros to log such events (just because you would need them in each
`gen_server` module).

We don't need to log state or state names for such events.

```erlang
%% We don't always handle unexpected calls.
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, unexpected_call}, State}.

%% We don't always handle unexpected casts.
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

%% We SHOULD ignore all unexpected messages, because they could arrive in case
%% of gen_server call timeouts.
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

```

These macros translate into warning logs with the following keys, respectively:

```erlang
#{what => unexpected_cast, msg => Msg}.
#{what => unexpected_info, msg => Msg}.
#{what => unexpected_call, msg => Msg, call_from => From}.
```
