## Module Description

This module implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html).
This extension allows user to block IQs, messages, presences, or all, based on JIDs, subscription, and roster groups.

## Options

### `modules.mod_privacy.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`.
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

Database backend used to store the privacy lists.

## Example Configuration

```toml
[modules.mod_privacy]
  backend = "rdbms"
```

## Metrics

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` labels associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be either `rdbms` or `mnesia`.

=== "Prometheus"

    | Backend action | Type | Function | Description (when it gets incremented) |
    | -------------- | ---- | -------- | -------------------------------------- |
    | `mod_privacy_Backend_count` | counter | `get_privacy_list` | A privacy list is retrieved from a database. |
    | `mod_privacy_Backend_time` | histogram | `get_privacy_list` | Time spent retrieving a privacy list is retrieved from a database. |
    | `mod_privacy_Backend_count` | counter | `get_list_names` | Names of user's privacy lists are fetched from a database. |
    | `mod_privacy_Backend_time` | histogram | `get_list_names` | Time to fetch names of user's privacy lists from a database. |
    | `mod_privacy_Backend_count` | counter | `get_default_list` | A default privacy list for a user is fetched from a database. |
    | `mod_privacy_Backend_time` | histogram | `get_default_list` | Time to fetch a default privacy list for a user from a database. |
    | `mod_privacy_Backend_count` | counter | `set_default_list` | A default list's name for a user is set in a database. |
    | `mod_privacy_Backend_time` | histogram | `set_default_list` | Time to set a default list's name for a user in a database. |
    | `mod_privacy_Backend_count` | counter | `forget_default_list` | A default list's name for a user is removed from a database. |
    | `mod_privacy_Backend_time` | histogram | `forget_default_list` | Time to remove a default list's name for a user from a database. |
    | `mod_privacy_Backend_count` | counter | `remove_privacy_list` | A privacy list is deleted from a database. |
    | `mod_privacy_Backend_time` | histogram | `remove_privacy_list` | Time to delete a privacy list from a database. |
    | `mod_privacy_Backend_count` | counter | `replace_privacy_list` | A privacy list is updated (replaced) in a database. |
    | `mod_privacy_Backend_time` | histogram | `replace_privacy_list` | Time to update a privacy list is updated (replaced) in a database. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_privacy_Backend, get_privacy_list, count]` | spiral | A privacy list is retrieved from a database. |
    | `[HostType, mod_privacy_Backend, get_privacy_list, time]` | histogram | Time spent retrieving a privacy list is retrieved from a database. |
    | `[HostType, mod_privacy_Backend, get_list_names, count]` | spiral | Names of user's privacy lists are fetched from a database. |
    | `[HostType, mod_privacy_Backend, get_list_names, time]` | histogram | Time to fetch names of user's privacy lists from a database. |
    | `[HostType, mod_privacy_Backend, get_default_list, count]` | spiral | A default privacy list for a user is fetched from a database. |
    | `[HostType, mod_privacy_Backend, get_default_list, time]` | histogram | Time to fetch a default privacy list for a user from a database. |
    | `[HostType, mod_privacy_Backend, set_default_list, count]` | spiral | A default list's name for a user is set in a database. |
    | `[HostType, mod_privacy_Backend, set_default_list, time]` | histogram | Time to set a default list's name for a user in a database. |
    | `[HostType, mod_privacy_Backend, forget_default_list, count]` | spiral | A default list's name for a user is removed from a database. |
    | `[HostType, mod_privacy_Backend, forget_default_list, time]` | histogram | Time to remove a default list's name for a user from a database. |
    | `[HostType, mod_privacy_Backend, remove_privacy_list, count]` | spiral | A privacy list is deleted from a database. |
    | `[HostType, mod_privacy_Backend, remove_privacy_list, time]` | histogram | Time to delete a privacy list from a database. |
    | `[HostType, mod_privacy_Backend, replace_privacy_list, count]` | spiral | A privacy list is updated (replaced) in a database. |
    | `[HostType, mod_privacy_Backend, replace_privacy_list, time]` | histogram | Time to update a privacy list is updated (replaced) in a database. |
