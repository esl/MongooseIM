## Configuration
This module contains command definitions which are loaded when the module is activated.
There are no options to be provided, therefore the following entry in the config file is sufficient:

```toml
[modules.mod_inbox_commands]
```

## Admin endpoint

### Bin flush for a user
To clean the bin for a given user, the following admin API request can be triggered:

```http
DELETE /api/inbox/<domain>/<user>/<days>/bin,
```
where `<domain>` and `<user>` are the domain and name parts of the user's jid, respectively, and `<days>` is the required number of days for an entry to be considered old enough to be removed, zero allowed (which clears all).

The result would be a `200` with the number of rows that were removed as the body, or a corresponding error. For example, if only one entry was cleaned:
```http
HTTP/1.1 200 OK
server: Cowboy,
date: Wed, 30 Mar 2022 14:06:20 GMT,
content-type: application/json,
content-length: 1

1
```

### Global bin flush
If all the bins were desired to be cleared, the following API can be used instead:

```http
DELETE /api/inbox/<host_type>/<days>/bin,
```
where as before, `<days>` is the required number of days for an entry to be considered old enough to be removed, and `<host_type>` is the host type where inbox is configured.

The result would look analogously:
```http
HTTP/1.1 200 OK
server: Cowboy,
date: Wed, 30 Mar 2022 14:06:20 GMT,
content-type: application/json,
content-length: 1

42
```
