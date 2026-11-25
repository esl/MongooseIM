# mod_invites

## Module Description

An implementation of what is known as [Great Invitations](https://blog.prosody.im/great-invitations/) and is present today in most modern clients. In more technical terms this module implements [XEP-0379: Pre-Authenticated Roster Subscription](https://xmpp.org/extensions/xep-0379.html), [XEP-0401: Ad-hoc Account Invitation Generation](https://xmpp.org/extensions/xep-0401.html) and [XEP-0445: Pre-Authenticated In-Band Registration](https://xmpp.org/extensions/xep-0445.html). It allows you to create invites for account creation, pre-authenticated roster subscriptions as well as password reset tokens. This is done by adding new commands/API calls as well as [Adhoc commands](https://xmpp.org/extensions/xep-0050.html).

This module comes with an integrated landing page, that guides recipients through the process. Furthermore there's a start page included that lets you create such invites too, in case some clients don't support that.

## Options

### `modules.mod_invites.access_create_account`
* **Syntax:** string
* **Default:** `"none"`
* **Example:** `access_create_account = "all"`

An ACL which tells who's allowed to generate account creation invites. Note that admins are always allowed to create them.

### `modules.mod_invites.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

Where to store persistent data.

### `modules.mod_invites.landing_page`
* **Syntax:**  string
* **Default:** `"none"`
* **Example:** `landing_page = "https://{{ host }}/xmpp/invites/{{ invite.token }}"`

Template for landing page URL. If you don't want to host your own landing page you can still use an external service, e.g. `landing_page = "https://invite.joinjabber.org/{{ invite.token_uri|strip_protocol }}"`. Note that only the integrated landing page lets you register via web form.

### `modules.mod_invites.max_invites`
* **Syntax:** non-negative integer or `infinity`
* **Default:** `infinity`
* **Example:** `max_invites = 5`

Number of invites a regular account matching the `access_create_account` ACL is allowed to issue. Note that this limit does not apply to user invitations for preauthenticated roster subscriptions. If the issuing account matches the ACL there will always be a "ibr=y" suffix, indicating that account creation *might* be possible, but is not guaranteed. In case `max_invites` is exceeded, trying to register using a user invitation will fail.

### `modules.mod_invites.site_name`
* **Syntax:** string
* **Default:** empty
* **Example:** `site_name = "My beautiful laundrette"`

A string used to represent your site on the landing pages.

### `modules.mod_invites.template_dir`
* **Syntax:** string
* **Default:** `"priv/mod_invites"`
* **Example:** `template_dir = "/home/mongooseim/my_custom_templates"`

An alternative directory where to load templates from.

### `modules.mod_invites.token_expire_seconds`
* **Syntax:** non-negative integer or `infinity`
* **Default:** `5 * 86400`
* **Example:** `token_expire_seconds = 3600`

Seconds after which an invite expires.

### `modules.mod_invites.webchat_url`
* **Syntax:** string
* **Default:** `"none"`
* **Example:** `webchat_url = "https://example.com/conversejs"`

URL to a web based chat, that will be suggested to be used if no client is chosen while registering account via the landing page.

## Example Configuration

```toml
[modules.mod_invites]
  backend = "rdbms"
  token_expire_seconds = 86400
  max_invites = 5000
  access_create_account = "all"
  landing_page = "http://{{host}}/xmpp/invites/{{invite.token}}"
  site_name = "My beautiful laundrette"
```

To enable the landing page you need to configure a listener:

```toml
[[listen.http]]
...
  [[listen.http.handlers.mod_invites_http]]
    host = "_"
    path = "/invites"
```

## List of New Commands and API calls

* cleanupExpired
* deleteInviteByToken
* expireInviteByToken
* expireTokens
* generateInvite
* generateResetToken
* listInvites

Use `mongooseimctl invites` to get detailed descriptions.
