# HTTP roster module

## Overview

The purpose of this module is to connect with an external REST API and delegate the roster operations to it whenever possible. The component must implement the API described in one of the next sections for `mod_roster_http` to work out of the box.

The module can be especially useful for users maintaining their own, central user database with contacts and friends information, which is shared with other services.

## Configuration

### How to enable

TODO

## Authentication service API

User JIDs have two parts: `user_name` and `domain`. To extract contact list for particular JID, you provide those two parts in the URL in the reverse order.
This way, it is possible to route requests based on the server part.
Both `:domain` and `:user` are URL encoded strings.

### `get_roster`

* **Method:** GET
* **url**: /:domain/:user
* **Return values:**
    * status 200
    * **JSON**
```
{ "ver": version # (optional, opaque string)
  "items": [item]
}
```
Where item is:
```
{ "jid": jid,                   # (the only mandator field)
  "approved": approved,         # "true" | "false",
  "ask": ask,                   # "subscribe" | "unsubcribe",
  "name": name,                 # string, how contact should be displayed in roster
  "subscription": subscription, # "none" | "to" | "from" | "both"
  "groups": [group]}            # where group is a string
```
* **Description:** Get entire roster for given user.

### `replace_user_roster`

* **Method:** PUT
* **url**: /:domain/:user
* **Return values:**
    * status 200
    * **JSON** 
```
{ "ver": version # (optional, opaque string)
  "items": [item]
}
```
* **Description:** Erase old roster and use this one.

### `edit_roster_contact`

* **Method:** PUT
* **url** /:domain/:user/contacts
* **input JSON**
```
{ "item": item }
```
* **Return values:**
    * status 200
    * **JSON** The same as in the request.
* **Description:** If item with provided JID does not exist, it will be created. If item exists, it will be modified.

### `delete_roster_contact`

* **Method:** DELETE
* **url** /:domain/:user/contacts/:domain2/:user2
* **Return values:**
    * status 200
* **Description:** Deletes item if it exists. The item to delete is provided in the url.