## Overview

The purpose of this method is to make it possible to authenticate a user without
the need for real authentication. In other words, using this module allows to
connect any user to the server without providing any password, certificate, etc.

This kind of authorization sometimes really comes in handy, especially during development and testing.

The backend just accepts every authentication attempt and introduces a random delay (50-500ms) to an authorization response. The delay works like
```erlang
    timer:sleep(Base + rand:uniform(Variance)),
```
where `Base` is `base_time` and `Variance` is `variance`, as configured below.

## Configuration

### `auth.dummy.base_time`
* **Syntax:** non-negative integer
* **Default:** 50
* **Example:** `base_time = 5`

### `auth.dummy.variance`
* **Syntax:** positive integer
* **Default:** 450
* **Example:** `variance = 10`

### Example

```toml
[auth.dummy]
  base = 5
  variance = 10
```
