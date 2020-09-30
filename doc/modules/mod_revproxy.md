### Module Description

MongooseIM can be used as a reverse proxy thanks to `mod_revproxy` module.
To enable this functionality, configure the appropriate listener and change the
module options in `mongooseim.toml`.

#### Configuring routes

To define reverse proxy rules, add entries defining routes to `modules.mod_revproxy.routes`.

#### `modules.mod_revproxy.routes.host`
* **Syntax:** non-empty string
* **Default:** no default
* **Example:** `host = "www.erlang-solutions.com"`

#### `modules.mod_revproxy.routes.path`
* **Syntax:** string
* **Default:** no default
* **Example:** `path = "/"`

#### `modules.mod_revproxy.routes.method`
* **Syntax:** string
* **Default:** `"_"`
* **Example:** `method = "_"`

#### `modules.mod_revproxy.routes.upstream`
* **Syntax:** non-empty string
* **Default:** no default
* **Example:** `upstream = "https://www.erlang-solutions.com/"`

Routes are defined in the options of mod_revproxy module using `host`, `path`,
`method` and `upstream` keys. All except `method` are mandatory.
`"_"` can be used as a wildcard for `host`, `path` and `method` and it matches on everything.

Upstreams can be defined either by host (just `http(s)://host:port`) or URI.
The difference between them is that the host upstreams are concatenated by the
whole request path while the URI upstreams are concatenated only by the remainder
that follows the matched `path`.
This behaviour is similar to the nginx's proxy_pass rules.

Moreover, bindings may be used to match certain parts of host and/or path.
They will be overlaid with appropriate parts of the upstream URI.

### Example configuration

For example, for the shown example configuration, requests for:

* `Host: www.erlang-solutions.com /admin/resources/case-studies` will be rewritten to `https://www.erlang-solutions.com/resources/case-studies` (rule 1)
* `Host: domain.com /domain/index.html` will be rewritten to `http://localhost:8080/index.html` (rule 2, since binding `:var` matches in both host and path)
* `Host: abc.com /def` will be rewritten to `http://localhost:8080/abc/def` (rule 3)

```
[[modules.mod_revproxy.routes]]
  host = "www.erlang-solutions.com"
  path = "/admin"
  method = "_"
  upstream = "https://www.erlang-solutions.com/"

[[modules.mod_revproxy.routes]]
  host = ":var.com"
  path = "/:var"
  upstream = "http://localhost:8080/"

[[modules.mod_revproxy.routes]]
  host = ":domain.com"
  path = "/"
  method = "_"
  upstream = "http://localhost:8080/:domain"
```
