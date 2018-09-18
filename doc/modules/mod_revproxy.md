### Module Description

MongooseIM can be used as a reverse proxy thanks to `mod_revproxy` module. 
To enable this functionality, add a new entry to the listeners and modules sections in the mongooseim.cfg file.

#### Setting up listener

Add the following entry in the listeners section:

```Erlang
 { {8090, ejabberd_cowboy, [
      {num_acceptors, 10},
      {max_connections, 1024},
      {modules, [

          %% Example usage of mod_revproxy, please note that mod_revproxy
          %% needs to be included in MODULES as well.
          {"_", "/[...]", mod_revproxy, [{timeout, 5000},
                                         % time limit for upstream to respond
                                         {body_length, 8000000},
                                         % maximum body size (may be infinity)
                                         {custom_headers, [{<<"header">>,<<"value">>}]}
                                         % list of extra headers that are send to upstream
                                        ]},

      ]}
  ]},


```

For more details about the listeners configuration please take a look at [ejabberd_cowboy section on Listeners config page](../advanced-configuration/Listener-modules.md#ejabberd_cowboy)

#### Configuring routes

To define reverse proxy rules, add the following entry to the modules section.

```Erlang
{mod_revproxy,
   [{routes, [{"www.erlang-solutions.com", "/admin", "_",
               "https://www.erlang-solutions.com/"},
              {":var.com", "/:var", "_", "http://localhost:8080/"},
              {":domain.com", "/", "_", "http://localhost:8080/:domain"}]
     }]},
```

Routes are defined in the options of mod_revproxy module using either `{Host, Path, Method, Upstream}` or `{Host, Path, Upstream}`.
The latter one is the equivalent of `{Host, Path, "_", Upstream}`. 
"_" can be used as a wildcard for `Host`, `Path` and `Method` and it matches on everything.

Upstreams can be defined either by host (just `http(s)://host:port`) or URI.
The difference between them is that the host upstreams are concatenated by the whole request path while the URI upstreams are concatenated only by the remainder that follows the matched `Path`.
This behaviour is similar to the nginx's proxy_pass rules.

Moreover, bindings may be used to match certain parts of host and/or path.
They will be overlaid with appropriate parts of the upstream URI.

### Example configuration

For example, for the shown example configuration, requests for:

* `Host: www.erlang-solutions.com /admin/resources/case-studies` will be rewritten to `https://www.erlang-solutions.com/resources/case-studies` (rule 1)
* `Host: domain.com /domain/index.html` will be rewritten to `http://localhost:8080/index.html` (rule 2, since binding `:var` matches in both host and path)
* `Host: abc.com /def` will be rewritten to `http://localhost:8080/abc/def` (rule 3)
