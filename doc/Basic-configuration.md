## vars.config

`vars.config` file can be found in `[MongooseIM root]/rel/`. It allows changing the most important settings quickly and without touching `ejabberd.cfg` or `vm.args` files directly. Recommended for basic usage.

The file contains erlang tuples terminated with period ('.'). For users not familiar with Erlang syntax, here is a quick cheat sheet:

* Each config option (key and value) is a tuple. Tuples are (Erlangers, forgive us the simplification) other Erlang terms separated with commas and enclosed in curly brackets ({}).
* Tuples (at least the top-level ones) in `vars.config` are always 2-element. 
* The first element of each tuple is the name (Erlang atom). The file contains all possible keys so you will never have to change the first element or add new tuple.
* The second element is a string (in quotes: "").  Remeber to escape quote with backslash ('\') if you ever use one inside a string.
* A value can be a list. Erlang lists are other Erlang terms separated with commas and enclosed in square brackets ([]).
* If a value is terminated with a period (e.g. `odbc_server`) or a comma (e.g. `mod_privacy`), don't change it.
* Config options that are "features", can be disabled by using empty string as the value or prepending the actual value with  '%' ('%' starts one-line comment in Erlang, like '//' in C or Java).

### Options

There are 2 types of options: params and features. Unlike params, features can be disabled.

* **hosts** - param
    * **Description:** List of supported XMPP domains. Usually it's best to stick with just one or two domains.
    * **Warning:** extension modules and database backends will be started separately for every domain, so when increasing the number of domains please make sure you have enough resources available (e.g. connection limit set in DBMS).
    * **Example:** `"[\"localhost\", \"domain2\"]"`

* **host_config** - feature
    * **Description:** List of specific options for chosen XMPP domains. They will override the global ones. Allowed keys are marked on [Advanced configuration](Advanced-configuration.md) page
 
* **Syntax:** `"{host_config, \"overridden-domain\", [{key, value}]}."`
    * **Example:** `"{host_config, \"localhost2\", [{auth_method, anonymous}, {allow_multiple_connections, false}]}." `

* **odbc_server** - feature
    * **Description:** SQL DB connection configuration. Currently supported DB types are `mysql` and `pgsql`. To enable the connection, remove '%%' prefix from value.
    * **Syntax:** `"{odbc_server, {Type, Host, Port, DBName, Username, Password}}."`

* **riak_server** - feature
    * **Description:** Riak connection pool configuration. Currently only one endpoint can be specified, to connect to more riak nodes you have to use load balancing techniques, for more details see:
    [Load Balancing riak](http://docs.basho.com/riak/latest/ops/advanced/configs/load-balancing-proxy/) from basho.
    To enable, remove '%%' prefix from value.
    * **Syntax:** `"{riak_server, [{pool_size, Size}, {address, Host}, {port, Port}]}."`

* **cassandra_servers** - feature
    * **Description:** Cassandra connection pools configuration. Several endpoints can be specified:
    * **Syntax**: `{cassandra_servers, [{default, []}]}.`
    * [Advanced Cassandra configuration](advanced-configuration/Cassandra.md)

* **auth_ldap** - feature
    * **Description:** Put [[LDAP configuration]] here.

* **s2s_addr** - feature
    * **Description:** Override DNS lookup for specific non-local XMPP domain and use predefined server IP and port for S2S connection (server-to-server).
    * **Syntax:** `"{ {s2s_addr, \"some-domain\"}, { {10,20,30,40}, 7890 } }."`

* **s2s_default_policy** - param
    * **Description:** Default policy for new S2S (server-to-server) **both incoming and outgoing** connection to/from unknown remote server. 

* **outgoing_s2s_port** - param
    * **Description:** Port to be used locally when establishing outgoing S2S (server-to-server) connection. Default is 5269.

* **node_name** - param
    * **Description:** Erlang node name. Should be changed when deploying MongooseIM cluster, otherwise not relevant.

* **ejabberd_c2s_port** - param
    * **Description:** Port to listen on for standard incoming XMPP connections. Default is 5222.

* **ejabberd_s2s_port** - param
    * **Description:** Port to listen on for incoming S2S (server-to-server) connections. Default is 5269.

* **cowboy_port** - param
    * **Description:** Port for all HTTP-based MongooseIM services like BOSH or Websockets. Default is 5280.

* **mod_last, mod_offline, mod_privacy, mod_private, mod_roster, mod_vcard, mod_snmp** - feature
    * **Description:** Allows enabling/disabling specific modules and configuring them. Read more on the [Modules](advanced-configuration/Modules.md) page.

* **sm_backend** - param
    * **Description:** Defines the session management module (session storage backend).
    * **Valid values:** `mnesia`, `redis`

* **auth_method** - param
    * **Description:** Chooses authentication modules. Can be either a single module or a list of modules to be tried in sequence until one of them succeeds.
    * **Valid values:** `internal`, `odbc`, `external`, `anonymous`, `ldap`, `riak`
    * `internal` means Mnesia-based
    * **Examples:** `"odbc"`, `"[internal, anonymous]"`

* **ext_auth_script** - feature
    * **Description:** Path to the authentication script used by `external` auth module. Script API specification can be found in [[External authentication script]].

* **tls_config** - feature
    * **Description:** Allows enabling StartTLS feature in client-to-server XMPP connections. Just remove '%%' prefix and set path to PEM file containing certificate and (not protected by password) private key in X.509 format.

* **zlib** - feature
    * **Description:** Controls the zlib compression feature for client-to-server XMPP connections. To enable it, remove '%%' prefix. You can define a limit for output data size to prevent killing the server with [zlib bomb](http://xmpp.org/resources/security-notices/uncontrolled-resource-consumption-with-highly-compressed-xmpp-stanzas/). Set it to `unlimited` to bypass the check (**not recommended**).
