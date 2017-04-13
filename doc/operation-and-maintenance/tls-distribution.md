# Distribution over TLS

It's possible to use TLS for communication between MongooseIM cluster nodes.
To enable it, find the directory of your release, below it look for `etc/vm.dist.args` and, inside the file, the section about
the distribution protocol:

```
## Use TLS for connections between Erlang cluster members.
## Don't forget to override the paths to point to your certificate(s) and key(s)!
## Once a connection is established, Erlang doesn't differentiate between
## a client and a server - the same certs/keys can be used on both sides.
#-proto_dist inet_tls
#-ssl_dist_opt server_certfile   /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/fake_cert.pem client_certfile   /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/fake_cert.pem
#              server_keyfile    /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/fake_key.pem  client_keyfile    /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/fake_key.pem
#              server_cacertfile /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/cacert.pem    client_cacertfile /Users/erszcz/work/esl/mongooseim/_build/mim1/rel/mongooseim/priv/ssl/cacert.pem
#              client_verify     verify_peer
#              server_verify     verify_peer
#              server_fail_if_no_peer_cert true
```

By default, the `proto_dist` as well as the following options for configuring the cluster member are commented out.
Enable them and provide the correct paths to your CA certificate, server certificate and server key.

**There's a number of caveats to remember about when running Erlang distribution over TLS**:

-   TLS-enabled and non-TLS Erlang nodes can't communicate with one another.
    Remember about it when trying to run `erl -[s]name ...` and communicating with the server.

-   Establishing a TLS connection will fail if a certificate isn't found in the specified location.
    You might receive a log message indicating that when nodes try to connect:

    ```
    2017-03-10 16:16:03.844 [warning] <0.4218.2> global: mongooseim@localhost failed to connect to fed1@localhost
    ```

    If the pointed-at certificate/key/CA-certificate file doesn't exist, it won't be reported before trying to connect.
    Look for (grep) the log message on all cluster nodes, as the message doesn't have to appear on all nodes if a connection fails.

-   You can switch a cluster from running non-TLS distribution, to TLS distribution by shutting down a node, enabling TLS on it, starting it up again, and repeating the steps for each remaining node.
    Again, nodes with and without TLS enabled won't be able to communicate with one another.

-   It's possible to fortify an Erlang cluster further than the Mongoose's preconfigured `vm.dist.args` does.
    This includes: checking certificate revocation status against a CA's Certificate Revocation List, securing/disabling EPMD (Erlang Port Mapper Daemon), using custom certificate verification functions.
    For details on these steps please refer to [Erlang Distribution over TLS][erlang-over-tls] and [Erlang (and Elixir) distribution without epmd][no-epmd].

[erlang-over-tls]: https://www.erlang-solutions.com/blog/erlang-distribution-over-tls.html
[no-epmd]: https://www.erlang-solutions.com/blog/erlang-and-elixir-distribution-without-epmd.html
