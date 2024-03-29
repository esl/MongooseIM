## Environment configuration

### File descriptors

To handle large traffic, some of the system variables need to be tuned.
Number one on that list is the maximum number of file descriptors which often is set to 1024.
Each MongooseIM connection consumes ~1 file descriptor, so the default value will not suffice for larger installations - when it is exceeded, emfile errors will appear in logs.

To check the current limit execute: `ulimit -n`.

To list all limits execute: `ulimit -a`.

In the example below we set limits for a `mongooseim` user.
To increase the limit the following entries should be added in `/etc/security/limits.conf`:

```
mongooseim   soft   nofile   1000000
mongooseim   hard   nofile   1000000
```

If you are using **Ubuntu**, all `/etc/pam.d/common-session*` files should include `session required pam_limits.so`.

### `vm.args` file

This file contains Erlang options used when starting the VM.
It is located in `REL_ROOT/etc/vm.args` where `REL_ROOT` is the path to a MongooseIM release
(ie. `_build/prod/rel/mongooseim` if you build MongooseIM from source).

When using an SSL/TLS connection we advise to increase `ERL_MAX_PORTS` to `350000`.
This value specifies how many ports (files, drivers, sockets etc) can be used by the Erlang VM.
Be cautious - it preallocates some structures inside the VM and will have impact on the memory usage.
We suggest 350000 for 100 k users when using an SSL/TLS connection or 250000 in other cases.

To check how memory consumption changes depending on `ERL_MAX_PORTS`, use the following command:

```bash
env ERL_MAX_PORTS=[given value] erl -noinput -eval 'io:format("~p~n",[erlang:memory(system)]).' -s erlang halt
```

Another change you need to make when building a MongooseIM cluster is setting the `-sname`.
To do it, just set the `-sname` option in `vm.args` with node's hostname,
which **must** be resolvable on other nodes in the cluster.

### Port range

To connect to other nodes, a freshly started node uses a port from the range `inet_dist_listen_min` to `inet_dist_listen_max`.

To enable this, add the following line to the `vm.args` file:

```
-kernel inet_dist_listen_min 50000 inet_dist_listen_max 50010
```

Make sure that the range you set provides enough ports for all the nodes in the cluster.

Remember to keep an epmd port open (port 4369) if any firewall restrictions are required.
Epmd keeps track of which Erlang node is using which ports on the local machine.

## Connecting nodes

Checklist:

- working directory `rel/mongooseim` (root of a MongooseIM release or installation)
- the same cookie across all nodes (`vm.args` `-setcookie` parameter)
- each node should be able to ping other nodes using its sname
   (ex. `net_adm:ping('mongoose@localhost')`)
- RDBMS backend is configured, so CETS could discover nodes

### Initial node

=== "CETS"

    Clustering is automatic. There is no difference between nodes.

=== "Mnesia"

    There is no action required on the initial node.

     Just start MongooseIM using `mongooseim start` or `mongooseim live`.

### New node - joining cluster

=== "CETS"

    Clustering is automatic.

=== "Mnesia"

    ```bash
    mongooseimctl start
    mongooseimctl started #waits until MongooseIM starts
    mongooseimctl join_cluster ClusterMember
    ```

    `ClusterMember` is the name of a running node set in `vm.args` file, for example `mongooseim@localhost`.
    This node has to be part of the cluster we'd like to join.

    First, MongooseIM will display a warning and a question if the operation should proceed:

    ```text
    Warning. This will drop all current connections and will discard all persistent data from Mnesia. Do you want to continue? (yes/no)
    ```

    If you type `yes` MongooseIM will start joining the cluster.
    Successful output may look like the following:

    ```text
    You have successfully joined the node mongooseim2@localhost to the cluster with node member mongooseim@localhost
    ```

    In order to skip the question you can add option `-f` which will perform the action
    without displaying the warning and waiting for the confirmation.

### Leaving cluster

=== "CETS"

    Stopping the node is enough to leave the cluster.
    If you want to avoid the node joining the cluster again, you have to specify a different `cluster_name`
    option in the CETS backend configuration. A different Erlang cookie is a good idea too.

=== "Mnesia"

    To leave a running node from the cluster, call:

    ```bash
    mongooseimctl leave_cluster
    ```

    It only makes sense to use it if the node is part of a cluster, e.g `join_cluster` was called on that node before.

    Similarly to `join_cluster` a warning and a question will be displayed unless the option `-f` is added to the command.

    The successful output from the above command may look like the following:

    ```text
    The node mongooseim2@localhost has successfully left the cluster
    ```

### Removing a node from the cluster

=== "CETS"

    A stopped node would be automatically removed from the node discovery table in RDBMS database after some time.
    It is needed so other nodes would not try to connect to the stopped node.

=== "Mnesia"

    To remove another node from the cluster, call the following command from one of the cluster members:

    ```bash
    mongooseimctl remove_from_cluster RemoteNodeName
    ```

    where `RemoteNodeName` is the name of the node that we'd like to remove from our cluster.
    This command could be useful when the node is dead and not responding and we'd like to remove it remotely.
    The successful output from the above command may look like the following:

    ```text
    The node mongooseim2@localhost has been removed from the cluster
    ```

### Cluster status

=== "CETS"

    Run the command:

    ```bash
    mongooseimctl cets systemInfo
    ```

    `joinedNodes` should contain a list of properly joined nodes:

    ```json
    "joinedNodes" : [
      "mongooseim@node1",
      "mongooseim@node2"
    ]
    ```

    It should generally be equal to the list of `discoveredNodes`.

    If it is not equal, you could have some configuration or networking issues.
    You can check the `unavailableNodes`, `remoteNodesWithUnknownTables`,
    and `remoteNodesWithMissingTables` lists for more information (generally, these lists should be empty).
    You can read the description for other fields of `systemInfo` in the
    [GraphQL API reference](../graphql-api/admin-graphql-doc.html#definition-CETSSystemInfo).

    For a properly configured 2 nodes cluster the metrics would show something like that:

    ```json
    mongooseimctl metric getMetrics --name '["global", "cets", "system"]'
    {
      "data" : {
        "metric" : {
          "getMetrics" : [
            {
              "unavailable_nodes" : 0,
              "type" : "cets_system",
              "remote_unknown_tables" : 0,
              "remote_nodes_without_disco" : 0,
              "remote_nodes_with_unknown_tables" : 0,
              "remote_nodes_with_missing_tables" : 0,
              "remote_missing_tables" : 0,
              "name" : [
                "global",
                "cets",
                "system"
              ],
              "joined_nodes" : 2,
              "discovery_works" : 1,
              "discovered_nodes" : 2,
              "conflict_tables" : 0,
              "conflict_nodes" : 0,
              "available_nodes" : 2
            }
          ]
        }
      }
    }
    ```

=== "Mnesia"

    You can use the following commands on any of the running nodes to examine the cluster
    or to see if a newly added node is properly clustered:

    ```bash
    mongooseimctl mnesia info | grep "running db nodes"
    ```

    This command shows all running nodes.
    A healthy cluster should contain all nodes here.
    For example:
    ```bash
    running db nodes = [mongooseim@node1, mongooseim@node2]
    ```
    To see stopped or misbehaving nodes the following command can be useful:

    ```bash
    mongooseimctl mnesia info | grep "stopped db nodes"
    ```

    This command shows which nodes are considered stopped.
    This does not necessarily indicate that they are down but might be a symptom of a communication problem.

## Load Balancing

### Elastic Load Balancer (ELB)

When using ELB please be advised that some warm-up time may be needed before
the load balancer works efficiently for a big load.

### Software load balancer

A good example of load balancing on the application layer are HAProxy and Nginx.

### DNS-based load balancing

Load balancing can be performed on a DNS level.
A DNS response can have a number of IP addresses that can be returned to the client side in a random order.

On the AWS stack this type of balancing is provided by Route53.
The description of their service can be found in the [Route53 Developer's Guide](http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/WeightedResourceRecordSets.html).

### Other

The approaches described above can be mixed - we can use DNS load balancing to pick a software load balancer which will select one of the nodes.
