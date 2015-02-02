Environment configuration
---

### File descriptors

To handle large traffic some of the system variables need to be tuned. Number one on that list is the maximum number of file descriptors which by default is set to 1024. Each MongooseIM connection consumes ~1 file descriptor, so the default value will not suffice for larger installations - when it is exceeded, emfile errors will appear in logs.

To check the current limit execute: `ulimit -n`.
To list all lmits execute `ulimit -a`.


In the example below we set limits for an mongooseim user. To increase the limit the following entries should be added in `/etc/security/limits.conf`:

> mongooseim   soft   nofile   1000000

> mongooseim   hard   nofile   1000000

If you are using **Ubuntu** all `/etc/pam.d/common-session*` files should include
`session required pam_limits.so`.


### vm.args and vars.config

Before making a release some values in `rel/files/vm.args' have to be changed. When using an SSL connection we advise to increase ERL_MAX_PORTS to 350000. This value
specifies how many ports (files, drivers, sockets etc) can be used by Erlang VM.
Be cautious - it preallocates some structures inside the VM and will have impact on memory usage. We suggest 350000 for 100K users when using an ssl connection or 250000 in other cases.

**Note: Make sure running mongooseim has -args_file parameter set to this file `rel/files/vm.args'. By default it is pointing to 'etc/vm.args'. You can view this by running 'mongooseim live' command. **
 
To check how memory consumption changes depending on ERL_MAX_PORTS use the following command:
`env ERL_MAX_PORTS=[given value] erl -noinput -eval 'io:format("~p~n",[erlang:memory(system)]).' -s erlang halt`

Another change you need to make when building a MongooseIM cluster is setting the -sname. To do it just set node_name in `rel/vars.config` with node's hostname, which **must** be resolvable on other nodes in the cluster.

### Port range

To connect to other nodes a freshly started node uses a port from the range `inet_dist_listen_min` to `inet_dist_listen_max`.
To enable this, add the following line to the `vm.args` file:

`-kernel inet_dist_listen_min 50000 inet_dist_listen_max 50010`
Make sure that the range you set provides enough ports for all the nodes in the cluster.

Remember to keep an epmd port open (port 4369) if any firewall restrictions are required.
Epmd keeps track of which erlang node is using which ports on the local machine.


Load Balancing
---

### Elastic Load balancer

We do not recommend using ELB. Many of our users reported it to prematurely drop connections, especially when using BOSH. All known AWS installations are using custom load balancing solutions for XMPP.

### DNS based load balancing

Load balancing can be performed on a DNS level. A DNS response can have a number of IP addresses that can be returned to the client side in a random order. 

On the AWS stack this type of balancing is provided by Route53. The description of their service can be found at:
http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/WeightedResourceRecordSets.html

### Software load balancer

A good examples of load balancing on the application layer are HAProxy and Nginx. 

### Other
The approaches described above can be mixed - we can use DNS load balancing to pick a
software load balancer which will select one of the nodes.


Connecting nodes
---

Checklist:
- working directory `rel/mongooseim`
- the same cookie across all nodes (vm.args -setcookie parameter)
- each node should be able to ping other nodes using its sname
   (ex. `net_adm:ping('mongoose@localhost')`)
- no mnesia files - no `Mnesia.mongooseim@[host name]` directory in cwd on new node's release

### Initial node

There is no action required on the initial node. 
Just start MongooseIM using `mongooseim start` or `mongooseim live`

### New node

#### MongooseIM 1.5.0 and newer
Since MongooseIM 1.5.0 adding new node to a cluster is as simple as executing following command:

```bash
mongooseimctl add_to_cluster NodeName
```

`NodeName` is the name of running node set in `vm.args` file, for example `mongooseim@localhost`.
The successful output from above command starts with `Node added to cluster`. 

Now MongooseIM can be started using `mongooseim start/live` command.

#### MongooseIM before 1.5.0

Start an Erlang shell with `mongooseim console_clean`, and enter:

```erlang
    mnesia:start().
    OtherNode = 'mongooseim@node1'.
    {ok, [OtherNode]} = mnesia:change_config(extra_db_nodes, [OtherNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).
    [mnesia:add_table_copy(Tab, node(), ram_copies) ||
        Tab <- [acl,config,iq_response,local_config,node,route,s2s,
                      session,sm_session,sql_pool]].

```

Exit shell and start MongooseIM using `mongooseim start/live`

### Remove node

#### MongooseIM 1.5.0 and newer

To remove a node, connect to a different one and execute enter:

    mongooseimctl remove_from_cluster RemovedNodeName

Where `RemovedNodeName` is for example `mongooseim@node2`

#### MongooseIM before 1.5.0

To remove a node, connect to a different one and use `mongooseim debug` and execute following commands:

```erlang
DeadNode = 'mongooseim@node2'.
mnesia:del_table_copy(schema, DeadNode).
```

### Cluster status

To see if newly added node is properly clustered or to see how the cluster looks like following commands can be useful. (on any running node in the cluster)

```bash
mongooseimctl mnesia info | grep "running db nodes"
```

This command shows all running nodes. Health cluster should contain all nodes here. For example:

    running db nodes   = [mongooseim@node1, mongooseim@node2]

To see stopped or misbehaving nodes following command can be useful:

```bash
mongooseimctl mnesia info | grep "stopped db nodes"
```

This command shows which nodes are considered as stopped. They not necessarily have to be down but there might be a communication problem with these nodes.