## Rolling upgrade
For all MongooseIM production deployments we recommend running multiple server nodes connected in a cluster behind a load-balancer.
Rolling upgrade is a process of upgrading MongooseIM cluster, one node at a time.
Make sure you have at least the number of nodes able to handle your traffic plus one before the rolling upgrade to guarantee the availability and minimise the downtime.
Running different MongooseIM versions at the same time beyond the duration of the upgrade is not recommended and not supported.

Rolling upgrade procedure is recommended over configuration reload which is not supported since version 4.1.

Please note that more complex upgrades that involve schema updates, customisations or have functional changes might require more specific and specially crafted migration procedure.

If you want just to make the changes to the configuration file, please follow steps 1, 3, 4, 6, 7, 8.
This type of change can also be done one node at a time.
It would require you to check the cluster status, modify the configuration file and restart the node.

The usual MongooseIM cluster upgrade can be achieved with the following steps:

### 1. Check the cluster status.

Use the following command on the running nodes and examine the status of the cluster:

```bash
mongooseimctl mnesia info | grep "running db nodes"

running db nodes = [mongooseim@node1, mongooseim@node2]
```

This command shows all running nodes.
A healthy cluster should list all nodes that are part of the cluster.

Should you have any issues related to node clustering, please refer to [Cluster configuration and node management](Cluster-configuration-and-node-management.md) section.

### 2. Copy the configuration file.

Make a copy of the configuration file before the upgrade, as some package managers might override your custom configuration with the default one.
Please note that since version 4.1 `*.cfg` MongooseIM configuration format is no longer supported and needs to be rewritten in the new `*.toml` format.

### 3. Apply the changes from the migration guide.

All modifications of the configuration file or updates of the database schema, that are required to perform version upgrade, can be found in the Migration Guide section.
When upgrading more than one version, please make sure to go over all consecutive migration guides.

For example, when migrating from MongooseIM 3.7 to 4.1, please familiarize yourself with and apply all necessary changes described in the following pages of the Migration Guide section.

* 3.7.0 to 4.0.0
* 4.0.0 to 4.0.1
* 4.0.1 to 4.1.0

### 4. Stop the running node.

Use the following command to stop the MongooseIM node:

```bash
mongooseimctl stop
```

### 5. Install new MongooseIM version.

You can get the new version of MongooseIM by either [building MongooseIM from source code](../tutorials/How-to-build.md) or [downloading and upgrading from package](../getting-started/Installation.md).

### 6. Start the node.

Use the following command to start and check the status of the MongooseIM node and the cluster:

```bash
mongooseimctl start
mongooseimctl status

mongooseimctl mnesia info | grep "running db nodes"
```

### 7. Test the cluster.

Please verify that the nodes are running and part of the same cluster.
If the cluster is working as expected, the migration of the node is complete.

### 8. Upgrade the remaining nodes.

Once all the prior steps are completed successfully, repeat the process for all nodes that are part of the MongooseIM cluster.

## Further cluster upgrade considerations

Another way to perform a cluster upgrade with minimising possible downtime would be to setup a parallel MongooseIM cluster running newer version.
You can redirect the incoming traffic to the new cluster with use of a load-balancer.

Once no connections are handled by the old cluster, it can by safely stopped and the migration is complete.

We highly recommend testing new software release in staging environment before it is deployed on production.

Should you need any help with the upgrade, deployments or load testing of your MongooseIM cluster, please reach out to us. MongooseIM consultancy and support is part of [our commercial offering](https://www.erlang-solutions.com/products/mongooseim.html).
