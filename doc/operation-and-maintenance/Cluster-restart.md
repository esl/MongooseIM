When we are using a MongooseIM cluster with Mnesia that is storing the data as disc copies there could occur an issue related to the distributed Mnesia nodes.

## How to restart a cluster:

Having Node A and Node B, the cluster restart procedure should occur in the following way:

![How to restart a cluster](cluster_restart.png)

## How NOT to restart a cluster:

Having Node A and Node B

![How not to restart a cluster](incorrect_cluster_restart.png)

Changing the order of the restarted nodes can cause issues with distributed Mnesia that has tables with disc_copies.

For more information related to the cluster configuration and maintenance, please see [Cluster configuration and node management](Cluster-configuration-and-node-management.md) section.
