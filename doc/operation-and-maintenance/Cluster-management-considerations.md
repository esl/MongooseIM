This applies whatever the technology: bare metal, virtualisation, hypervisor, containers, etc.

## Single-node MongooseIM

With single-node MongooseIM, one can setup a vertically scalable system, that is function of the server resources. MongooseIM could scale from hundreds to tens of thousands of concurrent users.

In single-node MongooseIM, there is no load distribution, and no fallback or failover in case of failure.

This applies low-scale deployments, such as testing and development environments, whether it is on embedded devices, personal computers, or servers.

## Dual-node MongooseIM

With dual-node MongooseIM, one can setup a vertically scalable system, that is function of the servers resources. We recommend that servers with the same power are used. One node could handle a different set of services, given these non-MongooseIM services consumes roughly the same resources on both servers. In this setup, MongooseIM could scale up to hundred of thousands of concurrent users.

In dual-node MongooseIM, there is 50-50 load distribution. There is a possible fallback or failover in case of a node failure, but then the remaining node should be able to handle the full load in order to avoid degrading the service.

This applies from low to mid-scale deployments, such as functional and load testing, and initial production environments. We recommend real dedicated servers, although MongooseIM could run in cluster mode with low-power machines, such as embedded devices.

## Multi-node MongooseIM

With multi-node MongooseIM, one can setup a vertically and horizontally scalable system, that is still function of the servers resources. We highly recommend that servers with the same available power are used. We also recommend that no node handles a different set of services, because a risk of unbalance can appear. In this setup, MongooseIM could scale up to tens of millions of concurrent users.

In multi-node MongooseIM, with `n` nodes, there is `1/n` load distribution. There is a possible fallback or failover in case of a node failure, but then the remaining nodes should be able to handle the `1/n` load in order to avoid degrading the service.

This applies from mid to large-scale deployments, such as production environments. We recommend real dedicated, powerful servers.

## Multi-datacenter MongooseIM

With multi-datacenter MongooseIM, one can setup a vertically and horizontally highly scalable system. The MongooseIM clusters are simply distributed across continents for local, low-lag client connections, and the clusters are interconnected via high-speed links. In this setup, MongooseIM could scale up to hundreds of millions of concurrent users.

This applies from large to very large-scale deployments.

Contact us.

## Summary table

Setup: reflects the number of nodes in your cluster
Purpose: is the goal and use of this cluster
Low-end: low-power machines, such as laptops, embedded devides, entry-level cloud or bare metal
High-end: powerful machines, with lots of memory, multi-core CPU, whether they or cloud or bare metal

Setup | Purpose | Low-end | High-end
------|---------|---------|---------
Single-node | Functional testing, development       | 100  to  10k   | 100k to 500k
Dual-node | Low-end production system, load testing |   1k to 100k   |   1M to   5M
Multi-node | High-end production system             |  10k to   1M   |   2M to  10M
Multi-datacenter | Very large scale product system  | 100k to  10M   |  10M to 100M

IMPORTANT NOTES: scalability highly depends a lot on various variables, such as:
* machines power, such as memory, CPU, I/O
* the number of concurrent users:
  * most iOS apps are not connected in the background, they use APNS to push info to the device
  * web clients use websockets, with fallback on BOSH (HTTP long-polling)
  * client-side and backend-side REST API
* how much archiving is needed and the latency for storage and querying, which depends a lot on storage backend architecture and scalability
* message throughtput:
  * one-to-one
  * MUC
  * MUC light
  * PubSub
  * Presences
  * HTTP notifications (with queuing systems such as RabbitMQ or Kafka)
* latency of messaging, both real-time and archived messages
