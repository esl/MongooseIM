This applies whatever the technology: bare metal, virtualisation, hypervisor, containers, etc.

## Single-node MongooseIM

With single-node MongooseIM, one can setup a vertically scalable system, that is function of the server resources. MongooseIM could scale from hundreds to tens of thousands of concurrent users.

In single-node MongooseIM, there is no load distribution, and no fallback or failover in case of failure.

This applies low-scale deployments, such as testing and development environments, whether it is on embadded devices, personal computers, or servers.

## Dual-node MongooseIM

With dual-node MongooseIM, one can setup a vertically scalable system, that is function of the servers resources. We recommend that servers with the same power are used. One node could handle a different set of services, given these non-MongooseIM services consumes roughly the same resources on both servers. In this setup, MongooseIM could scale up to hundred of thousands of concurrent users.

In dual-node MongooseIM, there is 50-50 load distribution. There is a possible fallback or failover in case of a node failure, but then the remaining node should be able to handle the full load in order to avoid degrading the service.

This applies low to mid-scale deployments, such as functional and load testing, and initial production environments. We recommend real dedicated servers, although MongooseIM coul run in cluster mode with embedded devices.

## Multi-node MongooseIM

With multi-node MongooseIM, one can setup a vertically and horizontally scalable system, that is still function of the servers resources. We highly recommend that servers with the same power are used. We also recommend that no node handles a different set of services, because a risk of unbalance can appear. In this setup, MongooseIM could scale up to tens of millions of concurrent users.

In multi-node MongooseIM, with `n` nodes, there is `1/n` load distribution. There is a possible fallback or failover in case of a node failure, but then the remaining nodes should be able to handle the `1/n` load in order to avoid degrading the service.

This applies mid to large-scale deployments, such as production environments. We recommend real dedicated servers.

## Multi-datacenter MongooseIM

With multi-datacenter MongooseIM, one can setup a vertically and horizontally highly scalable system. The MongooseIM clusters are simply distributed across continents, and interconnected via high-speed links. In this setup, MongooseIM could scale up to hundreds of millions of concurrent users.

This applies mid to very large-scale deployments.
