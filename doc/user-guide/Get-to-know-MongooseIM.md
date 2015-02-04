## Overview

MongooseIM is Erlang Solutions' robust and efficient XMPP server, aimed at large installations. Specifically designed for enterprise purposes, it is fault-tolerant, can utilise the resources of multiple clustered machines, and easily scale when more capacity is required (by just adding a box/VM). It provides support for WebSockets and reimplemented BOSH.

## Architecture

MongooseIM brings configurability, scalability and fault-tolerance to the core feature of XMPP – routing messages. Its architecture is based on a set of pluggable modules that enable different features, including:

-   Websockets
-   BOSH
-   MUC (Multi-User Chat)
-   Rosters
-   MAM (Message Archive Management)
-   Last activity
-   Metrics
-   Offline messages
-   Privacy settings
-   vCards

This modular architecture allows high customisability and easy access to the required features.  

MongooseIM enables authenticating users using external or internal databases (Mnesia, SQL), LDAP or external scripts. It also allows connecting anonymous users, when required.

For storing persistent data, MongooseIM uses Mnesia (the distributed internal Erlang database) and also MySQL and Postgres (SQL databases). If necessary, MongooseIM can be customised to work with a database chosen by the customer.

Basic MongooseIM session storage is handled in Mnesia, but using Redis is also possible.

## Deployment and management

MongooseIM can be deployed for a number of scenarios fitting customer needs. The default installation setup consists of a single MongooseIM node using Mnesia, so it does not require any additional services. This primary system is sufficient for fast deployment and connecting XMPP clients.

A more scalable solution would be deploying MongooseIM with an external database for persistent data. Such a setup requires a cluster of MongooseIM nodes, an external database, and a load balancer to manage the traffic from the client applications.
If deployed on a 16 GB RAM machine with at least 4 cores, a single MongooseIM node can handle 200-300 K online users. This setup is suitable for systems with up to 10 nodes.

If the service requires a cluster of more than 10 nodes, we recommend using Redis instead of Mnesia for session storage. To avoid a single point of failure, a master-slave Redis setup is advisable. 

MongooseIM allows connecting different clusters as parts of larger systems. This feature is used in geo-localised services handling massive traffic from all over the world.

MongooseIM gathers over 40 different XMPP-related metrics, allowing close monitoring of what happens inside the nodes. To manage the users, rosters, messages and general settings, we provide a command-line tool, `mongooseimctl`.
General knowledge of Erlang and XMPP allows complete control over the system and its components.