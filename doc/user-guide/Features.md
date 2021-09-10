# MongooseIM Features

MongooseIM is Erlang Solutions' robust, scalable and efficient XMPP server, aimed at large installations.
Specifically designed for enterprise purposes, it is fault-tolerant, can utilise the resources of multiple clustered machines, and easily scale when more capacity is required by just adding nodes (new hardware boxes or VMs).

Some of the traits that make it unique include:

* Massive scalability: for greater and faster growth, costs-effectiveness as well as resource utilisation
* Platform approach: designed with consistency, end-to-end battle testing across the whole ecosystem (all server and client components, and tools)
* Dynamic domains: thanks to the support for multi-tenancy, it is possible to set up thousands of domains dynamically without a noticeable performance overhead.
* Code quality: extensive refactoring, substantial optimisations, continuous integration and deployment
* Extensive testing: automated continuous functional code coverage, integration testing, end-to-end testing with real clients
* Continuous load testing
* Unique version: no proprietary extensions, fully open source, fully open standards
* Contributions to ([XMPP Standards Foundation](https://xmpp.org/)): implementations of XEPs, innovations contributed
* Professional support, and flexible customer service
* Contributions to third party open source codebases: strengthening the ecosystem


## Architecture

MongooseIM brings configurability, scalability and fault-tolerance to the core feature of XMPP – routing messages. Its architecture is based on a set of pluggable modules that enable different features, including:

-   Websockets: long-lived connections in the browser
-   BOSH: HTTP long-polling
-   MUC (Multi-User Chat): group chat
-   Rosters: contact list, and subscriptions to users' presences
-   MAM: Message Archive Management
-   Message Carbons: for multi-device, real-time copies of all messages
-   Last activity
-   Metrics
-   Offline messages
-   Privacy settings
-   vCards: user profiles

This modular architecture allows high customisability and easy access to the required features.

MongooseIM enables authenticating users using external or internal databases (Mnesia, RDBMS, NOSQL), LDAP or external scripts. It also allows connecting anonymous users, when required.

For storing persistent data, MongooseIM uses Mnesia (the distributed internal Erlang database) and also MySQL and PostgreSQL (RDBMS databases), and Riak KV (NOSQL). If necessary, MongooseIM can be customised to work with a database chosen by the customer.

Basic MongooseIM session storage is handled in Mnesia, but using Redis is also possible.

## Deployment and management

MongooseIM can be deployed for a number of scenarios fitting customer needs. The default installation setup consists of a single MongooseIM node using Mnesia, so it does not require any additional services. This primary system is sufficient for fast deployment and connecting XMPP clients.

A more scalable solution would be deploying MongooseIM with an external database for persistent data. Such a setup requires a cluster of MongooseIM nodes, an external database, and a load balancer to manage the traffic from the client applications.

A single MongooseIM node can handle as many as 2.5 million online users.
Based on our load tests, for deployments with multiple nodes, we are confident that 10 million online users is well within reach.
Please note that such scalability numbers depend on the selected feature set that your MongooseIM installation is running.

For more details please see our blogpost: [Scaling a Mongoose: How scalable is the MongooseIM XMPP server? ](https://www.erlang-solutions.com/blog/scaling-a-mongoose-how-scalable-is-the-mongooseim-xmpp-server.html)

If the service requires a cluster of more than 10 nodes, we recommend using Redis instead of Mnesia for session storage. To avoid a single point of failure, a master-slave Redis setup is advisable.

MongooseIM allows connecting different clusters as parts of larger systems. This feature is used in geo-localised services handling massive traffic from all over the world.

MongooseIM gathers over 50 different XMPP-related metrics, allowing close monitoring of what happens inside the nodes. To manage the users, rosters, messages and general settings, we provide a command-line tool, `mongooseimctl`.

Erlang Solutions also provides [WombatOAM](https://www.erlang-solutions.com/products/wombat-oam.html), an erlang VM monitoring solution, that enables ops and devs to better understand what going on in a MongooseIM cluster.

For load testing consider [Tide](http://tide.erlang-solutions.com/), another Erlang Solutions' tool that enables devs and ops to validate their scalability, given the clients scenarios.

## Multi-tenancy (dynamic domains)

MongooseIM supports multi-tenancy.
This makes it possible to set up thousands of domains dynamically without a noticeable performance overhead.
On more information on how to set up this feature, see [dynamic domains configuration](../advanced-configuration/general.md#generalhost_types) and [REST API for dynamic domains](../rest-api/Dynamic-domains.md).

## Client side

In order to build client applications, the MongooseIM team recommends the following libraries:

| |XMPP|REST API|
| ------------- | ------------- | ------------- |
|iOS|[XMPPframework](https://github.com/robbiehanson/XMPPFramework), Objective-C|[Jayme](https://github.com/inaka/Jayme), Swift|
|Android|[Smack](https://github.com/igniterealtime/Smack), Java|[Retrofit](https://github.com/square/retrofit), Java|
|Web|[Stanza.io](https://github.com/otalk/stanza.io)/[Strophe.js](https://github.com/strophe/strophejs), JavaScript||

General knowledge of Erlang and XMPP allows complete control over the system and its components.

## Integration with other platform components

### MongoosePUSH
MongooseIM can be integrated with [MongoosePush](https://github.com/esl/MongoosePush).
For more details visit the push notification [user guide](./push-notifications/Push-notifications.md).

### MongooseICE
You can also connect Mongoose with [MongooseICE](https://github.com/esl/MongooseICE).
To get started, we recommend going through [this tutorial](ICE_tutorial.md).
