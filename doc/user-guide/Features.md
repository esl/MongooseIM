# MongooseIM Features

MongooseIM is Erlang Solutions' robust, scalable and efficient XMPP server, aimed at large installations.
Specifically designed for enterprise purposes, it is fault-tolerant and can utilise the resources of multiple clustered machines.

Some traits that make it unique include:

* Massive scalability: simple growth through adding nodes provides costs-effectiveness as well as great resource utilisation.
* Platform approach: designed with consistency, end-to-end battle testing across the whole ecosystem (all server and client components, and tools) can be performed.
* Dynamic domains: thanks to the support for multi-tenancy, it is possible to set up thousands of domains dynamically without a noticeable performance overhead.
* Code quality: extensive refactoring, substantial optimisations, continuous integration and deployment.
* Extensive testing: automated continuous functional code coverage, integration testing, end-to-end testing with real clients.
* Unique openness: no proprietary extensions, fully open source, fully open standards.
* Contributions to ([XMPP Standards Foundation](https://xmpp.org/)): implementations of XEPs, innovations contributed.
* Professional support and flexible customer service.
* Contributions to third party open source projects: strengthening the Erlang and XMPP ecosystems.

## Architecture

MongooseIM brings configurability, scalability and fault-tolerance to the core feature of XMPP – routing messages.
Its architecture is based on a set of pluggable [extension modules](../configuration/Modules.md) that enable different features, including:

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

MongooseIM enables authenticating users using external or internal [databases](../authentication-methods/rdbms.md) (Mnesia, RDBMS, NoSQL), [LDAP](../authentication-methods/ldap.md), [HTTP](../authentication-methods/http.md) or [external scripts](../authentication-methods/external.md). It also allows connecting [anonymous users](../authentication-methods/anonymous.md), when required.

For storing persistent data, MongooseIM uses Mnesia (the distributed internal Erlang database), relational databases: MySQL, PostgreSQL or NoSQL alternative: Cassandra.
Please take a look at [database backends configurations](../configuration/database-backends-configuration.md) to learn more.
If necessary, MongooseIM can be customised to work with a different database.
You can [contact us](https://www.erlang-solutions.com/contact/) to learn more.

Basic MongooseIM session storage is handled in Mnesia, but using Redis is also possible.
It is also possible to store user [Message Archives](../modules/mod_mam.md) using ElasticSearch or Cassandra.

## Deployment and management

MongooseIM can be deployed for a number of scenarios fitting your needs. The simplest installation setup consists of a single MongooseIM node using Mnesia, so it does not require any additional services. Such system is sufficient for fast deployment and connecting XMPP clients.

A more scalable solution would be deploying MongooseIM with an external database for persistent data. Bigger setups may require a cluster of MongooseIM nodes, and a load balancer to manage the traffic from the client applications.

A single MongooseIM node can handle as many as 2.5 million online users.
Based on our load tests, for deployments with multiple nodes, we are confident that 10 million online users is well within reach.
Please note that such scalability numbers depend on the selected feature set that your MongooseIM installation is running.

For more details please see our blogpost: [Scaling a Mongoose: How scalable is the MongooseIM XMPP server? ](https://www.erlang-solutions.com/blog/scaling-a-mongoose-how-scalable-is-the-mongooseim-xmpp-server.html)

If the service requires a cluster of more than 10 nodes, we recommend using Redis instead of Mnesia for session storage. To avoid a single point of failure, a master-slave Redis setup is advisable.

MongooseIM allows connecting different clusters as parts of larger systems. This feature is used in geo-localised services handling massive traffic from all over the world.

MongooseIM gathers over 50 different XMPP-related metrics, allowing close monitoring of what happens inside the nodes. To manage the users, rosters, messages and general settings, we provide a GraphQL API which can be utilized via HTTP or command-line tool `mongooseimctl`(see [`GraphQL Admin API`](../graphql-api/Admin-GraphQL.md)).

Erlang Solutions also provides [WombatOAM](https://www.erlang-solutions.com/products/wombat-oam.html), an Erlang VM monitoring solution, that enables ops and devs to better understand what going on in a MongooseIM cluster.

For load testing we use [our own tools](../Contributions.md#amoc), that enable us to validate MongooseIM's scalability, given different scenarios.

## Multi-tenancy (dynamic domains)

MongooseIM supports multi-tenancy.
This makes it possible to set up thousands of domains dynamically without a noticeable performance overhead.
On more information on how to set up this feature, see [dynamic domains configuration](../configuration/general.md#generalhost_types).

## Integration with other platform components

### Client applications

In order to build client applications, the MongooseIM team recommends the following libraries:

| |XMPP|REST API|
| ------------- | ------------- | ------------- |
|iOS|[XMPPframework](https://github.com/robbiehanson/XMPPFramework), Objective-C|[Jayme](https://github.com/inaka/Jayme), Swift|
|Android|[Smack](https://github.com/igniterealtime/Smack), Java|[Retrofit](https://github.com/square/retrofit), Java|
|Web|[Stanza.io](https://github.com/otalk/stanza.io)/[Strophe.js](https://github.com/strophe/strophejs), JavaScript||

### MongoosePUSH
MongooseIM can be integrated with [MongoosePush](https://github.com/esl/MongoosePush).
For more details visit the push notification [user guide](../tutorials/push-notifications/Push-notifications.md).

### MongooseICE
You can also connect Mongoose with [MongooseICE](https://github.com/esl/MongooseICE).
To get started, we recommend going through [this tutorial](../tutorials/ICE_tutorial.md).
