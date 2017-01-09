## Inside MongooseIM

![Inside MongooseIM](Inside_MongooseIM.png)

### Modules

MongooseIM is basically a huge router, that comes with a large set of modules. These modules modify and extend the behaviour and features of MongooseIM. Sysadmins and DevOps can configure these modules and potentially enable them. A wide range of modules is available, such as authentication, privacy, storage, backend integration, mobile optimisations... See '[Extension Modules](../advanced-configuration/Modules.md)' for more info.

### Databases

Remember that MongooseIM manages two sets of data: transient for session data management, and persistent for archive and configurations.

Please refer to '[Database Backends](../advanced-configuration/database-backends-configuration.md)' doc for more configuration information.

#### Transient databases

Mnesia will run on the same nodes as MongooseIM. Corollary: each MongooseIM node host has a Mnesia node.

Redis will be a separate cluster, not using the same nodes as MongooseIM.

No need to backup here, since the transient data naturally rebuilds as clients reconnect massively.

#### Persistant databases

Both RDBMS/SQL (MySQL/PostgreSQL) and NOSQL (Riak KV) databases are supported.

Backups should be regular, and tested.

#### LDAP directory

LDAP will also run on a separate cluster.

Backups should be regular, and tested.

## Outside MongooseIM: ecosystem in a datacenter

![MongooseIM high-level architecture](MongooseIM_high-level_architecture.png)

### Frontend

Native clients, on platforms such as Android, iOS, Windows, Linux, macOS, will preferrably use a plain XMPP over TCP connections.

Web clients will preferrably use XMPP over websockets, or the now less relevant XMPP over BOSH (using long-lived HTTP connections, more and more used as fallback) because web clients cannot use TCP connections.

Any client could use the client REST API, which is using HTTP request/responses.

All these client connections will hit a frontend load balancer before reaching the MongooseIM cluster.

### Backend

MongooseIM can communicate both ways with other backend services in the datacenter infrastructure.

The MongooseIM REST API is available for control/management of MongooseIM's operations as well as functional aspects.

An HTTP notification enables the forwarding of events to any other external HTTP service.

### Management and monitoring

WombatOAM enables the monitoring and management of MongooseIM clusters, as well as Riak KV, RabbitMQ, and any other Erlang and Elixir based system.

### ICE server (STUN/TURN)

Contact us.

### Mobile Push Notifications (APNS, GCM)

Contact us.

## MongooseIM in a worldwide, multi-datacenter configuration

![MongooseIM worlwide architecture](MongooseIM_worlwide_architecture.png)

The MongooseIM platform enables a service to scale worlwide, with proximity servers across continents and datacenters. It leverages the use of the open standard S2S (server-to-server) protocol.

Contact us.
