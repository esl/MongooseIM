# MongooseIM platform

<img align="left" src="MongooseIM_logo.png" alt="MongooseIM platform's logo" />

MongooseIM is Erlang Solutions' robust and efficient XMPP platform aimed at large installations. Specifically designed for enterprise purposes, it is fault-tolerant, can utilize resources of multiple clustered machines and easily scale in need of more capacity (by just adding a box/VM). MongooseIM can accept client sessions over vanilla XMPP, Websockets, HTTP long-polling (a.k.a. BOSH), and a REST API.

The MongooseIM platform comes with server-side components and client libraries. We provide a test suite and a monitoring server. We recommand third-party, open source client libraries for XMPP and REST API.

Its home on GitHub is at http://github.com/esl/MongooseIM.

The product page is available at https://www.erlang-solutions.com/products/mongooseim.html

<img src="mongoose_top_banner_800.jpeg" alt="MongooseIM platform's mongooses faces" />

The MongooseIM platform documentation:

* User Guide
    * [Features and supported standards](user-guide/Features-and-supported-standards.md) contains the list of supported XEPs, RFCs and database backends.
    * [Get to know MongooseIM](user-guide/Get-to-know-MongooseIM.md) contains the overview of our application, its architecture and deployment strategies.
    * [Getting started](user-guide/Getting-started.md) is a step-by-step guide on how to:
        * Build MongooseIM on a supported OS
        * Perform basic configuration
        * Use the main administration script, `mongooseimctl`
* [Contributions](Contributions.md)
* [Roadmap](Roadmap.md)
* Configuration
    * [Basic configuration](Basic-configuration.md)
    * [Advanced configuration](Advanced-configuration.md)
        * [Overview](Advanced-configuration.md)
        * [Database backends configuration](advanced-configuration/database-backends-configuration.md)
        * [Listener modules](advanced-configuration/Listener-modules.md)
        * [Extension modules](advanced-configuration/Modules.md)
        * [HTTP authentication module](advanced-configuration/HTTP-authentication-module.md)
* Operation and maintenance
    * [Metrics](operation-and-maintenance/Mongoose-metrics.md)
    * [Logging & monitoring](operation-and-maintenance/Logging-&-monitoring.md)
    * [Cluster configuration and node management](operation-and-maintenance/Cluster-configuration-and-node-management.md)
    * [Reloading configuration on a running system](operation-and-maintenance/Reloading-configuration-on-a-running-system.md)
    * [HTTP Administration API](http-api/http-administration-api-documentation.md)
* For developers
    * [Testing MongooseIM](developers-guide/Testing-MongooseIM.md)
    * [Hooks and handlers](developers-guide/Hooks-and-handlers.md)
    * [REST Interface to Metrics](developers-guide/REST-interface-to-metrics.md)
    * [mod_amp developer's guide](developers-guide/mod_amp_developers_guide.md)
    * [mod_muc_light developer's guide](developers-guide/mod_muc_light_developers_guide.md)
    * [xep-tool usage](developers-guide/xep_tool.md)
