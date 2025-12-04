# MongooseIM platform

[![GitHub release](https://img.shields.io/github/release/esl/MongooseIM.svg)](https://github.com/esl/MongooseIM/releases)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/esl/MongooseIM/tree/rel-6.5.svg?style=shield)](https://app.circleci.com/pipelines/github/esl/MongooseIM?branch=rel-6.5)
[![Codecov](https://codecov.io/gh/esl/MongooseIM/branch/rel-6.5/graph/badge.svg)](https://app.codecov.io/gh/esl/MongooseIM/tree/rel-6.5)
[![GitHub Actions](https://github.com/esl/MongooseIM/actions/workflows/ci.yml/badge.svg?branch=rel-6.5)](https://github.com/esl/MongooseIM/actions/workflows/ci.yml?query=branch%3Arel-6.5)
[![Coveralls](https://coveralls.io/repos/github/esl/MongooseIM/badge.svg?branch=rel-6.5)](https://coveralls.io/github/esl/MongooseIM?branch=rel-6.5)

* [Getting started](https://esl.github.io/MongooseDocs/latest/getting-started/Installation/)
* [Developer's guide](https://esl.github.io/MongooseDocs/latest/developers-guide/Testing-MongooseIM/)
* [Packages](https://github.com/esl/MongooseIM/releases/latest)
* Product page: [https://www.erlang-solutions.com/products/mongooseim.html](https://www.erlang-solutions.com/products/mongooseim.html)
* Documentation: [https://esl.github.io/MongooseDocs/](https://esl.github.io/MongooseDocs/latest/)
* Try it now: [https://trymongoose.im](https://trymongoose.im)

## Get to know MongooseIM
MongooseIM is a robust, scalable and efficient XMPP server at the core of an Instant Messaging platform aimed at large installations.

<img align="left" src="doc/MongooseIM_logo.png" alt="MongooseIM platform's logo"/>

Designed for enterprise, it is fault-tolerant, can utilise the resources of multiple clustered machines, and easily scales for more capacity by simply adding a box or a VM.

MongooseIM can accept client sessions over vanilla XMPP, REST API and SSE, as well as Websockets, and BOSH (HTTP long-polling).

As a platform, MongooseIM includes several server-side (backend) and client-side (frontend) components.

We provide a test suite, metrics, a load testing platform, and a monitoring server.
We recommend third-party, open source client libraries for XMPP and REST API.

It is brought to you by [Erlang Solutions](https://www.erlang-solutions.com/).

**MongooseIM platform components**:

<img src="doc/MongooseIM_Platform_components.png" alt="MongooseIM platform schema" />

**Learn more:**

* Home: [http://github.com/esl/MongooseIM](http://github.com/esl/MongooseIM)
* Product page: [https://www.erlang-solutions.com/products/mongooseim.html](https://www.erlang-solutions.com/products/mongooseim.html)
* Documentation: [https://esl.github.io/MongooseDocs/](https://esl.github.io/MongooseDocs/)

## Download packages

For a quick start just download:

* The [Docker image](https://hub.docker.com/r/erlangsolutions/mongooseim/) ([source code repository](https://github.com/esl/mongooseim-docker))
* The [Helm chart](https://artifacthub.io/packages/helm/mongoose/mongooseim) ([source code repository](https://github.com/esl/MongooseHelm))
* The [pre-built packages](https://github.com/esl/MongooseIM/releases/latest) that suit your platform (Ubuntu, Debian, CentOS compatible: AlmaLinux and Rocky Linux)

## Public testing

Check out our test results:

* CI testing:
  * [GH Actions](https://github.com/esl/MongooseIM/actions?query=workflow%3ACI)
  * [CircleCI](https://app.circleci.com/pipelines/github/esl/MongooseIM)
* Code coverage:
  * [Codecov](https://codecov.io/gh/esl/MongooseIM) - reported by CircleCI.
  * [Coveralls](https://coveralls.io/github/esl/MongooseIM) - reported by GH Actions.

## Documentation

See the documentation for the latest releases:

* [Master](https://esl.github.io/MongooseDocs/latest/)
* [6.5.0](https://esl.github.io/MongooseDocs/6.5.0/)
* [6.4.0](https://esl.github.io/MongooseDocs/6.4.0/)
* [6.3.3](https://esl.github.io/MongooseDocs/6.3.3/)
* [6.3.2](https://esl.github.io/MongooseDocs/6.3.2/)
* [6.3.1](https://esl.github.io/MongooseDocs/6.3.1/)
* [6.3.0](https://esl.github.io/MongooseDocs/6.3.0/)
* [6.2.1](https://esl.github.io/MongooseDocs/6.2.1/)
* [6.2.0](https://esl.github.io/MongooseDocs/6.2.0/)
* [6.1.0](https://esl.github.io/MongooseDocs/6.1.0/)
* [6.0.0](https://esl.github.io/MongooseDocs/6.0.0/)
* [5.1.0](https://esl.github.io/MongooseDocs/5.1.0/)
* [5.0.0](https://esl.github.io/MongooseDocs/5.0.0/)
* [4.2.0](https://esl.github.io/MongooseDocs/4.2.0/)
* [4.1.0](https://esl.github.io/MongooseDocs/4.1.0/)
* [4.0.1](https://esl.github.io/MongooseDocs/4.0.1/)
* [3.7.1](https://esl.github.io/MongooseDocs/3.7.1/)
* [3.6.2](https://esl.github.io/MongooseDocs/3.6.2/)
* [3.5.0](https://esl.github.io/MongooseDocs/3.5.0/)
* [3.4.1](https://esl.github.io/MongooseDocs/3.4.1/)
* [3.3.0](https://esl.github.io/MongooseDocs/3.3.0/)
* [3.2.0](https://esl.github.io/MongooseDocs/3.2.0/)
* [3.1.1](https://esl.github.io/MongooseDocs/3.1.1/)
* [3.0.1](https://esl.github.io/MongooseDocs/3.0.1/)

**MongooseIM documentation highlights:**

When developing new features/modules, please make sure you add basic documentation to the ['doc'](doc) directory, and add a link to your document in ['mkdocs.yml'](mkdocs.yml).

* [Tutorials](https://esl.github.io/MongooseDocs/latest/tutorials/How-to-build/). Learn how to:
    * [Build MongooseIM from source code](https://esl.github.io/MongooseDocs/latest/tutorials/How-to-build/)
    * [Set up MongoosePush](https://esl.github.io/MongooseDocs/latest/tutorials/push-notifications/Push-notifications/)
    * [Set up MongooseICE](https://esl.github.io/MongooseDocs/latest/tutorials/ICE_tutorial/)
    * [Build an iOS messaging app](https://esl.github.io/MongooseDocs/latest/tutorials/iOS_tutorial/)
* [User Guide](https://esl.github.io/MongooseDocs/latest/user-guide/Features/). Learn all about how to use MongooseIM in your project. Explore its features, supported XEPs, RFCs and database backends, as well as its architecture and deployment strategies.
* [Configuration](https://esl.github.io/MongooseDocs/latest/configuration/configuration-files/). Explore available options including database backend configuration, access control lists, listener and extension modules.
* [REST API](https://esl.github.io/MongooseDocs/latest/rest-api/Client-frontend/). Explore MongooseIM features using our REST API and [Swagger documentation](https://esl.github.io/MongooseDocs/latest/swagger/index.html).
* [Operation and maintenance](https://esl.github.io/MongooseDocs/latest/operation-and-maintenance/Cluster-management-considerations/). See what to consider when building, monitoring, testing and distributing MongooseIM clusters.
* [Server developer's guide](https://esl.github.io/MongooseDocs/latest/developers-guide/Testing-MongooseIM/). Get all the information you need to expand the MongooseIM platform.

## Participate!

Suggestions, questions, thoughts? Contact us directly:

* Raise a [GitHub issue](https://github.com/esl/MongooseIM/issues)
* Email us at <a href='mailto:mongoose-im@erlang-solutions.com'>mongoose-im@erlang-solutions.com</a>
* Follow our [Twitter account](https://twitter.com/MongooseIM)
