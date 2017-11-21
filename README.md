# MongooseIM platform

[![Build Status](https://travis-ci.org/esl/MongooseIM.svg?branch=master)](https://travis-ci.org/esl/MongooseIM) [![Documentation Status](https://readthedocs.org/projects/mongooseim/badge/?version=latest)](http://mongooseim.readthedocs.org/en/latest/?badge=latest) [![Coverage Status](https://img.shields.io/coveralls/esl/MongooseIM.svg)](https://coveralls.io/r/esl/MongooseIM?branch=master) [![GitHub release](https://img.shields.io/github/release/esl/MongooseIM.svg)](https://github.com/esl/MongooseIM/releases)

* [Getting started](http://mongooseim.readthedocs.io/en/latest/user-guide/Getting-started/)
* [Developer's guide](http://mongooseim.readthedocs.io/en/latest/developers-guide/Testing-MongooseIM/)
* [Packages](https://www.erlang-solutions.com/resources/download.html)
* Product page: [https://www.erlang-solutions.com/products/mongooseim.html](https://www.erlang-solutions.com/products/mongooseim.html)
* Documentation: [http://mongooseim.readthedocs.org/](http://mongooseim.readthedocs.org/)

## Get to know MongooseIM 
MongooseIM is a robust and efficient chat (or instant messaging) platform aimed at large installations. 

<img align="left" src="doc/MongooseIM_logo.png" alt="MongooseIM platform's logo"/>

Designed for enterprise, it is fault-tolerant, can utilise the resources of multiple clustered machines, and easily scales for more capacity by simply adding a box or VM.

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
* Documentation: [http://mongooseim.readthedocs.org/](http://mongooseim.readthedocs.org/)




## Download packages

For a quick start just download:

* The [pre-built packages](https://www.erlang-solutions.com/resources/download.html) that suit your platform (Ubuntu, Debian, CentOS, and macOS)
* The [Docker image](https://hub.docker.com/r/mongooseim/mongooseim/): [https://hub.docker.com/r/mongooseim/mongooseim/](https://hub.docker.com/r/mongooseim/mongooseim/) (source code repository: [https://github.com/esl/mongooseim-docker](https://github.com/esl/mongooseim-docker))

## Public testing

Check out our test results:

* Continuous integration: [https://travis-ci.org/esl/MongooseIM](https://travis-ci.org/esl/MongooseIM)
* Code coverage: [https://coveralls.io/github/esl/MongooseIM](https://coveralls.io/github/esl/MongooseIM)
* Continuous Load Testing: [https://tide.erlang-solutions.com/](https://tide.erlang-solutions.com/)
* Load test history:  
  ![Load test history](https://tide.erlang-solutions.com/charts/bidaily_last_year.png)


## Documentation

Up-to-date documentation for the MongooseIM master branch can be found on ReadTheDocs: [http://mongooseim.readthedocs.org/en/latest/](http://mongooseim.readthedocs.org/en/latest/).

Latest releases:
* [2.1.0](http://mongooseim.readthedocs.io/en/rel-2.1/)
* [2.0.1](http://mongooseim.readthedocs.org/en/2.0.1/)
* [2.0.0](http://mongooseim.readthedocs.org/en/2.0.0/)
* [1.6.2](http://mongooseim.readthedocs.org/en/1.6.2/)
* [1.6.1](http://mongooseim.readthedocs.org/en/1.6.1/)
* [1.6.0](http://mongooseim.readthedocs.org/en/1.6.0/)


 
**MongooseIM documentation highligts:**

When developing new features/modules, please make sure you add basic documentation to the 'doc/' directory, and add a link to your document in 'doc/README.md.'

* [Tutorials](http://mongooseim.readthedocs.io/en/latest/user-guide/How-to-build/). Learn how to:
    * [Build MongooseIM from source code](doc/user-guide/How-to-build.md)
    * [Set up MongoosePush](doc/user-guide/Push-notifications.md)
    * [Set up MongooseICE](doc/user-guide/ICE_tutorial.md)
    * [Build an iOS messaging app](doc/user-guide/iOS_tutorial.md)
* [User Guide](http://mongooseim.readthedocs.io/en/latest/user-guide/Get-to-know-MongooseIM/). Learn all about how to use MongooseIM in your project. Explore its features, supported XEPs, RFCs and database backends, as well as its architecture and deployment strategies.
* [Platform](http://mongooseim.readthedocs.io/en/latest/Roadmap/). Get to know our roadmap, MongooseIM history and see how we contribute to third-party opensource projects.
* [Configuration](http://mongooseim.readthedocs.io/en/latest/Basic-configuration/). Explore some of the available options including database backend configuration, access control lists, listener and extension modules.
* [REST API](http://mongooseim.readthedocs.io/en/latest/rest-api/Client-frontend/). Explore MongooseIM features using our REST API and [Swagger documentation](http://mongooseim.readthedocs.io/en/latest/swagger/index.html).
* [Operation and maintenance](http://mongooseim.readthedocs.io/en/latest/operation-and-maintenance/Cluster-management-considerations/). See what to consider when building, monitoring, testing and distributing MongooseIM clusters.
* [Server developer's guide](doc/developers-guide/Testing-MongooseIM.md). Get all the information you need to expand MongooseIM platform.
    

## Participate!

Suggestions, questions, thoughts? Contact us directly:

* Raise a [GitHub issue](https://github.com/esl/MongooseIM/issues): https://github.com/esl/MongooseIM/issues
* Email us at <a href='mailto:mongoose-im@erlang-solutions.com'>mongoose-im@erlang-solutions.com</a>
* Follow our [Twitter account](https://twitter.com/MongooseIM): [https://twitter.com/MongooseIM](https://twitter.com/MongooseIM)
* Like our [Facebook page](https://www.facebook.com/MongooseIM/): [https://www.facebook.com/MongooseIM/](https://www.facebook.com/MongooseIM/)
* Subscribe to our [mailing list](https://groups.google.com/d/forum/mongooseim-announce) at [https://groups.google.com/d/forum/mongooseim-announce](https://groups.google.com/d/forum/mongooseim-announce) to receive no more than two montly emails as well as access to the free and open archives.
