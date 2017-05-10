# Differentiators

MongooseIM provides:

* Massive scalability: for greater and faster growth, costs-effectiveness as well as resource utilisation
* Platform approach: designed with consistency, end-to-end battle testing across the whole ecosystem (all server and client components, and tools)
* Code quality: extensive refactoring, substantial optimisations, continuous integration and deployment
* Extensive testing: automated continuous functional code coverage, integration testing, end-to-end testing with real clients
* Continuous load testing
* Unique version: no proprietary extensions, fully open source, fully open standards
* Contributions to ([XMPP Standards Foundation](https://xmpp.org/)): implementations of XEPs, innovations contributed
* Professional support, and flexible customer service
* Contributions to third party open source codebases: strenghthening the ecosystem

## Initial differences from the parent project

This project began its life as a fork of ejabberd v.2.1.8 back in 2011, and later underwent major cleanup, refactoring and optimization.

Major steps performed at the time:

*   Bringing the project source tree to compliance with OTP project structure recommendations
*   Swapping `autotools` for the Erlang community-standard build tool `rebar`
*   Removal of obsolete and/or rarely used modules to reduce maintenance burden
*   Reduction of runtime memory consumption by refactoring the code
    to use Erlang's binary data type for string manipulation and storage
    instead of operating on linked lists of characters
*   Functional test coverage of the system according to corresponding
    RFCs and XEPs
