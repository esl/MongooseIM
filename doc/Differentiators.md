# Differentiators

MongooseIM provides:

* Massive scalability, for greater growth and costs-effectiveness
* Platform approach, designed from the start with consistency accross server and client components, and tools/ecosystem
* Code quality, through extensive refactoring, substantial optimisations, and continuous integration
* Unique version, fully open source, fully open standards, innovations contributed to the XSF
* Professional support, and flexible customer service

## Initial differences from the parent project

This project began its life as a fork of
[ejabberd v.2.1.8](https://github.com/processone/ejabberd) back in 2011, and later underwent some major cleanup, refactorization and optimization.

Major steps performed at that time:

*   bringing the project source tree to compliance with OTP project structure recommendations
*   swapping `autotools` for the Erlang community-standard build tool `rebar`
*   removal of obsolete and/or rarely used modules to reduce maintenance burden
*   reduction of runtime memory consumption by refactoring the code
    to use Erlang's binary data type for string manipulation and storage
    instead of operating on linked lists of characters
*   functional test coverage of the system according to corresponding
    RFCs and XEPs
