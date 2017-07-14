# MongooseIM's command set

## Purpose

This is a basic set of administration and client commands, which provide sort of an API for MongooseIM.
The goal is to provide a consistent, easy to use API, providing enough information to allow for auto-generation of access methods.
It is currently used to provide REST API, both admin and client interface, and in the future may replace current implementation of mongooseimctl.


## Configuration

This module contains command definitions which are loaded when the module is activated.
There are no more configuration parameters, so the following entry in config file is sufficient:

```
{mod_commands, []]},
```

## Command definition

The module contains a list of command definitions.
Each definitions contain the following entries:

* name (uniquely identifies the command)
* category (used for listing commands and for generating urls for REST API)
* optional: subcategory
* desc (a brief description)
* module, function (what is called when the command is executed)
* action (create|read|update|delete)
* optional: security_policy (info to be used by the caller)
* args (a list of two-element tuples specifying name and type of argument)
* result (what the command (and its underlying function) is supposed to return)

A simple command definition may look like this:

```
[
    {name, list_contacts},
    {category, <<"contacts">>},
    {desc, <<"Get roster">>},
    {module, ?MODULE},
    {function, list_contacts},
    {action, read},
    {security_policy, [user]},
    {args, [{caller, binary}]},
    {result, []}
]
```

## Command registration and interface

Command registry is managed by `mongoose_commands` module.
To register a command simply call:

```
mongoose_commands:register(list_of_command_definitions)
```

The registry provides functions for listing commands, retrieving their signatures,
and also calling. To call the above method you'd do:
```
mongoose_commands:execute(admin, list_contacts) % if you want superuser privileges
or
mongoose_commands:execute(<<"alice@wonderland.lit">>, list_contacts)
```

and it will return a list of jids. REST API would expose this command as
```
http://localhost/api/contacts % use GET, since it is 'read'
```
and return a json list of strings. Since this is a user command, REST would expose it on the "client"
interface and require authorisation headers.
