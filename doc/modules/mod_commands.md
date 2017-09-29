# MongooseIM's command set

## Purpose

This is a basic set of administration and client commands.
Our goal is to provide a consistent, easy to use API for MongooseIM.
Both backend and client commands provide enough information to allow auto-generating access methods.
We currently use it in our admin and client REST API interface.
In the future it may replace the current `mongooseimctl` implementation.


## Configuration

This module contains command definitions loaded when the module is activated.
There are no more configuration parameters, so the following entry in the config file is sufficient:

```
{mod_commands, []]},
```

## Command definition

The module contains a list of command definitions.
Each definition contains the following entries:

* name (uniquely identifies the command)
* category (used for listing commands and for generating URLs for REST API)
* subcategory (optional)
* desc (a brief description)
* module, function (what is called when the command is executed)
* action (create|read|update|delete)
* optional: security_policy (info to be used by the caller)
* args (a list of two-element tuples specifying name and type of an argument)
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
and also calling. To call the above method you should do:
```
mongoose_commands:execute(admin, list_contacts) % if you want superuser privileges
or
mongoose_commands:execute(<<"alice@wonderland.lit">>, list_contacts)
```

and it will return a list of JIDs. REST API would expose this command as
```
http://localhost/api/contacts % use GET, since it is 'read'
```
and return a JSON list of strings. Since this is a user command, REST would expose it on the "client"
interface and require authorisation headers.

