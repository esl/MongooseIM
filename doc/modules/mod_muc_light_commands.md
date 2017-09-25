# MongooseIM's multi-user chat light commands set

## Purpose
This is a set of commands, providing actions related to multi-user chat light features.
It is currently used to expose the part of REST API responsible for managing rooms.

## Configuration
This module contains command definitions which are loaded when the module is activated.
There are no options to be provided, therefore the following entry in the config file is sufficient:

```
{mod_muc_light_commands, []}
```

## Commands
This file consists of [commands definitions](mod_commands/).
Following commands (along with functions necessary for them to run) are defined:
+ `create_muc_light_room`  
Creates a MUC Light room.  
    Args:  
        - `domain` (binary)  
        - `name`  (binary)  
        - `owner` (binary)  
        - `subject` (binary)  
+ `create_identifiable_muc_light_room`  
Updates a MUC Light room.  
    Args:  
        - `domain` (binary)  
        - `id` (binary)  
        - `name` (binary)  
        - `owner` (binary)  
        - `subject` (binary)  
+ `invite_to_room`  
    Invites to a MUC Light room.  
    Args:  
        - `domain` (binary)  
        - `name` (binary)  
        - `sender` (binary)  
        - `recipient` (binary)  
+ `send_message_to_muc_light_room`  
Sends a message to a MUC Light room.  
    Args:  
        - `domain` (binary)  
        - `name` (binary)  
        - `from` (binary)  
        - `body` (binary)  

## Running commands
Commands must be [registered and then run](mod_commands/) using the module `mongoose_commands`.
