# MongooseIM's multi-user chat light commands set

## Purpose
This is a set of commands, providing actions related to multi-user chat light features.
These commands are used by REST API modules.

## Configuration
This module contains command definitions which are loaded when the module is activated.
There are no options to be provided, therefore the following entry in the config file is sufficient:

```
{mod_muc_light_commands, []}
```

## Commands
This file consists of [commands definitions](mod_commands.md).
Following commands (along with functions necessary for them to run) are defined:
+ `create_muc_light_room`  
Create a MUC Light room with unique username part in JID.
    Args:  
        - `domain` (binary)  
        - `name`  (binary)  
        - `owner` (binary)  
        - `subject` (binary)  
+ `create_identifiable_muc_light_room`  
Creates a MUC Light room with user-provided username part in JID.
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
Commands must be [registered and then run](mod_commands.md) using the module `mongoose_commands`.
