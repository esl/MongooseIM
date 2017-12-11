# MongooseIM's multi-user chat commands set

## Purpose
This is a set of commands, providing actions related to multi-user chat features.

## Configuration
This module contains command definitions which are loaded when the module is activated.
There are no options to be provided, therefore the following entry in the config file is sufficient:

```
{mod_muc_commands, []}
```

## Commands
This file consists of [commands definitions](mod_commands.md).
Following commands (along with functions necessary for them to run) are defined:
+ `create_muc_room`  
Creates a MUC room.  
    Args:  
        - `host` (binary)  
        - `name`  (binary) - room name  
        - `owner` (binary) - the XMPP entity that would normally request an instant MUC room  
        - `nick` (binary)  
+ `kick_user_from_room`  
Kick a user from a MUC room (on behalf of a moderator).  
    Args:  
        - `host` (binary)  
        - `name` (binary)  
        - `nick` (binary)   
+ `invite_to_muc_room`  
Sends a MUC room invite (direct) from one user to another.  
    Args:  
        - `host` (binary)  
        - `name` (binary)  
        - `sender` (binary)  
        - `recipient` (binary)  
        - `reason` (binary)  
+ `send_message_to_room`  
Sends a message to a MUC room from a given room.  
    Args:  
        - `host` (binary)  
        - `name` (binary)  
        - `from` (binary)  
        - `body` (binary)  

## Running commands
Commands must be [registered and then run](mod_commands.md) using the module `mongoose_commands`.
