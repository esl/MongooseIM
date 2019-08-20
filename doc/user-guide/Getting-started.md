In this short guide we will set MongooseIM up and get your users chatting right away.
This will guide also you through basic operation, and validation.
Training and getting to know MongooseIM is the goal here.
This setup is not intended for production.

## Installation

We recommand, you install mongooseIM binaries from a package Erlang Solutions delivers.

Alternatively, check out our tutorial [How to build MongooseIM from source code](How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

### Download a package

Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) section of the Erlang Solution website, and choose the version of MongooseIM you want. The following sections describe the installation process for different operating systems.

### Ubuntu and Debian

Once the deb file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

```bash
$ sudo dpkg -i mongooseim_[version here].deb
```

### CentOS

An ODBC (RDBMS) driver must be installed on your machine to unpack and install from rpm packages. Enter the following command in a terminal window to install the latest unixODBC driver:
```bash
$ sudo yum install unixODBC
```
Once the rpm file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:
```bash
$ sudo rpm -i mongooseim_[version here].rpm
```

## Running MongooseIM

MongooseIM will use its default database Mnesia, which is faster and simpler tu setup, but not intended for production purposes.

Please note that it is possible at anytime to use external databases, check at the end of this guide.

The following command will start the MongooseIM server:
```bash
$ mongooseimctl start
```

Use the following command to stop the MongooseIM server:
```bash
$ mongooseimctl stop
```
This takes a few seconds.

At any given time, the following command shows the status of a MongooseIM server:
```bash
$ mongooseimctl status
```
If the command replies `nodedown` then MongooseIM is not running.
If MongooseIM is properly running, it will show its version.

Alternatively, you can also launch the server in the interactive mode:
```bash
$ mongooseimctl live
```
This will allow you to better detect and understand the errors in the configuration.
When MongooseIM is properly running, the Erlang shell/console is then shown.
Just type twice Control-C to exit, the server will then be shut down.

For running MongooseIM in a non-interactive way within a supervision system (e.g. systemd) it is
recommended to use the foreground mode:
```bash
$ mongooseimctl foreground
```
Typing Control-C will stop the server.

## Chat users

### Registering (creating) a user

The default XMPP domain served by MongooseIM right after installation is `localhost`.

You can register a user with the `mongooseimctl` utility.

This command registers the user `user@domain` using password `password`.
```
mongooseimctl register_identified user domain password
```
Example:
```
mongooseimctl register_identified alice localhost qwerty
mongooseimctl register_identified bob localhost 12345678
mongooseimctl register_identified carol localhost abc123
```

You can check that it has correctly been created:
```
mongooseimctl check_account user host
```
Example:
```
mongooseimctl check_account alice localhost
mongooseimctl check_account bob localhost
mongooseimctl check_account carol localhost
```

Now you can list all registered users in your host:
```
mongooseimctl registered_users host
```
Example:
```
mongooseimctl registered_users localhost
```

### Populate their contact lists (rosters)

Fo a given user (`localuser` and `localserver`), add a contact (`user` and `server`):
```
mongooseimctl add_rosteritem localuser localserver user server nick group subs 
```
Example:
```
mongooseimctl add_rosteritem alice localhost bob localhost bob friends both
mongooseimctl add_rosteritem bob localhost alice localhost alice friends both
```

Verify the contact list:
```
mongooseimctl get_roster user host 
```
Example:
```
mongooseimctl get_roster alice localhost
mongooseimctl get_roster bob localhost
```

## Basic MongooseIM configuration

You can edit the `mongooseim.cfg` file:
```
/etc/mongooseim/mongooseim.cfg 
```

We recommand you do not touch the advanced settings at this stage.
For each change, save (and optionally backup) the configuration file and restart the MongooseIM server.

### Logging

Change your own loglevel:
```erlang
% {loglevel, 3}.
{loglevel, 4}.
```

Restart and check your loglevel from the command line:
```
mongooseimctl get_loglevel
```

Read the `ejabberd.log` file:
```
/var/log/mongooseim/ejabberd.log
```

You can use commands such `cat`, `more` or `less`, even `head` or `tail`.
In order to see live logs:
```
tail -f /var/log/mongooseim/ejabberd.log
```

### MUC (Multi-User Chat) for groupchats

Enable MUC (Multi-User Chat) for groupchats in the `mongooseim.cfg` file:
```erlang
{mod_muc, [{host, "muc.@HOST@"},
           {access, muc},
           {access_create, muc_create}
          ]},
```

### Roster versionning

For faster contact list downloads at each (re)connection:
```erlang
{mod_roster, [
              {versioning, true},
              {store_current_id, true}
             ]}
```

## Using an XMPP client/app

The following steps uses the registered users on the MongooseIM server, done above.

Users that are registered on your server can now add their accounts in a chat application like Gajim (specifying either the server’s IP address or domain name), and start chatting!

### Connect Gajim

Gajim is available on Ubuntu, CentOS & Windows.

Warning: Gajim has largely obsolete UX
Gajim is still maintained, and has console that is extremely useful for debugging and testing/validation purposes.

1. Launch Gajim. Ignore the window with Plugin updates.
2. Go to Edit -> Accounts.
3. Click Add in the left part of the window and select **I already have an account I want to use**, click Forward
4. Enter the user, domain and password for the already registered account, click Forward and then Finish.
5. Close the Account window.

Repeat for `alice`, `bob`, and `carol`.

### Chat with another person

Use `alice`'s account to send messages directly to `bob` and `carol.
Use `bob`'s account to send messages directly to `alice` and `carol`.
Use `carol`'s account to send messages directly to `alice` and `bob`.

TODO

### Group chats

Use `alice`'s account to create a groupchat on your `muc.localhost` service, and invite`bob` and `carol.

TODO

### Contact lists

Use `carol`'s account to add `alice` and `bob` to her contact list.

Use `alice`'s and `bob`'s accounts accept those additions.

Verify on the MongooseIM server:
```
mongooseimctl get_roster carol localhost
```

TODO

### Profile (vCard)

Use `alice`'s account to edit her user's profile (vCard).

Verify on the MongooseIM server:
```
mongooseimctl get_vcard carol host name
```

TODO

## Verify on MongooseIM side

Check what users are currently connected:
```
mongooseimctl connected_users_info
```

Send a direct chat message from a user to another:
```
mongooseimctl send_message_chat from to body
```
Example:
```
mongooseimctl send_message_chat alice bob hello
```
Check in your app/client that the message has well been received.

```
mongooseimctl stats
```

TODO
