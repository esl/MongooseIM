In this short guide we will set MongooseIM up and get users chatting right away.
The goal is to get to know MongooseIM, set it up, go through basic operations and validation.

Warning: This setup is not intended for production.

Note: This procedure has been tested on an Ubuntu 18.04.x LTS.

## Installation

We recommend, you install mongooseIM binaries from a package Erlang Solutions delivers.

Alternatively, check out our tutorial on [How to build MongooseIM from source code](How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

### Download a package

Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) section of the Erlang Solutions website, and choose the version of MongooseIM you want. The following sections describe the installation process for different operating systems.

### Ubuntu and Debian

Once the deb file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

```bash
sudo dpkg -i mongooseim_[version here].deb
```

### CentOS

An ODBC (RDBMS) driver must be installed on your machine to unpack and install from RPM packages. Enter the following command in a terminal window to install the latest unixODBC driver:
```bash
sudo yum install unixODBC
```
Once the RPM file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:
```bash
sudo rpm -i mongooseim_[version here].rpm
```

## Running MongooseIM

Warning: MongooseIM will use its default database - Mnesia, which is faster and simpler to setup, but not intended for production purposes when it comes to persistent data.

Note: It is possible at anytime to use external databases. For more information see the end of this guide.

The following command will start the MongooseIM server:
```bash
mongooseimctl start
```

When you change the config file and want to restart the MongooseIM server:
```bash
mongooseimctl restart
```

Use the following command to stop the MongooseIM server:
```bash
mongooseimctl stop
```
This takes a few seconds.

At any given time, the following command shows the status of a MongooseIM server:
```bash
mongooseimctl status
```
If the command replies `nodedown` then MongooseIM is not running. Else it will show its status `starting`, `started`, or `stopping`, and its version.

When needed, you can also launch the server in the interactive mode:
```bash
mongooseimctl live
```
This will allow you to better detect and understand the errors in the configuration.
When MongooseIM is properly running, the Erlang shell/console is then shown.
Just type Control-C twice to exit, the server will then be shut down.

For running MongooseIM in a non-interactive way within a supervision system (e.g. systemd), it is recommended to use the foreground mode:
```bash
mongooseimctl foreground
```
Typing Control-C will stop the server.

You can check server loglevel:
```bash
mongooseimctl get_loglevel
```

## Chat users

### Registering (creating) users

The default XMPP domain served by MongooseIM right after installation is `localhost`.

You can register (create) users with the `mongooseimctl` utility.

This command registers the user `user@domain` using password `password`.
```bash
mongooseimctl register_identified user domain password
```
Examples:
```bash
mongooseimctl register_identified alice localhost qwerty
mongooseimctl register_identified bob localhost 12345678
mongooseimctl register_identified carol localhost abc123
mongooseimctl register_identified dan localhost dan
```
Warning: The password is entered manually in the command line and history is accessible to the command line users. This method is not recommended for production use, you may prefer for example LDAP.

You can check that it has correctly been created:
```bash
mongooseimctl check_account user host
```
Example:
```bash
mongooseimctl check_account alice localhost
mongooseimctl check_account bob localhost
mongooseimctl check_account carol localhost
mongooseimctl check_account dan localhost
```

Now you can list all registered users in your host:
```bash
mongooseimctl registered_users host
```
Example:
```bash
mongooseimctl registered_users localhost
```

If you want delete users in your host:
```bash
mongooseimctl  unregister user host
```
Example:
```bash
mongooseimctl  unregister dan localhost
```

### Populate the contact lists (rosters)

Fo a given user (`localuser` and `localserver`), add a contact (`user` and `server`):
```bash
mongooseimctl add_rosteritem localuser localserver user server nick group subs 
```
Examples:
```bash
mongooseimctl add_rosteritem alice localhost bob localhost bob friends both
mongooseimctl add_rosteritem bob localhost alice localhost alice friends both
```

Note: The `subs`parameter is the "subscription" to a user's presence. Possible values are: `none`, `from`, `to`, or `both`. A subscription in `both` direction means each user will receive each other's presence.

Verify the contact list:
```bash
mongooseimctl get_roster user host 
```
Examples:
```bash
mongooseimctl get_roster alice localhost
mongooseimctl get_roster bob localhost
mongooseimctl get_roster carol localhost
```

## Basic MongooseIM configuration

You can edit the `mongooseim.cfg` file:
```bash
/etc/mongooseim/mongooseim.cfg 
```

Warning: We recommend you do not touch the advanced settings at this stage.

For each change, edit the configuration file using the right Linux/Unix user.
Save (and optionally backup, archive, or version) the configuration file and restart the MongooseIM server.

### Logging

Change your own loglevel in the configuration file:
```erlang
% {loglevel, 3}.
{loglevel, 4}.
```

Save and exit your editor, restart MongooseIM and check your loglevel from the command line:
```bash
mongooseimctl get_loglevel
```

Read the `ejabberd.log` file:
```bash
/var/log/mongooseim/ejabberd.log
```

You can use commands such `cat`, `more` or `less`, even `head` or `tail`.
In order to see live logs:
```bash
tail -f /var/log/mongooseim/ejabberd.log
```
Type `Ctrl+C` to exit.

### MUC (Multi-User Chat) for groupchats

Enable MUC, or Multi-User Chat, for groupchats/channels in the `mongooseim.cfg` file:
```erlang
{mod_muc, [{host, "muc.@HOST@"},
           {access, muc},
           {access_create, muc_create}
          ]},
```

Verify with:
```bash
mongooseimctl print_flat_config | grep muc
{[h,<<"localhost">>,module,mod_muc_commands],'FLAT'}.
{[h,<<"localhost">>,module,mod_muc_light_commands],'FLAT'}.
{[h,<<"localhost">>,module,mod_muc],'FLAT'}.
{[h,<<"localhost">>,module_opt,mod_muc,host],"muc.@HOST@"}.
{[h,<<"localhost">>,module_opt,mod_muc,access],muc}.
{[h,<<"localhost">>,module_opt,mod_muc,access_create],muc_create}.
```

### Roster versioning

For faster contact list downloads at each client/app (re)connection, edit the configuration file:
```erlang
{mod_roster, [
              {versioning, true},
              {store_current_id, true}
             ]},
```
Verify with:
```bash
mongooseimctl print_flat_config | grep roster
 [{service_admin_extra,[{submods,[node,accounts,sessions,vcard,gdpr,roster,
{[h,<<"localhost">>,module,mod_roster],'FLAT'}.
{[h,<<"localhost">>,module_opt,mod_roster,versioning],true}.
{[h,<<"localhost">>,module_opt,mod_roster,store_current_id],true}.
```
### Review configuration

Read and double-check your configuration in a different way, after starting MongooseIM:
```bash
mongooseimctl print_flat_config
```

If MongooseIM does not start because the configuration file is broken in some way:
```bash
mongooseimctl live
```


## Using an XMPP/Jabber client/app

The following steps use the registered users on the MongooseIM server, done above.

Users that are registered on your server can now add their accounts in a chat application like Gajim (specifying either the server’s IP address or domain name), and start chatting!

#### Note about session conflicts

If you're going to connect several clients with the same username and domain (for example a phone and a laptop), please make sure they are using different resource names (a kind of device/client identifier).
This should be configurable in the account settings of every XMPP client.

Otherwise, the clients will keep disconnecting each other, because MongooseIM always terminates the older session in case of a conflict.

### Connect Gajim

Gajim is available on Ubuntu, CentOS & Windows.

Warning: Gajim has an obsolete UX. However, it is still well maintained, and has a console that is extremely useful for debugging and testing/validation purposes at the XMPP protocol level.

1. Launch Gajim. Ignore the window with Plugin updates.
2. Go to Edit -> Accounts.
3. Click **Add** in the left part of the window and select **I already have an account I want to use**, click **Forward**
4. Enter the **user**, **domain** and **password** for the accounts registered previously on the command line
5. Click **Forward** and then **Finish**
6. Ignore the TLS/SSL error/warning and continue
5. Close the Account window.

Add your three created users: `alice`, `bob`, and `carol`.

Check what users are currently connected:
```bash
mongooseimctl connected_users_info
```

### Chat with another person

Use `alice`'s account to send messages directly to `bob` and use `bob`'s account to reply directly to `alice`.

From the MongooseIM command line:
```bash
mongooseimctl send_message_chat from to body
```
Examples:
```bash
mongooseimctl send_message_chat carol@localhost alice@localhost hello
mongooseimctl send_message_chat carol@localhost bob@localhost hi
```

### Group chats

Use `alice`'s account to create a groupchat `channel` on your `muc.localhost` service, and configure it by making it persistent. Invite `bob` and `carol`. From `bob`'s' and `carol`'s accounts, accept the invitation and join the `channel` groupchat. All three users exchange messages.

### Contact lists

Use `carol`'s account to add `alice` and `bob` to her contact list. Use `alice`'s and `bob`'s accounts accept those additions.

Verify on the MongooseIM server:
```
mongooseimctl get_roster user host
```
Examples:
```
mongooseimctl get_roster alice localhost
mongooseimctl get_roster bob localhost
mongooseimctl get_roster carol localhost
```

### Profile (vCard)

Edit `alice`'s profile (vCard) in Gajim: **Modify Account...**, then **Profile**, just set her **Name** to `Alice`.

Verify on the MongooseIM server:
```bash
mongooseimctl get_vcard alice localhost FN
```

## Summary

Now you have the minimum knowledge: you know how to deploy MongooseIM, configure some basic features, check/verify a few useful items, validate it both on the client and server side, and utilize a few good practices.

### Summary: command line

You know `mongooseimctl`, with commands such as:

* `start`, `restart`, `stop`, `status`, `live`, `foreground`
* `get_loglevel`
* `register_identified`, `check_account`, `registered_users`, `unregister`
* `add_rosteritem`, `get_roster`
* `print_flat_config`

You can even run `mongooseimctl` without arguments for a list of available commands.

### Summary: files

You know basic entries in files:
`/etc/mongooseim/mongooseim.cfg`
`/var/log/mongooseim/ejabberd.log`

### Summary: client/app

In an app, you know how to:

* connect
* chat with another user
* create/join groupchats
* manage contact lists (roster)
* edit profile (vCard)

## Go further

For the next steps, we now encourage you to:

1. Deploy it as a single node, on a publicly accessible server, with a real routable domain name with its certificate
1. Add an RDBMS for persistent data, and LDAP for user directory
1. Enable message history with MAM (Message Archive Management)
1. Enable file exchange with HTTP file upload, with an S3-compatible object storage server
1. Use a mobile app for users to chat
