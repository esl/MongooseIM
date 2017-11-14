## Installation

In this short guide we will set MongooseIM up and get your users chatting right away.
You can either compile everything from the source code or install binaries from a package.

### Install from source code

Check out our tutorial [How to build MongooseIM from source code](How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

### Install from DMG

Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) page of Erlang Solution website, and download the version of MongooseIM you want.
Once the DMG is downloaded, double click it and the contents of the package will open.
Double click the .pkg file and follow the instructions of the installation wizard.


## Running MongooseIM

```bash
$ cd _build/prod/rel/mongooseim/bin # assuming you are in the MongooseIM directory
$ ./mongooseim start # start the MongooseIM server
$ ./mongooseim ping  # if the response is `pong`, the server is running
$ ./mongooseim debug # connect to the console of the live MongooseIM node
```

Alternatively, you can also run the server in the interactive mode: `./mongooseim live`


## Registering a user

The default XMPP domain served by MongooseIM right after installation is `localhost`.
Users on a different computer can register using the server’s IP address.

You can register a user with the `mongooseimctl` utility.
The following command registers the user `user@domain` using password `password`.

```
mongooseimctl register user domain password
```

## Connecting with an XMPP client

### Adium

1. Launch Adium. If the Adium Setup Assistant opens, close it.
2. In the **Adium** menu, select **Preferences**, and then select the **Accounts** tab.
3. Click the **+** button and select **XMPP (Jabber)**.
4. Enter a Jabber ID (for example, “user1@localhost”) and password, and then click **Register New Account**.
5. In the **Server** field, enter the following:
	* users registering on the computer on which MongooseIM is running: `localhost`,
	* users registering from a different computer: the MongooseIM server’s IP address.
6. Click **Request New Account**.

After registration, the user will connect automatically.

Registered users wishing to add an existing account to Adium should enter the MongooseIM server’s IP address in the **Connect Server** field on the **Options** tab.


## Domains

To use your system’s domain name instead of localhost, edit the MongooseIM configuration file: `MongooseIM/_build/prod/rel/mongooseim/etc/ejabberd.cfg`.
Find and replace the line:

```erlang
{hosts ["localhost"] }.
```

using your own hostname, for example:

```erlang
{hosts ["example.org"] }.
```

Save the configuration file and restart the MongooseIM server.
A user's Jabber ID will then contain the new domain instead of localhost, for example: `user1@example.org`.
Note that existing user accounts will not be automatically migrated to the new domain.

You can also configure multiple domains for one server:

```erlang
{hosts, ["example1.org", "example2.org"] }.
```


## Get chatting!

Users that are registered on your server can now add their accounts in a chat application like Adium (specifying either the server’s IP address or domain name), add each other as contacts, and start chatting!
