## Installation

In this short guide we will set MongooseIM up and get your users chatting right away.
You can either compile everything from the source code or install binaries from a package.

### Install from source code

Check out our tutorial [How to build MongooseIM from source code](How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

### Install from package

Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) section of the Erlang Solution website, and choose the version of MongooseIM you want. The following sections describe the installation process for different operating systems.

#### Mac

Once the DMG is downloaded, double click it and the contents of the package will open.
Double click the .pkg file and follow the instructions of the installation wizard.

#### Ubuntu

Once the deb file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

```bash
$ sudo dpkg -i mongooseim_[version here].deb
```

#### CentOS

An ODBC driver must be installed on your machine to unpack and install from rpm packages. Enter the following command in a terminal window to install the latest unixODBC driver:
```bash
$ sudo yum install unixODBC
```
Once the rpm file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:
```bash
$ sudo rpm -i mongooseim_[version here].rpm
```

## Running MongooseIM installed from package

The following command will start the MongooseIM server:
```bash
$ mongooseimctl start
```
The following command shows the status of a started MongooseIM server:
```bash
$ mongooseimctl status
```
Use the following command to stop the MongooseIM server:
```bash
$ mongooseimctl stop
```

Alternatively, you can also run the server in the interactive mode:
```bash
$ mongooseimctl live
```

For running MongooseIM in a non-interactive way within a supervision system (e.g. systemd) it is
recommended to use the foreground mode:
```bash
$ mongooseimctl foreground
```

## Registering a user

The default XMPP domain served by MongooseIM right after installation is `localhost`.
Users on a different computer can register using the server’s IP address.

You can register a user with the `mongooseimctl` utility.
The following command registers the user `user@domain` using password `password`.

```
mongooseimctl register user domain password
```

## Connecting with an XMPP client

### Adium (Mac)

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

### Pidgin (Ubuntu & CentOS)

1. Launch Pidgin.
2. Click the Add button to configure your account.
3. In the **Basic** tab choose **XMPP** as protocol, then enter Username, Domain and Password and click **Add**. After Registration the user will connect automatically.

### Gajim (Ubuntu & CentOS)

The following steps assumes that you have already registered a user on the MongooseIM server, see section **Registering a user** above.
1. Launch Gajim. Ignore the window with Plugin updates.
2. Go to Edit -> Accounts.
3. Click Add in the left part of the window and select **I already have an account I want to use**, click Forward
4. Enter the user, domain and password for the already registered account, click Forward and then Finish.
5. Close the Account window.

## Domains

To use your system’s domain name instead of localhost, edit the MongooseIM configuration file: `MongooseIM/_build/prod/rel/mongooseim/etc/mongooseim.cfg`.
Find and replace the line:

```erlang
{hosts, ["localhost"] }.
```

using your own hostname, for example:

```erlang
{hosts, ["example.org"] }.
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
