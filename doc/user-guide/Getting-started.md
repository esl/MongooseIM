## Installation

### Getting started with MongooseIM on Mac

In this short guide we will set MongooseIM up and get your users chatting right away.
You can either compile everything from the source code or install binaries from a DMG image on Mac.

#### Install from source code

##### Prerequisites for source code installation

MongooseIM is supported on Mac OS X / macOS 10.6.8 and later.
Before you can compile and run MongooseIM, you will need to install the following packages:

* C and C++ compiler
* Git
* Erlang/OTP 18.3 or higher
* OpenSSL

We recommend using [Homebrew](http://brew.sh) to manage packages on your Mac.
With Homebrew installed, getting all dependencies is a matter of running the following commands:

```bash
$ xcode-select --install # install compilation tools
$ brew install git erlang openssl
```


##### Build and install from source code

To build and install MongooseIM from the source code, do the following:

```bash
$ git clone https://github.com/esl/MongooseIM.git
$ cd MongooseIM
$ export LDFLAGS="-L/usr/local/opt/openssl/lib -undefined dynamic_lookup $LDFLAGS"
$ export CXXFLAGS="-I/usr/local/opt/openssl/include $CXXFLAGS"
$ make rel
```

For more advanced release generation and installation please see [Release/Installation configuration](release_config.md)


#### Install from DMG

Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) page of Erlang Solution website, and download the version of MongooseIM you want.
Once the DMG is downloaded, double click it and the contents of the package will open.
Double click the .pkg file and follow the instructions of the installation wizard.


#### Running MongooseIM

```bash
$ cd _build/prod/rel/mongooseim/bin # assuming you are in the MongooseIM directory
$ ./mongooseim start # start the MongooseIM server
$ ./mongooseim ping  # if the response is `pong`, the server is running
$ ./mongooseim debug # connect to the console of the live MongooseIM node
```

Alternatively, you can also run the server in the interactive mode: `./mongooseim live`


#### Registering a user

The default XMPP domain served by MongooseIM right after installation is `localhost`.
Users on a different computer can register using the server’s IP address.

You can register a user with the `mongooseimctl` utility.
The following command registers the user `user@domain` using password `password`.

```
mongooseimctl register user domain password
```

##### Adium

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


#### Domains

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


#### Get chatting!

Users that are registered on your server can now add their accounts in a chat application like Adium (specifying either the server’s IP address or domain name), add each other as contacts, and start chatting!
