## Installation 

### Getting started with MongooseIM on Mac OS X

This short guide will show you how to compile MongooseIM from source code on Mac OS X, and get users chatting right away.

#### Before you start

MongooseIM is supported on Mac OS X 10.6.8 and later. Before you can compile and run MongooseIM, you also need the following to be installed on your system: 

* Gnu Make and GCC (the GNU Compiler Collection). To ensure that these are installed, you can install the Command Line Tools for Xcode, available via Xcode or from the Apple Developer website.
* Git – `brew install git`
* Erlang /OTP R15B or higher – `brew install erlang`

An easy way to install some of the packages is by using a package manager, such as [Homebrew](http://brew.sh) – the Homebrew commands are provided here.

#### Installation

To build and install MongooseIM from source code, do the following:

1. Clone the Git repository:  `git clone https://github.com/esl/MongooseIM.git`
2. Go to your MongooseIM directory.
3. Run the following commands: `make` and then `make rel`.

#### Running MongooseIM

* From your MongooseIM directory, go to the release directory:  `cd rel/mongooseim`
* To start the MongooseIM server, run the following command:  `bin/mongooseim start`
* To verify that MongooseIM is running, enter the following:  `bin/mongooseim ping`<br \>If the response is `pong`, then MongooseIM is running.
* To connect to the MongooseIM console after starting the server:  `bin/mongooseim debug`
* Alternatively, you can also run the server in interactive mode:  `bin/mongooseim live`

#### Registering a user

The default XMPP domain served by MongooseIM right after installation is `localhost`. Users on a different computer can register using the server’s IP address.

##### Adium

1. Launch Adium. If the Adium Setup Assistant opens, close it.
2. In the **Adium** menu, select **Preferences**, and then select the **Accounts** tab.
3. Click the **+** button and select **XMPP (Jabber)**.
4. Enter a Jabber ID (for example, “user1@localhost”) and password, and then click **Register New Account**.
5. In the **Server** field, enter the following:
	* Users registering on the computer on which MongooseIM is running: `localhost`
	* Users registering from a different computer: the MongooseIM server’s IP address
6. Click **Request New Account**.

After registration, the user will connect automatically.

Registered users wishing to add an existing account to Adium should enter the MongooseIM server’s IP address in the **Connect Server** field on the **Options** tab.

##### Command line

You can register a user with the `mongooseimctl` utility:  
`mongooseimctl register user domain password`

For example:  
`mongooseimctl register user1 localhost GJ9TuHq8`

#### Domains

To use your system’s domain name instead of localhost, edit the following MongooseIM configuration file: `$REPO/rel/mongooseim/etc/ejabberd.cfg` (where `$REPO` is the repository root). Find the line that contains the following: `{hosts ["localhost"] }.` Replace localhost with your domain name, for example: `{hosts, ["example.org"] }.` Save the configuration file and restart the MongooseIM server. A user’s Jabber ID will then use the domain instead of localhost, for example: `user1@example.org`

You can also configure multiple domains for one server:  
`{hosts, ["example1.org", "example2.org"] }.`

#### Get chatting!

Users that are registered on your server can now add their accounts in a chat application like Adium (specifying either the server’s IP address or domain name), add each other as contacts, and start chatting.