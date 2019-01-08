## Advanced release configuration

It's now possible to install MongooseIM from source in two modes:

* `system` - it's used internally to generate Linux packages (.deb, .rpm)
* `user`  - which is the default mode and used for testing on travis and
    in development

You can also build OS specific packages by using the tools in `[MongooseIM repo root]/tools/pkg` - refer to `README.md` therein.

### Configure script

The `tools/configure` script can be used to specify which 3rd party
dependencies should be included in the final release or to set the installation
prefix and installation mode. More details can found in the tool's help.
The help is printed when the script is run without any parameters
`tools/configure`:

```
configure: OPTIONS

Specifies which 3rd party deps will be included in the release.
Writes configure.out file as output - this file can be sourced with:

    . configure.out

Writes rel/configure.vars.config which can be used as Reltool input.

3rd party apps:

    with-none           include no 3rd party drivers
    with-all            include all drivers
    with-mysql          include mysql driver
    with-odbc           include an ODBC driver (requires unixodbc to compile)
    with-pgsql          include pgsql driver
    with-redis          include redis driver
    with-riak           include riak driver

Options:

    prefix    Installation PREFIX directory. Default: /usr/local
    system    Install files into $PREFIX/{bin, etc, ...} instead of a completely self contained release. Default: no
    user      System user to run the server as. Default:
```

This script is also accessible via the make `configure` target.

### Example

If `mysql` and `redis` are the only drivers that should be included in the
release, run the following command before `make rel`:

    $ ./tools/configure with-mysql with-redis

You only need to run the `./tools/configure` command once (unless changing the release's config is needed to 
include some other dependencies).

### System install

To manually test the installation run `tools/test-install.sh`.
This script is intended for careful inspection by a human user, not for automation.
Results should be similar to those described below.

On Mac:

```sh
./tools/configure with-all user=erszcz prefix=/tmp/mim-sandbox-system system=yes
cat configure.out rel/configure.vars.config
RUNNER_GROUP=staff make install
```

Overriding `RUNNER_GROUP` on a Mac is necessary, as users by default don't
have private groups of the same name as their usernames.

Generated build configs:

```sh
$ cat configure.out rel/configure.vars.config
export MONGOOSEIM_CONFIGURED="yes"
export APPS="mysql eodbc epgsql eredis riakc nksip cqerl tirerl erlcloud"
export PREFIX="/tmp/mim-sandbox-system"
export RELTOOL_VARS="rel/configure.vars.config"
export SYSTEM="yes"
export RUNNER_USER="erszcz"
export BIN_DIR="$PREFIX/usr/bin"
export ETC_DIR="$PREFIX/etc/mongooseim"
export LIB_DIR="$PREFIX/usr/lib/mongooseim"
export LOG_DIR="$PREFIX/var/log/mongooseim"
export MDB_DIR="$PREFIX/var/lib/mongooseim"
export LOCK_DIR="$PREFIX/var/lock/mongooseim"
{mongooseim_runner_user, "erszcz"}.
{mongooseim_script_dir, "/tmp/mim-sandbox-system/usr/lib/mongooseim/bin"}.
{mongooseim_etc_dir, "/tmp/mim-sandbox-system/etc/mongooseim"}.
{mongooseim_log_dir, "/tmp/mim-sandbox-system/var/log/mongooseim"}.
{mongooseim_mdb_dir, "/tmp/mim-sandbox-system/var/lib/mongooseim"}.
{mongooseim_mdb_dir_toggle, []}.
{mongooseim_lock_dir, "/tmp/mim-sandbox-system/var/lock/mongooseim"}.
```

Installed tree:

```
$ tree mim-sandbox-system/ -L 3
mim-sandbox-system/
├── etc
│   └── mongooseim
│       ├── app.config
│       ├── mongooseim.cfg
│       └── vm.args
├── usr
│   ├── bin
│   │   └── mongooseimctl
│   └── lib
│       └── mongooseim
└── var
    ├── lib
    │   └── mongooseim
    ├── lock
    │   └── mongooseim
    └── log
        └── mongooseim

13 directories, 4 files
```

Files which change after starting and stopping such an installation:

```
var/lib/mongooseim/DECISION_TAB.LOG
var/lib/mongooseim/LATEST.LOG
var/lib/mongooseim/last_activity.DCD
var/lib/mongooseim/muc_registered.DCD
var/lib/mongooseim/muc_room.DCD
var/lib/mongooseim/offline_msg.DAT
var/lib/mongooseim/passwd.DCD
var/lib/mongooseim/privacy.DCD
var/lib/mongooseim/private_storage.DAT
var/lib/mongooseim/roster.DCD
var/lib/mongooseim/roster_version.DCD
var/lib/mongooseim/schema.DAT
var/lib/mongooseim/vcard.DAT
var/lib/mongooseim/vcard_search.DCD
var/log/mongooseim/crash.log
var/log/mongooseim/ejabberd.log
var/log/mongooseim/erlang.log.1
var/log/mongooseim/run_erl.log
```


### Caveats

* Running `make install` will blindly overwrite any configs it encounters on its way.
  Mnesia database and log files are preserved only due to the fact that they're
  not build process artifacts.
