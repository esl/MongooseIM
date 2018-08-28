# Moving to single application project structure

This document explains how MongooseIM codebase was migrated from the umbrella application structure
to a single application `rebar3` project.

## Moving official MongooseIM to the new structure

### Move all apps/ejabberd subdirectories to the repo root

- mv `apps/ejabberd/src/* src/`
- mv `apps/ejabberd/include/* include`
- mv `apps/ejabberd/priv/* priv`
- mv `apps/ejabberd/test/* test`
- mv `apps/ejabberd/c_src/* c_src`
- mv `apps/ejabberd/asn1/* asn1`

When committing make sure that no new files were added (i.e. they are marked as `renamed`).

### Move tools files

#### cover.spec

`mv apps/ejabberd/src/cover.spec .`

#### rebar.config.script

1. from `apps/ejabberd/src` copy the `MaybeFIPSSupport/1` function
2. paste it into `rebar.config.script`
3. call it on a result of calling `SetupIncludedApps/2` in `rebar.config.script`

#### rebar.config

1. In `rebar.config` remove `{i, "apps"}` line from `erl_opts` tuple
2. Move `{d, xml_nif}` and `{parse_tranform, lager_transform}` lines to `erl_opts` tuple in `rebar.config`
3. From `apps/ejabberd/rebar.config` move all three `erl_first_files`, `xref_checks` and `port_specs` tuples
   to `rebar.config`
4. Add `{cover_print_enabled, true}` line below `{conver_enabled, ...` line in `rebar.config`
5. In `rebar.config`, remove the whole `[{override, ejabberd...` structure from `overrides` tuple
6. In `rebar.config`, `pre` tuple, add `{compile, {pc, compile}}` tuple (after `{compile, {asn..`)
7. In `rebar.config`, `post` tuple, add `{clean, {pc, clean}}` tuple (after `{clean, {asn..`)

#### Delete apps/

`rm -rf apps/`

### Changes in source files

#### Application resource files

1. In `src/ejabberd.app.src`
  * Change application name to `mongooseim`
  * Change `vsn` to `{cmd, "tools/generate_vsn.sh"}`
2. Delete `src/mongooseim.app.src.disabled`
3. `mv src/ejabberd.app.src src/mongooseim.app.src`

### Includes

This is the big one. Since we don't use the umbrella structure anymore, all `-include(..)`
directives pointing to `ejabberd/include` need to be rewritten.

(SED is `sed` on Linuxes or `gsed` on Macs)

* `gsed -i 's/-include_lib("ejabberd\/include\//-include("/' -- src/*.erl src/*.hrl include/*.hrl test/*.erl`
* `gsed -i 's/-include_lib("ejabberd\/src\//-include("/' -- src/*.erl src/*.hrl include/*.hrl test/*.erl`

### Release scripts

In `rel/files/mongooseim` the variable `EJABBERD_SO_PATH` contains an `ejabberd-2.1.8*` fragment.
Replace it with `mongooseim-*`.

In `rel/files/mongooseimctl` the variable `EJABBERD_EBIN_PATH` contains an `ejabberd-*` fragment.
Replace it with `mongooseim-*`.

### Tests & tools

After previous changes, lots of tests fail, mostly because we changed the application name from
`ejabberd` to `mongooseim`. Functions to look for are:
- `application:set_env`
- `application:get_env`
- `application:get_key`
- `application:load`
- `application:start`
- `application:stop`
- `application:ensure_all_started`
- `code:priv_dir`
- `application:which_applications`

Those function are also called via RPC from big test suites. A good method to find them all is to
search using `application.*ej` regex.

Files which require changes:
- `include/ejabberd.hrl`
- `src/ejabberd.erl`
- `src/ejabberd_app.erl`
- `src/ejabberd_config.erl`
- `src/mod_version.erl`
- `src/translate.erl`
- `test/ejabberd_config_SUITE.erl`
- `test/ejabberd_helper.erl`
- `test/ejabberd_listener_SUITE.erl`
- `test/zlib_driver_SUITE.erl`
- `big_tests/README.md`
- `big_tests/run_common_test.erl`
- `big_tests/tests/conf_reload_SUITE.erl`
- `big_tests/tests/connect_SUITE.erl`
- `big_tests/tests/reload_helper.erl`
- `tools/extract_translations/extract_translations.erl`
- `tools/xep_tool/xep_tool.escript`
- `src/mongoose_cluster.erl` (2 calls to `when_app_stopped/1` function)

We also need to remove remaining occurences of `apps/ejabberd` directory structure. Some of the
files are scripts assembling the path programatically - the trick is to search for the `apps/` keyword.

Affected files:
- `CONTRIBUTING.md`
- `Makefile`
- `elvis.config`
- `src/amp_strategy.erl`
- `src/mod_mam_rdbms_arch.erl`
- `big_tests/Makefile`
- `big_tests/run_common_test.erl`
- `big_tests/tests/vcard_SUITE.erl`
- `tools/cd_tools/provision_mysql.sh`
- `tools/cd_tools/provision_pgsql.sh`
- `tools/fill_roster.erl`
- `tools/privacy.erl`
- `tools/travis-setup-db.sh`
- `tools/travis-test.sh`
- `tools/travis-upload-to-s3.sh`
- `tools/zabbix_add_counters.py`

## Moving a MongooseIM's fork to a new structure

This section describes how to move a customized fork of MongooseIM to a new structure.

1. Move all directories from the `apps/ejabberd` directory to the repository root, e.g. `apps/ejabberd/src/`
   should be moved to just `src/`. Be sure to move all the important files, because `apps/` directory
   will be wiped out completely.

2. Make all the changes described in the `Move tools files` section.  If you've made any changes to
   `apps/ejabberd/rebar.config` or `apps/ejabberd/rebar.config/script`, be sure to merge these
   with `rebar.config` and `rebar.config.script` in the repository root. Remove `apps/` directory.

3. Make all the changes described in the `Application resource files` section.

4. Make all the changes described in the `Includes` section. Make sure that none of the include directives
   in your source code (in `src/` and `test/`) has the following structure `-include_lib("ejabberd/include/<header_file>")`.
   They should now refer to bare header file names, i.e. `-include(<header_file>)`.

5. Make all the changes described in the `Release scripts` section. Make sure that all customizations
   made to release scripts and release process are aware of the new directory structure (i.e. none
   of them assumes `apps/ejabberd` path exists).

6. Make all the changes described in the `Tests & tools` section. Since `ejabberd` application was
   renamed to `mongooseim`, make sure that all the code which was starting or stopping `ejabberd`,
   now does the same for `mongooseim`. You might want to check the following function calls:
   - most of the functions from the `application` module
   - `code:priv_dir/1`
   - calls to `rpc` module which would call functions listed above


