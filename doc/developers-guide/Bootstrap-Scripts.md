# Bootstrap scripts

The scripts are located in the `rel/files/scripts/` directory in the MongooseIM repository.

By default the `bootstrap` command executes `bootstrap01-hello.sh`, which just prints the information below:

```
./_build/prod/rel/mongooseim/bin/mongooseimctl bootstrap

Execute /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim/scripts/bootstrap01-hello.sh
Hello from /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim/scripts/bootstrap01-hello.sh script.
MongooseIM is installed into /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim
```

Execution of scripts stops with an error, if any of scripts fail.

Environment variables, available from scripts:

- `ERTS_PATH` - path to Erlang Runtime System, used by MongooseIM.
- `MIM_DIR` - MongooseIM release installation directory.


## Templating bootstrap script

The script `bootstrap20-template.escript` renders files from the `templates/` directory and writes
result files into the `etc/` directory. If you need the result files in a separate directory,
create another script `bootstrap30-template.sh`, that moves files into a proper location.

The `etc/templates.ini` file contains default template variables.

A template config example:

```ini
[options]
  demo_session_lifetime = 600
  demo_tls_versions = 'tlsv1.2', 'tlsv1.3'
```

Only lowercase variables are allowed in `templates.ini`.

You can redeclare options using environment variables when executing the bootstrap script:

```bash
MIM_DEMO_SESSION_LIFETIME=700 mongooseimctl bootstrap
```

Environment variables should have a `MIM_` prefix. The variable names are case-insensitive
(but we suggest to use the uppercase variable names for consistency).

## Demo template

A demo template is located in `rel/files/templates/demo.config`.
It is copied into the `/templates` directory inside your release directory.

## Testing templating scripts

Templating script source code: `rel/files/scripts/bootstrap20-template.escript`.

Testing script code:

```bash
tools/pkg/scripts/smoke_test.sh
tools/pkg/scripts/smoke_templates.escript
```

Testing command:

```bash
PRESET=pkg pkg_PLATFORM=centos_7 ESL_ERLANG_PKG_VER=23.3.1-2 ./tools/test.sh
```
