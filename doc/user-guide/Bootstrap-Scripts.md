# Bootstrap scripts

The scripts are located in the `rel/files/scripts/` directory in the MongooseIM repository.

By default the `bootstrap` command executes `bootstrap01-hello.sh`, which just prints the information below:

```erlang
./_build/prod/rel/mongooseim/bin/mongooseimctl bootstrap

Execute /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim/scripts/bootstrap01-hello.sh
Hello from /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim/scripts/bootstrap01-hello.sh script.
MongooseIM is installed into /Users/mikhailuvarov/erlang/esl/MongooseIM/_build/prod/rel/mongooseim
```

Environment variables, available from scripts:

- `ERTS_PATH` - path to Erlang Runtime System, used by MongooseIM.
- `MIM_DIR` - MongooseIM release installation directory.
