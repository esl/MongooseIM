{application, pgsql,
 [{description, "PostgreSQL Library"},
  {vsn, "1.0"},
  {modules, [pgsql,
             pgsql_proto,
             pgsql_tcp,
             pgsql_util]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
