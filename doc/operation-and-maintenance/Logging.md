# Configuring logging

The main configuration for logging is in the Application Config file.
You can find it in `mongooseim/etc/app.config` in the release directory.


# Primary log level

Primary log level sets maximum log level in the system.
This check is applied for any event in the system before
the event is passed to any handler.

Primary log level, that is used before MongooseIM config is loaded:

```erlang
[
    {kernel, [
         {logger_level, notice}
    ]}
].
```

Once MongooseIM config is loaded, `loglevel` option is used instead.


# Primary filters

Functions from the filters section are applied for any message once it passes
primary log level check.

Keep that block of configuration as it is,
unless you are planning to extend the filtering logic.

```erlang
[{kernel, [
  {logger, [
    %% Default filters applied to all events before passing them to handlers:
    {filters, log, [
           %% If we want to see complete accumulator in logs
        %  {preserve_acc_filter, {fun mongoose_log_filter:preserve_acc_filter/2, no_state}},
           {format_packet_filter, {fun mongoose_log_filter:format_packet_filter/2, no_state}},
           {format_acc_filter, {fun mongoose_log_filter:format_acc_filter/2, no_state}},
           {format_c2s_state_filter, {fun mongoose_log_filter:format_c2s_state_filter/2, no_state}},
           {format_stacktrace_filter, {fun mongoose_log_filter:format_stacktrace_filter/2, no_state}}
        ]},
....
}}].
```

`preserve_acc_filter` filter is disabled by default, but could be enabled,
if you are iterested in debugging accumulator logic (`mongoose_acc` module).


# Shell log handler

- Controls what MongooseIM prints to the standard output.
- [Erlang OTP docs for logger_std_h](https://erlang.org/doc/man/logger_std_h.html)

```erlang
    {handler, shell_log, logger_std_h, #{
         %% Default log level for handlers is to log everything, that
         %% passes primary log level and module log levels
         level => all,
         formatter => {mongoose_flatlog_formatter, #{
           map_depth => 3,
           term_depth => 50,
           template_name => mongoose_default_log_template
         }}
    }},
```


# File log handler

- Controls what and how MongooseIM prints into files.
- [Erlang OTP docs for logger_disk_log_h](https://erlang.org/doc/man/logger_disk_log_h.html)
- You can have several file handlers.
- File handlers should have different handler ids (i.e. `disk_log`, `disk_json_log`)
- There are two file log handlers defined by default: one that formats in JSON
  and one that formats in Logfmt format (`key=value` pairs).
- You can disable some log handlers, if you are not interested in them.
- Check information below about log formatters.

```erlang
    {handler, disk_log, logger_disk_log_h, #{
         level => all,
         config => #{
           file => "{{mongooseim_log_dir}}/mongooseim.log",
           type => wrap,
           max_no_files => 5,
           max_no_bytes => 2097152,
           sync_mode_qlen => 2000, % If sync_mode_qlen is set to the same value as drop_mode_qlen,
           drop_mode_qlen => 2000, % synchronous mode is disabled. That is, the handler always runs
           flush_qlen => 5000,     % in asynchronous mode, unless dropping or flushing is invoked.
           overload_kill_enable => true
           % Documentation about Overload protection, together with default values, can be found here:
           % http://erlang.org/doc/apps/kernel/logger_chapter.html#protecting-the-handler-from-overload
         },
         formatter => ...
    }},
```


# Logfmt file log handler

Wrapper around [https://github.com/ferd/flatlog](flatlog) library with
custom template options configured by default.

Options:

- `map_depth`
- `term_depth`

```erlang
         formatter => {mongoose_flatlog_formatter, #{
           map_depth => 3,
           term_depth => 50
         }}
```


# JSON file log handler

JSON formatted file. It could be used to store messages in ELK or in Splunk.
You can use [Filebeat](https://www.elastic.co/guide/en/beats/filebeat/current/filebeat-input-log.html#filebeat-input-log-config-json)
to send messages from the file into ELK.

Options:

- `format_depth`
- `format_chars_limit`
- `depth`

```erlang
         formatter => {mongoose_json_formatter, #{
           format_depth => 10,
           format_chars_limit => 3000,
           depth => 10
         }}
```


# Different log level for a specific module

Motivation:

- Sometimes we are interested in debug messages from a particular module.
- Useful to debug new or experimental modules.

This example:

- Changes log level for one particular module.
- Forwards the log messages to any enabled handler.

Changes:

- Enable module log level for `ejabberd_c2s`.

```erlang
    %% Module log level
    {module_level, debug, [ejabberd_c2s]},
```


# Separate log for module debugging
 
Motivation:

- Sometimes we are only interested in log messages from one particular module.
- Useful for debugging and development.
- Does not affect overload protection in other handlers.

This example:

- Forwards all logging from a module `ejabberd_c2s` to a separate file.
- Keeps the other handlers intact.

Changes:

- Modify any existing handler to explicitly set log level.
- Enable module log level for `ejabberd_c2s`.
- Add a new custom handler into `kernel.logger` options.

Issues:

- This would also disable module log level logic for other handlers.

```erlang
    %% Existing handlers
    {handler, shell_log, logger_std_h, #{
         level => notice, %% was level => all
         ...
    },
    {handler, disk_log, logger_disk_log_h, #{
          level => notice,
          ...
    },
    ...
    %% Module log level
    {module_level, debug, [ejabberd_c2s]},
    %% New handler
    {handler, disk_log_c2s, logger_disk_log_h, #{
         level => debug,
         config => #{
           %% Choose destination:
           file => "{{mongooseim_log_dir}}/ejabberd_c2s.log",
           %% Common options:
           type => wrap,
           max_no_files => 5,
           max_no_bytes => 2097152,
           sync_mode_qlen => 2000,
           drop_mode_qlen => 2000,
           flush_qlen => 5000,
           overload_kill_enable => true
         },
         formatter => {mongoose_flatlog_formatter, #{
           map_depth => 3,
           term_depth => 50,
           template_name => mongoose_default_log_template
         }},
         filters => [
           %% That filter matches messages from ejabberd_c2s module
           {module_filter, {fun mongoose_log_filter:filter_module/2, [ejabberd_c2s]}}
         ]
    }}
```

