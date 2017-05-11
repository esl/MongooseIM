### Module Description
A logging submodule for [mod_muc](mod_muc.md). 
Is must be explicitly configured to work. 
It writes room-related information (configuration) and events (messages, presences) to files on the disk.

### Options

* `outdir` (string, default: `"www/muc"`): Filesystem directory where the files are stored.
* `access_log` (atom, default: `muc_admin`): ACL that defines who can enable/disable logging for specific rooms.
* `dirtype` (atom, default: `subdirs`): Specifies the log directory structure.
    * `subdirs`: Module will use the following directory structure `[Logs root]/[dirname]/YYYY/MM/` with file names being `DD.[extension]`.
    * `plain`: Module will use the following directory structure `[Logs root]/[dirname]/` with file names being `YYYY-MM-DD.[extension]`.
* `dirname` (atom, default: `room_jid`): Specifies directory name created for each room.
    * `room_jid`: Uses the room bare JID.
    * `room_name`: Uses the room name from its configuration.
* `file_format` (atom, default: `html`):
    * `html`: The output is a fancy-formatted HTML page.
    * `plaintext`: Just a text file, better suited for processing than HTML.
* `css_file` (binary or atom, default: `false`):
    * `false`: Uses default styles for HTML logs.
    * `<<"path to custom CSS file">>`: Links custom CSS inside HTML logs. Please note it won't be copied to the logs directory but the given path will be linked in HTML files instead.
* `timezone` (atom, default: `local`):
    * `local`: Uses the local server timezone in dates written into the logs.
    * `universal`: Uses GMT in dates written into the logs.
* `top_link` (default: `{"/", "Home"}): Allows setting a custom link at the top of the HTML log file. 
 First tuple element is the link target and the second one is the text to be displayed. 
 You can put any HTML instead of just plain text.
* `spam_prevention` (boolean, default: `true`): When enabled, MongooseIM will enforce `rel="nofollow"` attribute in links sent by user and written to MUC logs.


### Example Configuration

```
  {mod_muc_log,
        [
        {outdir, "/tmp/muclogs"},
        {access_log, muc}
        ]},
```
