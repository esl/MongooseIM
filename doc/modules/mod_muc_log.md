## Module Description
A logging submodule for [mod_muc](mod_muc.md). 
Is must be explicitly configured to work. 
It writes room-related information (configuration) and events (messages, presences) to files on the disk.

## Options

### `modules.mod_muc_log.outdir`
* **Syntax:** string
* **Default:** `"www/muc"`
* **Example:** `outdir = "www/muc"`

Filesystem directory where the files are stored.

### `modules.mod_muc_log.access_log`
* **Syntax:** non-empty string
* **Default:** `"muc_admin"`
* **Example:** `access_log = "muc_admin"`

ACL that defines who can enable/disable logging for specific rooms.

### `modules.mod_muc_log.dirtype`
* **Syntax:** string, one of `"subdirs"`, `"plain"`
* **Default:** `"subdirs"`
* **Example:** `dirtype = "subdirs"`

Specifies the log directory structure:

* `"subdirs"`: Module will use the following directory structure `[Logs root]/[dirname]/YYYY/MM/` with file names being `DD.[extension]`.
* `"plain"`: Module will use the following directory structure `[Logs root]/[dirname]/` with file names being `YYYY-MM-DD.[extension]`.

### `modules.mod_muc_log.dirname`
* **Syntax:** string, one of `"room_jid"`, `"room_name"`
* **Default:** `"room_jid"`
* **Example:** `dirname = "room_jid"`

Specifies directory name created for each room:

* `"room_jid"`: Uses the room bare JID.
* `"room_name"`: Uses the room name from its configuration.


### `modules.mod_muc_log.file_format`
* **Syntax:** string, one of `"html"`, `"plaintext"`
* **Default:** `"html"`
* **Example:** `file_format = "html"`

Specifies the format of output files:

* `"html"`: The output is a fancy-formatted HTML page.
* `"plaintext"`: Just a text file, better suited for processing than HTML.

### `modules.mod_muc_log.css_file`
* **Syntax:** non-empty string
* **Default:** not set - the default styles for HTML logs are used
* **Example:** `css_file = "path/to/css/file"`

Specifies the css file used for logs rendering.
Please note it won't be copied to the logs directory but the given path will be linked in HTML files instead.

### `modules.mod_muc_log.timezone`
* **Syntax:** string, one of `"local"`, `"universal"`
* **Default:** `"local"`
* **Example:** `timezone = "universal"`

Specifies the timezone to be used in timestamps written into the logs:

* `local`: Uses the local server timezone.
* `universal`: Uses GMT.

### `modules.mod_muc_log.top_link`
* **Syntax:** TOML table with the following mandatory keys: `"target"`, `"text"` and string values.
* **Default:** `{target = "/", text = "Home"}`
* **Example:** `top_link = {target = "/top", text = "Top page"}`

Allows setting a custom link at the top of the HTML log file. 
First tuple element is the link target and the second one is the text to be displayed. 
You can put any HTML instead of just plain text.

### `modules.mod_muc_log.spam_prevention`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `spam_prevention = false`

When enabled, MongooseIM will enforce `rel="nofollow"` attribute in links sent by user and written to MUC logs.

## Example Configuration

```toml
[modules.mod_muc_log]
  outdir = "/tmp/muclogs"
  access_log = "muc"
  dirtype = "plain"
  dirname = "room_name"
  file_format = "html"
  css_file = "path/to/css/file"
  timezone = "universal"
  top_link.target = "/"
  top_link.text = "Home"
```
