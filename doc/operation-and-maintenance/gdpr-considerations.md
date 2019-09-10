# GDPR considerations

This page describes what GDPR implies in terms of server management.

## Data affected by GDPR commands

* inbox - All entries in the subject's inbox. If their messages are stored in other users' inbox, they will not be removed.
* message archive - Same as above for 1-1 messages. In case of group chat messages, they are retrieved as personal data but not removed.
* offline storage - All messages stored for delivery.
* roster - All entries in the subject's roster. Other users' rosters are NOT affected, even if they include the subject's JID or other data.
* vCard - The entire content of the subject's vCard.
* private XML storage - All items stored by the subject will be removed.
* publish-subscribe
    * retrieval: all subject's subscriptions and nodes (with their payloads included). 
    * removal: subject's subscriptions, push and PEP nodes (with their data included). 

## GDPR CLI commands

All CLI commands are accessible via `mongooseimctl` command, located in `bin/` inside MIM release.

Personal data retrieval requires `service_admin_extra` with `gdpr` group enabled.

### Creating a GDPR-safe user account

`mongooseimctl register <domain> <password>`

This command will create an anonymised JID with a random username part.
It ensures that no personal information will be leaked via logs or database entries, which include the user's JID.

#### Example

```
$ mongooseimctl register localhost abc123
User 1567-420657-155810-C1CEC31F5C993258@localhost successfully registered
```

### Retrieval of Personal Data

`mongooseimctl retrieve_personal_data <username> <domain> <filepath for the output as a zip>`

It retrieves personal data accessible to the server (see "Technical limitations" section below).
The directory where the zip file will be created must already exist.

After the execution is complete, a zip file will appear in the specified folder with personal information in CSV files grouped by type.

#### Example

`mongooseimctl retrieve_personal_data 1567-420657-155810-C1CEC31F5C993258 localhost /home/mongooseim/gdpr/1567-420657-155810-C1CEC31F5C993258.zip`

### Removal of Personal Data

`mongooseimctl unregister <username> <domain>`

It removes the user's account along with all associated personal data accessible to the server (see "Technical limitations" section below).

#### Example

`mongooseimctl unregister 1567-420657-155810-C1CEC31F5C993258 localhost`

## Technical limitations of GDPR retrieval and removal

Both GDPR retrieval and removal will process the data available via configured extensions and database(s).
If a part of personal information is managed by an extension that is e.g. temporarily disabled, it won't be retrieved/deleted.

If any MIM extension you had enabled on production is now disabled or you've switched one of them (or e.g. auth module) to another database, it is possible that some personal data will not be retrieved or removed as expected.
In such case, please consider starting a separate MIM instance that is configured to access all places, where personal data may be stored.
You may also extract the missing pieces of information on your own, however we won't cover the details of this method in this guide.

Please also visit [Known issues page](known-issues.md) to learn about a `mod_mam_muc` issue that may manifest in some environments.

