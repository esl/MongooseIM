-module(service_admin_extra_gdpr).

-include("ejabberd_commands.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-export([commands/0]).
-ignore_xref([commands/0]).

-spec commands() -> [ejabberd_commands:cmd()].
commands() -> [
        #ejabberd_commands{name = retrieve_personal_data, tags = [gdpr],
            desc = "Retrieve user's presonal data.",
            longdesc = "Retrieves all personal data from MongooseIM for a given user. Example:\n"
                       " mongooseimctl retrieve_personal_data alice localhost /home/mim/alice.smith.zip ",
            module = gdpr_api,
            function = retrieve_all,
            args = [{username, binary}, {domain, binary}, {path, binary}],
            result = {res, rescode}}
    ].
