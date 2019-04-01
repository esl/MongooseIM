-module(service_admin_extra_gdpr).

-include("ejabberd_commands.hrl").

-export([commands/0, retrieve_all/1]).

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() -> [
    #ejabberd_commands{name = retrieve, tags = [gdpr],
                desc = "Retrieve user's presonal data.",
                longdesc = "Retrieves all personal data from MongooseIM for a given user. Example:\n"
                " %TODO ", % TODO add example
                module = ?MODULE,
                function = retrieve_all,
                args = [{username, binary}], % TODO add arguments if needed
                result = {records, binary}}  % TODO check if returned type is correct and convinient in use

].

retrieve_all(_Username) ->
    % TODO Implement logics for retrieve data here
    [].
