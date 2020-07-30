%%% @doc Parsing of the 'cfg' file to Erlang terms
-module(mongoose_config_terms).

-export([get_plain_terms_file/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

%% @doc Read an ejabberd configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
-spec get_plain_terms_file(string()) -> [term()].
get_plain_terms_file(File1) ->
    File = mongoose_config_utils:get_absolute_path(File1),
    case file:consult(File) of
        {ok, Terms} ->
            include_config_files(Terms);
        {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = describe_config_problem(File, Reason, LineNumber),
            ?ERROR_MSG(ExitText, []),
            mongoose_config_utils:exit_or_halt(ExitText);
        {error, Reason} ->
            ExitText = describe_config_problem(File, Reason),
            ?ERROR_MSG(ExitText, []),
            mongoose_config_utils:exit_or_halt(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for 'include_config_file'

%% @doc Include additional configuration files in the list of terms.
-spec include_config_files([term()]) -> [term()].
include_config_files(Terms) ->
    Filenames = config_filenames_to_include(Terms),
    Configs = lists:map(fun(Filename) ->
            {Filename, get_plain_terms_file(Filename)}
        end, Filenames),
    include_config_files(Terms, Configs).

config_filenames_to_include([{include_config_file, Filename} | Terms]) ->
    [Filename|config_filenames_to_include(Terms)];
config_filenames_to_include([{include_config_file, Filename, _Options} | Terms]) ->
    [Filename|config_filenames_to_include(Terms)];
config_filenames_to_include([_Other | Terms]) ->
    config_filenames_to_include(Terms);
config_filenames_to_include([]) ->
    [].

include_config_files(Terms, Configs) ->
    include_config_files(Terms, Configs, []).

include_config_files([], _Configs, Res) ->
    Res;
include_config_files([{include_config_file, Filename} | Terms], Configs, Res) ->
    include_config_files([{include_config_file, Filename, []} | Terms],
                         Configs, Res);
include_config_files([{include_config_file, Filename, Options} | Terms],
                     Configs, Res) ->
    IncludedTerms = find_plain_terms_for_file(Filename, Configs),
    Disallow = proplists:get_value(disallow, Options, []),
    IncludedTerms2 = delete_disallowed(Disallow, IncludedTerms),
    AllowOnly = proplists:get_value(allow_only, Options, all),
    IncludedTerms3 = keep_only_allowed(AllowOnly, IncludedTerms2),
    include_config_files(Terms, Configs, Res ++ IncludedTerms3);
include_config_files([Term | Terms], Configs, Res) ->
    include_config_files(Terms, Configs, Res ++ [Term]).

find_plain_terms_for_file(Filename, Configs) ->
    case lists:keyfind(Filename, 1, Configs) of
        false ->
            %% Terms were not provided by caller for this file
            erlang:error({config_not_found, Filename});
        {Filename, Terms} ->
            Terms
    end.

%% @doc Filter from the list of terms the disallowed.
%% Returns a sublist of Terms without the ones which first element is
%% included in Disallowed.
-spec delete_disallowed(Disallowed :: [atom()],
                        Terms :: [term()]) -> [term()].
delete_disallowed(Disallowed, Terms) ->
    lists:foldl(
      fun(Dis, Ldis) ->
          delete_disallowed2(Dis, Ldis)
      end,
      Terms,
      Disallowed).

delete_disallowed2(Disallowed, [H | T]) ->
    case element(1, H) of
        Disallowed ->
            ?WARNING_MSG("event=ignore_disallowed_option option=~p", [Disallowed]),
            delete_disallowed2(Disallowed, T);
        _ ->
            [H | delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].

%% @doc Keep from the list only the allowed terms.
%% Returns a sublist of Terms with only the ones which first element is
%% included in Allowed.
-spec keep_only_allowed(Allowed :: [atom()],
                        Terms :: [term()]) -> [term()].
keep_only_allowed(all, Terms) ->
    Terms;
keep_only_allowed(Allowed, Terms) ->
    {As, NAs} = lists:partition(
                  fun(Term) ->
                      lists:member(element(1, Term), Allowed)
                  end,
                  Terms),
    [?WARNING_MSG("event=ignore_disallowed_option option=~p", [NA])
     || NA <- NAs],
    As.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

-type config_problem() :: atom() | {integer(), atom() | tuple(), _}. % spec me better

-spec describe_config_problem(Filename :: string(),
                              Reason :: config_problem()) -> string().
describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading MongooseIM config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.


-spec describe_config_problem(Filename :: string(),
                              Reason :: config_problem(),
                              Line :: pos_integer()) -> string().
describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
                          ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = mongoose_config_utils:get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
               " relevant to the error: ~n~s", [Lines]),
    ExitText.
