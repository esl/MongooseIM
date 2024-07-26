-module(mongoose_traffic).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").

%% gen_mod API
-export([start/2, stop/1]).
-export([supported_features/0]).
%% hook handler
-export([trace_traffic/3]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% cowboy handler for serving main page
-export([init/2]).
-ignore_xref([init/2]).

-define(SERVER, ?MODULE).
-type state() :: [pid()].

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    gen_hook:add_handlers(hooks(HostType)),
    case maps:get(standalone, Opts, false) of
        true ->
            gen_server:start_link(?MODULE, [], []);
        false ->
        case whereis(?MODULE) of
            undefined ->
                Traffic = {mongoose_traffic,
                             {gen_server, start_link, [?MODULE, [], []]},
                             permanent, 1000, supervisor, [?MODULE]},
                % there has to be another layer
                % channel will set up its own traces, someone has to watch and distribute stanzas
                ejabberd_sup:start_child(Traffic);
            _ ->
                ok
        end
    end.

-spec stop(mongooseim:host_type()) -> ok.
stop(Host) ->
    gen_hook:delete_handlers(hooks(Host)),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

hooks(_HostType) ->
    [{c2s_debug, global, fun ?MODULE:trace_traffic/3, #{}, 50}].

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

-spec trace_traffic(mongoose_acc:t(), #{arg => mongoose_debug:debug_entry()}, term()) ->
    {ok, mongoose_acc:t()}.
trace_traffic(Acc, #{arg := {client_to_server, From, El}}, _) ->
    traffic(client_to_server, From, El),
    {ok, Acc};
trace_traffic(Acc, #{arg := {server_to_client, To, El}}, _) ->
    traffic(server_to_client, To, El),
    {ok, Acc}.

-spec traffic(mongoose_debug:direction(), jid:jid(), exml:element()) ->
    ok.
traffic(Dir, Jid, El) ->
    St = iolist_to_binary(fix_and_format(El)),
    UserSessionPid = self(),
    gen_server:cast(?MODULE, {message, Dir, UserSessionPid, Jid, St}),
    ok.

-spec init(term()) -> {ok, state()}.
init([]) ->
    register(?MODULE, self()),
    {ok, []}.

handle_call({register, Pid}, _From, State) ->
    monitor(process, Pid),
    {reply, ok, [Pid | State]};
handle_call({unregister, Pid}, _From, State) ->
    {reply, ok, lists:delete(Pid, State)};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({message, _, _, _, _} = Msg, State) ->
    lists:map(fun(Pid) -> Pid ! Msg end, State),
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
    {noreply, lists:delete(Pid, State)}.

-spec init(cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    {ok, Cwd} = file:get_cwd(),
    Base = Cwd ++ "/web/traffic",
    File = case cowboy_req:path_info(Req) of
               [] -> "session.html";
               P -> filename:join(P)
           end,
    Path = filename:join(Base, File),
    Size = filelib:file_size(Path),
    Req1 = cowboy_req:reply(200,
                            #{},
                            {sendfile, 0, Size, Path}, Req),
    {ok, Req1, State}.

fix_and_format(El) when is_binary(El) ->
    El;
fix_and_format({xmlstreamend, _}) ->
    <<"</stream:stream>">>;
fix_and_format({Tag, Name}) ->
    exml:to_pretty_iolist({Tag, Name, []});
fix_and_format({Tag, Name, Attrs}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs)});
fix_and_format({Tag, Name, Attrs, Children}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs), Children}).

fix_attrs(Attrs) ->
    lists:filter(fun is_defined/1, Attrs).

is_defined({_, undefined}) -> false;
is_defined({_, _}) -> true.
