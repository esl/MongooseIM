-module(script_helper).

-export([start/2, write/2, read/1]).

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

-spec start(Cmd :: string(), Args :: [string() | binary()]) -> port().
start(Cmd, Args) ->
    RepoDir = os:getenv("PWD"),
    FullPath = filename:join(RepoDir, Cmd),
    erlang:open_port({spawn_executable, FullPath},
                     [exit_status, stream, use_stdio, binary,
                      {args, Args}, {env, [{"DEBUG", "1"}]}]).

write(Port, Data) ->
    DataLenBin = integer_to_binary(byte_size(Data)),
    ToSend = <<DataLenBin/binary, 10, Data/binary>>,
    port_command(Port, ToSend).

read(Port) ->
    read(Port, <<>>).

%% ----------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------

read(Port, Buffer) ->
    receive
        {Port, {data, Data}} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case binary:split(NewBuffer, <<"\n">>) of
                [_] ->
                    read(Port, NewBuffer);
                [LenBin, DataPart] ->
                    read(Port, DataPart, binary_to_integer(LenBin))
            end;
        {Port, {exit_status, ExitStatus}} ->
            erlang:error(#{ reason => script_has_terminated,
                            exit_status => ExitStatus,
                            data_received => Buffer })
    after 5000 ->
        erlang:error(#{ reason => timeout,
                        data_received => Buffer })
    end.

read(_Port, _Buffer, ErrorCode) when ErrorCode < 0 ->
    {error, ErrorCode};
read(_Port, Buffer, ExpectedLen) when byte_size(Buffer) == ExpectedLen ->
    Buffer;
read(Port, Buffer, ExpectedLen) ->
    receive
        {Port, {data, Data}} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            read(Port, NewBuffer, ExpectedLen);
        {Port, {exit_status, ExitStatus}} ->
            erlang:error(#{ reason => script_has_terminated,
                            exit_status => ExitStatus,
                            data_received => Buffer,
                            received_len => byte_size(Buffer),
                            expected_len => ExpectedLen })
    after 5000 ->
              erlang:error(#{ reason => timeout,
                              data_received => Buffer,
                              received_len => byte_size(Buffer),
                              expected_len => ExpectedLen })
    end.

