%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(http_helper).

%% API
-export([listen_once/2, listen_once/3, listen_once/4]).

%% @doc Same as listen_once(PPid, Port, undefined, "OK")
listen_once(PPid, Port) ->
  listen_once(PPid, Port, undefined).

%% @doc Same as listen_once(PPid, Port, ReqPattern, "OK")
listen_once(PPid, Port, ReqPattern) ->
  listen_once(PPid, Port, ReqPattern, "OK").

%% @doc Starts an http listener which waits for one request; when it receives
%% expected request it sends a message {ok, got_http_request, Packet} to
%% the "owner" process and terminates
%% PPid - process waiting for notification
%% Port - self-explanatory
%% ReqPattern - either a binary or a list of binaries; incoming request must match all of them
%%   to be accepted
%% Response - what is to be sent back to the http client when a proper request comes, after 200 header
%%   default is "OK"; by passing 'none' as Response you can create a "broken" listener which accepts
%%   connections but does not respond
listen_once(PPid, Port, ReqPattern, Response) ->
  spawn(fun() -> onetime_http_server(PPid, Port, ReqPattern, Response) end).

onetime_http_server(PPid, Port, ReqPattern, Response) ->
  {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0},
    {active, false}, {reuseaddr, true}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  do_recv(Sock, <<>>, PPid, ReqPattern, Response),
  gen_tcp:close(Sock).

do_recv(Sock, _, _, _, none) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, _} ->
      do_recv(Sock, none, none, none, none);
    {error, closed} ->
      ok
  end;
do_recv(Sock, Bs, PPid, ReqPattern, Response) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      Packet = <<Bs/binary, B/binary>>,
      case check_packet(Packet, ReqPattern) of
        true ->
          %% this is the one we are waiting for, notify owner and terminate
          Resp = make_response(Response),
          gen_tcp:send(Sock, Resp),
          PPid ! {ok, got_http_request, Packet},
          ok;
        false ->
          %% not the one, send a response to keep server happy and keep working
          Resp = make_response(Response),
          gen_tcp:send(Sock, Resp),
          do_recv(Sock, <<>>, PPid, ReqPattern, Response);
        invalid ->
          %% not a complete packet, keep collecting data
          do_recv(Sock, Packet, PPid, ReqPattern, Response)
      end;
    {error, closed} ->
      ok
  end.

make_response(Response) when is_binary(Response) ->
  make_response(binary_to_list(Response));
make_response(Response) ->
  Len = length(Response),
  Dt = httpd_util:rfc1123_date(erlang:universaltime()),
  ["HTTP/1.1 200 OK\r\nDate: ", Dt,
    "\r\nContent-Length: ", integer_to_list(Len), "\r\nContent-Type: text/html\r\nServer: MongooseTest\r\n\r\n",
    Response].

check_packet(Packet, ReqPattern) ->
  case erlang:decode_packet(http, Packet, []) of
    {ok, {http_request, 'POST', _, _}, Body} ->
      check_body(Body, ReqPattern);
    _ ->
      invalid
  end.

check_body(_, undefined) ->
  true;
check_body(Body, Pattern) when is_list(Pattern) ->
  R = lists:map(fun(P) -> check_body(Body, P) end, Pattern),
  lists:foldl(fun(A,B) -> A and B end, true, R);
check_body(Body, Pattern) when is_binary(Pattern) ->
  case binary:match(Body, Pattern) of
    nomatch -> false;
    {_, _} -> true
  end;
check_body(_, _) ->
  false.
