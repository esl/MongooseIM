-module(mongoose_doctor).

-export([format_stanza/1,
         c2s_io_stanza/1,
         c2s_io/0,
         is_c2s_io/1]).

-include("ejabberd_c2s.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("erlang_doctor/include/tr.hrl").

format_stanza(#{ts := Ts,
                pid := Pid,
                user := User,
                dir := Dir,
                stanza := Stanza}) ->
    io_lib:format("~s ~p ~s ~p~n~s~n", [Ts, Pid, User, Dir, exml:to_pretty_iolist(Stanza)]).

c2s_io_stanza(T = #tr{pid = Pid}) ->
    #state{user = User} = c2s_state(T),
    #{ts => tr:ts(T),
      pid => Pid,
      user => User,
      dir => c2s_dir(T),
      stanza => c2s_stanza(T)}.

c2s_state(#tr{data = [_, State]}) -> State;
c2s_state(#tr{data = [_, _, State]}) -> State.

c2s_stanza(#tr{data = [{xmlstreamelement, El}, _]}) -> El;
c2s_stanza(#tr{data = [_, El, _]}) -> El.

c2s_dir(#tr{data = [{xmlstreamelement, _El}, _]}) -> in;
c2s_dir(#tr{data = [_, _El, _]}) -> out.

c2s_io() -> tr:filter(fun is_c2s_io/1).

is_c2s_io(#tr{event = call,
              mfa = {ejabberd_c2s, _StateFun, _},
              data = [{xmlstreamelement, _El}, _]}) -> true;
is_c2s_io(#tr{event = call,
              mfa = {ejabberd_c2s, send_element, _}}) -> true;
is_c2s_io(_) -> false.
