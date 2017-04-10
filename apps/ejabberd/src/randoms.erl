%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Random generation utils
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-module(randoms).
-author('alexey@process-one.net').

-export([get_string/0]).
-export([good_seed/0]).

-spec get_string() -> string().
get_string() ->
    R = crypto:rand_uniform(0, 16#10000000000000000),
    integer_to_list(R, 16).

-spec good_seed() -> {integer(), integer(), integer()}.
good_seed() ->
    % Good seed, according to random:seed/3 documentation
    {erlang:phash2([node()]), p1_time_compat:monotonic_time(), p1_time_compat:unique_integer()}.

