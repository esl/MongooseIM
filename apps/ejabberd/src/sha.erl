%%%----------------------------------------------------------------------
%%% File    : sha.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 20 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(sha).
-export([sha1_hex/1]).
-include("ejabberd.hrl").
-define(SHA1_SIZE, 160).
-define(SHA1_STRLEN, 40).

-spec sha1_hex(binary()) -> binary().
sha1_hex(Text) ->
    hexstring(crypto:hash(sha, Text)).

-spec hexstring(binary()) -> binary().
hexstring(<<X:?SHA1_SIZE/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~40.16.0b", [X]))).
