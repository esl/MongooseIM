%%%----------------------------------------------------------------------
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-record(roster, {usj :: {jid:simple_bare_jid(), mod_roster:contact()},
                 us :: jid:simple_bare_jid(),
                 jid :: mod_roster:contact(),
                 name = <<>> :: binary(),
                 subscription = none :: mod_roster:subscription_state(),
                 ask = none :: subscribe | unsubscribe | in | out | both | none,
                 groups = [] :: [binary()],
                 askmessage = <<>>,
                 xs = [] :: [exml:element()]}).

-record(roster_version, {us,
                        version}).
