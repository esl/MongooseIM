%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-define(LDAP_PORT, 389).

-define(LDAPS_PORT, 636).

-type scope() :: baseObject | singleLevel | wholeSubtree.

-record(eldap_search,
        {scope = wholeSubtree,
         base = <<"">>,
         filter,
         limit = 0,
         attributes = [],
         types_only = false,
         deref_aliases = neverDerefAliases,
         timeout = 0}).

-record(eldap_search_result, {entries = []   :: [eldap:eldap_entry()],
                              referrals = [] :: iodata()}).

-record(eldap_entry, {object_name = <<>> :: iodata(),
                      attributes = []    :: [{iodata(), [iodata()]}]}).
