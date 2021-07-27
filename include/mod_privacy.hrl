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

-record(privacy, {
          us :: jid:simple_bare_jid(),
          default = none :: mod_privacy:list_name(),
          lists = [] :: [{mod_privacy:list_name(), mod_privacy:list_item()}]
         }).

-record(listitem, {
          type = none :: mod_privacy:privacy_item_type(),
          value = none,
          action :: allow | deny | block,
          order :: undefined | non_neg_integer(),
          match_all = false :: boolean(),
          match_iq = false :: boolean(),
          match_message = false :: boolean(),
          match_presence_in = false :: boolean(),
          match_presence_out = false :: boolean()
         }).

-record(userlist, {
          name = none :: mod_privacy:list_name(),
          list = [] :: [mod_privacy:list_item()],
          needdb = false :: boolean()
         }).
