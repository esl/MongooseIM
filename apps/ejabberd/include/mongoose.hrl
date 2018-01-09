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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%% This macro returns a string of the ejabberd version running, e.g. "2.3.4"
%% If the ejabberd application description isn't loaded, returns atom: undefined
-define(VERSION, element(2, application:get_key(ejabberd,vsn))).

-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME,  hd(ejabberd_config:get_global_option(hosts))).
-define(MYLANG,  ejabberd_config:get_global_option(language)).

-define(CONFIG_PATH, "etc/ejabberd.cfg").

-define(MONGOOSE_URI, <<"https://www.erlang-solutions.com/products/mongooseim.html">>).

%%-define(DBGFSM, true).

%% ---------------------------------
%% Logging mechanism
-include("mongoose_logger.hrl").

-record(external_component, {domain, handler, node}).

-define(DEPRECATED,
    ok).
%%    lager:error("Deprecated call", [])).
%% Not to break things, we often change a function arity or pattern but keep the old one
%% so that parts of the code not yet rewritten still work. Eventually all those things
%% will go away. This macro denotes a function called in a deprecated way.

-define(TEMPORARY, ok).
%% just a marker - oftentimes we create a mongoose_acc just because we call a hook
%% while the 'real' accumulator doesn't yet reach this point, so for compatibility
%% we have to mock it. This macro is to mark such places in the code.
