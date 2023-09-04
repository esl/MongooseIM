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

%% This macro returns a string of the ejabberd version running, e.g. "2.3.4"
%% If the ejabberd application description isn't loaded, returns atom: undefined
-define(MONGOOSE_VERSION, element(2, application:get_key(mongooseim,vsn))).

-define(MYHOSTS, mongoose_config:get_opt(hosts)).
-define(ALL_HOST_TYPES, mongoose_config:get_opt(hosts) ++ mongoose_config:get_opt(host_types)).
-define(MYNAME, mongoose_config:get_opt(default_server_domain)).
-define(MYLANG, mongoose_config:get_opt(language)).
-define(XMPP_VERSION, <<"1.0">>).

-define(CONFIG_PATH, "etc/mongooseim.toml").

-define(MONGOOSE_URI, <<"https://www.erlang-solutions.com/products/mongooseim.html">>).

%% ---------------------------------
%% Logging mechanism
-include("mongoose_logger.hrl").

-ifdef(PROD_NODE).
-define(ASSERT_MODULE(M), ok).
-else.
-define(ASSERT_MODULE(M), M:module_info(module)).
-endif.
