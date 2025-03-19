-module(mongoose_transport).
-author('piotr.nosek@erlang-solutions.com').

-include_lib("public_key/include/public_key.hrl").

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type peer() :: {inet:ip_address(), inet:port_number()}.
-type peername_return() :: {ok, peer()} | {error, inet:posix()}.
-type peercert_return() :: no_peer_cert | {ok, #'Certificate'{}}.

-export_type([peer/0, peername_return/0, peercert_return/0]).
