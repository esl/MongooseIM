%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(jid).

-export([make/3]).
-export([make/1]).
-export([make_noprep/3]).
-export([make_noprep/1]).
-export([are_equal/2]).
-export([are_bare_equal/2]).
-export([from_binary/1]).
-export([to_binary/1]).
-export([is_nodename/1]).
-export([nodeprep/1]).
-export([nameprep/1]).
-export([resourceprep/1]).
-export([to_lower/1]).
-export([to_lus/1]).
-export([to_bare/1]).
-export([replace_resource/2]).
-export([binary_to_bare/1]).

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl"). % only used to define stream types
-include("jlib.hrl").

-define(SANE_LIMIT, 1024).

-spec make(User :: ejabberd:user(), Server :: ejabberd:server(),
           Resource :: ejabberd:resource()) ->
    ejabberd:jid()  | error.
make(User, Server, Resource) ->
    case nodeprep(User) of
        error -> error;
        LUser ->
            case nameprep(Server) of
                error -> error;
                LServer ->
                    case resourceprep(Resource) of
                        error -> error;
                        LResource ->
                            #jid{user = User,
                                 server = Server,
                                 resource = Resource,
                                 luser = LUser,
                                 lserver = LServer,
                                 lresource = LResource}
                    end
            end
    end.

-spec make(ejabberd:simple_jid()) ->  ejabberd:jid()  | error.
make({User, Server, Resource}) ->
    make(User, Server, Resource).

-spec make_noprep(User     :: ejabberd:luser(),
                  Server   :: ejabberd:lserver(),
                  Resource :: ejabberd:lresource()) -> ejabberd:jid().
make_noprep(LUser, LServer, LResource) ->
    #jid{user = LUser,
         server = LServer,
         resource = LResource,
         luser = LUser,
         lserver = LServer,
         lresource = LResource}.

-spec make_noprep(ejabberd:simple_jid()) -> ejabberd:jid() | error.
make_noprep({LUser, LServer, LResource}) ->
    make_noprep(LUser, LServer, LResource).

-spec are_equal(ejabberd:jid(), ejabberd:jid()) ->  boolean().
are_equal(#jid{luser = LUser, lserver = LServer, lresource = LRes},
          #jid{luser = LUser, lserver = LServer, lresource = LRes}) ->
    true;
are_equal(_, _) ->
    false.

%% @doc Returns true if `are_equal(to_bare(A), to_bare(B))'
are_bare_equal(#jid{luser = LUser, lserver = LServer},
               #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal(_, _) ->
    false.




-spec from_binary(binary()) ->  error  | ejabberd:jid().
from_binary(J) ->
    binary_to_jid1(J, []).


-spec binary_to_jid1(binary(), [byte()]) -> 'error' | ejabberd:jid().
binary_to_jid1(<<$@, _J/binary>>, []) ->
    error;
binary_to_jid1(<<$@, J/binary>>, N) ->
    binary_to_jid2(J, lists:reverse(N), []);
binary_to_jid1(<<$/, _J/binary>>, []) ->
    error;
binary_to_jid1(<<$/, J/binary>>, N) ->
    binary_to_jid3(J, [], lists:reverse(N), []);
binary_to_jid1(<<C, J/binary>>, N) ->
    binary_to_jid1(J, [C | N]);
binary_to_jid1(<<>>, []) ->
    error;
binary_to_jid1(<<>>, N) ->
    make(<<>>, list_to_binary(lists:reverse(N)), <<>>).


%% @doc Only one "@" is admitted per JID
-spec binary_to_jid2(binary(), [byte()], [byte()]) -> 'error' | ejabberd:jid().
binary_to_jid2(<<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_jid2(<<$/, _J/binary>>, _N, []) ->
    error;
binary_to_jid2(<<$/, J/binary>>, N, S) ->
    binary_to_jid3(J, N, lists:reverse(S), []);
binary_to_jid2(<<C, J/binary>>, N, S) ->
    binary_to_jid2(J, N, [C | S]);
binary_to_jid2(<<>>, _N, []) ->
    error;
binary_to_jid2(<<>>, N, S) ->
    make(list_to_binary(N), list_to_binary(lists:reverse(S)), <<>>).


-spec binary_to_jid3(binary(), [byte()], [byte()], [byte()]) -> 'error' | ejabberd:jid().
binary_to_jid3(<<C, J/binary>>, N, S, R) ->
    binary_to_jid3(J, N, S, [C | R]);
binary_to_jid3(<<>>, N, S, R) ->
    make(list_to_binary(N), list_to_binary(S), list_to_binary(lists:reverse(R))).


-spec to_binary(ejabberd:simple_jid() | ejabberd:simple_bare_jid() | ejabberd:jid()) ->  binary().
to_binary(Jid) when is_binary(Jid) ->
    % sometimes it is used to format error messages
    Jid;
to_binary(#jid{user = User, server = Server, resource = Resource}) ->
    to_binary({User, Server, Resource});
to_binary({User, Server}) ->
    to_binary({User, Server, <<>>});
to_binary({Node, Server, Resource}) ->
    S1 = case Node of
             <<>> ->
                 <<>>;
             _ ->
                 <<Node/binary, "@">>
         end,
    S2 = <<S1/binary, Server/binary>>,
    S3 = case Resource of
             <<>> ->
                 S2;
             _ ->
                 <<S2/binary, "/", Resource/binary>>
         end,
    S3.

-spec is_nodename(<<>> | binary()) -> boolean().
is_nodename(<<>>) ->
    false;
is_nodename(J) ->
    nodeprep(J) /= error.

-spec nodeprep(ejabberd:user()) -> 'error' | ejabberd:lserver().
nodeprep(S) when is_binary(S), size(S) < ?SANE_LIMIT ->
    R = stringprep:nodeprep(S),
    if
        size(R) < ?SANE_LIMIT -> R;
        true -> error
    end;
nodeprep(_) ->
    error.


-spec nameprep(ejabberd:server()) -> 'error' | ejabberd:luser().
nameprep(S) when is_binary(S), size(S) < ?SANE_LIMIT ->
    R = stringprep:nameprep(S),
    if
        size(R) < ?SANE_LIMIT -> R;
        true -> error
    end;
nameprep(_) ->
    error.


-spec resourceprep(ejabberd:resource()) ->
    'error' | ejabberd:lresource().
resourceprep(S) when size(S) < ?SANE_LIMIT ->
    R = stringprep:resourceprep(S),
    if
        size(R) < ?SANE_LIMIT -> R;
        true -> error
    end;
resourceprep(_) ->
    error.


-spec to_lower(JID :: ejabberd:simple_jid() | ejabberd:jid()) ->
    error | ejabberd:simple_jid().
to_lower(#jid{luser = U, lserver = S, lresource = R}) ->
    {U, S, R};
to_lower({U, S, R}) ->
    case jid:nodeprep(U) of
        error -> error;
        LUser ->
            case jid:nameprep(S) of
                error -> error;
                LServer ->
                    case jid:resourceprep(R) of
                        error -> error;
                        LResource ->
                            {LUser, LServer, LResource}
                    end
            end
    end.

-spec to_lus(JID :: ejabberd:jid()) -> error | ejabberd:simple_bare_jid().
to_lus(#jid{luser = U, lserver = S}) ->
    {U, S}.

-spec to_bare(ejabberd:simple_jid()  | ejabberd:jid()) -> 
                 ejabberd:simple_jid()  | ejabberd:jid().
to_bare(#jid{} = JID) ->
    JID#jid{resource = <<>>, lresource = <<>>};
to_bare({U, S, _R}) ->
    {U, S, <<>>}.


-spec replace_resource(ejabberd:jid(), ejabberd:resource()) ->
                          error  | ejabberd:jid().
replace_resource(JID, Resource) ->
    case resourceprep(Resource) of
        error -> error;
        LResource ->
            JID#jid{resource = Resource, lresource = LResource}
    end.

-spec binary_to_bare(binary()) -> error | ejabberd:jid().
binary_to_bare(JID) when is_binary(JID) ->
    case from_binary(JID) of
        error ->
            error;
        #jid{} = Result ->
            to_bare(Result)
    end.
