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
-on_load(load/0).

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

-type user()      :: binary().
-type server()    :: binary().
-type resource()  :: binary().
-type luser()     :: binary().
-type lserver()   :: binary().
-type lresource() :: binary().

-type jid() :: #jid{}.
-type ljid() :: {luser(), lserver(), lresource()}.

%% A tuple-style JID
-type simple_jid() :: {user(), server(), resource()}.

-type simple_bare_jid() :: {LUser :: luser(), LServer :: lserver()}.

%% A tuple-style JID without resource part
-type literal_jid() :: binary().

-export_type([jid/0,
              ljid/0,
              simple_bare_jid/0,
              simple_jid/0,
              literal_jid/0,
              user/0, server/0, resource/0,
              luser/0, lserver/0, lresource/0
             ]).

-define(SANE_LIMIT, 1024).
-define(XMPP_JID_SIZE_LIMIT, 3071).
% Maximum JID size in octets (bytes) as defined in
% https://tools.ietf.org/html/rfc7622#section-3.1

-spec make(User :: user(), Server :: server(), Res :: resource()) -> jid()  | error.
make(User, Server, Res) ->
    case {nodeprep(User), nameprep(Server), resourceprep(Res)} of
        {error, _, _} -> error;
        {_, error, _} -> error;
        {_, _, error} -> error;
        {LUser, LServer, LRes} ->
            #jid{user = User,
                 server = Server,
                 resource = Res,
                 luser = LUser,
                 lserver = LServer,
                 lresource = LRes}
    end.

-spec make(simple_jid()) ->  jid()  | error.
make({User, Server, Resource}) ->
    make(User, Server, Resource).

-spec make_noprep(User     :: luser(),
                  Server   :: lserver(),
                  Resource :: lresource()) -> jid().
make_noprep(LUser, LServer, LResource) ->
    #jid{user = LUser,
         server = LServer,
         resource = LResource,
         luser = LUser,
         lserver = LServer,
         lresource = LResource}.

-spec make_noprep(simple_jid()) -> jid() | error.
make_noprep({LUser, LServer, LResource}) ->
    make_noprep(LUser, LServer, LResource).

-spec are_equal(jid(), jid()) ->  boolean().
are_equal(#jid{luser = LUser, lserver = LServer, lresource = LRes},
          #jid{luser = LUser, lserver = LServer, lresource = LRes}) ->
    true;
are_equal(_, _) ->
    false.

%% @doc Returns true if `are_equal(to_bare(A), to_bare(B))'
-spec are_bare_equal(jid() | ljid(), jid() | ljid()) -> boolean().
are_bare_equal(#jid{luser = LUser, lserver = LServer},
               #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal(#jid{luser = LUser, lserver = LServer}, {LUser, LServer, _}) ->
    true;
are_bare_equal({LUser, LServer, _}, #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal({LUser, LServer, _}, {LUser, LServer, _}) ->
    true;
are_bare_equal(_, _) ->
    false.

-spec from_binary(binary()) ->  error  | jid().
from_binary(J) when is_binary(J), byte_size(J) < ?XMPP_JID_SIZE_LIMIT ->
    case from_binary_nif(J) of
        {U, H, R} -> make(U, H, R);
        error -> error
    end;
from_binary(_) ->
    error.

%% Original Erlang equivalent can be found in test/jid_SUITE.erl,
%% together with `proper` generators to check for equivalence
-spec from_binary_nif(binary()) ->  error | simple_jid().
from_binary_nif(_) ->
    erlang:nif_error(not_loaded).

%% Original Erlang equivalent can be found in test/jid_SUITE.erl,
%% together with `proper` generators to check for equivalence
-spec to_binary(simple_jid() | simple_bare_jid() | jid()) ->  binary().
to_binary(_) ->
    erlang:nif_error(not_loaded).

-spec is_nodename(<<>> | binary()) -> boolean().
is_nodename(<<>>) ->
    false;
is_nodename(J) ->
    nodeprep(J) /= error.

-spec validate_binary_size(binary()) -> binary() | error.
validate_binary_size(R) when size(R) < ?SANE_LIMIT ->
    R;
validate_binary_size(_) ->
    error.

-spec nodeprep(user()) -> 'error' | lserver().
nodeprep(S) when is_binary(S), size(S) < ?SANE_LIMIT ->
    R = stringprep:nodeprep(S),
    validate_binary_size(R);
nodeprep(_) ->
    error.

-spec nameprep(server()) -> 'error' | luser().
nameprep(S) when is_binary(S), size(S) < ?SANE_LIMIT ->
    R = stringprep:nameprep(S),
    validate_binary_size(R);
nameprep(_) ->
    error.

-spec resourceprep(resource()) ->
    'error' | lresource().
resourceprep(S) when size(S) < ?SANE_LIMIT ->
    R = stringprep:resourceprep(S),
    validate_binary_size(R);
resourceprep(_) ->
    error.

-spec to_lower(JID :: simple_jid() | jid()) ->
    error | simple_jid().
to_lower(#jid{luser = U, lserver = S, lresource = R}) ->
    {U, S, R};
to_lower({U, S, R}) ->
    case {jid:nodeprep(U), jid:nameprep(S), jid:resourceprep(R)}  of
      {LUser, LServer, LResource} when LUser /= error, LServer /= error, LResource /= error ->
        {LUser, LServer, LResource};
      _Error ->
        error
    end.

-spec to_lus(JID :: jid() | ljid()) -> simple_bare_jid().
to_lus(#jid{luser = U, lserver = S}) ->
    {U, S};
to_lus({U, S, _}) ->
    {U, S}.

-spec to_bare(simple_jid()) -> simple_jid();
             (jid()) -> jid().
to_bare(#jid{} = JID) ->
    JID#jid{resource = <<>>, lresource = <<>>};
to_bare({U, S, _R}) ->
    {U, S, <<>>}.

-spec replace_resource(jid(), resource()) -> error  | jid().
replace_resource(JID, Resource) ->
    case resourceprep(Resource) of
        error -> error;
        LResource ->
            JID#jid{resource = Resource, lresource = LResource}
    end.

-spec binary_to_bare(binary()) -> error | jid().
binary_to_bare(JID) when is_binary(JID) ->
    case from_binary(JID) of
        error ->
            error;
        #jid{} = Result ->
            to_bare(Result)
    end.


%%%===================================================================
%%% Load NIF
%%%===================================================================
-spec load() -> any().
load() ->
    SoName = case code:priv_dir(mongooseim) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, jid]);
                _ ->
                    filename:join([priv, jid])
            end;
        Dir ->
            filename:join([Dir, lib, ?MODULE_STRING])
    end,
    erlang:load_nif(SoName, 0).
