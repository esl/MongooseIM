%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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

%% Records and macros for testing MAM

-include_lib("exml/include/exml.hrl").

-define(assert_equal(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tValue ~p~n", [(E), (V)])
     || (E) =/= (V)]
    )).

-define(assert_equal_extra(E, V, Extra), (
    [ct:fail("assert_equal_extra(~s, ~s)~n\tExpected ~p~n\tValue ~p~nExtra ~p~n",
             [(??E), (??V), (E), (V), (Extra)])
     || (E) =/= (V)]
    )).

-define(_assert_equal_extra(E, V, Extra), (
    [ct:pal("assert_equal_extra(~s, ~s)~n\tExpected ~p~n\tValue ~p~nExtra ~p~n",
            [(??E), (??V), (E), (V), (Extra)])
     || (E) =/= (V)]
    )).

-record(rsm_in, {
        max         :: non_neg_integer() | undefined,
        direction   :: before | 'after' | undefined,
        id          :: binary() | undefined,
        index       :: non_neg_integer() | undefined,
        after_id    :: binary() | undefined,
        before_id   :: binary() | undefined,
        from_id     :: binary() | undefined,
        to_id       :: binary() | undefined,
        simple = false :: boolean(),
        opt_count = false :: boolean()
        }).

-record(forwarded_message, {
    from           :: binary() | undefined,
    to             :: binary() | undefined,
    result_queryid :: binary() | undefined,
    result_id      :: binary() | undefined,
    delay_from     :: binary() | undefined,
    delay_stamp    :: binary() | undefined,
    message_to     :: binary() | undefined,
    message_from   :: binary() | undefined,
    message_type   :: binary() | undefined,
    message_body   :: binary() | undefined,
    message_xs = [] :: [#xmlel{}],
    has_x_user_element :: boolean()
}).

-record(result_iq, {
    from            :: binary(),
    to              :: binary(),
    id              :: binary(),
    first           :: binary() | undefined,
    first_index     :: non_neg_integer() | undefined,
    last            :: binary() | undefined,
    query_id        :: binary() | not_supported,
    count           :: non_neg_integer()
}).

-record(error_iq, {
    id              :: binary(),
    type            :: binary(),
    error_type      :: binary(),
    condition       :: binary(),
    text            :: binary()
}).

-record(prefs_result_iq, {
    default_mode    :: binary() | undefined,
    always_jids = [] :: [binary()],
    never_jids  = [] :: [binary()]
}).

-record(mam_archive_respond, {
          respond_messages,
          respond_iq,
          respond_fin
         }).
