-module(jid_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

-import(prop_helper, [prop/2]).

all() -> [
          binary_to_jid_succeeds_with_valid_binaries,
          binary_to_jid_fails_with_invalid_binaries,
          binary_to_jid_fails_with_empty_binary,
          make_jid_fails_on_binaries_that_are_too_long,
          jid_to_lower_fails_if_any_binary_is_invalid,
          jid_replace_resource_failes_for_invalid_resource,
          nodeprep_fails_with_too_long_username,
          nameprep_fails_with_too_long_domain,
          resourceprep_fails_with_too_long_resource,
          nodeprep_fails_with_incorrect_username,
          resourceprep_fails_with_incorrect_resource,
          nameprep_fails_with_incorrect_domain,
          is_nodename_fails_for_empty_binary,
          compare_bare_jids
         ].

init_per_suite(C) ->
    application:start(stringprep),
    C.

end_per_suite(C) ->
    C.


binary_to_jid_succeeds_with_valid_binaries(_C) ->
    Prop = ?FORALL(BinJid, (jid_gen:jid()),
                   (is_record(jid:from_binary(BinJid), jid))),
    prop(binary_to_jid_succeeds_with_valid_binaries, Prop).


binary_to_jid_fails_with_invalid_binaries(_C) ->
    Prop = ?FORALL(BinJid, jid_gen:invalid_jid(),
                   error == jid:from_binary(BinJid)),
    run_property(Prop, 100, 1, 42).

binary_to_jid_fails_with_empty_binary(_) ->
    error = jid:from_binary(<<>>).

make_jid_fails_on_binaries_that_are_too_long(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   case element_length_is_too_big([U,S,R]) of
                        true -> error == jid:make(U,S,R);
                        false -> is_record(jid:make(U,S,R), jid)
                   end),
    run_property(Prop, 100, 500, 1500).

element_length_is_too_big(Els) ->
    lists:any(fun(El) -> size(El) >= 1024 end, Els).

jid_to_lower_fails_if_any_binary_is_invalid(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:maybe_valid_username(), jid_gen:maybe_valid_domain(), jid_gen:maybe_valid_resource()},
                   case jid:to_lower({U, S, R}) of
                       {LU, LS, LR} ->
                           jid:nodeprep(U) == LU andalso
                           jid:nameprep(S) == LS andalso
                           jid:resourceprep(R) == LR;
                       error ->
                           jid:nodeprep(U) == error orelse
                           jid:nameprep(S) == error orelse
                           jid:resourceprep(R) == error
                   end),

    run_property(Prop, 150, 1, 42).

nodeprep_fails_with_too_long_username(_C) ->
    Prop = ?FORALL(Bin, jid_gen:username(),
                   error == jid:nodeprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

nameprep_fails_with_too_long_domain(_C) ->
    Prop = ?FORALL(Bin, jid_gen:domain(),
                   error == jid:nameprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

resourceprep_fails_with_too_long_resource(_C) ->
    Prop = ?FORALL(Bin, jid_gen:resource(),
                   error == jid:resourceprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

jid_replace_resource_failes_for_invalid_resource(_) ->
    Prop = ?FORALL({BinJid, MaybeCorrectRes},
                   {jid_gen:bare_jid(), jid_gen:maybe_valid_resource()},
                   jid_replace_resource(BinJid, MaybeCorrectRes)),
    prop(jid_replace_resource, Prop).

jid_replace_resource(BinJid, Res) ->
    Jid = jid:from_binary(BinJid),
    Jid2 = jid:replace_resource(Jid, Res),
    check_jid_replace_resource_output(Res, Jid2).

check_jid_replace_resource_output(Resource, error) ->
    jid:resourceprep(Resource) == error;
check_jid_replace_resource_output(Resource, #jid{}) ->
    jid:resourceprep(Resource) =/= error.


run_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).

nodeprep_fails_with_incorrect_username(_) ->
    prop(incorrect_username_property,
         ?FORALL(Bin, jid_gen:invalid_username(),
                 error == jid:nodeprep(Bin))).

resourceprep_fails_with_incorrect_resource(_) ->
    prop(incorrect_resource_property,
         ?FORALL(Bin, jid_gen:invalid_resource(),
                 error == jid:resourceprep(Bin))).

nameprep_fails_with_incorrect_domain(_) ->
    prop(incorrect_domain_property,
         ?FORALL(Bin, jid_gen:invalid_domain(),
                 error == jid:nameprep(Bin))).

is_nodename_fails_for_empty_binary(_) ->
    false = jid:is_nodename(<<>>).

compare_bare_jids(_) ->
    prop(compare_bare_jids,
         ?FORALL({A, B}, {jid_gen:jid(), jid_gen:jid()},
                 begin
                    AA = jid:from_binary(A),
                    BB = jid:from_binary(B),
                    equals(jid:are_equal(jid:to_bare(AA), jid:to_bare(BB)),
                           jid:are_bare_equal(AA, BB))
                 end)).
