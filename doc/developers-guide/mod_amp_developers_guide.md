The Developer's Guide to mod_amp
================================

This is a quick, introductory guide for developers wishing to extend `mod_amp` or plug into the message processing system.

Source Files, Headers and Tests
-------------------------------

  * `include/amp.hrl`
    This header file contains the amp XML namespace and the types used by mod_amp: `amp_rule()` and `amp_strategy()` are the top-level points of interest.

  * `src/mod_amp.erl`
    This module is responsible for plugging in all the other components. 
    It's main driving function is `filter_packet`. 
    After determining that a given message contains amp rules, the module proceeds by determining its strategy for the message and comparing it against the rules.
    The server may return an error at multiple points in its work-flow. 
    This is signaled by calling the function `send_error_and_drop/3` or `send_errors_and_drop/2`.

  * `src/amp.erl`
    This module is responsible for parsing rules from incoming elements and serializing server responses in the proper format.
    `binaries_to_rule/3` can return either a proper `amp_rule()`, or an `amp_invalid_rule()`, which does not contain sensible values, but can be used by the server to create an appropriate error message.

  * `test/amp_SUITE.erl`
    Tests for the API functions exported by `amp.erl`

  * `src/amp_strategy.erl`
    This module is where the server-side hook for determining a default action for a given message is performed. 
    Calls to `ejabberd_sm` are made here.

  * `src/amp_resolver.erl`
    This module models the resolution of amp rules, given a certain strategy. 
    Also, the function verify_rule_support is hard-coded here to return an `unsupported-` type error for unsupported rule actions and values.

  * `test/amp_resolver_SUITE.erl`
    These tests verify that the `amp_resolver:check_condition/4` hook works as intended, i.e: that the rules which would be triggered given a particular server-side strategy actually do get triggered, and that all others get rejected.

  * `test/amp_gen.erl`
    This module contains PropEr generators for server-side strategies, as well as valid and invalid amp rules. 
    Used in both test suites.


Hooks for Other Modules
-----------------------

If your module would like to have some say in the amp decision making process, please refer to the hooks: `amp_determine_strategy` and `amp_check_condition`.
Remeber that the hook for check_condition is a fold on a boolean(), and should behave like a variadic `or`. 
I.e: once a rule is deemed to apply, other hooks SHOULD NOT revert this value to false.

Cf. this code from `amp_resolver`:

    -spec check_condition(any(), amp_strategy(), amp_condition(), amp_value())
                              -> boolean().
    check_condition(HookAcc, Strategy, Condition, Value) ->
        case HookAcc of
            true -> true;   %% SOME OTHER HOOK HAS DECIDED THAT THIS RULE APPLIES %%
            _    -> resolve(Strategy, Condition, Value) %% PERFORM LOCAL CHECK %%
        end.
    

Ideas for Further Development
-----------------------------

### Easy

  * Implement the 'alert' and 'drop' action types.
  * Implement support for the 'stored' value for 'deliver'

### Medium

  * Implement the security policy described in the third bullet point of XEP-0079, Section 9 (Security Considerations). 
  This will require that `amp_resolver:verify_support` also take the `{From, To, Packet}` `:: hook_data()` parameter and check that `From` is permitted to know about `To`'s presence. 
  If they are not, then the server should treat this as a `not-acceptable` amp request.

  * Make support for various actions, conditions and values configurable.
    This will require implementing an intelligent mechanism for matching the user-supplied rules with what's configured server-side. 
    Currently, server-side support is hard-coded in several places:
    
    1.  Disco announcements are in `mod_amp:amp_features/0`
    2.  Rule support is in `amp_resolver:verify_rule_support/1`
    3.  Every other function that deals with rules can handle unsupported rules, but ignores their meaning and decides that these rules don't apply.


### Hard

  * Implement support for the 'expire-at' condition.



