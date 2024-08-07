Nonterminals
filter filtercomp filterlist item
simple present substring extensible
initial any final matchingrule xattr
attr value.

Terminals str
'(' ')' '&' '|' '!' '=' '~=' '>=' '<=' '=*' '*' ':dn' ':' ':='.

Rootsymbol filter.

filter -> '(' filtercomp ')': '$2'.
filtercomp -> '&' filterlist: 'and'('$2').
filtercomp -> '|' filterlist: 'or'('$2').
filtercomp -> '!' filter: 'not'('$2').
filtercomp -> item: '$1'.
filterlist -> filter: '$1'.
filterlist -> filter filterlist: flatten(['$1', '$2']).

item -> simple: '$1'.
item -> present: '$1'.
item -> substring: '$1'.
item -> extensible: '$1'.

simple -> attr '=' value: equal('$1', '$3').
simple -> attr '~=' value: approx('$1', '$3').
simple -> attr '>=' value: greater('$1', '$3').
simple -> attr '<=' value: less('$1', '$3').

present -> attr '=*': present('$1').

substring -> attr '=' initial '*' any: substrings('$1', ['$3', '$5']).
substring -> attr '=' '*' any final: substrings('$1', ['$4', '$5']).
substring -> attr '=' initial '*' any final: substrings('$1', ['$3', '$5', '$6']).
substring -> attr '=' '*' any: substrings('$1', ['$4']).
any -> any value '*': 'any'('$1', '$2').
any -> '$empty': [].
initial -> value: initial('$1').
final -> value: final('$1').

extensible -> xattr ':dn' ':' matchingrule ':=' value: extensible('$6', ['$1', '$4']).
extensible -> xattr ':' matchingrule ':=' value: extensible('$5', ['$1', '$3']).
extensible -> xattr ':dn' ':=' value: extensible('$4', ['$1']).
extensible -> xattr ':=' value: extensible('$3', ['$1']).
extensible -> ':dn' ':' matchingrule ':=' value: extensible('$5', ['$3']).
extensible -> ':' matchingrule ':=' value: extensible('$4', ['$2']).
xattr -> value: xattr('$1').
matchingrule -> value: matchingrule('$1').

attr -> str: value_of('$1').
value -> str: value_of('$1').

Erlang code.

'and'(Value)                -> eldap:'and'(eldap_utils:maybe_b2list(Value)).
'or'(Value)                 -> eldap:'or'(eldap_utils:maybe_b2list(Value)).
'not'(Value)                -> eldap:'not'(Value).
equal(Desc, Value)          -> eldap:equalityMatch(Desc, eldap_utils:maybe_b2list(Value)).
approx(Desc, Value)         -> eldap:approxMatch(Desc, eldap_utils:maybe_b2list(Value)).
greater(Desc, Value)        -> eldap:greaterOrEqual(Desc, eldap_utils:maybe_b2list(Value)).
less(Desc, Value)           -> eldap:lessOrEqual(Desc, eldap_utils:maybe_b2list(Value)).
present(Value)              -> eldap:present(eldap_utils:maybe_b2list(Value)).
extensible(Value, Opts)     -> eldap:extensibleMatch(eldap_utils:maybe_b2list(Value), Opts).
substrings(Desc, ValueList) -> eldap:substrings(Desc, flatten(ValueList)).
initial(Value)              -> {initial, eldap_utils:maybe_b2list(Value)}.
final(Value)                -> {final, eldap_utils:maybe_b2list(Value)}.
'any'(Token, Value)         -> [Token, {any, eldap_utils:maybe_b2list(Value)}].
xattr(Value)                -> {type, eldap_utils:maybe_b2list(Value)}.
matchingrule(Value)         -> {matchingRule, eldap_utils:maybe_b2list(Value)}.
value_of(Token)             -> iolist_to_binary(element(3, Token)).
flatten(List)               -> lists:flatten(List).
