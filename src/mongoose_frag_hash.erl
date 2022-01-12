%% Based on mnesia_frag_hash
%% But allows to pass simple_hash_module option
-module(mongoose_frag_hash).
-behaviour(mnesia_frag_hash).

-export([init_state/2,
         add_frag/1,
         del_frag/1,
         key_to_frag_number/2,
         match_spec_to_frag_numbers/2]).

-record(hash_state, {n_fragments, next_n_to_split, n_doubles, simple_hash_module}).

%% State is hash_state from docs
init_state(_Tab, State) when is_list(State) ->
    {simple_hash_module, SimpleHashModule} = lists:keyfind(simple_hash_module, 1, State),
    #hash_state{n_fragments = 1,
        next_n_to_split = 1,
        n_doubles = 0,
        simple_hash_module = SimpleHashModule}.

add_frag(State = #hash_state{}) ->
    SplitN = State#hash_state.next_n_to_split,
    P = SplitN + 1,
    L = State#hash_state.n_doubles,
    NewN = State#hash_state.n_fragments + 1,
    State2 = case trunc(math:pow(2, L)) + 1 of
         P2 when P2 == P ->
             State#hash_state{n_fragments = NewN,
                      n_doubles = L + 1,
                      next_n_to_split = 1};
         _ ->
             State#hash_state{n_fragments = NewN,
                      next_n_to_split = P}
         end,
    {State2, [SplitN], [NewN]}.

del_frag(State = #hash_state{}) ->
    P = State#hash_state.next_n_to_split - 1,
    L = State#hash_state.n_doubles,
    N = State#hash_state.n_fragments,
    if
    P < 1 ->
        L2 = L - 1,
        MergeN = trunc(math:pow(2, L2)),
        State2 = State#hash_state{n_fragments = N - 1,
                      next_n_to_split = MergeN,
                      n_doubles = L2},
        {State2, [N], [MergeN]};
    true ->
        MergeN = P,
        State2 = State#hash_state{n_fragments = N - 1,
                      next_n_to_split = MergeN},
        {State2, [N], [MergeN]}
    end.

key_to_frag_number(State = #hash_state{simple_hash_module = SimpleHashModule}, Key) ->
    SimpleKey = SimpleHashModule:simple_key(Key),
    L = State#hash_state.n_doubles,
    A = erlang:phash(SimpleKey, trunc(math:pow(2, L))),
    P = State#hash_state.next_n_to_split,
    if
    A < P ->
        erlang:phash(SimpleKey, trunc(math:pow(2, L + 1)));
    true ->
        A
    end.

%% [{{mnesia_session,{<<"localhost">>,<<"test50">>,<<>>,'_'},'_','_'},[],['$_']}]
match_spec_to_frag_numbers(State = #hash_state{simple_hash_module = SimpleHashModule}, MatchSpec) ->
    case MatchSpec of
    [{HeadPat, _, _}] when is_tuple(HeadPat), size(HeadPat) > 2 ->
        KeyPat = element(2, HeadPat),
        if
            is_tuple(KeyPat) ->
                SimpleKeyPat = SimpleHashModule:simple_key(KeyPat),
                case has_var(SimpleKeyPat) of
                false ->
                    [key_to_frag_number(State, KeyPat)];
                true ->
                    all_segments(State)
                end;
            true ->
                all_segments(State)
        end;
    _ ->
        all_segments(State)
    end.

all_segments(State) ->
    lists:seq(1, State#hash_state.n_fragments).

has_var(Pat) ->
    mnesia:has_var(Pat).
