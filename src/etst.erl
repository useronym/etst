-module(etst).

-export([new/0, insert/3, lookup/2]).


-type tree() :: {key(), value(), tree(), tree(), tree()} | empty.

-type key() :: binary() | none.
-type value() :: term().


%% API
-spec new() -> tree().
new() ->
    {none, none, empty, empty, empty}.


-spec insert(key(), value(), tree()) -> tree().
insert(Key, Value, {NodeKey, _, Lo, Eq, Hi}) when NodeKey == none orelse Key == NodeKey ->
    {Key, Value, Lo, Eq, Hi};
insert(Key = <<KHead:1/binary, _/binary>>, Value, {NodeKey = <<NHead:1/binary, _/binary>>, NodeVal, Lo, Eq, Hi}) when KHead < NHead ->
    {NodeKey, NodeVal, insert(Key, Value, ensure_nonempty(Lo)), Eq, Hi};
insert(Key = <<KHead:1/binary, _/binary>>, Value, {NodeKey = <<NHead:1/binary, _/binary>>, NodeVal, Lo, Eq, Hi}) when KHead > NHead ->
    {NodeKey, NodeVal, Lo, Eq, insert(Key, Value, ensure_nonempty(Hi))};
insert(Key, Value, {NodeKey, NodeVal, Lo, Eq, Hi}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<Pref:PrefLen/binary, RestPref/binary>> = NodeKey,
    <<Pref:PrefLen/binary, RestKey/binary>> = Key,
    case RestPref of
        <<>> ->
            {NodeKey, NodeVal, Lo, insert(RestKey, Value, ensure_nonempty(Eq)), Hi};
        _ when RestKey /= <<>> ->
            {Pref, none, empty, insert(RestKey, Value, {RestPref, NodeVal, Lo, Eq, Hi}), empty};
        _ when RestKey == <<>> ->
            {Pref, Value, empty, {RestPref, NodeVal, Lo, Eq, Hi}, empty}
    end.

-spec ensure_nonempty(tree()) -> tree().
ensure_nonempty(empty) ->
    new();
ensure_nonempty(T) ->
    T.


-spec lookup(key(), tree()) -> value().
lookup(<<H:8, T/binary>>, Tree) ->
    lookup1(H, T, Tree).

-spec lookup1(integer(), key(), tree()) -> {key(), value()}.
lookup1(H, T, {<<NodeH:8, NodeT/binary>>, NodeVal, Lo, Eq, Hi}) ->
    if
        H < NodeH -> lookup1(H, T, Lo);
        H > NodeH -> lookup1(H, T, Hi);
        true ->
            NodeTSize = byte_size(NodeT),
            case T of
                <<NodeT:NodeTSize/binary, Rest/binary>> ->
                    if
                        Rest == <<>> -> NodeVal;
                        true -> lookup(Rest, Eq)
                    end;
                _ -> none
            end
    end;
lookup1(_, _, _) ->
    none.
