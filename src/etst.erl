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
lookup(Key, Tree) ->
    case lookup1(Key, Tree, <<>>) of
        {Key, Value} -> Value;
        _ -> none
    end.

-spec lookup1(key(), tree(), key()) -> {key(), value()}.
lookup1(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _NodeVal, Lo, _Eq, _Hi}, FullKey) when KHead < NHead ->
    lookup1(Key, Lo, FullKey);
lookup1(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _NodeVal, _Lo, _Eq, Hi}, FullKey) when KHead > NHead ->
    lookup1(Key, Hi, FullKey);
lookup1(Key, {Key, Value, _, _, _}, FullKey) ->
    {<<FullKey/binary, Key/binary>>, Value};
lookup1(Key, {NodeKey, _NodeVal, _Lo, Eq, _Hi}, FullKey) ->
    PrefLen = byte_size(NodeKey),
    <<_Pref:PrefLen/binary, RestKey/binary>> = Key,
    lookup1(RestKey, Eq, <<FullKey/binary, NodeKey/binary>>);
lookup1(_Key, _, FullKey) ->
    {FullKey, none}.
