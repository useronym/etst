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
insert(Key, Value, {none, none, empty, empty, empty}) ->
    {Key, Value, empty, empty, empty};
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
        _ ->
            {Pref, none, empty, insert(RestKey, Value, {RestPref, NodeVal, Lo, Eq, Hi}), empty}
    end.

-spec ensure_nonempty(tree()) -> tree().
ensure_nonempty(empty) ->
    new();
ensure_nonempty(T) ->
    T.


-spec lookup(key(), tree()) -> value().
lookup(Key, {Key, Value, _, _, _}) ->
    Value;
lookup(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _NodeVal, Lo, _Eq, _Hi}) when KHead < NHead ->
    lookup(Key, Lo);
lookup(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _NodeVal, _Lo, _Eq, Hi}) when KHead > NHead ->
    lookup(Key, Hi);
lookup(Key, {NodeKey, _NodeVal, _Lo, Eq, _Hi}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<_Pref:PrefLen/binary, RestKey/binary>> = Key,
    lookup(RestKey, Eq);
lookup(_Key, empty) ->
    none.
