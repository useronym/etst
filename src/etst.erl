-module(etst).

-export([new/0, insert/3, lookup/2]).


%% API
new() ->
    {none, none, empty, empty, empty}.


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

ensure_nonempty(empty) ->
    new();
ensure_nonempty(T) ->
    T.


lookup(Key, {Key, _, _, _}) ->
    true;
lookup(_, {_, empty, empty, empty}) ->
    false;
lookup(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, Lo, _Eq, _Hi}) when KHead < NHead ->
    lookup(Key, Lo);
lookup(Key = <<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _Lo, _Eq, Hi}) when KHead > NHead ->
    lookup(Key, Hi);
lookup(Key, {NodeKey, _Lo, Eq, _Hi}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<_Pref:PrefLen/binary, RestKey/binary>> = Key,
    lookup(RestKey, Eq).
