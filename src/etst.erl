-module(etst).

-export([new/0, insert/2, lookup/2]).


%% API
new() ->
    {none, empty, empty, empty}.


insert(Key, {none, empty, empty, empty}) ->
    {Key, empty, empty, empty};
insert(Key = <<KHead:1/binary, _/binary>>, {NodeKey = <<NHead:1/binary, _/binary>>, Lo, Eq, Hi}) when KHead < NHead ->
    {NodeKey, insert(Key, ensure_nonempty(Lo)), Eq, Hi};
insert(Key = <<KHead:1/binary, _/binary>>, {NodeKey = <<NHead:1/binary, _/binary>>, Lo, Eq, Hi}) when KHead > NHead ->
    {NodeKey, Lo, Eq, insert(Key, ensure_nonempty(Hi))};
insert(Key, {NodeKey, Lo, Eq, Hi}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<Pref:PrefLen/binary, RestPref/binary>> = NodeKey,
    <<Pref:PrefLen/binary, RestKey/binary>> = Key,
    case RestPref of
        <<>> ->
            {NodeKey, Lo, insert(RestKey, ensure_nonempty(Eq)), Hi};
        _ ->
            {Pref, empty, insert(RestKey, {RestPref, Lo, Eq, Hi}), empty}
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
