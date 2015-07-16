-module(etst).

-export([new/0, insert/2]).


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
