-module(etst_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


insert_test() ->
    T1 = etst:new(),
    T2 = etst:insert(<<"k1">>, T1),
    ?assertEqual({<<"k1">>, empty, empty, empty}, T2),
    T3 = etst:insert(<<"k12">>, T2),
    ?assertEqual({<<"k1">>,
        empty,
        {<<"2">>, empty, empty, empty},
        empty},
    T3),
    T4 = etst:insert(<<"abc">>, T3),
    ?assertEqual({<<"k1">>,
        {<<"abc">>, empty, empty, empty},
        {<<"2">>, empty, empty, empty},
        empty},
    T4),
    T5 = etst:insert(<<"abde">>, T4),
    ?assertEqual({<<"k1">>,
        {<<"ab">>,
            empty,
            {<<"c">>, empty, empty, {<<"de">>, empty, empty, empty}},
            empty},
        {<<"2">>, empty, empty, empty},
        empty},
    T5),
    T6 = etst:insert(<<"z">>, T5),
    T7 = etst:insert(<<"xa">>, T6),
    ?assertEqual({<<"k1">>,
        {<<"ab">>,
            empty,
            {<<"c">>, empty, empty, {<<"de">>, empty, empty, empty}},
            empty},
        {<<"2">>, empty, empty, empty},
        {<<"z">>, {<<"xa">>, empty, empty, empty}, empty, empty}},
    T7).
