-module(etst_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


insert_test() ->
    T1 = etst:new(),
    T2 = etst:insert(<<"k1">>, val1, T1),
    ?assertEqual({<<"k1">>, val1, empty, empty, empty}, T2),
    T3 = etst:insert(<<"k12">>, val2, T2),
    ?assertEqual({<<"k1">>, val1,
        empty,
        {<<"2">>, val2, empty, empty, empty},
        empty},
        T3),
    T4 = etst:insert(<<"abc">>, val3, T3),
    ?assertEqual({<<"k1">>, val1,
        {<<"abc">>, val3, empty, empty, empty},
        {<<"2">>, val2, empty, empty, empty},
        empty},
        T4),
    T5 = etst:insert(<<"abde">>, val4, T4),
    ?assertEqual({<<"k1">>, val1,
        {<<"ab">>, none,
            empty,
            {<<"c">>, val3, empty, empty, {<<"de">>, val4, empty, empty, empty}},
            empty},
        {<<"2">>, val2, empty, empty, empty},
        empty},
        T5),
    T6 = etst:insert(<<"z">>, val5, T5),
    T7 = etst:insert(<<"xa">>, val6, T6),
    ?assertEqual({<<"k1">>, val1,
        {<<"ab">>, none,
            empty,
            {<<"c">>, val3, empty, empty, {<<"de">>, val4, empty, empty, empty}},
            empty},
        {<<"2">>, val2, empty, empty, empty},
        {<<"z">>, val5, {<<"xa">>, val6, empty, empty, empty}, empty, empty}},
        T7).

lookup_test() ->
    T1 = etst:new(),
    T2 = etst:insert(<<"k1">>, T1),
    T3 = etst:insert(<<"k12">>, T2),
    T4 = etst:insert(<<"abc">>, T3),
    T5 = etst:insert(<<"abde">>, T4),
    T6 = etst:insert(<<"z">>, T5),
    T7 = etst:insert(<<"xa">>, T6),
    ?assert(etst:lookup(<<"k12">>, T7)),
    ?assert(etst:lookup(<<"abde">>, T7)),
    ?assert(etst:lookup(<<"z">>, T7)),
    ?assert(etst:lookup(<<"xa">>, T7)),
    ?assertNot(etst:lookup(<<"nonexistent">>, T7)),
    ?assertNot(etst:lookup(<<"k1-nonexistent">>, T7)),
    ?assertNot(etst:lookup(<<"abde-nonexistent">>, T7)).
