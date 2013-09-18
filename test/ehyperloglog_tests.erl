-module(ehyperloglog_tests).
-include_lib("eunit/include/eunit.hrl").

general_test() ->
    New = ehyperloglog:new(16),
    %% call function with 10000 distinct values
    HLL = lists:foldl(fun ehyperloglog:update/2, New, lists:seq(1,10000)),
    %% should give an estimate close to 100000
    ?assertEqual(9997, trunc(ehyperloglog:cardinality(HLL))).

merge_test() ->
    New = ehyperloglog:new(16),
    HLL1 = lists:foldl(fun ehyperloglog:update/2, New, lists:seq(1,50000)),
    HLL2 = lists:foldl(fun ehyperloglog:update/2, New, lists:seq(50000,100000)),
    HLL = ehyperloglog:merge(HLL1, HLL2),
    ?assertEqual(99772, trunc(ehyperloglog:cardinality(HLL))).
