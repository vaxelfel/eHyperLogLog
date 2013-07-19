-module(ehyperloglog_tests).
-include_lib("eunit/include/eunit.hrl").

general_test() ->
    New = ehyperloglog:new(16),
    %% call function with 10000 distinct values
    HLL = lists:foldl(fun ehyperloglog:update/2, New, lists:seq(1,10000)),
    %% should give an estimate close to 100000
    ?assertEqual(9997, trunc(ehyperloglog:cardinality(HLL))).
