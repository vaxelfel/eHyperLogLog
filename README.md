eHyperLogLog
============

HyperLogLog, a distinct values estimator, in erlang

Reference
---------
[HyperLogLog] paper
[HyperLogLog]: algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf "HyperLogLog"

Build
-----
```shell
./rebar compile
```
or

```shell
make
```

Example
-------
1. Initialize
```erlang
New = ehyperloglog:new(16).
```

2. 10000 distinct values
```erlang
HLL = lists:foldl(fun ehyperloglog:update/2, New, lists:seq(1,100000)).
```

3. Should give an estimate close to 100000
```erlang
ehyperloglog:cardinality(HLL).
```

Bitmap (Utility)
---------------
A compact erlang bitstring for storing integers exposing 0-based array like interface.

1. A length 3 bitmap  4-bits cell width
```erlang
B  = bitmap:new(3, 4).
```

2. Set cell 1 to 7
```erlang
B1 = bitmap:set(B, 1, 7).
7  = bitmap:get(B1, 1).
```

3. Set cell 1 to 17 (overflow)
```erlang
B2 = bitmap:set(B1, 1, 17).
1  = bitmap:get(B2, 1). %% truncated to 1
```

Unit Tests
----------
```shell
./rebar eunit
```
