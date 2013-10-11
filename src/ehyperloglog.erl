%% Implementatin of practical HyperLogLog
%% algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf

-module(ehyperloglog).

%% exports
-export([new/1, update/2, cardinality/1, merge/2]).

%% definitions
-record(hll, {b :: pos_integer(), registers :: registers()}).

-define(HASH_SIZE, 32).
-define(BIT32, (1 bsl ?HASH_SIZE)). %% 2^32

%% internal types
-type registers() :: bitmap:bitmap().

%% exported types
-opaque hll() :: #hll{}.
-export_type([hll/0]).

%% exported functions
-spec new(pos_integer()) -> hll().
new(B) when B >= 4; B =< 16 -> #hll{ b = B, registers = reg_new(B) }.

-spec update(any(), hll()) -> hll().
update(V, Hll) -> Hll#hll{registers = update(V, Hll#hll.b, Hll#hll.registers)}.

-spec cardinality(hll()) -> pos_integer().
cardinality(Hll) ->
    cardinality(1 bsl Hll#hll.b, Hll#hll.registers).

%% internal functions
-spec reg_new(pos_integer()) -> registers().
reg_new(B) -> bitmap:new(1 bsl B, B).

-spec reg_get(registers(), non_neg_integer()) -> non_neg_integer().
reg_get(R, Idx) -> bitmap:get(R, Idx).

reg_foldl(F, Acc, R) -> bitmap:foldl(F, Acc, R).

-spec reg_set(registers(), non_neg_integer(), integer()) -> registers().
reg_set(R, Idx, Val) -> bitmap:set(R, Idx, Val).

%% 32 bits hash
-spec hash32(any()) -> bitstring().
hash32(V) ->
    Hash = erlang:phash2(V, ?BIT32),
    << Hash:?HASH_SIZE >>.

-spec update(any(), pos_integer(), registers()) -> registers().
update(V, B, R) ->
    << Idx:B, Rest/bits >> = hash32(V),
    reg_set(R, Idx, erlang:max(reg_get(R, Idx), count_first_zeros(Rest))).
    
-spec merge(hll(), hll()) -> hll().
merge(Hll1, Hll2) ->
    R = bitmap:merge(Hll1#hll.registers, Hll2#hll.registers),
    #hll{b = Hll1#hll.b, registers = R}.    

-spec cardinality(pos_integer(), registers()) -> pos_integer().
cardinality(M, R) ->
    correction(M, raw_estimate(M, R), R).

-spec raw_estimate(pos_integer(), registers()) -> pos_integer().
raw_estimate(M, R) ->
    F = fun(Int, Acc) -> 1/ (1 bsl Int) + Acc end,
    alpha(M) * M * M * 1/reg_foldl(F, 0, R).

-spec correction(pos_integer(), pos_integer(), registers()) -> pos_integer().
correction(M, Estimate, R) when Estimate =< 5*M/2 ->
    V = num_of_zeros(R),
    if V =/= 0 -> M * math:log(M/V);
       true    -> Estimate
    end;
correction(_M, Estimate, _R) when Estimate =< ?BIT32/30 ->
    Estimate;
correction(_M, Estimate, _R) ->
    -?BIT32* math:log(1 - Estimate/?BIT32).

-spec alpha(pos_integer()) -> float().
alpha(M) when M == 16  -> 0.673;
alpha(M) when M == 32  -> 0.697;
alpha(M) when M == 64  -> 0.709;
alpha(M) when M >= 128 -> 0.7213/(1+1.079/M).

-spec num_of_zeros(registers()) -> non_neg_integer().
num_of_zeros(R) ->
    reg_foldl(fun count_zeros/2, 0, R).

-spec count_zeros(0|1, integer()) -> integer().
count_zeros(0, Acc) -> Acc +1;
count_zeros(_, Acc) -> Acc.

-spec count_first_zeros(bitstring()) -> pos_integer().
count_first_zeros(Bits) -> count_first_zeros(Bits, 1).

-spec count_first_zeros(bitstring(), pos_integer()) -> pos_integer().
count_first_zeros(<<>>, N)                  -> N;
count_first_zeros(<< 1:1, _Rest/bits >>, N) -> N;
count_first_zeros(<< 0:1, Rest/bits >>,  N) ->
    count_first_zeros(Rest, N+1).
