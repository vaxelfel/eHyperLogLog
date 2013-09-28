-module(bitmap).
-export([new/0, new/2,set/3,get/2,foldl/3, merge/2]).
%% bitmap record accessors
-export([get_width/1, get_bits/1,
         set_width/2, set_bits/2,
         size/1, length/1]).

-record(bitmap, { width = 0    :: non_neg_integer() %% cell width
                , bits  = <<>> :: bitstring()
                }).

-opaque bitmap() :: #bitmap{}.

-export_type([bitmap/0]).

-define(NON_NEG_INTEGER(N), is_integer(N) andalso N >= 0).
-define(POS_INTEGER(N), is_integer(N) andalso N > 0).

-spec get_width(bitmap()) -> non_neg_integer().
get_width(BM) -> BM#bitmap.width.

-spec set_width(bitmap(), non_neg_integer()) -> bitmap().
set_width(BM, Wd) -> BM#bitmap{width = Wd}.

-spec get_bits(bitmap()) -> bitstring().
get_bits(BM) -> BM#bitmap.bits.

-spec set_bits(bitmap(), bitstring()) -> bitmap().
set_bits(BM, Bits) -> BM#bitmap{bits = Bits}.

%% size of the underlaying bitstring
-spec size(bitmap()) -> non_neg_integer().
size(BM) -> bit_size(get_bits(BM)).

%% number of cells
-spec length(bitmap()) -> non_neg_integer().
length(BM) -> case get_width(BM) of
                  0 -> 0;
                  Wd -> bit_size(get_bits(BM)) div Wd
              end.

-spec new() -> bitmap().
new() -> #bitmap{}.

-spec new(non_neg_integer(), non_neg_integer()) -> bitmap().
new(N, _) when N < 0   -> erlang:error(negative_length);
new(_, Wd) when Wd < 0 -> erlang:error(negative_width);
new(N, Wd) when N*Wd == 0 -> new();
new(N, Wd) when ?POS_INTEGER(N), ?POS_INTEGER(Wd) ->
    set_bits(set_width(new(), Wd), init_bitstring(N, Wd)).

-spec init_bitstring(non_neg_integer(), non_neg_integer()) -> bitstring().
init_bitstring(N, Wd) -> << 0:(N*Wd) >>.

-spec set(bitmap(), non_neg_integer(), integer()) -> bitmap().
set(BM = #bitmap{width = Wd, bits = Bits}, Nth, Val) when
      ?NON_NEG_INTEGER(Nth), Nth*Wd < bit_size(Bits) ->
    HeadWd = Nth * Wd,
    << Head:HeadWd/bits, _:Wd, Tail/bits >> = Bits,
    NewBits = erlang:list_to_bitstring([Head, << Val:Wd >>, Tail]),
    set_bits(BM, NewBits);
set(BM, _, _) when not is_record(BM, bitmap) -> erlang:error(invalid_bitmap);
set(_, _, _)                                 -> erlang:error(invalid_index).

-spec get(bitmap(), non_neg_integer()) -> pos_integer().
get(BM = #bitmap{width = Wd, bits = Bits}, Nth) when
      ?NON_NEG_INTEGER(Nth), Nth*Wd < bit_size(Bits) ->
    Bits = get_bits(BM),
    Wd = get_width(BM),
    HeadWd = Nth*Wd,
    <<_Head:HeadWd, Val:Wd, _Tail/bits >> = Bits,
    Val;
get(BM, _) when not is_record(BM, bitmap) -> erlang:error(invalid_bitmap);
get(_, _)                                 -> erlang:error(invalid_index).

-spec foldl(fun(), any(), bitmap()) -> any().
foldl(F, Acc, BM) ->
    foldl(F, Acc, get_width(BM), get_bits(BM)).

-spec foldl(fun(), any(), non_neg_integer(), bitstring()) -> any().
foldl(_F, Acc, _Wd, <<>>) -> Acc;
foldl(F, Acc, Wd, Bits) ->
    << X:Wd, Rest/bits >> = Bits,
    foldl(F, F(X, Acc), Wd, Rest).

-spec merge(bitmap(), bitmap()) -> bitmap().
merge(BM1, BM2)->
    merge(new(?MODULE:length(BM1), get_width(BM1)), BM1, BM2).

-spec merge(bitmap(), bitmap(), bitmap()) -> bitmap().
merge(Acc, BM1, BM2) ->
    F = fun({R1, R2, Idx}, _Acc) -> set(_Acc, Idx, erlang:max(R1,R2) ) end,
    merge(F, Acc, get_width(BM1), get_bits(BM1), get_width(BM2), get_bits(BM2), 0).

-spec merge(fun(), bitmap(), non_neg_integer(), bitstring(), non_neg_integer(), bitstring(), non_neg_integer()) -> bitmap().
merge(_F, Acc, _Wd1, <<>>, _Wd2, <<>>, _) -> Acc;
merge(F, Acc, Wd1, Bits1, Wd2, Bits2, Idx) ->
    << X1:Wd1, Rest1/bits >> = Bits1,
    << X2:Wd2, Rest2/bits >> = Bits2,
    merge(F, F({X1, X2, Idx}, Acc), Wd1, Rest1, Wd2, Rest2, Idx+1).
    
