-module(bitmap_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Bitmap_0_0 = bitmap:new(0, 0),
    ?assertEqual(0, bitmap:get_width(Bitmap_0_0)),
    ?assertEqual(<<>>, bitmap:get_bits(Bitmap_0_0)),
    ?assertEqual(0, bitmap:size(Bitmap_0_0)),
    ?assertEqual(0, bitmap:length(Bitmap_0_0)),
    Bitmap_1_0 = bitmap:new(1, 0),
    ?assertEqual(0, bitmap:get_width(Bitmap_1_0)),
    ?assertEqual(<<>>, bitmap:get_bits(Bitmap_1_0)),
    ?assertEqual(0, bitmap:size(Bitmap_1_0)),
    ?assertEqual(0, bitmap:length(Bitmap_1_0)),
    Bitmap_0_1 = bitmap:new(0, 1),
    ?assertEqual(0, bitmap:get_width(Bitmap_0_1)),
    ?assertEqual(<<>>, bitmap:get_bits(Bitmap_0_1)),
    ?assertEqual(0, bitmap:size(Bitmap_0_1)),
    ?assertEqual(0, bitmap:length(Bitmap_0_1)),
    Bitmap_2_3 = bitmap:new(2, 3),
    ?assertEqual(3, bitmap:get_width(Bitmap_2_3)),
    ?assertEqual(<<0:3, 0:3>> , bitmap:get_bits(Bitmap_2_3)),
    ?assertEqual(6, bitmap:size(Bitmap_2_3)),
    ?assertEqual(2, bitmap:length(Bitmap_2_3)).

get_set_test() ->
    Idx = 65520,
    N = 65536,
    New = bitmap:new(N, 16),
    Bit_16 = 16#FFFF,
    Bit_17 = Bit_16 bsl 1, %% overflow
    Bitmap = bitmap:set(New, Idx, Bit_16),
    Bitmap1 = bitmap:set(New, Idx, Bit_17),
    ?assertEqual(0,        bitmap:get(Bitmap, Idx-1)),
    ?assertEqual(Bit_16,   bitmap:get(Bitmap, Idx)),
    ?assertEqual(Bit_16-1, bitmap:get(Bitmap1, Idx)),
    ?assertEqual(0,        bitmap:get(Bitmap, Idx+1)).

foldl_test() ->
    Bitmap0 = bitmap:new(),
    Bitmap = bitmap:set(bitmap:new(65536, 16), 0, 99),
    SumF = fun(V, Acc) -> V + Acc end,
    ?assertEqual(99, bitmap:foldl(SumF, 0, Bitmap)),
    ?assertEqual(initial, bitmap:foldl(SumF, initial, Bitmap0)).

negative_test() ->
    %% new
    ?assertError(negative_length, bitmap:new(-1, 0)),
    ?assertError(negative_width, bitmap:new(0, -1)),
    %% accessors
    Idx = 65520,
    N = 65536,
    New = bitmap:new(N, 16),
    Bit_16 = 16#FFFF,
    Bitmap = bitmap:set(New, Idx, Bit_16),

    ?assertError(invalid_bitmap, bitmap:set(wrong_arg, Idx, 123)),
    ?assertError(invalid_index,  bitmap:set(Bitmap, -1, 123)),
    ?assertError(invalid_index,  bitmap:set(Bitmap, N, 123)),

    ?assertError(invalid_bitmap, bitmap:get(wrong_arg, Idx)),
    ?assertError(invalid_index,  bitmap:get(Bitmap, -1)),
    ?assertError(invalid_index,  bitmap:get(Bitmap, N)).
