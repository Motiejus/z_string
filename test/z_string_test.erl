-module(z_string_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

z_string_test_() ->
    [
        {"z_string:sanitize_utf8 -> unicode.erl",
            proper_utils:qc_(s_utf8a())},
        {"unicode.erl -> z_string:sanitize_utf8",
            proper_utils:qc_(s_utf8b())},
        {"For every utf8 binary unicode.erl and z_string:sanitize_utf8",
            proper_utils:qc_(s_utf8c())}
    ].

%% @doc For every random binary if s_utf8 claims it's utf8,
%% unicode.erl agrees
s_utf8a() ->
    ?FORALL(
        ProbablyUtf8,
        ?SUCHTHAT(B, binary(), z_string:sanitize_utf8(B) == B),
        unicode:characters_to_binary(ProbablyUtf8) =:= ProbablyUtf8
    ).

%% @doc For every random binary if unicode.erl claims it's utf8,
%% s_utf agrees
s_utf8b() ->
    ?FORALL(
        ProbablyUtf8,
        ?SUCHTHAT(
            B,
            binary(),
            unicode:characters_to_binary(B) == B),
        z_string:sanitize_utf8(ProbablyUtf8) =:= ProbablyUtf8
    ).

s_utf8c() ->
    ?FORALL(
        Utf8,
        utf8_string(),
        case unicode:characters_to_binary(Utf8) == Utf8 of
            false ->
                io:format("Wrong generator! ~p~n", [Utf8]),
                false;
            true ->
                z_string:sanitize_utf8(Utf8) =:= Utf8
        end
    ).

%% @doc PropErly generate valid utf8 string of various lengths
utf8_string() ->
    ?LET(
        CodePoints,
        ?LET(Len, binary_len(), vector(Len, utf8_codepoint())),
        iolist_to_binary(CodePoints)
    ).

%% @doc Valid sub-UTF-8 code point binary (1-3 byte length)
utf8_codepoint() ->
    ?LET(
        {Codepoint, Octets},
        union([
                {integer(0, 16#7F), 1},
                {integer(16#80, 16#7FF), 2},
                {integer(16#800, 16#D7FF), 3},
                {integer(16#E000, 16#FFFD), 3},
                {integer(16#10000, 16#10FFFF), 4}
            ]),
        case Octets of
            1 ->
                <<Codepoint:8>>;
            2 ->
                <<A:5, B:6>> = <<Codepoint:11>>,
                <<2#110:3, A:5, 2#10:2, B:6>>;
            3 ->
                <<A:4, B:6, C:6>> = <<Codepoint:16>>,
                <<2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6>>;
            4 ->
                <<A:3, B:6, C:6, D:6>> = <<Codepoint:21>>,
                <<2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6>>
        end
    ).

%% @doc Create binaries of length 0..2^12 bytes (0 - 4KiB)
%%
%% To generate a binary we first generate a Number := uniform(0, 11).
%% Then Length := uniform(1 << Number, 1 << (Number + 1))
binary_len() ->
    ?LET(
        Len,
        integer(0, 11),
        integer(1 bsl Len, 1 bsl (Len + 1))
    ).