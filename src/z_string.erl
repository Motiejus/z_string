-module(z_string).

-export([sanitize_utf8/1]).

%% @doc Sanitize an utf-8 string, remove all non-utf-8 characters.
sanitize_utf8(L) when is_list(L) -> sanitize_utf8(iolist_to_binary(L));
sanitize_utf8(B) when is_binary(B) -> s_utf8(B, <<>>).

%% @doc Check if given string is valid utf8 acceptable by emysql
s_utf8(<<>>, Acc) ->
    Acc;

%% 1 byte
s_utf8(<<X, Rest/binary>>, Acc) when X < 128 ->
    s_utf8(Rest, <<Acc/binary, X>>);

%% 2 bytes
s_utf8(<<2#110:3, A:5, 2#10:2, B:6, Rest/binary>>, Acc) when
        <<0:5, A:5, B:6>> >= <<16#80:16>>,
        <<0:5, A:5, B:6>> =< <<16#7FF:16>> ->
    s_utf8(Rest, <<Acc/binary, 2#110:3, A:5, 2#10:2, B:6>>);

%% 3 bytes
s_utf8(<<2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6, Rest/binary>>, Acc) when
        <<0:7, A:5, B:6, C:6>> >= <<16#800:24>> andalso
        <<0:7, A:5, B:6, C:6>> =< <<16#D7FF:24>>
        orelse
        <<0:7, A:5, B:6, C:6>> >= <<16#E000:24>> andalso
        <<0:7, A:5, B:6, C:6>> =< <<16#FFFD:24>> ->
    s_utf8(Rest, <<Acc/binary, 2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6>>);

%% 4 bytes
s_utf8(<<2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6, Rest/binary>>,
        Acc) when
        <<0:3, A:3, B:6, C:6, D:6>> >= <<16#10000:24>> andalso
        <<0:3, A:3, B:6, C:6, D:6>> =< <<16#10FFFF:24>> ->
    s_utf8(Rest,
        <<Acc/binary, 2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6>>);

%% Drop illegal utf-8 character.
s_utf8(<<_, Rest/binary>>, Acc) ->
    s_utf8(Rest, Acc).
