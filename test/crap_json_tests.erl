-module(crap_json_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Tests for `crap_json:`.  You can call `proper_gen:sample( crap_json_tests:gen_ascii_list() )` or 
%% any other `gen_` stochastic generator, if you want to see what it makes.
%%
%% `crap_json:test()` or `crap_json:test(quiet)` to test.  Tests require [PropEr](https://github.com/manopapad/proper).
%%
%% @TODO whargarbl still wants single language test cases





gen_ascii_char() ->

    proper_types:range(0,255).





gen_ascii_list() ->

    proper_types:list(gen_ascii_char()).





gen_unicode_char() ->

    % http://en.wikipedia.org/wiki/List_of_Unicode_characters
    % All valid ranges, weighted by size, so that individual characters are equivalently likely

    proper_types:weighted_union( [

        {16, proper_types:range(16#000000, 16#00ffff)}, % basic multilingual plane
        {4,  proper_types:range(16#010000, 16#013fff)}, % supplementary multilingual plane A
        {1,  proper_types:range(16#016000, 16#016fff)}, % supplementary multilingual plane B
        {1,  proper_types:range(16#01b000, 16#01bfff)}, % supplementary multilingual plane C
        {3,  proper_types:range(16#01d000, 16#01ffff)}, % supplementary multilingual plane D
        {12, proper_types:range(16#020000, 16#02bfff)}, % supplementary ideographic plane A
        {1,  proper_types:range(16#02f000, 16#02ffff)}, % supplementary ideographic plane B
        {1,  proper_types:range(16#0e0000, 16#0e0fff)}, % supplementary special purpose plane
        {16, proper_types:range(16#0f0000, 16#0fffff)}, % supplementary private use area A
        {16, proper_types:range(16#100000, 16#10ffff)}  % supplementary private use area b

    ] ).





gen_unicode_list() ->

    proper_types:list(gen_unicode_char()).





prop_any_int_yields_binary() ->

   ?FORALL( Int, proper_types:integer(), is_binary(crap_json:to_json(Int)) ).





prop_any_float_yields_binary() ->

   ?FORALL( Float, proper_types:float(), is_binary(crap_json:to_json(Float)) ).





prop_any_ascii_yields_binary() ->

   ?FORALL( Ascii, gen_ascii_list(), is_binary(crap_json:to_json(Ascii)) ).





prop_any_unicode_yields_binary() ->

   ?FORALL( Unicode, gen_unicode_list(), is_binary(crap_json:to_json(Unicode)) ).





prop_any_unicode_character_encodes_to_a_list() ->

   ?FORALL( UnicodeChar, gen_unicode_char(), is_list(crap_json:escape_char(UnicodeChar)) ).





%% @doc post-I-say-post-I-say-post-quoting, that is

expected_length($\b)                             -> 2;
expected_length($\f)                             -> 2;
expected_length($\r)                             -> 2;
expected_length($\n)                             -> 2;
expected_length($\t)                             -> 2;
expected_length($\v)                             -> 2;
expected_length($\0)                             -> 2;
expected_length($\\)                             -> 2;
expected_length($")                              -> 2;
expected_length($')                              -> 2;
expected_length(C) when C < 16#10                -> 3;
expected_length(C) when C < 16#20                -> 4;
expected_length(C) when C > 16#ff, C < 16#1000   -> 5;
expected_length(C) when C > 16#ff, C < 16#10000  -> 6;
expected_length(C) when C > 16#ff, C < 16#100000 -> 7;
expected_length(_Char)                           -> 1.





%% @doc Checks that the correct size (origin length plus two for the injected quotes) is maintained.
%% will need a histograph to cope with character expansion

prop_ascii_result_right_length() ->

    ?FORALL( Ascii, gen_ascii_list(), size(crap_json:to_json(Ascii)) == lists:sum([ expected_length(Char) || Char <- Ascii ]) + 2 ).





escape_string_test_() ->

    { "escape_string tests", [

        { "empty string", ?_assert( ""            =:= crap_json:escape_string("")                  ) },
        { "a",            ?_assert( "a"           =:= crap_json:escape_string("a")                 ) },
        { "abc",          ?_assert( "abc"         =:= crap_json:escape_string("abc")               ) },
        { "\\r",          ?_assert( "\\r"         =:= crap_json:escape_string("\r")                ) },
        { "\\n",          ?_assert( "\\n"         =:= crap_json:escape_string("\n")                ) },
        { "\\r\\n",       ?_assert( "\\r\\n"      =:= crap_json:escape_string("\r\n")              ) },
        { "a\\bc",        ?_assert( "a\\bc"       =:= crap_json:escape_string("a\bc")              ) },
        { "\\vc\\r",      ?_assert( "\\vc\\r"     =:= crap_json:escape_string("\vc\r")             ) },
        { "BC(zh)DE",     ?_assert( "BC\\u6F22DE" =:= crap_json:escape_string([66,67,28450,68,69]) ) }  % chinese

    ] }.





escape_char_test_() ->

    { "escape_char tests", [

        { "backspace",       ?_assert( "\\b"    =:= crap_json:escape_char($\b) ) },
        { "form feed",       ?_assert( "\\f"    =:= crap_json:escape_char($\f) ) },
        { "newline",         ?_assert( "\\n"    =:= crap_json:escape_char($\n) ) },
        { "carriage return", ?_assert( "\\r"    =:= crap_json:escape_char($\r) ) },
        { "tab",             ?_assert( "\\t"    =:= crap_json:escape_char($\t) ) },
        { "vertical tab",    ?_assert( "\\v"    =:= crap_json:escape_char($\v) ) },
        { "null",            ?_assert( "\\0"    =:= crap_json:escape_char($\0) ) },
        { "backslash",       ?_assert( "\\\\"   =:= crap_json:escape_char($\\) ) },
        { "single quote",    ?_assert( "\\\'"   =:= crap_json:escape_char($\') ) },
        { "double quote",    ?_assert( "\\\""   =:= crap_json:escape_char($\") ) },
        { "low ascii",       ?_assert( "\\xE"   =:= crap_json:escape_char(14)  ) },
        { "high unicode",    ?_assert( "\\u12C" =:= crap_json:escape_char(300) ) },
        { "capital D",       ?_assert( "D"      =:= crap_json:escape_char($D)  ) },

        { "Stochastic: any ASCII or unicode character escapes to a list", ?_assert( proper:quickcheck(prop_any_unicode_character_encodes_to_a_list()) ) }

    ] }.





to_json_test_() ->

    { "to_json tests", [

        { "true",                                         ?_assert( <<"true">>                        =:= crap_json:to_json(true)                      ) },
        { "false",                                        ?_assert( <<"false">>                       =:= crap_json:to_json(false)                     ) },
        { "null",                                         ?_assert( <<"null">>                        =:= crap_json:to_json(null)                      ) },

        { "Int Zero",                                     ?_assert( <<"0">>                           =:= crap_json:to_json(0)                         ) },
        { "Int Two",                                      ?_assert( <<"2">>                           =:= crap_json:to_json(2)                         ) },
        { "Int Neg Two",                                  ?_assert( <<"-2">>                          =:= crap_json:to_json(-2)                        ) },

        { "Float Zero",                                   ?_assert( <<"0.0">>                         =:= crap_json:to_json(0.0)                       ) },
        { "Float Two",                                    ?_assert( <<"2.0">>                         =:= crap_json:to_json(2.0)                       ) },
        { "Float Neg Two",                                ?_assert( <<"-2.0">>                        =:= crap_json:to_json(-2.0)                      ) },
        { "Float Two Point Five",                         ?_assert( <<"2.5">>                         =:= crap_json:to_json(2.5)                       ) },
        { "Float Neg Two Point Five",                     ?_assert( <<"-2.5">>                        =:= crap_json:to_json(-2.5)                      ) },
        { "Float Zero Point One",                         ?_assert( <<"0.1">>                         =:= crap_json:to_json(0.1)                       ) },
        { "Float Neg Zero Point One",                     ?_assert( <<"-0.1">>                        =:= crap_json:to_json(-0.1)                      ) },

        { "Empty string",                                 ?_assert( <<"\"\"">>                        =:= crap_json:to_json("")                        ) },
        { "ASCII one-char string",                        ?_assert( <<"\"a\"">>                       =:= crap_json:to_json("a")                       ) },
        { "ASCII string",                                 ?_assert( <<"\"abc ABC\"">>                 =:= crap_json:to_json("abc ABC")                 ) },

        { "Unicode one-char string",                      ?_assert( <<"\"\\u6F22\"">>                 =:= crap_json:to_json([28450])                   ) }, % chinese
        { "Unicode string",                               ?_assert( <<"\"\\u6C49\\u8BED\"">>          =:= crap_json:to_json([27721,35821])             ) }, % chinese
% todo arabic thai hindi math music emoji mixed-contents stoch-contents + 1ch variants + other oddity + zalgo variants

        { "PL Obj 1-prop",                                ?_assert( <<"{\"a\":\"b\"}">>               =:= crap_json:to_json([{"a","b"}])               ) },
        { "PL Obj 2-prop",                                ?_assert( <<"{\"a\":\"b\",\"c\":\"d\"}">>   =:= crap_json:to_json([{"a","b"},{"c","d"}])     ) },

        { "Tuple array, empty",                           ?_assert( <<"[]">>                          =:= crap_json:to_json({})                        ) },
        { "{1,2,3}",                                      ?_assert( <<"[1,2,3]">>                     =:= crap_json:to_json({1,2,3})                   ) },
        { "{1,true,\"foo\",{{3,null}}}",                  ?_assert( <<"[1,true,\"foo\",[[3,null]]]">> =:= crap_json:to_json({1,true,"foo",{{3,null}}}) ) },


        { "Stochastic: any integer yields binary",        ?_assert( proper:quickcheck(prop_any_int_yields_binary())                                    ) },
        { "Stochastic: any float yields binary",          ?_assert( proper:quickcheck(prop_any_float_yields_binary())                                  ) },
        { "Stochastic: any ASCII list yields binary",     ?_assert( proper:quickcheck(prop_any_ascii_yields_binary())                                  ) },
        { "Stochastic: any Unicode list yields binary",   ?_assert( proper:quickcheck(prop_any_unicode_yields_binary())                                ) },
        { "Stochastic: ASCII list result correct length", ?_assert( proper:quickcheck(prop_ascii_result_right_length())                                ) }

    ] }.
