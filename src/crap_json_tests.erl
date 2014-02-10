
-module(crap_json_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Tests for `crap_json:`.  You can call `proper_gen:sample( crap_json_tests:gen_ascii_list() )` or 
%% any other `gen_` stochastic generator, if you want to see what it makes.
%%
%% `crap_json:test()` or `crap_json:test(quiet)` to test.  Requires PropEr.  todo whargarbl add link to proper





gen_ascii_char() ->

    proper_types:range(0,255).





gen_ascii_list() ->

    proper_types:list(gen_ascii_char()).





prop_any_int_yields_binary() ->

   ?FORALL( Int, proper_types:integer(), is_binary(crap_json:to_json(Int)) ).





prop_any_float_yields_binary() ->

   ?FORALL( Float, proper_types:float(), is_binary(crap_json:to_json(Float)) ).





prop_any_ascii_yields_binary() ->

   ?FORALL( Ascii, gen_ascii_list(), is_binary(crap_json:to_json(Ascii)) ).





%% @doc post-I-say-post-I-say-post-quoting, that is

expected_length($\b)                        -> 2;
expected_length($\f)                        -> 2;
expected_length($\r)                        -> 2;
expected_length($\n)                        -> 2;
expected_length($\t)                        -> 2;
expected_length($\v)                        -> 2;
expected_length($\0)                        -> 2;
expected_length($\\)                        -> 2;
expected_length($")                         -> 2;
expected_length($')                         -> 2;
expected_length(C) when C < 10              -> 2;
expected_length(C) when C < 32              -> 3;
expected_length(C) when C > 255, C < 1000   -> 4;
expected_length(C) when C > 255, C < 10000  -> 5;
expected_length(C) when C > 255, C < 100000 -> 6;
expected_length(_Char)                      -> 1.





%% @doc Checks that the correct size (origin length plus two for the injected quotes) is maintained.
%% will need a histograph to cope with character expansion

prop_ascii_result_right_length() ->

    ?FORALL( Ascii, gen_ascii_list(), size(crap_json:to_json(Ascii)) == lists:sum([ expected_length(Char) || Char <- Ascii ]) + 2 ).





to_json_test_() ->

    { "to_json tests", [

        { "true",                                         ?_assert( <<"true">>        =:= crap_json:to_json(true)                ) },
        { "false",                                        ?_assert( <<"false">>       =:= crap_json:to_json(false)               ) },
        { "null",                                         ?_assert( <<"null">>        =:= crap_json:to_json(null)                ) },
        { "undefined",                                    ?_assert( <<"undefined">>   =:= crap_json:to_json(undefined)           ) },

        { "Int Zero",                                     ?_assert( <<"0">>           =:= crap_json:to_json(0)                   ) },
        { "Int Two",                                      ?_assert( <<"2">>           =:= crap_json:to_json(2)                   ) },
        { "Int Neg Two",                                  ?_assert( <<"-2">>          =:= crap_json:to_json(-2)                  ) },

        { "Float Zero",                                   ?_assert( <<"0.0">>         =:= crap_json:to_json(0.0)                 ) },
        { "Float Two",                                    ?_assert( <<"2.0">>         =:= crap_json:to_json(2.0)                 ) },
        { "Float Neg Two",                                ?_assert( <<"-2.0">>        =:= crap_json:to_json(-2.0)                ) },
        { "Float Two Point Five",                         ?_assert( <<"2.5">>         =:= crap_json:to_json(2.5)                 ) },
        { "Float Neg Two Point Five",                     ?_assert( <<"-2.5">>        =:= crap_json:to_json(-2.5)                ) },
        { "Float Zero Point One",                         ?_assert( <<"0.1">>         =:= crap_json:to_json(0.1)                 ) },
        { "Float Neg Zero Point One",                     ?_assert( <<"-0.1">>        =:= crap_json:to_json(-0.1)                ) },

        { "Empty string",                                 ?_assert( <<"\"\"">>        =:= crap_json:to_json("")                  ) },
        { "ASCII one-char string",                        ?_assert( <<"\"a\"">>       =:= crap_json:to_json("a")                 ) },
        { "ASCII string",                                 ?_assert( <<"\"abc ABC\"">> =:= crap_json:to_json("abc ABC")           ) },
% todo  { "Chinese one-char string",                      ?_assert( <<"\"abc ABC\"">> =:= crap_json:to_json("abc ABC")           ) }, whargarbl
% todo  { "Chinese string",                               ?_assert( <<"\"abc ABC\"">> =:= crap_json:to_json("abc ABC")           ) },
% todo arabic thai hindi math music emoji mixed-contents stoch-contents + 1ch variants + other oddity + zalgo variants

        { "Stochastic: any integer yields binary",        ?_assert( proper:quickcheck(prop_any_int_yields_binary())     ) },
        { "Stochastic: any float yields binary",          ?_assert( proper:quickcheck(prop_any_float_yields_binary())   ) },
        { "Stochastic: any ASCII list yields binary",     ?_assert( proper:quickcheck(prop_any_ascii_yields_binary())   ) },
        { "Stochastic: ASCII list result correct length", ?_assert( proper:quickcheck(prop_ascii_result_right_length()) ) }

    ] }.