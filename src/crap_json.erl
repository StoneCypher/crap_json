
%% @doc World's garbage-est to-JSON encoder.  Uni-directional.  Assumes all lists of integers are 
%% strings.  Assumes all 2-ary proplists are objects.  Assumes all tuples are arrays.  Represents
%% integers and floats.  Converts binaries to strings (!).  Atoms true, false, and null are 
%% converted to the equivalent literals; others catch fire and die (JSON does not have undefined; 
%% thanks Gocy).  Nests sanely from tuples and proplists *only*.  Will flatten improper iolists.
%% Dies violently on any other type (ports, PIDs, references, etc.)
%%
%% The new Erlang type "maps" have not yet been handled.
%%
%% TODO: should probably treat maps and records as objects.  Lazy.

-module(crap_json).





-export([

    to_json/1,

    escape_string/1,
      escape_char/1,

    test/0,
      test/1

]).





-type json_kw()             :: true | false | null.
-type json_array()          :: [] | [json_term()].
-type json_object()         :: [] | [{json_escaped_string(), json_term()}].
-type json_term()           :: Term :: integer() | float() | json_object() | json_array() | binary() | json_kw().
-type json_escaped_string() :: list().
-type json_escaped_binary() :: binary().





-export_type([
    json_kw/0,
    json_array/0,
    json_object/0,
    json_term/0,
    json_escaped_string/0,
    json_escaped_binary/0
]).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Escapes a string for use in JSON; unicode safe. ```1> crap_json:escape_string("a").
%% "a"
%%
%% 2> crap_json:escape_string("abc def").
%% "abc def"
%%
%% 3> crap_json:escape_string("abc \r \n def").
%% "abc \\r \\n def"
%%
%% 4> crap_json:escape_string("汉语").
%% "\\u6C49\\u8BED"'''
%%
%% Nine unit tests (special characters and one Chinese character.)

-spec escape_string(String::list()) -> json_escaped_string().

escape_string(String) ->

    lists:flatten([ escape_char(Char) || Char <- String ]).





%% @doc <span style="color: #0a3; font-weight: bold;">Stoch tested</span> Escapes a single character for use in JSON; unicode safe. ```1> crap_json:escape_char($a).
%% "a"
%%
%% 2> crap_json:escape_char($汉).
%% "\\u6C49"
%%
%% 3> crap_json:escape_char($\r).
%% "\\r"'''
%%
%% Thirteen unit tests (mostly special characters) and one stochastic test (any character escapes to a list.)

-spec escape_char(Char::non_neg_integer()) -> json_escaped_string().

escape_char($\b)            -> "\\b";
escape_char($\f)            -> "\\f";
escape_char($\n)            -> "\\n";
escape_char($\r)            -> "\\r";
escape_char($\t)            -> "\\t";
escape_char($\v)            -> "\\v";
escape_char($\0)            -> "\\0";
escape_char($\\)            -> "\\\\";
escape_char($")             -> "\\\"";
escape_char($')             -> "\\'";
escape_char(C) when C > 255 -> "\\u" ++ integer_to_list(C, 16);
escape_char(C) when C < 32  -> "\\x" ++ integer_to_list(C, 16);
escape_char(OtherChar)     -> [OtherChar].





%% @doc <span style="color: #0a3; font-weight: bold;">Stoch tested</span> Escapes a single character for use in JSON; unicode safe. ```1> crap_json:to_json("a").
%% <<"\"a\"">>
%%
%% 2> crap_json:to_json("abc \r \n def").
%% <<"\"abc \\r \\n def\"">>
%%
%% 3> crap_json:to_json("汉语").
%% <<"\"\\u6C49\\u8BED\"">>
%%
%% 4> crap_json:to_json(1).
%% <<"1">>
%%
%% 5> crap_json:to_json(0.1).      % good handling of rounding error
%% <<"0.1">>
%%
%% 6> crap_json:to_json(true).
%% <<"true">>
%%
%% 7> crap_json:to_json( [ {"height", "2in"}, {"width", "3in"} ]).
%% <<"{\"height\":\"2in\",\"width\":\"3in\"}">>
%%
%% 8> crap_json:to_json( {1,2,3} ).
%% <<"[1,2,3]">>
%%
%% 9> crap_json:to_json( {1, {2,3}, {true,false,null} } ).
%% <<"[1,[2,3],[true,false,null]]">>'''
%%
%% 26 unit tests (special characters, chinese, keywords, various types) and five stochastic tests (any int, float, ascii string, unicode string escapes to a binary; length of result checking.)

-spec to_json(Term :: integer() | float() | tuple() | list() | binary() | json_kw()) -> json_escaped_binary().

to_json(Int) when is_integer(Int) ->

    integer_to_binary(Int);





to_json(Float) when is_float(Float) ->

    list_to_binary(io_lib:format("~w", [Float]));





to_json({}) ->

    <<"[]">>;





to_json(T) when is_tuple(T) ->

    % first doesn't get a comma before it, so special rules
    [ First | Rest ] = tuple_to_list(T),

    binary:list_to_bin([ <<"[">>, to_json(First) ] ++ [ [ <<",">>, to_json(Rem) ] || Rem <- Rest ] ++ [<<"]">>]);





to_json([]) ->

    <<"\"\"">>;





to_json([ {K, _V} | _ ] = List) when is_list(K); is_atom(K); is_binary(K) ->

    LBoAStr = fun    % huhu, String Or Binary -> String = my sob story; forces not-other-types
        (X) when is_list(X)   -> X;
        (X) when is_binary(X) -> binary_to_list(X);
        (X) when is_atom(X)   -> atom_to_list(X)
    end,

    Weld = fun(IK, IV) ->
        SK = to_json(LBoAStr(IK)),
        JV = to_json(IV),
        << SK/binary, <<":">>/binary, JV/binary >>
    end,

    [ _ | BL ] = lists:flatten([ [ <<",">>, Weld(LK, LV) ] || {LK, LV} <- List ]),  % incomplete
    B = binary:list_to_bin(BL),
    
    << <<"{">>/binary, B/binary, <<"}">>/binary >>;





to_json([N | _ ] = List) when is_list(List), is_integer(N) ->

    B = list_to_binary(escape_string(List)),
    << <<"\"">>/binary, B/binary, <<"\"">>/binary >>;





to_json(true)      -> <<"true">>;
to_json(false)     -> <<"false">>;
to_json(null)      -> <<"null">>;





to_json(Binary) when is_binary(Binary) ->

    << <<"\"">>/binary, Binary/binary, <<"\"">>/binary >>.





%% @doc Runs the test cases verbosely. ```1> crap_json:test().
%% ======================== EUnit ========================
%% module 'crap_json'
%%   module 'crap_json_tests'
%%     escape_string tests
%%       crap_json_tests:144: escape_string_test_ (empty string)...ok
%%       crap_json_tests:145: escape_string_test_ (a)...ok
%%       crap_json_tests:146: escape_string_test_ (abc)...ok
%%       crap_json_tests:147: escape_string_test_ (\r)...ok
%%       crap_json_tests:148: escape_string_test_ (\n)...ok
%%       crap_json_tests:149: escape_string_test_ (\r\n)...ok
%%       crap_json_tests:150: escape_string_test_ (a\bc)...ok
%%       crap_json_tests:151: escape_string_test_ (\vc\r)...ok
%%       crap_json_tests:152: escape_string_test_ (BC(zh)DE)...ok
%%       [done in 0.140 s]
%%     escape_char tests
%%       crap_json_tests:164: escape_char_test_ (backspace)...ok
%%       crap_json_tests:165: escape_char_test_ (form feed)...ok
%%       crap_json_tests:166: escape_char_test_ (newline)...ok
%%       crap_json_tests:167: escape_char_test_ (carriage return)...ok
%%       crap_json_tests:168: escape_char_test_ (tab)...ok
%%       crap_json_tests:169: escape_char_test_ (vertical tab)...ok
%%       crap_json_tests:170: escape_char_test_ (null)...ok
%%       crap_json_tests:171: escape_char_test_ (backslash)...ok
%%       crap_json_tests:172: escape_char_test_ (single quote)...ok
%%       crap_json_tests:173: escape_char_test_ (double quote)...ok
%%       crap_json_tests:174: escape_char_test_ (low ascii)...ok
%%       crap_json_tests:175: escape_char_test_ (high unicode)...ok
%%       crap_json_tests:176: escape_char_test_ (capital D)...ok
%%       crap_json_tests:178: escape_char_test_ (Stochastic: any ASCII or unicode character escapes to a list)...[0.047 s] ok
%%       [done in 0.266 s]
%%     to_json tests
%%       crap_json_tests:190: to_json_test_ (true)...ok
%%       crap_json_tests:191: to_json_test_ (false)...ok
%%       crap_json_tests:192: to_json_test_ (null)...ok
%%       crap_json_tests:194: to_json_test_ (Int Zero)...ok
%%       crap_json_tests:195: to_json_test_ (Int Two)...ok
%%       crap_json_tests:196: to_json_test_ (Int Neg Two)...ok
%%       crap_json_tests:198: to_json_test_ (Float Zero)...ok
%%       crap_json_tests:199: to_json_test_ (Float Two)...ok
%%       crap_json_tests:200: to_json_test_ (Float Neg Two)...ok
%%       crap_json_tests:201: to_json_test_ (Float Two Point Five)...ok
%%       crap_json_tests:202: to_json_test_ (Float Neg Two Point Five)...ok
%%       crap_json_tests:203: to_json_test_ (Float Zero Point One)...ok
%%       crap_json_tests:204: to_json_test_ (Float Neg Zero Point One)...ok
%%       crap_json_tests:206: to_json_test_ (Empty string)...ok
%%       crap_json_tests:207: to_json_test_ (ASCII one-char string)...ok
%%       crap_json_tests:208: to_json_test_ (ASCII string)...ok
%%       crap_json_tests:210: to_json_test_ (Empty binary string)...ok
%%       crap_json_tests:211: to_json_test_ (ASCII binary one-char string)...ok
%%       crap_json_tests:212: to_json_test_ (ASCII binary string)...ok
%%       crap_json_tests:214: to_json_test_ (Unicode one-char string)...ok
%%       crap_json_tests:215: to_json_test_ (Unicode string)...ok
%%       crap_json_tests:218: to_json_test_ (PL Obj 1-prop)...ok
%%       crap_json_tests:219: to_json_test_ (PL Obj 2-prop)...ok
%%       crap_json_tests:221: to_json_test_ (Tuple array, empty)...ok
%%       crap_json_tests:222: to_json_test_ ({1,2,3})...ok
%%       crap_json_tests:223: to_json_test_ ({1,true,"foo",{{3,null}}})...ok
%%       crap_json_tests:226: to_json_test_ (Stochastic: any integer yields binary)...ok
%%       crap_json_tests:227: to_json_test_ (Stochastic: any float yields binary)...ok
%%       crap_json_tests:228: to_json_test_ (Stochastic: any ASCII list yields binary)...ok
%%       crap_json_tests:229: to_json_test_ (Stochastic: any Unicode list yields binary)...ok
%%       crap_json_tests:230: to_json_test_ (Stochastic: ASCII list result correct length)...ok
%%       [done in 0.484 s]
%%     [done in 0.890 s]
%%   [done in 0.890 s]
%% =======================================================
%%   All 54 tests passed.
%% ok'''
%%
%% Not test-worthy per se.  The above list may be out of date.  Run time for tests will vary by machine.

-spec test() -> ok | error.

test() ->

    test(verbose).




%% @doc Runs the test cases verbosely or quietly.  Verbosely is as shown above. ```1> crap_json:test(quiet).  
%%   All 54 tests passed.
%% ok'''

-spec test(quiet | verbose) -> ok | error.

test(quiet) ->

     eunit:test(crap_json);





test(verbose) ->

    eunit:test(crap_json, [verbose]).
