
-module(crap_json).





-export([

    to_json/1,

    escape_string/1,

    test/0,
      test/1

]).





%% @doc World's garbage-est to-JSON encoder.  Uni-directional.  Assumes all lists of integers are 
%% strings.  Assumes all 2-ary proplists are objects.  Assumes all tuples are arrays.  Represents
%% integers and floats.  Converts binaries to strings (!).  Atoms true, false, null, and undefined
%% are converted to the equivalent literals; others catch fire and die.  Nests sanely from tuples
%% and proplists *only*.  Will flatten improper iolists.  Dies violently on any other type (ports, 
%% PIDs, references, etc.)
%%
%% TODO: should probably treat maps and records as objects.  Lazy.





escape_string(String) ->

    lists:flatten([ escape_char(Char) || Char <- String ]).





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
escape_char(OtherChar)     -> OtherChar.





to_json(Int) when is_integer(Int) ->

    integer_to_binary(Int);





to_json(Float) when is_float(Float) ->

    list_to_binary(io_lib:format("~w", [Float]));





% whargarbl todo: tuple -> array notation

to_json({}) ->

    <<"[]">>;





to_json([]) ->

    <<"\"\"">>;





to_json([ {K, _V} | _ ] = List) when is_list(K) ->

    SobStr = fun    % huhu, String Or Binary -> String = my sob story; forces not-other-types
        (X) when is_binary(X) -> binary_to_list(X);
        (X) when is_list(X)   -> X
    end,

    Weld = fun(IK, IV) ->
        SK = to_json(SobStr(IK)), 
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
to_json(undefined) -> <<"undefined">>;





to_json(Binary) when is_binary(Binary) ->

    << <<"\"">>/binary, Binary/binary, <<"\"">>/binary >>.





test() ->

    test(verbose).





test(quiet) ->

     eunit:test(crap_json);





test(verbose) ->

    eunit:test(crap_json, [verbose]).
