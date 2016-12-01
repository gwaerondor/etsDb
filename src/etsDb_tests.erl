-module(etsDb_tests).
-include_lib("eunit/include/eunit.hrl").

database_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"It should be possible to get the value of a key that has been set",
       fun it_should_be_possible_to_get_value/0},
      {"Trying to get a value of an unset key should return an error",
       fun getting_unset_key_should_return_error/0},
      {"It should be possible to set an unset key",
       fun it_should_be_possible_to_set_an_unset_key/0},
      {"Setting an already set key should overwrite the value",
       fun setting_a_set_key_should_update_value/0},
      {"It should be possible to delete an entry from the database",
       fun it_should_be_possible_to_delete_entry/0},
      {"Trying to delete a key that is not in the database should work",
       fun trying_to_delete_non_existing_key_should_work/0},
      {"It should be possible to transform the value of a given key",
       fun transforming_value_should_work/0},
      {"Transformation resulting in a crash should return an error",
       fun transforming_with_crash_should_return_error/0},
      {"Transformation should not work on a non-existing entry",
       fun transforming_non_existing_should_not_work/0}]
    }.

setup() ->
    etsDb:start(),
    timer:sleep(1),
    database ! {set, a, 1, self()},
    database ! {set, b, 2, self()},
    database ! {set, c, 3, self()}.

cleanup(_) ->
    database ! {close, self()},
    exit(whereis(database), kill).

it_should_be_possible_to_get_value() ->
    database ! {get, a, self()},
    Result = receive_response(),
    Expected = 1,
    ?assertEqual(Expected, Result).

getting_unset_key_should_return_error() ->
    database ! {get, f, self()},
    Result = receive_response(),
    Expected = {error, "No such key"},	
    ?assertEqual(Expected, Result).

it_should_be_possible_to_set_an_unset_key() ->
    Message = "This is my new value!",
    database ! {set, new_key, Message, self()},
    Set_result = receive_response(),
    ?assertEqual(ok, Set_result),
    database ! {get, new_key, self()},
    Get_result = receive_response(),
    ?assertEqual(Message, Get_result).    

setting_a_set_key_should_update_value() ->
    Message = "Out of the way, 2",
    database ! {set, b, Message, self()},
    Set_result = receive_response(),
    ?assertEqual(ok, Set_result),
    database ! {get, b, self()},
    Get_result = receive_response(),
    ?assertEqual(Message, Get_result).

it_should_be_possible_to_delete_entry() ->
    database ! {delete, c, self()},
    Delete_result = receive_response(),
    ?assertEqual(ok, Delete_result),
    database ! {get, c, self()},
    Get_result = receive_response(),
    Expected = {error, "No such key"},
    ?assertEqual(Expected, Get_result).

trying_to_delete_non_existing_key_should_work() ->
    database ! {delete, i_am_not_a_real_key, self()},
    Delete_result = receive_response(),
    ?assertEqual(ok, Delete_result).

transforming_value_should_work() ->
    Transform = fun(X) ->
			integer_to_list(X) ++ " bottles of beer on the wall"
		end,
    database ! {transform, b, Transform, self()},
    Transform_result = receive_response(),
    ?assertEqual(ok, Transform_result),
    database ! {get, b, self()},
    Get_result = receive_response(),
    Expected = "2 bottles of beer on the wall",
    ?assertEqual(Expected, Get_result).    

transforming_with_crash_should_return_error() ->
    Transform = fun(X) ->
			X/0
		end,
    database ! {transform, a, Transform, self()},
    Transform_expected = {error, "Transformation function not valid for element"},
    Transform_result = receive_response(),
    ?assertEqual(Transform_expected, Transform_result),
    database ! {get, a, self()},
    Get_result = receive_response(),
    Get_expected = 1,
    ?assertEqual(Get_expected, Get_result).    

transforming_non_existing_should_not_work() ->
    Transform = fun(X) ->
			io_lib:format("~p~n", [X])
		end,
    database ! {transform, d, Transform, self()},
    Result = receive_response(),
    Expected = {error, "No such key"},
    ?assertEqual(Expected, Result).			

receive_response() ->
    receive
	X ->
	    X
    after 100 ->
	    {error, "No response from database"}
    end.
	    
