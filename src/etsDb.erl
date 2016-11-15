-module(etsDb).

start() ->
    Tid = ets:new(database, [global, bag, {keypos, 1}]),
    loop(Tid).

loop(Tid) ->
    receive
	{get, Key, Sender} ->
	    Sender ! get_value(Tid, Key);
	{delete, Key, Sender} ->
	    Sender ! delete_value(Tid, Key);
	{save, Key, Value, Sender} ->
	    Sender ! save_value(Tid, Key, Value);
	{transform, Key, Function, Sender} ->
	    Sender ! transform_value(Tid, Key, Function)
    end,
    loop(Tid).
