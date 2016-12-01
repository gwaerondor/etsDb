-module(etsDb).
-export([start/0]).

start() ->
    Tid = ets:new(database, [set, public, {keypos, 1}]),
    spawn(fun() -> register(database, self()), 
		   loop(Tid)
	  end).

loop(Tid) ->
    receive
	{get, Key, Sender} ->
	    Sender ! get_value(Tid, Key);
	{delete, Key, Sender} ->
	    Sender ! delete_value(Tid, Key);
	{set, Key, Value, Sender} ->
	    Sender ! set_value(Tid, Key, Value);
	{transform, Key, Function, Sender} ->
	    Sender ! transform_value(Tid, Key, Function);
	{close, Sender} ->
	    Sender ! close(Tid),
	    exit(closed)
    end,
    loop(Tid).

get_value(Tid, Key) ->
    case ets:lookup(Tid, Key) of
	[{_, Value}] ->
	    Value;
	[] ->
	    {error, "No such key"}
    end.

delete_value(Tid, Key) ->
    ets:delete(Tid, Key),
    ok.

set_value(Tid, Key, Value) ->
    ets:insert(Tid, {Key, Value}),
    ok.

transform_value(Tid, Key, Function) ->
    case ets:lookup(Tid, Key) of
	[{_, Value}] ->
	    perform_transformation(Tid, Key, Function, Value);
	[] ->
	    {error, "No such key"}
    end.

perform_transformation(Tid, Key, Function, Value) ->
    case catch Function(Value) of
	{'EXIT', _} ->
	    {error, "Transformation function not valid for element"};
	Transformed ->
	    ets:insert(Tid, {Key, Transformed}),
	    ok
    end.

close(Tid) ->
    ets:delete(Tid),
    ok.
