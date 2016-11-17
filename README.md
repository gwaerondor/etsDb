Simple database process in Erlang
A toy example of a looping process in Erlang that handles simple data operations from a {Key, Value} "database".

Available operations:
* get
* delete
* save
* transform

The database is represented by a simple ets table.

The usefulness of this program is very limited as you could just use ets tables directly in your code anyway. The only purpose of this program is to demonstrate message passing and ets tables.

Done:
* Nothing

Not done:
* Everything