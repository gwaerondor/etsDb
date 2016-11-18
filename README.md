# ets table database
A toy example of a looping process in Erlang that handles simple data operations from a {Key, Value} "database".

## Available operations:
* get
* delete
* save
* transform

## How it works
The database is represented by a simple ets table.

The usefulness of this program is very limited as you could just use ets tables directly in your code anyway without having to call any process.

The only purpose of this program is to demonstrate message passing and ets tables.

The test module etsDb_tests.erl shows examples of how it can be used.

## Compiling
From the root directory of the project, etsDb, run:
```make```
Compiled modules will be output into etsDb/bin/

## Running tests
```make tests```

## Project progress
### Done: Everything.
### Not done: Nothing! This is all you'll get.