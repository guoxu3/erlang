-module(use_lists).
-export([print_lists/1, lists_map/1]).

print(X) ->
    io:format("~p~n", [X]).

print_lists(L) ->
    lists:foreach(fun print/1, L).

lists_map(Li) ->
    lists:map(fun(X) -> X * X end, Li).
