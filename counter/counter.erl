-module(counter).
-export([start/0, stop/1, get/1, inc/1]).

start() ->
    spawn(counter, core, []).

core() ->
    C = 0,
    loop(C).

loop(C) ->
    receive
	{get, From} ->
	    From ! C,
	    loop(C);
	{inc, From} ->
	    From ! ok,
	    loop(C + 1)
		    
    end.
	
get(Pid) ->	
    Pid ! {get, self()},
    receive
	Result ->
	    io:format("~p~n", [Result])
    end.

inc(Pid) ->
    Pid ! {inc, self()},
    receive
	Result ->
	    io:format("~p~n", [Result])
    end.

stop(Pid) ->
    exit(Pid, "byebye...").
