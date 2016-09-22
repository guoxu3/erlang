-module(send_and_receive).
-export([prob/1, prob_1_a/0, prob_1_b/0, prob_2_a/0, prob_2_b/0, prob_3_a/0, prob_3_b/0, prob_4_a/0, prob_4_b/0,  prob_5_a/0, prob_5_b/0, prob_6_a/0, prob_6_b/0, prob_7_a/0, prob_7_b/0, prob_8_a/0, prob_8_b/1, prob_8_c/0, prob_9_a/0, prob_9_b/0]).

prob(1) ->
    spawn(send_and_receive, prob_1_a, []);
prob(2) ->
    spawn(send_and_receive, prob_2_a, []);
prob(3) ->
    spawn(send_and_receive, prob_3_a, []);
prob(4) ->
    spawn(send_and_receive, prob_4_a, []);
prob(5) ->
    spawn(send_and_receive, prob_5_a, []);
prob(6) ->
    spawn(send_and_receive, prob_6_a, []);
prob(7) ->
    spawn(send_and_receive, prob_7_a, []);
prob(8) ->
    spawn(send_and_receive, prob_8_a, []);
prob(9) ->
    spawn(send_and_receive, prob_9_a, []).


%% -------prob 1-----------
prob_1_a() ->
    spawn(send_and_receive, prob_1_b, []).

prob_1_b() ->
    io:format("hello~n",[]),
    timer:sleep(1000),
    prob_1_b().

%% -------prob 2-----------
prob_2_a() ->
    Pid2 = spawn(send_and_receive, prob_2_b, []),
    Pid2 ! ping.

prob_2_b() ->
    receive
	ping ->
	    io:format("pong~n",[])
    end.

%% -------prob 3-----------
prob_3_a() ->
    Pid3 = spawn(send_and_receive, prob_3_b, []),
    Pid3 ! {self(), ping},
    receive
	Msg1 ->
	    io:format("~p~n",[Msg1])
    end.
    
prob_3_b() ->
    receive
	{From,ping} ->
	    From ! pong
	    %%	    io:format("pong~n",[])
    end.

%% -------prob 4-----------
prob_4_a() ->
    Pid4 = spawn(send_and_receive, prob_4_b, []),
    timer:sleep(10000),
    exit(Pid4,"byebye").

prob_4_b() ->
    io:format("hello~n",[]),
    timer:sleep(1000),
    prob_4_b().


%% -------prob 5-----------
prob_5_a() ->
    Pid5 = spawn(send_and_receive, prob_5_b, []),
    timer:sleep(10000),
    Pid5 ! stop.

prob_5_b() ->
    io:format("hello~n",[]),
    timer:sleep(1000),
    receive
	stop ->
	    ignore
    after 0 ->
	   prob_5_b() 
    end.

%% -------prob 6-----------
prob_6_a() ->
    Pid6 = spawn(send_and_receive, prob_6_b, []),
    timer:sleep(10000),
    exit(Pid6,"byebye"),
    Ref = monitor(process, Pid6),
    receive
	{'DOWN', Ref, process, Pid6, Why} ->
	    io:format("~p~n", [Why])
    end.

prob_6_b() ->
    io:format("hello~n",[]),
    timer:sleep(1000),
    prob_6_b().

%% -------prob 7-----------
prob_7_a() ->
    Pid7 = spawn(send_and_receive, prob_7_b, []),
    link(Pid7),
    timer:sleep(10000),
    exit(Pid7,"byebye"),
    timer:sleep(10000),
    io:format("process A~n", []).
    

prob_7_b() ->
    io:format("hello,b~n",[]),
    timer:sleep(1000),
    prob_7_b().


%% -------prob 8-----------
prob_8_a() ->
    prob_8_b(100000).

prob_8_b(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Max allowed processes:~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> prob_8_c() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    io:format("Process spwan time= ~p (~p) microseconds~n", [U1, U2]).
			   

prob_8_c() ->
    receive
	die ->
    ignore
    end.

for(N, N, F) ->
    [F()];
for(I, N, F) ->
    [F() | for(I+1, N, F)].

%% -------prob 9-----------
prob_9_a() ->
    Pid9 = spawn(send_and_receive, prob_9_b, []),
    timer:sleep(10000),
    exit(Pid9,"byebye"),
    Ref = monitor(process, Pid9),
    receive
	{'DOWN', Ref, process, Pid9, Why} ->
	    io:format("~p~n", [Why]),
	    prob_9_a()
    end.

prob_9_b() ->
    io:format("hello~n",[]),
    timer:sleep(1000),
    prob_9_b().
