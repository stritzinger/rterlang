-module(rt_test).

-export([start/1]).
-export([enter_task/1]).
% init

start(N) ->
    lager:start(),
    rt:init(),
    lager:info("Started tasks"),
    Tasks = [ {Id, spawn(?MODULE, enter_task, [Id])} || Id <- lists:seq(1,N)],
    % let them initialize
    timer:sleep(10),
    lager:info("Tasks: ~p", [Tasks]),
    lager:info("Sending messages"),
    lager:info([{rt_start, erlang:monotonic_time(microsecond)}], "GOGOGO"),
    [ Pid ! {self(), test_msg, Id} || {Id, Pid} <- Tasks ].
%    lists:map(fun ({Id, Pid}) -> Pid ! {self, test_msg, Id}  end, Tasks).

enter_task(Id) ->
    rt:make_rt(Id),
    loop(Id).

loop(Id) ->
     receive
	{From, Msg, Id} -> 
	    handle_msg(From, Msg, Id),
	    loop(Id)
    end.

handle_msg(From, Msg, Id) ->
    Deadline = floor(rand:uniform() * 800 * 1000 * 1000), %nanoseconds
    lager:info("Task ~p: ~p received ~p from ~p. Deadline ~p", [Id, self(), Msg, From, Deadline]),
    rt:new_msg(Deadline),
    Exec1 = floor(rand:uniform() * 100),
    Exec2 = floor(rand:uniform() * 100),
    lager:info("Task ~p working for ~p", [Id, Exec1]),
    burn_time(Exec1),
    rt:yield(),
    lager:info("Task ~p working for ~p", [Id, Exec2]),
    burn_time(Exec2),
    rt:done().

burn_time(T) ->
    Start = erlang:monotonic_time(millisecond),
    burn_time(Start, T, Start).

burn_time(Start, T, Now) when Now >= Start + T ->
    ok;

burn_time(Start, T, _Now) ->
    count(0, floor(rand:uniform() * 50000)),
    Now2 = erlang:monotonic_time(millisecond),
    burn_time(Start, T, Now2).

count(H, H) -> ok;
count(L, H) ->
    count(L+1, H).

     
