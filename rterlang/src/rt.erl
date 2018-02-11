-module(rt).

-export([make_rt/0, make_rt/1,
	 remove_rt/0,
	 new_msg/1,
	 done/0,
	 yield/0,
	 kill_missed_deadlines/0,
	 next_to_run/0,
	 try_wakeup/0
	]).

-define(NANONOW, {now, erlang:monotonic_time(microsecond)}).

%% rt_deadlines: {deadline, pid}
%% rt_queued: {pid, deadline}
%% rt_active: {pid}
%% rt_processes: {pid}

make_rt() ->
    make_rt(undefined).

make_rt(Id) ->
    put(rt, true),
    put(id, Id),
    ets:insert(rt_processes, self()),
    erlang:process_flag(priority, max).

remove_rt() ->
    put(rt, undefined),
    ets:delete(rt_processes, self()).

new_msg(Deadline) -> 
    % make sure deadline does not exist already
    Now = erlang:monotonic_time(nanosecond),
    Deadline_absolute = Now + Deadline,
    %% case ets:lookup(rt_deadline, Deadline_absolute) of
    %% 	[{Deadline_absolute, _Pid}] ->
    %% 	    Deadline_absolute2 = Deadline_absolute + 1;
    %% 	[] -> 
    %% 	    Deadline_absolute2 = Deadline_absolute
    %% end,
    %we log in microseconds...
    lager:info([{rt_dispatch, get(id)}, {rt_deadline, Deadline_absolute/1000}, ?NANONOW], "T ~p absolute DL ~p", [get(id), Deadline_absolute]),
    ets:insert(rt_deadline, {Deadline_absolute, self()}),
    ets:insert(rt_queued, {self(), Deadline_absolute}),
    put(deadline, Deadline_absolute),
    yield().

done() ->
    lager:info([{rt_done, get(id)}, ?NANONOW], "Task ~p is done", [get(id)]),
    kill_missed_deadlines(),
    garbage_collect(),
    ets:delete(rt_active, self()),
    ets:delete(rt_deadline, get(deadline)),
    ets:delete(rt_queued, self()),
    erlang:process_flag(priority, max),
    % DID WE MISS DEADLINE? -> IF YES EXIT 
    try_wakeup().

% Called repetetively
yield() ->
    lager:info([{rt_yield, get(id)}, ?NANONOW], "T ~p ~p yielded", [get(id), self()]),
    kill_missed_deadlines(),
    case get(rt) of
	true -> 
	    % Check if there are missed Deadlines, send kills
	    Self = self(),	    
	    try next_to_run() of
		none -> notok;
		Self ->
		    Self = self(),
		    Lookup = ets:lookup(rt_active, Self),
		    case length(Lookup) of
			0 ->
			    ets:insert(rt_active, {self()}),
			    erlang:process_flag(priority, high),
			    lager:info([{rt_run, get(id)}, ?NANONOW], "T ~p running", [get(id)]),
			    ok;
			1 -> 
			    ok
		    end;
		_Other ->
		    lager:info("T~p Removing from active list", [get(id)]),
		    ets:delete(rt_active, self()),
		    lager:info([{rt_sleep, get(id)}, ?NANONOW], "T~p ~p entering wakeup receive", [get(id), self()]),
		    receive
			'$wakeup' ->
			    lager:info("T~p ~p received wakeup, yield()..." , [get(id), self()]),
			    yield()
		    end
	    catch
		throw:table_changed -> yield() %start from scratch
	    end;
	undefined -> false
    end.

next_to_run() ->
    case ets:lookup(rt_deadline, ets:first(rt_deadline)) of
	[] -> none;
	[{_Deadline, Pid}] -> 
	    lager:info("Task ~p: Self: ~ Other: ~p", [get(id), self(), Pid]),
	    Pid
    end.

try_wakeup() ->
    case next_to_run() of
	none -> ok;
	Pid -> 
	    lager:info("Task ~p checks if ~p is already active", [get(id), Pid]),
	    N_active = length(ets:lookup(rt_active, Pid)),
	    if
		N_active =:= 0 -> 
		    lager:info("Task ~p is not active yet, sending wakeup", [Pid]),
		    Pid ! '$wakeup';
		true -> 
		    lager:info("~p is already running", [Pid]),
		    ok
	    end
    end.

kill_missed_deadlines() ->
    Now = erlang:monotonic_time(nanosecond),
    case ets:lookup(rt_deadline, ets:first(rt_deadline)) of
	[] ->
	    ok;
	[{Deadline, Pid}] ->
		if
		    Deadline - Now >= 0 ->
			lager:warning([{rt_missed_deadline, Pid}, ?NANONOW], "Task ~p missed deadline, sending kill", [Pid]),
			ets:delete(rt_deadlines, Deadline),
			ets:delete(rt_active, Pid),
			ets:delete(rt_processes, Pid),
			exit(Pid, kill), % if Pid =:= self() it's over here, otherwise there might be more
			kill_missed_deadlines();
		    true ->
			ok
		end
    end.
