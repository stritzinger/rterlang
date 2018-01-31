-module(rt).

-export([init/0,
	 make_rt/0, make_rt/1,
	 remove_rt/0,
	 new_msg/1,
	 done/0,
	 yield/0]).

-define(NANONOW, {now, erlang:monotonic_time(microsecond)}).

%% rt_deadlines: {deadline, pid}
%% rt_active: {pid}

init() ->
    ets:new(rt_deadline, [ordered_set, named_table, public]),
    ets:new(rt_active, [set, named_table, public]).

make_rt() ->
    make_rt(undefined).

make_rt(Id) ->
    put(rt, true),
    put(id, Id),
    erlang:process_flag(priority, max).

remove_rt() ->
    put(rt, undefined).

new_msg(Deadline) -> 
    % make sure deadline does not exist already
    Now = erlang:monotonic_time(nanosecond),
    Deadline_absolute = Now + Deadline,
    case ets:lookup(rt_deadline, Deadline_absolute) of
	[{Deadline_absolute}] -> Deadline_absolute2 = Deadline_absolute + 1;
	[] -> Deadline_absolute2 = Deadline_absolute
    end,
    %we log in microseconds...
    lager:info([{rt_dispatch, get(id)}, {rt_deadline, Deadline_absolute2/1000}, ?NANONOW], "T ~p absolute DL ~p", [get(id), Deadline_absolute2]),
    ets:insert(rt_deadline, {Deadline_absolute2, self()}),
    put(deadline, Deadline_absolute2),
    yield().

done() ->
    lager:info([{rt_done, get(id)}, ?NANONOW], "Task ~p is done", [get(id)]),
    ets:delete(rt_active, self()),
    ets:delete(rt_deadline, get(deadline)),
    erlang:process_flag(priority, max),
    try next_to_run() of
	none -> ok;
	Pid -> 
	    lager:info("Task ~p checks if ~p is already active", [get(id), Pid]),
	    N_active = length(ets:lookup(rt_active, Pid)),
	    if
		N_active =:= 0 -> 
		    lager:info("Task ~p is not active yet, sending wakeup", [Pid]),
		    Pid ! '$wakeup';
		true -> 
		    lager:info("~p is lready running", [Pid]),
		    ok
	    end
    catch
	throw:table_changed -> well
    end.

% Called repetetively
yield() ->
    lager:info([{rt_yield, get(id)}, ?NANONOW], "T ~p ~p yielded", [get(id), self()]),
    case get(rt) of
	true -> 
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
			1 -> ok
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
    case ets:first(rt_deadline) of
	'$end_of_table' -> none;
	First -> 
	    Lookup = ets:lookup(rt_deadline, First),
	    Contains_Self = lists:keymember(self(), 2, Lookup),
	    Other = element(2, lists:last(ets:lookup(rt_deadline, First))),
	    lager:info("Task ~p: LOOKUP ~p, Self: ~p Other: ~p", [get(id), Lookup, Contains_Self, Other]),
	    if 
		length(Lookup) =:= 0 -> throw(table_changed);
		Contains_Self -> self();
		true -> Other
	    end
    end.
