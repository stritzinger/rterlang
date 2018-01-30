-module(rt).

-export([init/0,
	 make_rt/0, make_rt/1,
	 remove_rt/0,
	 new_msg/1,
	 done/0,
	 yield/0]).

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
    lager:info("T ~p absolute DL ~p", [get(id), Deadline_absolute2]),
    ets:insert(rt_deadline, {Deadline_absolute2, self()}),
    put(deadline, Deadline_absolute2),
    yield().

done() ->
    lager:info("Task ~p is done", [get(id)]),
    ets:delete(rt_active, self()),
    ets:delete(rt_deadline, get(deadline)),
    erlang:process_flag(priority, max),
    try next_to_run() of
	none -> ok;
	Pid -> 
	    N_active = length(ets:lookup(rt_active, Pid)),
	    if
		N_active =:= 0 -> Pid ! '$wakeup';
		true -> ok
	    end
    catch
	throw:table_changed -> well
    end.

% Called repetetively
yield() ->
    lager:info("T ~p ~p yielded", [get(id), self()]),
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
				ok;
			    1 -> ok
			end;
		    _Other ->
			lager:info("T~p ~p entering wakeup receive", [get(id), self()]),
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
	    
		    
		    
	       
	    
%% Self = self(),
%% 	    case lists:map(
%% 			fun (First, Self) -> self;

%% 			ets:lookup(rt_deadline, First) of
%% 		     [] -> throw(table_changed);
%% 		     [{First, self()}] -> self;
%% 		     [{First, Other}] -> Other
%% 		 end,
%% 		 First
%%     end,
%%     lager:info("T~p ~p: Next to run: ~p", [get(id), self(), First]),
%%     First.
