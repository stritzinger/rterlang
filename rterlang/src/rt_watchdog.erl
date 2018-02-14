-module(rt_watchdog).

-export([start_link/0,
	 init/1,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

-define(INTERVAL, 5). %milliseconds
-define(NANONOW, {now, erlang:monotonic_time(microsecond)}).

% tasks:
%   * prevent system overload?!? -> make sure also "normal" processes can run, otherwise mailboxes become full
%      * Watchdog checks rt_deadline regularly

start_link() -> proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) -> 
%    process_flag(trap_exit, true),
    process_flag(priority, max),
    ets:new(rt_deadline, [ordered_set, named_table, public]),
    ets:new(rt_queued, [set, named_table, public]),
    ets:new(rt_active, [set, named_table, public]),
    ets:new(rt_processes, [set, named_table, public]),
    lager:info("Created ETS tabled for RT"),
    Timer = timer:send_after(?INTERVAL, self(), woof),
    proc_lib:init_ack({ok, self()}), loop(Parent, Timer).

loop(Parent, Timer) ->
    receive
        %% If you enable trap_exit, you also want this clause.
        {'EXIT', Parent, Reason} ->
            exit(Reason);
	{'DOWN', _MonitorRef, process, From, Info} ->
	    lager:warn([{watchdog_received_down_from, From},
			{watchdog_received_down_info, Info},
			{watchdog_received_down_when, ?NANONOW}],
		       "Received DOWN from ~p. Reaseon ~p~n", [From, Info]),
	    ets:delete(rt_processes, {From}),
	    ets:delete(rt_active, {From}),
% traverse table
	    Lookup = ets:lookup(rt_queued, From),
	    if
		length(Lookup) =:= 0 ->
		    ':-/'; %!
		true -> 
		    {_Pid, Deadline} = lists:last(Lookup),
		    ets:delete(rt_deadline, {Deadline})
	    end,
	    ets:delete(rt_queued, self()),
	    First = ets:first(rt_active),
	    if
		First =:= '$end_of_table' ->
		    rt:try_wakeup();
		true ->
		    ok
	    end,
	    loop(Parent, Timer);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {state, Parent}),
	    loop(Parent, Timer);
	%% {From, watch} -> 
	%%     Monitor_Ref = monitor(process, From),
	%%     ets:insert(rt_processes, From, MonitorRef);
	%% {From, unwatch} ->
	%%     ets:lookup(rt_processes, 
	%%     unlink(From),
	%%     ets:delete(rt_processes, From);
	{woof} -> 
	    erlang:cancel_timer(Timer),
	    lager:info([{woof, ?NANONOW}], "Watchdog: woof"),
	    rt:kill_missed_deadlines(),
	    New_Timer = erlang:send_after(?INTERVAL, self(), woof),
	    loop(Parent, New_Timer)
    end.

system_continue(_, _, {state, Parent, Loop}) -> loop(Parent, Loop).

system_terminate(Reason, _, _, _) -> exit(Reason).

system_code_change(Misc, _, _, _) -> {ok, Misc}.
		    
       
	
