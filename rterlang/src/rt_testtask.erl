-module(rt_testtask).

-export([start_link/1,
	 init/2,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

start_link(Name) -> proc_lib:start_link(?MODULE , init, [self(), Name]).

init(Parent, Id) ->
    Name = list_to_atom("task"++integer_to_list(Id)),
    register(Name, self()),
    rt:make_rt(Id),
    proc_lib:init_ack({ok, self()}), loop(Parent).


loop(Parent) ->
    receive
        %% If you enable trap_exit, you also want this clause.
	{'EXIT', Parent, Reason} ->
            exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {state, Parent}),
	    loop(Parent);
	{From, Msg, Id} ->
	    handle_msg(From, Msg, Id),
	    loop(Parent)		
    end.

system_continue(_, _, {state, Parent}) -> loop(Parent).

system_terminate(Reason, _, _, _) -> exit(Reason).

system_code_change(Misc, _, _, _) -> {ok, Misc}.

%----------- Internal ---------------


handle_msg(From, Msg, Id) ->
    Deadline = floor(rand:uniform() * 1000),
    lager:info("Task ~p: ~p received ~p from ~p. Deadline ~p", [Id, self(), Msg, From, Deadline]),
    rt:new_msg(Deadline),
    Exec1 = floor(rand:uniform() * 300),
    Exec2 = floor(rand:uniform() * 300),
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
