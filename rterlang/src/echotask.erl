-module(echotask).


-export([start_link/0,
	 init/1,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

start_link() -> proc_lib:start_link(?MODULE , init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    rt:make_rt(1),
    proc_lib:init_ack({ok, self()}),
    loop(Parent).


loop(Parent) ->
    receive
        %% If you enable trap_exit, you also want this clause.
	{'EXIT', Parent, Reason} ->
            exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {state, Parent}),
	    loop(Parent);
	{From, hello, Dispatchtime} ->
	    %% don't invoke yield and stuff
	    Now = erlang:monotonic_time(nanosecond),
	    From ! {self(), done, Dispatchtime, Now},
	    loop(Parent)
    end.

system_continue(_, _, {state, Parent}) -> loop(Parent).

system_terminate(Reason, _, _, _) -> exit(Reason).

system_code_change(Misc, _, _, _) -> {ok, Misc}.

%----------- Internal ---------------
