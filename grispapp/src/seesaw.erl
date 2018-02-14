%%%-------------------------------------------------------------------
%%% @author kilian <kilian@kili4n2>
%%% @copyright (C) 2018, kilian
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2018 by kilian <kilian@kili4n2>
%%%-------------------------------------------------------------------
-module(seesaw).

-export([start_link/0,
	 init/1,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

-define(DEGREES, 5).

start_link() -> proc_lib:start_link(?MODULE , init, [self()]).

init(Parent) ->
    register(?MODULE, self()),  
    io:fwrite("Opening port~n"),
    P = grisp_ir_drv:open(),
    PinConfig = [pullup, deglitch, debounce, it_rise_edge],
    io:fwrite("Registering interrupts"),
    grisp_ir_drv:register_ir(P, gpio1_3, PinConfig),
    grisp_ir_drv:register_ir(P, gpio1_4, PinConfig),
    io:fwrite("Initalize motor driver trinamic5130"),
    trinamic5130:init(),
    io:fwrite("Going into start position"),
    trinamic5130:init(degrees, ?DEGREES)
    io:fwrite("Activating interrupts"),
%    grisp_ir_drv:activate_ir(P, gpio1_3, 1),
    grisp_ir_drv:activate_ir(P, gpio1_4, 1),
    io:fwrite("Init complete, please put the ball into the tube without an impulse!"),
    rt:make_rt(1),
    proc_lib:init_ack({ok, self()}),
    loop(Parent, P).



loop(Parent, P) ->
    receive
        %% If you enable trap_exit, you also want this clause.
	{'EXIT', Parent, Reason} ->
            exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {state, Parent}),
	    loop(Parent);
	{P, {data, <<3>>}} -> 
	    trinamic5130:move_to(degrees, ?DEGREES),
	    grisp_ir_drv:activate_ir(P, gpio1_4, 1),
	    loop(Parent, P);
	{P, {data, <<4>>}} ->
	    trinamic5130:move_to(degrees, -?DEGREES),
	    grisp_ir_drv:activate_ir(P, gpio1_3, 1),
	    loop(Parent, P)
    end.


system_continue(_, _, {state, Parent}) -> loop(Parent).

system_terminate(Reason, _, _, _) -> exit(Reason).

system_code_change(Misc, _, _, _) -> {ok, Misc}.

%----------- Internal ---------------

