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
	 loop/6,
%	 reset/0,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4]).

-define(STOP_DEG, 7).
-define(ROLL_DEG, 2).
-define(P_VAL_T, 1000*1000*350).
-define(P_VAL_A, 1000*1000*6).
-define(SCHWELLE, 1000*100).

start_link() -> proc_lib:start_link(?MODULE , init, [self()]).

init(Parent) ->
    register(?MODULE, self()),  
    io:fwrite("Opening port~n"),
    P = grisp_ir_drv:open(),
    PinConfig = [pullup, deglitch, debounce, it_rise_edge],
    

    io:fwrite("Registering interrupts"),
    grisp_ir_drv:register_ir(P, spi1_pin9, PinConfig),
    grisp_ir_drv:activate_ir(P, spi1_pin9, 1),
    grisp_ir_drv:register_ir(P, spi1_pin10, PinConfig),
 %   grisp_ir_drv:activate_ir(P, spi1_pin10, 1),
    reset(),
%    io:fwrite("Going into start position"),

 %   io:fwrite("Activating interrupts"),
%    grisp_ir_drv:activate_ir(P, gpio1_3, 1),

    io:fwrite("Init complete, please put the ball into the tube without an impulse!"),
    process_flag(priority, high),
    proc_lib:init_ack({ok, self()}),
    ?MODULE:loop(Parent, P, links, erlang:monotonic_time(microsecond), 0, 0).

reset() ->
     io:fwrite("Initalize motor driver trinamic5130"),
     trinamic5130:init(),
     trinamic5130:move_to(degrees, 1.5).

pcontrol_t(Val) ->
    abs(round(?P_VAL_T / Val)).
pcontrol_a(Val) ->
    abs(round(?P_VAL_A / Val)).
  
loop(Parent, P, Richtung, Time, LastDiff, RollAngle) ->
    receive
        %% If you enable trap_exit, you also want this clause.
	{'EXIT', Parent, Reason} ->
            exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {state, Parent, P, Richtung, Time, LastDiff, RollAngle}),
	    ?MODULE:loop(Parent, P, Richtung, Time, LastDiff, RollAngle);
	{P, {data, <<19>>}} -> 
	    case Richtung of
		links ->
		    Now = erlang:monotonic_time(microsecond),
		    Diff = Now - Time,
		    trinamic5130:move_to(degrees, - pcontrol_a(Diff) - RollAngle),
		    timer:sleep(pcontrol_t(Diff)),
		    if
			Diff - LastDiff >= ?SCHWELLE -> RollAngleNew = RollAngle + 0.1;
			Diff - LastDiff =< -?SCHWELLE -> RollAngleNew = RollAngle - 0.1;
			true -> RollAngleNew = RollAngle
		    end,
		    trinamic5130:move_to(degrees, - ?ROLL_DEG - RollAngleNew),
		    grisp_ir_drv:activate_ir(P, spi1_pin9, 1), 
		    io:fwrite("Korrekur: ~p~n", [RollAngleNew]), 
		    ?MODULE:loop(Parent, P, rechts, Time, Diff, RollAngleNew);
		rechts ->
		    grisp_ir_drv:activate_ir(P, spi1_pin10, 1),
		    Now = erlang:monotonic_time(microsecond),
		    ?MODULE:loop(Parent, P, rechts, Now, LastDiff, RollAngle)
	    end;	    
	{P, {data, <<20>>}} ->
	    case Richtung of
		rechts ->
		    Now = erlang:monotonic_time(microsecond),
		    Diff = Now - Time,		    
		    trinamic5130:move_to(degrees, pcontrol_a(Diff) + RollAngle),
		    
		    timer:sleep(pcontrol_t(Diff)),
		    if
			Diff - LastDiff >= ?SCHWELLE -> RollAngleNew = RollAngle - 0.1;
			Diff - LastDiff =< -?SCHWELLE -> RollAngleNew = RollAngle + 0.1;
			true -> RollAngleNew = RollAngle
		    end,
		    trinamic5130:move_to(degrees, ?ROLL_DEG + RollAngleNew),
		    grisp_ir_drv:activate_ir(P, spi1_pin10, 1),
		    io:fwrite("Korrekur: ~p~n", [RollAngleNew]), 
		    ?MODULE:loop(Parent, P, links, Time, Diff, RollAngleNew);
		links ->
		    grisp_ir_drv:activate_ir(P, spi1_pin9, 1),
		    Now = erlang:monotonic_time(microsecond),
		    ?MODULE:loop(Parent, P, links, Now, LastDiff, RollAngle)
	    end;
	Msg -> io:fwrite("Got message: ~p~n", [Msg]),
	       ?MODULE:loop(Parent, P, Richtung, Time, LastDiff, RollAngle)
    end.

system_continue(_, _, {state, Parent, P, Richtung, Time, Diff, Angle}) -> loop(Parent, P, Richtung, Time, Diff, Angle).

system_terminate(Reason, _, _, _) -> exit(Reason).

system_code_change(Misc, _, _, _) -> {ok, Misc}.

%----------- Internal ---------------

