-module(test_ir).

-export([test/0, test/2]).

test() ->
    test(gpio1_3, [pullup, deglitch, debounce, it_fall_edge]).

test(Pin, Attr) ->
    io:fwrite("Starting port~n"),
    P = grisp_ir_drv:open(),
    io:fwrite("Register IR~n"),
    grisp_ir_drv:register_ir(P, Pin, Attr),
    io:fwrite("Activate IR~n"),
    grisp_ir_drv:activate_ir(P, Pin, 3),
    io:fwrite("Listening~n"),
    rec().

rec() ->
    receive
	Msg ->
	    io:fwrite("~p~n", [Msg]),
	    rec()
    end.
