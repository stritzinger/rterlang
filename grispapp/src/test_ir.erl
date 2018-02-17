-module(test_ir).

-export([init/0,
	 reg/3, rec/0]).

init() ->
    io:fwrite("Starting port~n"),
    P = grisp_ir_drv:open(),
    Conf = [pullup, deglitch, debounce, it_rise_edge],
    reg(P, spi1_pin9, Conf),
    reg(P, spi1_pin10, Conf).

reg(P, Pin, Attr) ->
    io:fwrite("Register IR~n"),
    grisp_ir_drv:register_ir(P, Pin, Attr),
    io:fwrite("Activate IR~n"),
    grisp_ir_drv:activate_ir(P, Pin, 1).

rec() ->
    receive
	Msg ->
	    io:fwrite("~p~n", [Msg]),
	    rec()
    end.
