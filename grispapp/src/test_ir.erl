-module(test_ir).

-export([test/1]).

test(Attr) ->
    io:fwrite("Starting port~n"),
    P = grisp_ir_drv:open(),
    io:fwrite("Register IR~n"),
    grisp_ir_drv:configure_ir(P, gpio1_3, Attr),
    io:fwrite("Listening~n"),
    grisp_ir_drv:rec().
