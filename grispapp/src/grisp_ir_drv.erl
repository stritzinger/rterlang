% @private
-module(grisp_ir_drv).

% API
-export([open/0]).
-export([rec/1]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_ir_drv"}, [binary]).

rec(Port) ->
%    Port ! {self(), {command, Command}},
    io:fwrite("Listening........"),
    receive
        {Port, Resp} ->
            io:fwrite("Got ~p~n", [Resp]),
	    rec(Port);
	Msg -> io:fwrite("Got other ~p~n", [Msg])
    end.
