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
    receive
        {Port, {data, Resp}} ->
            io:fwrite("Got ~p~n", [Resp])
%    after ?PORT_COMMAND_TIMEOUT ->
						%    exit({gpio_driver_timeout, Command})
    end.
