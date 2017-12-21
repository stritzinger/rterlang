% @doc mygrispapp public API.
% @end
-module(mygrispapp).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> 
	{ok, _Pid} = elli:start_link([{callback, elli_minimal_callback}, {port, 3000}]),
	mygrispapp_sup:start_link().

stop(_State) -> ok.
