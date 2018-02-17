% @doc mygrispapp top level supervisor.
% @end
-module(grispapp_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 10, 10},
		   [
		    %% {echotask, {echotask, start_link, []},
		    %%  permanent,
		    %%  1000,
		    %%  worker,
		    %%  [echotask]},
		    %% {disklogger, {disklogger, start_link, []},
		    %%  permanent,
		    %%  1000,
		    %%  worker,
		    %%  [disklogger]}
		     {seesaw, {seesaw, start_link, []},
		      permanent,
		      5000,
		      worker,
		      [seesaw]}
		   ]	   
		 }
	    }.
