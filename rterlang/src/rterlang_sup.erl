%%%-------------------------------------------------------------------
%% @doc rterlang top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rterlang_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    process_flag(priority, max),
    {ok, { #{strategy => one_for_all,
	     intensity => 1,
	     period => 1},
	   %% [
	   %%  {sched, {rt_sched, start_link, []},
	   %%   permanent,
	   %%   1000,
	   %%   worker,
	   %%   [rt_scheduler]},

	   %%  {rttest, {rttest, start_link, []},
	   %%   permanent,
	   %%   1000,
	   %%   worker,
	   %%   [rttest]},

	   %%  {rttest2, {rttest2, start_link, []},
	   %%   permanent,
	   %%   1000,
	   %%   worker,
	   %%   [rttest2]}
	   [
	    {rt_watchdog, {rt_watchdog, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [rt_watchdog]},
	    {echotask, {echotask, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [echotask]},
	    {disklogger, {disklogger, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [disklogger]}
	    ]	   
	 }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
