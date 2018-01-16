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
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1},
	   [
	    {sched, {rt_sched, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [rt_scheduler]},

	    {rttest, {rttest, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [rttest]},

	    {rttest2, {rttest2, start_link, []},
	     permanent,
	     1000,
	     worker,
	     [rttest2]}
]} }.

%%====================================================================
%% Internal functions
%%====================================================================
