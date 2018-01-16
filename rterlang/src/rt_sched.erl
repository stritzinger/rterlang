%%%-------------------------------------------------------------------
%%% @author kilian <kilian.holzinger@stritzinger.com>
%%% @copyright (C) 2018, kilian
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2018 by kilian <kilian@kili4n2>
%%%-------------------------------------------------------------------
-module(rt_sched).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(rt_task,
	{
	  name = "" :: string(),
	  id :: pid() | {pid(), atom()},
	  deadline :: number(),
	  arrival :: integer()
	}).

-record(state,
	{
	  running :: #rt_task{} | none,
	  waiting :: list(#rt_task{})
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],[{priority_level, max}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{running=none, waiting = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(done, From, State) when State#state.running#rt_task.id =:= element(1, From) ->
    NewState = State#state{running = none},
    io:format("~p is done~n", [From]),
    case length(State#state.waiting) of
	0 -> 
	    io:format("queue is empty~n"),
	    {reply, ok, NewState};
	_ -> 
	    io:format("queue is not empty ~n"),
	    {Next, NextState} = schedule(NewState),
	    Next#rt_task.id ! '$okrun',
	    FinalState = NextState#state{running = Next},
	    io:format("Next to run: ~p~n", [Next]),
	    {reply, ok, FinalState}
    end;
handle_call(Request, From, State) ->
    io:format("Unknown call: ~p From ~p~n", [Request, From]),
    Reply = error,
    throw(undefined),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @Private
%% @Doc
%% Handling cast messages
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({mayrun, TaskId, Deadline}, State) ->
    io:format("mayrun ~p ~p~n", [TaskId, Deadline]),
    NewTask = #rt_task{id = TaskId, deadline = Deadline, arrival = erlang:monotonic_time(nanosecond)},
    case {length(State#state.waiting), State#state.running} of
	{0, none} ->
	    io:format("go!!!~n"),
	    NewState = State#state{running = NewTask},
	    TaskId ! '$okrun',
	    {noreply, NewState};
	_ ->
	    NewState = State#state{waiting = [NewTask|State#state.waiting]},
	    io:format("wait!~n"),
%%	    {reply, wait, NewState}
	    {noreply, NewState}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine what to run next, inform processes about overload if deadline already missed
%%
%% @spec schedule(State) ->  {Next}
%% @end
%%--------------------------------------------------------------------
schedule(State) -> 
    Now = erlang:monotonic_time(nanosecond),
    %% 
    Compare = fun (A, B) -> 
		      if 
			  (Now + A#rt_task.deadline  - A#rt_task.arrival) < (B#rt_task.deadline  - B#rt_task.arrival) ->
			      false;
			  true -> true
		      end
	      end,				

    Sorted = lists:sort(Compare, State#state.waiting),

    Is_Missed = fun (T) -> 
			if
			    (Now + T#rt_task.deadline - T#rt_task.arrival) =< 0 ->
				true;
			    true -> false
			end
		end,
    {Missed, Remaining} = lists:splitwith(Is_Missed, Sorted),
    
    lists:map(fun (T) -> T#rt_task.id ! '$overload' end, Missed),
    [Next | Rest] = Remaining,
    NewState = State#state{waiting = Rest},
    {Next, NewState}.
