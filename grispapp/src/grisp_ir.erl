-module(grisp_ir).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([start_link/0]).
-export([configure_ir/2]).
-export([remove_ir/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {driver}).
% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, grisp_ir_drv, []).

%configure(Pin, Type) -> configure(Pin, Type, [default]).

configure_ir(Pin, Attr) ->
    Command = <<(grisp_pin:index(Pin)):8, 1:8, (grisp_pin:map_type(input)):8, (grisp_pin:map_attr(Attr)):8>>,
    gen_server:call(?MODULE, {command, Command}).

remove_ir(Pin) ->
    Command = <<(grisp_pin:index(Pin)):8, 2:8>>,
    gen_server:call(?MODULE, {command, Command}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({command, Command}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Result = DriverMod:command(Ref, Command),
    {reply, grisp_pin:bool(Result), State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------


