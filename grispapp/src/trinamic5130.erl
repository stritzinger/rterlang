-module(trinamic5130).

-export([
	 write_request/3,
	 read_request/2,
	 request/2,
	 test/0,
	 init/0,
	 move_to/2
	]).

-define(RW_WRITE, 2#1).
-define(RW_READ, 2#0).
-define(SPI_MODE, #{cpol => high, cpha => trailing}).

%% configure_pins(Slot) ->
%%      % Disable chip select for SPI1
%%     grisp_gpio:configure_slot(Slot, disable_cs),
%%     % Configure pin 9 and 10 for output pulled high
%%     grisp_gpio:configure(spi1_pin9, output_1),
%%     grisp_gpio:configure(spi1_pin10, output_1).

%% restore_pins(Slot) -> 
%%     grisp_gpio:configure_slot(Slot, enable_cs),
%%     grisp_gpio:configure(spi1_pin9, input),
%%     grisp_gpio:configure(spi1_pin10, input).


% TODO: Abstract layer for move_to(Degrees)
% Think about architecture of demonstrator
% IR-Receiver all logic should be sufficient

-record(spi_status,
	{
	  status_stop_r,
	  status_stop_l,
	  position_reached,
	  velocity_reached,
	  standstill,
	  sg2,
	  driver_error,
	  reset_flag
	}).

-record(gconf,
	{
	  i_scale_analog = 0,
	  internal_rsense = 0,
	  en_pwm_mode = 0,
	  enc_commutation = 0,
	  shaft = 0,
	  diag0_error = 0,
	  diag0_stall = 0,
	  diag1_stall = 0,
	  diag1_index = 0,
	  diag1_onstate = 0,
	  diag1_steps_skipped = 0,
	  diag0_int_pushpull = 0,
	  diag1_poscomp_pushpull = 0,
	  small_hysteresis = 0,
	  stop_enable = 0,
	  direct_mode = 0,
	  test_mode = 0
	}).

-record(gstat,
	{
	  reset,
	  drv_err,
	  uv_cp
	}).

init() ->
    raw(<<16#0000000000:40>>),
    raw(<<16#0100000000:40>>),
    raw(<<16#6F00000000:40>>),
    raw(<<16#0400000000:40>>),
    raw(<<16#3500000000:40>>),

    raw(<<16#8000000000:40>>),
    raw(<<16#EC000101D5:40>>), % chopconf
    %raw(<<16#9000070603:40>>), %ihold
    raw(<<16#90:8, 0:13, 2#1110:4, 0:3, 2#11110:5, 0:2, 2#10000:5>>),
    raw(<<16#91000000FF:40>>), % tpowerdown
    raw(<<16#F000000000:40>>),
    %raw(<<16#8000000004:40>>), %stealth chop
    %raw(<<16#93000001F4:40>>), %switching velocity
    %raw(<<16#F0000401C8:40>>), %pwm conf switch amplitude

    raw(<<16#A4000003E8:40>>), % 1000 first accel
    raw(<<16#A50000C350:40>>), % V1 = 50k
    raw(<<16#A60000C350:40>>), % AMAX 500
    raw(<<16#A7000186A0:40>>), % VMAX
%    raw(<<16#A8000002BC:40>>), % DMAX 700
    raw(<<16#AA00001578:40>>), % D1 = 1400
    raw(<<16#AB0000000A:40>>), % VSTOP 10
    raw(<<16#A000000000:40>>), % Rampmode 0

    raw(<<16#A100000000:40>>), %xactual 0 
    raw(<<16#AD00000000:40>>). %target 0

move_to(degrees, Degrees) ->
    One_Degree = (512000 / 10) * (1 / 360),
    Val = floor(Degrees * One_Degree),
    raw(<<16#AD:8,Val:32>>).

write_request(Slot, Reg, Val) ->
    request(Slot, <<?RW_WRITE:1, Reg:7, Val:32>>).

read_request(Slot, Reg) ->
    request(Slot, <<?RW_READ:1, Reg:7, 0:32>>).

request(Slot, Request) ->
    grisp_spi:send_recv(Slot, ?SPI_MODE, Request).

test() ->
    % WE GET:
    %% ENC_N_DCO, DRV_ENN_CFG6, ENCA_DCIN_CFG5
    raw(<<16#0000000000:40>>),
    raw(<<16#0100000000:40>>),
    raw(<<16#6F00000000:40>>),
    raw(<<16#0400000000:40>>),
    raw(<<16#3500000000:40>>),

    raw(<<16#8000000000:40>>),
    raw(<<16#EC000101D5:40>>), % chopconf
    raw(<<16#900007061A:40>>), %ihold
    raw(<<16#91000000AA:40>>), % tpowerdown
    raw(<<16#F000000000:40>>),
    %raw(<<16#8000000004:40>>), %stealth chop
    %raw(<<16#93000001F4:40>>), %switching velocity
    %raw(<<16#F0000401C8:40>>), %pwm conf switch amplitude

    raw(<<16#A4000003E8:40>>), % 1000 first accel
    raw(<<16#A5000186A0:40>>), % V1 = 50k
    raw(<<16#A60000C350:40>>), % AMAX 500
    raw(<<16#A7000186A0:40>>), % VMAX
%    raw(<<16#A8000002BC:40>>), % DMAX 700
    raw(<<16#AA00000578:40>>), % D1 = 1400
    raw(<<16#AB0000000A:40>>), % VSTOP 10
    raw(<<16#A000000000:40>>), % Rampmode 0

    raw(<<16#A100000000:40>>), %xactual 0 
    raw(<<16#AD00000000:40>>), %target 0
    
    raw(<<16#AD0000058e:40>>),
    timer:sleep(5000),
    raw(<<16#2100000000:40>>),
    raw(<<16#AD00000000:40>>),
    timer:sleep(5000),
    raw(<<16#2100000000:40>>).

    %% timer:sleep(1000),
    %% io:fwrite("GOGOGOGO!!!"),
    %% raw(<<16#ADFFFF3800:40>>),
    %% raw(<<16#2100000000:40>>),
    %% timer:sleep(20),
    %% raw(<<16#2100000000:40>>),
    %% raw(<<16#2100000000:40>>),
    %% timer:sleep(500),
    %% raw(<<16#2100000000:40>>).

raw(D) ->
    %io:fwrite("Send ~p~n", [D]),
    Res = request(spi2, D).
    %io:fwrite("Got ~p~n~n", [Res]).
    

    
%% register(gconf, Flag, Val, Rec) when is_integer(Val); Val >= 0; Val =< 1 ->
%%     Newrec = Rec#gconf{Flag = Val}.

%% bin_format(Reg, Name) ->
    
%% format_register(gconf, Rec) ->
%%     <<
%%       Rec#gconf.i_scale_analog:1,
%%       Rec#gconf.internal_rsense:1,
%%       Rec#gconf.en_pwm_mode:1,
%%       Rec#gconf.enc_commutation:1,
%%       Rec#gconf.shaft:1,
%%       Rec#gconf.diag0_error:1,
%%       Rec#gconf.diag_otpw:1
%%     >>.

%% parse_register(gconf, Bin)

%% parse_reply(Val) ->
%%     <<Status_stop_r:1,
%%       Status_stop_l:1,
%%       Position_reached:1,
%%       Velocity_reached:1,
%%       Standstill:1,
%%       Sg:1,
%%       Driver_error:1,
%%       Reset_flag:1>> = Val.
