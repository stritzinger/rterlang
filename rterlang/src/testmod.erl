-module(testmod).

-export([testrt/0]).

testrt() ->
    io:format("----Cast asd---------------------------------------"),
    gen_rt_server:cast(rttest, asd),
%    timer:sleep(300),
    io:format("----Cast qwertz----------------------------------"),
    gen_rt_server:cast(rttest2, qwerz),
    io:format("TEST DONE!").
