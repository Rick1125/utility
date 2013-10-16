-module(timer).
-export([start_link/0, init/0, now/0, now/1, days/0, week/1]).

start_link()->
  Pid = spawn_link(?MODULE, init,[]),
  {ok, Pid}.

init()->
  ets:new(?MODULE, [set,protected,named_table]),
  loop().

loop()->
  {M,S,_} = erlang:now(),
  N = M*1000*1000 + S,
  ets:insert(?MODULE, {now, N}),
  ets:insert(?MODULE, {now_str, time_now_str()}),
  ets:insert(?MODULE, {days, get_days()}),
  timer:sleep(1000),
  loop().

get_days()->
  {D, _} = calendar:local_time(),
  calendar:date_to_gregorian_days(D).

days()->
  case ets:lookup(?MODULE, days) of
    [{_,N}] -> N;
    _ -> get_days()
  end.

now(str)->
  case ets:lookup(?MODULE, now_str) of
    [{now,N}] -> N;
    _ -> time_now_str()
  end.

now()->
  case ets:lookup(?MODULE, now) of
    [{now,N}] -> N;
    _ -> 0
  end.

time_now_str()->
  {{Y,M, D}, {H,I,S}} = calendar:local_time(),
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B::~2.10.0B:~2.10.0B:~2.10.0B", 
    [Y, M, D, H, I, S]).

week(1)-> <<"Mon">>;
week(2)-> <<"Tur">>;
week(3)-> <<"Wed">>;
week(4)-> <<"Thu">>;
week(5)-> <<"Fri">>;
week(6)-> <<"Sat">>;
week(7)-> <<"Sun">>;
week(0)-> <<"Sun">>.

