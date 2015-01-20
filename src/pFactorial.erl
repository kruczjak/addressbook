%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2014 15:20
%%%-------------------------------------------------------------------
-module(pFactorial).
-author("piotr").

%% API
-export([factorial1/1, factorial2/1, factorial3/1, receiveResult/3, worker/2, manager/2, factorialTest/0]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

factorial1(N) -> [factorial(A) || A <- lists:seq(0, N)].

factorial2(N) -> manager(N, N).

factorial3(N) -> manager(erlang:system_info(logical_processors_available), N).

manager(Pool, N) ->
  Pid = spawn(pFactorial, receiveResult, [self(), Pool, []]),
  [spawn(pFactorial, worker, [Pid, lists:seq(I, N, Pool)]) || I <- lists:seq(0, Pool-1)],
  receive
    Result -> lists:sort(Result)
  end
.

receiveResult(Pid, 0, Result) -> Pid ! Result;
receiveResult(Pid, N, Result) ->
  receive
    Res -> receiveResult(Pid, N-1, Result ++ Res)
  end
.

worker(Pid, Queue) -> Pid ! [factorial(A) || A <- Queue].


factorialTest() ->
  N = 5000,
  {Factorial1, _} = timer:tc(pFactorial, factorial1, [N]),
  {Factorial2, _} = timer:tc(pFactorial, factorial2, [N]),
  {Factorial3, _} = timer:tc(pFactorial, factorial3, [N]),
  {factorial1, Factorial1/1000000, factorial2, Factorial2/1000000, factorial3, Factorial3/1000000}.