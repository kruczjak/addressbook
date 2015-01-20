%%%-------------------------------------------------------------------
%%% @author kruczjak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Dec 2014 2:00 PM
%%%-------------------------------------------------------------------
-module(ser).
-author("kruczjak").
-behaviour(gen_server).
%% API
-export([start_link/1, init/1, handle_call/3, increment/0, decrement/0]).

start_link(Value) ->
  gen_server:start_link({local, ser}, ser, [Value], []).

increment() ->
  gen_server:call(ser, {increment}).

decrement() ->
  gen_server:call(ser, {decrement}).


init(Args) ->
  {ok, Args}.

handle_call({Req}, From, State) ->
  {reply, State, State+1}.
