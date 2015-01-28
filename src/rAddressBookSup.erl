%%%-------------------------------------------------------------------
%%% @author paulina
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2015 3:30 AM
%%%-------------------------------------------------------------------
-module(rAddressBookSup).
-author("paulina").

%% API
-export([start_link/0, stop/0]).
%-export([init/0, loop/0]).

start_link() ->
  register(addressBookSup, spawn(?MODULE, init, [])).

init() ->
  rAddressBookSup:loop().

loop() ->
  process_flag(trap_exit, true),
  rAddressBook:start(),
  receive
    {'EXIT', _, _} ->
      rAddressBookSup:loop();
    stop -> ok
  end.

stop() -> addressBookSup ! stop.