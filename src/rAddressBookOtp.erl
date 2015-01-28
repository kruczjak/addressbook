%%%-------------------------------------------------------------------
%%% @author kruczjak
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2015 3:35 AM
%%%-------------------------------------------------------------------
-module(rAddressBookOtp).
-author("kruczjak").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([addContact/2, addEmail/3, addPhone/3, removeContact/2, removeEmail/1, removePhone/1,
  getEmails/2, getPhones/2, findByEmail/1, findByPhone/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({add_contact, Name, Surname}, _From, List) ->
  check(List, addressBook:addContact(Name, Surname, List));
handle_call({add_email, Name, Surname, Email}, _From, List) ->
  check(List, addressBook:addEmail(Name, Surname, Email, List));
handle_call({add_phone, Name, Surname, Phone}, _From, List) ->
  check(List, addressBook:addPhone(Name, Surname, Phone, List));
handle_call({remove_contact, Name, Surname}, _From, List) ->
  check(List, addressBook:removeContact(Name, Surname, List));
handle_call({remove_email, Email}, _From, List) ->
  check(List, addressBook:removeEmail(Email, List));
handle_call({remove_phone, Phone}, _From, List) ->
  check(List, addressBook:removePhone(Phone, List));
handle_call({get_emails, Name, Surname}, _From, List) ->
  {reply, addressBook:getEmails(Name, Surname, List), List};
handle_call({get_phones, Name, Surname}, _From, List) ->
  {reply, addressBook:getPhones(Name, Surname, List), List};
handle_call({get_by_email, Email}, _From, List) ->
  {reply, addressBook:findByEmail(Email, List), List};
handle_call({get_by_phone, Phone}, _From, List) ->
  {reply, addressBook:findByPhone(Phone, List), List}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

addContact(Name, Surname) ->
  gen_server:call(?MODULE, {add_contact, Name, Surname}).

addEmail(Name, Surname, Email) ->
  gen_server:call(?MODULE, {add_email, Name, Surname, Email}).

addPhone(Name, Surname, Phone) ->
  gen_server:call(?MODULE, {add_phone, Name, Surname, Phone}).

removeContact(Name, Surname) ->
  gen_server:call(?MODULE, {remove_contact, Name, Surname}).

removeEmail(Email) ->
  gen_server:call(?MODULE, {remove_email, Email}).

removePhone(Phone) ->
  gen_server:call(?MODULE, {remove_phone, Phone}).

getEmails(Name, Surname) ->
  gen_server:call(?MODULE, {get_emails, Name, Surname}).

getPhones(Name, Surname) ->
  gen_server:call(?MODULE, {get_phones, Name, Surname}).

findByEmail(Email) ->
  gen_server:call(?MODULE, {get_by_email, Email}).

findByPhone(Phone) ->
  gen_server:call(?MODULE, {get_by_phone, Phone}).

check(AddressBook, {error, Description}) ->
  {reply, {error, Description}, AddressBook};
check(_AddressBook, NewAddressBook) ->
  {reply, ok, NewAddressBook}.
