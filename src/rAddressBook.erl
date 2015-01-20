-module(rAddressBook).
-export([start/0,
  stop/0,
  addContact/2,
  addEmail/3,
  addPhone/3,
  removeContact/2,
  removeEmail/1,
  removePhone/1,
  getEmails/2,
  getPhones/2,
  findByEmail/1,
  findByPhone/1,
  addGroup/3,
  findGroup/1,
  crash/0]).
-behaviour(gen_server).
-record(contact, {first_name, last_name, phone=[], email =[], group=[]}).

addContact(First_name, Last_name) ->
  addressBookS ! {addContact, self(), First_name, Last_name}.

addEmail(First_name, Last_name, Email) ->
  addressBookS ! {addEmail, self(), First_name, Last_name, Email}.

addPhone(First_name, Last_name, Phone) ->
  addressBookS ! {addPhone, self(), First_name, Last_name, Phone}.

removeContact(First_name, Last_name) ->
  addressBookS ! {removeContact, self(), First_name, Last_name}.

removeEmail(Email) ->
  addressBookS ! {removeEmail, self(), Email}.

removePhone(Phone) ->
  addressBookS ! {removePhone, self(), Phone}.

getEmails(First_name, Last_name) ->
  addressBookS ! {getEmails, self(), First_name, Last_name}.

getPhones(First_name, Last_name) ->
  addressBookS ! {getPhones, self(), First_name, Last_name}.

findByEmail(Email) ->
  addressBookS ! {findByEmail, self(), Email}.

findByPhone(Phone) ->
  addressBookS ! {findByPhone, self(), Phone}.
%%grupy
addGroup(First_name, Last_name, Group) ->
  addressBookS ! {addGroup, self(), First_name, Last_name, Group}.

findGroup(Group) ->
  addressBookS ! {findGroup, self(), Group}.

start() ->
  register(addressBookS, spawn(?MODULE, init, [])).

stop() ->
  exit("rAddressBook", kill).

crash() ->
  addressBookS ! crash.

init() ->
  loop(addressBook:createAddressBook()).

loop(List) ->
  receive
    {addContact, Pid, First_name, Last_name} ->
      loop(checkError(List, Pid, addressBook:addContact(First_name, Last_name, List)));
    {addEmail, Pid, First_name, Last_name, Email} ->
      loop(checkError(List, Pid, addressBook:addEmail(First_name, Last_name, Email, List)));
    {addPhone, Pid, First_name, Last_name, Phone} ->
      loop(checkError(List, Pid, addressBook:addPhone(First_name, Last_name, Phone, List)));
    {removeContact, Pid, First_name, Last_name} ->
      loop(checkError(List, Pid, addressBook:removeContact(First_name, Last_name, List)));
    {removeEmail, Pid, Email} ->
      loop(checkError(List, Pid, addressBook:removeEmail(Email, List)));
    {removePhone, Pid, Phone} ->
      loop(checkError(List, Pid, addressBook:removePhone(Phone, List)));
    {get_emails, Pid, Name, Surname} ->
      Pid ! addressBook:getEmails(Name, Surname, List),
      loop(List);
    {get_phones, Pid, Name, Surname} ->
      Pid ! addressBook:getPhones(Name, Surname, List),
      loop(List);
    {get_by_email, Pid, Email} ->
      Pid ! addressBook:findByEmail(Email, List),
      loop(List);
    {get_by_phone, Pid, Phone} ->
      Pid ! addressBook:findByPhone(Phone, List),
      loop(List);
    crash ->
      2/0
  end.

checkError(List, Pid, {error, Desc}) ->
  Pid ! {error, Desc},
  List;
checkError(List, Pid, NewList) ->
  Pid ! ok,
  NewList.