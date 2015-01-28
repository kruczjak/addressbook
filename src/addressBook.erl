-module(addressBook).
-export([createAddressBook/0,
  addContact/3,
  addEmail/4,
  addPhone/4,
  removeContact/3,
  removeEmail/2,
  removePhone/2,
  getEmails/3,
  getPhones/3,
  findByEmail/2,
  findByPhone/2,
  addGroup/4,
  findGroup/2]).

-record(contact, {first_name, last_name, phone=[], email =[], group=[]}).

createAddressBook() -> [].

addContact(First_name, Last_name, List) ->
  case lists:any(fun(One) -> One == #contact{first_name = First_name, last_name = Last_name} end, List) of
    true -> List;
    false -> lists:append(List, [#contact{first_name = First_name, last_name = Last_name}])
  end.

emailUnique(_, []) -> true;
emailUnique(Email, [H|T]) ->
  case emailUniqueForContact(Email, H#contact.email) of
    true -> emailUnique(Email, T);
    false -> false
  end.

emailUniqueForContact(Email, List) -> not lists:any(fun(One) -> One == Email end, List).

addEmail(First_name, Last_name, Email, AB) ->
  case emailUnique(Email, AB) of
    true -> actuallyAddEmail(First_name, Last_name, Email, AB);
    false -> {error,"Email already exists"}
  end.
actuallyAddEmail(First_name, Last_name, Email, []) ->
  [#contact{first_name =First_name, last_name =Last_name, email =[Email]}];
actuallyAddEmail(First_name, Last_name, Email, [H=#contact{first_name =First_name, last_name =Last_name}|T]) ->
  [#contact{first_name =First_name, last_name =Last_name,phone=H#contact.phone, email =H#contact.email ++[Email]}|T];
actuallyAddEmail(First_name, Last_name, Email, [H|T]) -> [H|actuallyAddEmail(First_name, Last_name, Email, T)].

phoneUnique(_, []) -> true;
phoneUnique(Phone, [H|T]) ->
  case phoneUniqueForContact(Phone, H#contact.phone) of
    true -> phoneUnique(Phone, T);
    false -> false
  end.

phoneUniqueForContact(Phone, List) -> not lists:any(fun(One) -> One == Phone end, List).

addPhone(First_name, Last_name, Phone, AB) ->
  case phoneUnique(Phone, AB) of
    true -> actuallyAddPhone(First_name, Last_name, Phone, AB);
    false -> {error,"Phone already exists"}
  end.
actuallyAddPhone(First_name, Last_name, Phone, []) ->[#contact{first_name =First_name, last_name =Last_name, phone=[Phone]}];
actuallyAddPhone(First_name, Last_name, Phone, [H=#contact{first_name =First_name, last_name =Last_name}|T]) ->
  [#contact{first_name =First_name, last_name =Last_name,phone=H#contact.phone++[Phone], email =H#contact.email}|T];
actuallyAddPhone(First_name, Last_name, Phone, [H|T]) -> [H|actuallyAddPhone(First_name, Last_name, Phone, T)].

removeContact(First_name, Last_name, List) ->
  List -- [X || X<-List, X#contact.first_name =:= First_name, X#contact.last_name =:= Last_name].

removeEmail(_, []) -> [];
removeEmail(Email, [H|T]) -> [#contact{first_name =H#contact.first_name, last_name =H#contact.last_name, phone=H#contact.phone, email =[X || X<-H#contact.email, X/=Email]}|removeEmail(Email, T)].

removePhone(_, []) -> [];
removePhone(Phone, [H|T]) -> [#contact{first_name =H#contact.first_name, last_name =H#contact.last_name, phone=[X || X<-H#contact.phone, X/=Phone], email =H#contact.email}|removePhone(Phone, T)].

getEmails(_, _, []) -> {error, "Not found"};
getEmails(First_name, Last_name, [H=#contact{first_name =First_name, last_name =Last_name}|_]) -> H#contact.email;
getEmails(First_name, Last_name, [_|T]) -> getEmails(First_name, Last_name, T).

getPhones(_, _, []) -> {error, "Not found"};
getPhones(First_name, Last_name, [H=#contact{first_name =First_name, last_name =Last_name}|_]) -> H#contact.phone;
getPhones(First_name, Last_name, [_|T]) -> getPhones(First_name, Last_name, T).

findByEmail(_,[]) -> {error,"Not found"};
findByEmail(Email,[H|T]) ->
  case emailUniqueForContact(Email, H#contact.email) of
    false -> {H#contact.first_name, H#contact.last_name};
    true -> findByEmail(Email, T)
  end.

findByPhone(_,[]) -> {error,"Not found"};
findByPhone(Phone,[H|T]) ->
  case phoneUniqueForContact(Phone, H#contact.phone) of
    false -> {H#contact.first_name, H#contact.last_name};
    true -> findByPhone(Phone, T)
  end.

%%grupy
addGroup(First_name, Last_name, Group, List) ->
  case groupUnique(First_name, Last_name, Group, List) of
    true -> actuallyAddGroup(First_name, Last_name, Group, List);
    false -> {error,"Group already exists"}
  end.

groupUnique(First_name, Last_name, Group, List) ->
  [H] = lists:filter(fun(One) -> One#contact.first_name =:= First_name andalso One#contact.last_name =:= Last_name end,List),
  not lists:any(fun(One) -> One == Group end, H#contact.group).


actuallyAddGroup(First_name, Last_name, Group, []) ->
  [#contact{first_name =First_name, last_name =Last_name, group =[Group]}];
actuallyAddGroup(First_name, Last_name, Group, [H=#contact{first_name =First_name, last_name =Last_name}|T]) ->
  [#contact{first_name =First_name, last_name =Last_name,phone=H#contact.phone, email=H#contact.email, group =H#contact.group ++[Group]}|T];
actuallyAddGroup(First_name, Last_name, Group, [H|T]) -> [H|actuallyAddGroup(First_name, Last_name, Group, T)].

findGroup(Group, List) ->
  lists:filter(fun(One) -> lists:any(fun(Second) -> Second =:= Group end, One#contact.group) end, List).

