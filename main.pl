:- use_module(system_tda).
:- use_module(content).
:- use_module(element).
:- use_module(drive).
:- use_module(datestring).

listaVacia([]) :- true.
listaVacia([_ | _]) :- false.

getTargetPath(Path, [Drive | Rest]) :-
  split_string(Path, "/", "", [H | Rest]),
  atom_chars(H, [Letter | _]),
  atom_string(Letter, Drive).

equal(A, A):- true.
equal(_, _) :- false.

partesLista([H | T], H, T).

cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash, [Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash]).
element(Type, Name, Extra, Content, Security, CDate, MDate, Passkey ,[Type, Name, Extra, Content, Security, CDate, MDate, Passkey]).



file(Name, Content, File) :-
  element("file", Name, [], Content, [], "hora", "hora", 12345678902345678912345678, File).



%--- Funciones
% Dominio: String X System
% Descripción: Entrega un sistema con el nombre entregado
% Método: n/a
system(Name, NewSystem) :-
  getDateString(Date),
  cSystem(Name, [], [], [], [], [], Date, Date, [], NewSystem).

% Dominio: System X String X String or Char X int X System
% Descripción: Agrega un drive al sistema la información entregada
% Método: n/a
systemAddDrive(System, _, Letter, _, NewSystem) :-
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(Letter, Letters),
  setSystemDrives(System, Drives, NewSystem).

systemAddDrive(System, Name, Letter, Capacity, NewSystem) :-
  getSystemDrives(System, Drives),
  append([[Letter, Name, Capacity, []]], Drives, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Registra un usuario en el sistema
% Método: n/a
systemRegister(System, User, NewSystem) :-
  getSystemUsers(System, Users),
  \+ member(User, Users),
  append([User], Users, NewUsers),
  setSystemUsers(System, NewUsers, NewSystem).

systemRegister(System, _, System).

% Dominio: System X String X System
% Descripción: Inicia sesión en el sistema con el usuario entregado
% Método: n/a
systemLogin(System, User, NewSystem) :-
  getSystemUsers(System, Users),
  member(User, Users),
  setSystemActualU(System, User, NewSystem).

systemLogin(System, _, System).

% Dominio: System X System
% Descripción: Cierra la sesión del usuario actual en el sistema
% Método: 
systemLogout(System, NewSystem) :-
  getSystemActualU(System, User),
  \+ listaVacia(User),
  setSystemActualU(System, [], NewSystem).

% Dominio: System X String or Char, System
% Descripción: Selecciona el drive a ser utilizado en el sistema
% Método: n/a
systemSwitchDrive(System, Letter, NewSystem) :-
  getSystemActualU(System, U),
  \+ listaVacia(U),
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(Letter, Letters),
  setSystemActualD(System, [Letter], NewSystemMid),
  setSystemActualR(NewSystemMid, [Letter], NewSystem).

% Dominio: System X String X System
% Descripción: Añade una carpeta en el sistema con el nombre entregado, en la ruta actual
% Método: n/a
systemMkdir(System, Name, NewSystem) :-
  getSystemActualU(System, U),
  getSystemActualR(System, Route),
  element("folder", Name, U, [], [], "Date", "Date", 12345678901234567890, NewElement),
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, NewElement, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Cambia la ruta actual del sistema a partir de la ruta entregada
% Método: n/a
systemCd(System, Path, NewSystem) :-
  equal(Path, "/"),
  getSystemActualD(System, D),
  setSystemActualR(System, D, NewSystem).

systemCd(System, Path, NewSystem) :-
  split_string(Path, "/", "", PathList),
  clearPath(PathList, CleanPathList),
  setSystemPath(System, CleanPathList, NewSystem).

% Dominio: System X File X System
% Descripción: Agrega un archivo al sistema en la ruta actual.
% Método: n/a
systemAddFile(System, File, NewSystem) :-
  getSystemActualR(System, Route),
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, File, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Elimina todos elementos que cumplan el patrón entregado, en la ruta actual del sistema
% Método: n/a
systemDel(System, Pattern, NewSystem) :-
  getSystemActualR(System, Route),
  getElementsFromSystem(System, Pattern, Route, Elements),
  filterElementsFromSystem(System, Pattern, Route, NewInnerSystem),
  sendToTrash(NewInnerSystem, Elements, Route, NewSystem).

% Dominio: System X String X String X System
% Descripción: Copia todos los elementos que cumplan el patrón en la ruta actual del sistema, a la ruta entregada.
% Método: n/a
systemCopy(System, Pattern, Target, NewSystem) :-
  getSystemActualR(System, Route),
  getElementsFromSystem(System, Pattern, Route, Elements),
  getTargetPath(Target, TargetList),
  clearPath(TargetList, CleanTargetList),
  checkRouteExistsSystem(System, CleanTargetList),
  addElementsToSystem(System, Elements, CleanTargetList, NewSystem).

% Dominio: System X String X String X System
% Descripción: Mueve todos los elementos que cumplan el patrón en la ruta actual del sistema, a la ruta entregada.
% Método: n/a
systemMove(System, Pattern, Target, NewSystem) :-
  getSystemActualR(System, Route),
  getElementsFromSystem(System, Pattern, Route, Elements),
  \+ listaVacia(Elements),
  filterElementsFromSystem(System, Pattern, Route, NewInnerSystem),
  getTargetPath(Target, TargetList),
  clearPath(TargetList, CleanTargetList),
  addElementsToSystem(NewInnerSystem, Elements, CleanTargetList, NewSystem).

% Dominio: System X String X String X System
% Descripción: Renombra un elemento
% Método: n/a
systemRen(System, Name, NewName, NewSystem) :-
  getSystemActualR(System, Route),
  renameElementFromSystem(System, Name, NewName, Route, NewSystem).

% Dominio: System X List(String) X String
% Descripción: Entrega un texto formateado con los elementos
% Método: n/a
systemDir(System, Params, String) :-
  getSystemActualR(System, Route),
  getStringFromSystem(System, Params, Route, String).

% Dominio: System X Char X String X System
% Descripción: Formatea el drive con la letra entregada
% Método: n/a
systemFormat(System, Letter, Name, NewSystem) :-
  formatSystem(System, Letter, Name, NewSystem).