
:- module(system_tda, [getSystemUsers/2, getSystemDrives/2, getSystemActualU/2,
  getSystemActualD/2, getSystemActualR/2, getSystemTrash/2, setSystemUsers/3,
setSystemDrives/3, setSystemActualU/3, setSystemActualD/3, setSystemActualR/3,
getElementsFromSystem/4, addElementsToSystem/4, addElementToSystem/4,
filterElementsFromSystem/4, sendToTrash/4, checkRouteExistsSystem/2, setSystemPath/3,
checkRouteExistsSystem/2, renameElementFromSystem/5, getStringFromSystem/4, encryptSystem/5,
decryptSystem/5, formatSystem/4]).

:- use_module(content).
:- use_module(drive).
:- use_module(datestring).


partesLista( [ H | T], H, T).
equal(A, A).

cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash, [Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash]).

% Dominio: Lista (Átomos) x Lista(Átomos)
% Descripción: Elimina el último elemento de una lista
% Método: Recursión
removeLast([_], []).
removeLast([H|T], [H|Filtered]) :-
  removeLast(T, Filtered).

% Dominio: System x Lista(String)
% Descripción: Obtiene los usuarios registrados de un sistema
% Método: n/a
getSystemUsers(System, Users) :-
  cSystem(_, Users, _, _, _, _, _, _, _, System).

% Dominio: System x Lista(Drive)
% Descripción: Obtiene los Drives de un sistema
% Método: n/a
getSystemDrives(System, Drives) :-
  cSystem(_, _, Drives, _, _, _, _, _, _, System).

% Dominio: System x String
% Descripción: Obtiene el usuario actual de un sistema
% Método: n/a
getSystemActualU(System, ActualU) :-
  cSystem(_, _, _, ActualU, _, _, _, _, _, System).

% Dominio: System x String
% Descripción: Obtiene el drive actual de un sistema
% Método: n/a
getSystemActualD(System, ActualD) :-
  cSystem(_, _, _, _, ActualD, _, _, _, _, System).

% Dominio: System x Lista(String)
% Descripción: Obtiene la ruta actual del sistema
% Método: n/a
getSystemActualR(System, ActualR) :-
  cSystem(_, _, _, _, _, ActualR, _, _, _, System).

% Dominio: System x Trash
% Descripción: Obtiene la papelera del sistema
% Método: n/a
getSystemTrash(System, Trash) :-
  cSystem(_, _, _, _, _, _, _, _, Trash, System).

% Dominio: System x List(String) x System
% Descripción: Reemplaza los usuarios registrados de un sistema
% Método: 
setSystemUsers(System, NewUsers, NewSystem):-
  cSystem(Name, _, Drives, ActualU, ActualD, ActualR, CDate, _, Trash, System),
  getDateString(Date),
  cSystem(Name, NewUsers, Drives, ActualU, ActualD, ActualR, CDate, Date, Trash, NewSystem).

% Dominio: System x List(Drives) x System
% Descripción: Reemplaza la lista de Drives del sistema
% Método: n/a
setSystemDrives(System, NewDrives, NewSystem):-
  cSystem(Name, Users, _, ActualU, ActualD, ActualR, CDate, _, Trash, System),
  getDateString(Date),
  cSystem(Name, Users, NewDrives, ActualU, ActualD, ActualR, CDate, Date, Trash, NewSystem).

% Dominio: System x String x System
% Descripción: Reemplaza el usuario actual del sistema
% Método: n/a
setSystemActualU(System, User, NewSystem):-
  cSystem(Name, Users, Drives, _, ActualD, ActualR, CDate, _, Trash, System),
  getDateString(Date),
  cSystem(Name, Users, Drives, User, ActualD, ActualR, CDate, Date, Trash, NewSystem).

% Dominio: System x String x System
% Descripción: Reemplaza el drive actual del sistema
% Método: n/a
setSystemActualD(System, Drive, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, _, ActualR, CDate, _, Trash, System),
  getDateString(Date),
  cSystem(Name, Users, Drives, ActualU, Drive, ActualR, CDate, Date, Trash, NewSystem).

% Dominio: System x Route x System
% Descripción: Reemplaza la ruta actual del sistema
% Método: n/a
setSystemActualR(System, Route, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, _, CDate, _, Trash, System),
  getDateString(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, Route, CDate, Date, Trash, NewSystem).

% Dominio: System x Trash x Syste,
% Descripción: Reemplaza la papelera del sistema
% Método: n/a
setSystemTrash(System, Trash, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, _, _, System),
  getDateString(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, Date, Trash, NewSystem).

% Dominio: System x String x List(String) x Elements 
% Descripción: Obtiene los elementos que cumplan cierto patrón en la ruta, en el sistema
% Método: n/a
getElementsFromSystem(System, Pattern, Route, Elements) :-
  getSystemDrives(System, Drives),
  getElementsFromDrives(Drives, Pattern, Route, Elements).

% Dominio: System x Element x List(String) x System
% Descripción: Agrega un elemento al sistema en la ruta.
% Método: n/a
addElementToSystem(System, Element, Route, NewSystem) :-
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, Element, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System x Lista(Element) x List(String) x System
% Descripción: Agrega una lista de elementos al sistema en la ruta indicada
% Método: n/a
addElementsToSystem(System, Elements, Route, NewSystem) :-
  getSystemDrives(System, Drives),
  addElementsToDrives(Drives, Elements, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System x String x Lista(String) x System
% Descripción: Elimina los elementos que cumplan cierto patrón en una ruta en el sistema
% Método: n/a
filterElementsFromSystem(System, Pattern, Route, NewSystem) :-
  getSystemDrives(System, Drives),
  filterElementsFromDrives(Drives, Pattern, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System x Lista(Elemento) x Lista(String) x System
% Descripción: Envía elementos a la papelera del sistema
% Método: n/a
sendToTrash(System, Elements, Route, NewSystem) :-
  getSystemTrash(System, Trash),
  addElementsToTrash(Trash, Elements, Route, NewTrash),
  setSystemTrash(System, NewTrash, NewSystem).

% Dominio: Trash x Lista(Elemento) x Lista(string) x Trash
% Descripción: Agrega elementos a la papelera con la ruta de origen
% Método: Recursión
% Probar reordenar.
addElementsToTrash(_, [], _, []) :- !.

addElementsToTrash(Trash, [H | T], Route, [[H, Route] |NewTrash]) :-
  addElementsToTrash(Trash, T, Route, NewTrash).

% Dominio: System x Lista(string)
% Descripción: Confirma que una ruta exista en el sistema
% Método: n/a
checkRouteExistsSystem(System, Route) :-
  getSystemDrives(System, Drives),
  checkRouteExistsDrives(Drives, Route).

% Dominio: System x Lista(string) x System
% Descripción: Mueve la ruta del sistema a partir de la ruta entregada
% Método: Recursión
setSystemPath(System, [], System).

setSystemPath(System, [H | T], NewSystem) :-
  equal(H, ".."),
  getSystemActualR(System, Route),
  removeLast(Route, NewRoute),
  setSystemActualR(System, NewRoute, NewInnerSystem),
  setSystemPath(NewInnerSystem, T, NewSystem).

setSystemPath(System, [H | T], NewSystem) :-
  getSystemActualR(System, Route),
  addToEnd(H, Route, NewRoute),
  checkRouteExistsSystem(System, NewRoute),
  setSystemActualR(System, NewRoute, NewInnerSystem),
  setSystemPath(NewInnerSystem, T, NewSystem).

% Dominio: Element X List(Element) X List(Element)
% Descripción: Agrega al final de la lista
% Método: Backtracking
addToEnd(Element, [], [Element]).
addToEnd(Element, [H|T], [H|Updated]) :-
  addToEnd(Element, T, Updated).

% Dominio: System X String X String X List(String) X System
% Descripción: Renombra un elemento en la ruta entregada
% Método: n/a
renameElementFromSystem(System, Original, NewName, Route, NewSystem) :-
  getSystemDrives(System, Drives),
  renameElementFromDrives(Drives, Original, NewName, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X List(String) X List(String) X String
% Descripción: Obtiene el string con los elementos en la ruta entregada
% Método: n/a
getStringFromSystem(System, Parameters, Route, String) :-
  getSystemDrives(System, Drives),
  getStringFromDrives(Drives, Parameters, Route, String).

% Dominio: System X Char X String X System
% Descripción: Formatea el disco con la letra entregada
% Método: n/a
formatSystem(System, Letter, Name, NewSystem) :-
  getSystemDrives(System, Drives),
  formatDrive(Drives, Letter, Name, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).