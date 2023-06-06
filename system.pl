:- module(system_tda, [getSystemUsers/2, getSystemDrives/2, getSystemActualU/2,
  getSystemActualD/2, getSystemActualR/2, getSystemTrash/2, setSystemUsers/3,
setSystemDrives/3, setSystemActualU/3, setSystemActualD/3, setSystemActualR/3,
getElementsFromSystem/4, addElementsToSystem/4, addElementToSystem/4,
filterElementsFromSystem/4, sendToTrash/4, checkRouteExistsSystem/2, setSystemPath/3,
checkRouteExistsSystem/2]).

:- use_module(content).
:- use_module(drive).


partesLista( [ H | T], H, T).
equal(A, A).

cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash, [Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash]).
removeLast([_], []).
removeLast([H|T], [H|Filtered]) :-
  removeLast(T, Filtered).

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
  %get_current_date_time(Date),
  cSystem(Name, NewUsers, Drives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

% Dominio: System x List(Drives) x System
% Descripción: Reemplaza la lista de Drives del sistema
% Método: n/a
setSystemDrives(System, NewDrives, NewSystem):-
  cSystem(Name, Users, _, ActualU, ActualD, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, NewDrives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

% Dominio: System x String x System
% Descripción: Reemplaza el usuario actual del sistema
% Método: n/a
setSystemActualU(System, User, NewSystem):-
  cSystem(Name, Users, Drives, _, ActualD, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, User, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

% Dominio: System x String x System
% Descripción: Reemplaza el drive actual del sistema
% Método: n/a
setSystemActualD(System, Drive, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, _, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, Drive, ActualR, CDate, "Date", Trash, NewSystem).

% Dominio: System x Route x System
% Descripción: Reemplaza la ruta actual del sistema
% Método: n/a
setSystemActualR(System, Route, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, _, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, Route, CDate, "Date", Trash, NewSystem).

% Dominio: System x Trash x Syste,
% Descripción: Reemplaza la papelera del sistema
% Método: n/a
setSystemTrash(System, Trash, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, _, _, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

% Dominio: System x String x List(String) x Elements 
% Descripción: Obtiene los elementos que cumplan cierto patrón en la ruta, en el sistema
% Método: 
getElementsFromSystem(System, Pattern, Route, Elements) :-
  getSystemDrives(System, Drives),
  getElementsFromDrives(Drives, Pattern, Route, Elements).


getElementsFromSystem(System, Pattern, Route, Elements) :-
  getSystemDrives(System, Drives),
  getElementsFromDrives(Drives, Pattern, Route, Elements).
