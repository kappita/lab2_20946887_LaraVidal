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

getSystemUsers(System, Users) :-
  cSystem(_, Users, _, _, _, _, _, _, _, System).

getSystemDrives(System, Drives) :-
  cSystem(_, _, Drives, _, _, _, _, _, _, System).

getSystemActualU(System, ActualU) :-
  cSystem(_, _, _, ActualU, _, _, _, _, _, System).

getSystemActualD(System, ActualD) :-
  cSystem(_, _, _, _, ActualD, _, _, _, _, System).

getSystemActualR(System, ActualR) :-
  cSystem(_, _, _, _, _, ActualR, _, _, _, System).

getSystemTrash(System, Trash) :-
  cSystem(_, _, _, _, _, _, _, _, Trash, System).

setSystemUsers(System, NewUsers, NewSystem):-
  cSystem(Name, _, Drives, ActualU, ActualD, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, NewUsers, Drives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

setSystemDrives(System, NewDrives, NewSystem):-
  cSystem(Name, Users, _, ActualU, ActualD, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, NewDrives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).


setSystemActualU(System, User, NewSystem):-
  cSystem(Name, Users, Drives, _, ActualD, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, User, ActualD, ActualR, CDate, "Date", Trash, NewSystem).

setSystemActualD(System, Drive, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, _, ActualR, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, Drive, ActualR, CDate, "Date", Trash, NewSystem).

setSystemActualR(System, Route, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, _, CDate, _, Trash, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, Route, CDate, "Date", Trash, NewSystem).

setSystemTrash(System, Trash, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, _, _, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, "Date", Trash, NewSystem).


getElementsFromSystem(System, Pattern, Route, Elements) :-
  getSystemDrives(System, Drives),
  getElementsFromDrives(Drives, Pattern, Route, Elements).
