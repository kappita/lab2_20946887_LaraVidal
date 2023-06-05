:- module(drive, [getDriveLetter/2, getDriveName/2, getDriveContent/2,
  setDriveContent/3, getElementsFromDrive/4, addElementToDrive/4,
  addElementsToDrive/4, filterElementsFromDrive/4, checkRouteExistsDrive/2,
  checkRouteExistsDrives/2, getElementsFromDrives/4, addElementToDrives/4,
  addElementsToDrives/4, filterElementsFromDrives/4, getDrivesLetters/2,
  setDrivesByLetter/4]).

:- use_module(content).
:- use_module(element).

partesLista( [ H | T], H, T).

drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).

listaVacia([]).

getDriveLetter(Drive, Letter):-
  drive(Letter, _, _, _, Drive).

getDriveName(Drive, Name) :-
  drive(_, Name, _, _, Drive).

getDriveContent(Drive, Content) :-
  drive(_, _, _, Content, Drive).


setDriveContent(Drive, NewContent, NewDrive) :-
  drive(Letter, Name, Capacity, _, Drive),
  drive(Letter, Name, Capacity, NewContent, NewDrive).


getElementsFromDrive(Drive, Pattern, Route, Elements) :-
  listaVacia(Route),
  getDriveContent(Drive, Content),
  getElementsFromContent(Content, Pattern, Elements).
