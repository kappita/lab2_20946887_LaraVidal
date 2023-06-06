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

% Dominio: Drive x String
% Descripción: Obtiene la letra de un drive
% Método: n/a
getDriveLetter(Drive, Letter):-
  drive(Letter, _, _, _, Drive).

% Dominio: Drive x String
% Descripción: Obtiene el nombre de un drive
% Método: 
% ELIMINABLE
getDriveName(Drive, Name) :-
  drive(_, Name, _, _, Drive).

% Dominio: Drive x Lista(Element)
% Descripción: Obtiene el contenido de un drive
% Método: n/a
getDriveContent(Drive, Content) :-
  drive(_, _, _, Content, Drive).

% Dominio: Drive x Lista(Element) x Drive
% Descripción: Reemplaza los contenidos de un drive
% Método: n/a
setDriveContent(Drive, NewContent, NewDrive) :-
  drive(Letter, Name, Capacity, _, Drive),
  drive(Letter, Name, Capacity, NewContent, NewDrive).

% Dominio: drive x String x Lista(String) x List(Element)
% Descripción: Obtiene los elementos que cumplan un patrón de una ruta del drive
% Método: 
getElementsFromDrive(Drive, Pattern, Route, Elements) :-
  listaVacia(Route),
  getDriveContent(Drive, Content),
  getElementsFromContent(Content, Pattern, Elements).


getElementsFromDrive(Drive, Pattern, [H | T], Elements) :-
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Element),
  getElementsFromFolder(Element, Pattern, T, Elements).