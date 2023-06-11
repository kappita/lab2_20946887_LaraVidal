:- module(drive, [getDriveLetter/2, getDriveName/2, getDriveContent/2,
  setDriveContent/3, getElementsFromDrive/4, addElementToDrive/4,
  addElementsToDrive/4, filterElementsFromDrive/4, checkRouteExistsDrive/2,
  checkRouteExistsDrives/2, getElementsFromDrives/4, addElementToDrives/4,
  addElementsToDrives/4, filterElementsFromDrives/4, getDrivesLetters/2,
  setDrivesByLetter/4, renameElementFromDrives/5, getStringFromDrives/4,
  encryptDrives/5, encryptDrive/5, decryptDrives/5, decryptDrive/5,
  formatDrive/4]).

:- use_module(content).
:- use_module(element).
:- use_module(datestring).

% Dominio: 
% Descripción: 
% Método: 
partesLista([H | T], H, T).

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

% Dominio: Drive x Element x Lista(String) x Drive
% Descripción: Agrega un elemento a los contenidos del drive en la ruta.
% Método: n/a
addElementToDrive(Drive, Element, Route, NewDrive) :-
  listaVacia(Route),
  getDriveContent(Drive, Content),
  addElementToContent(Content, Element, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

addElementToDrive(Drive, Element, Route, NewDrive) :-
  partesLista(Route, H, T),
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  addElementToFolder(Folder, Element, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

% Dominio: Drive x Lista(Elemento) x Lista(string) x Drive
% Descripción: Agrega una lista de elementos a los contenidos del drive en la ruta.
% Método: n/a
addElementsToDrive(Drive, Elements, Route, NewDrive) :-
  listaVacia(Route),
  getDriveContent(Drive, Content),
  addElementsToContent(Content, Elements, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

addElementsToDrive(Drive, Elements, Route, NewDrive) :-
  partesLista(Route, H, T),
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  addElementsToFolder(Folder, Elements, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

% Dominio: Drive x String x Lista(string) x Drive
% Descripción: Filtra los elementos que cumplan un patrón de la ruta del contenido del drive
% Método: n/a
filterElementsFromDrive(Drive, Pattern, Route, NewDrive) :-
  listaVacia(Route),
  getDriveContent(Drive, Content),
  filterElementsFromContent(Content, Pattern, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

filterElementsFromDrive(Drive, Pattern, Route, NewDrive) :-
  partesLista(Route, H, T),
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  filterElementsFromFolder(Folder, Pattern, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

% Dominio: Drive x Lista(String)
% Descripción: Confirma que una ruta exista en el drive.
% Método: n/a
checkRouteExistsDrive(_, []).

checkRouteExistsDrive(Drive, [H | T]) :-
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Element),
  checkRouteExistsFolder(Element, T).

% Dominio: Lista(Drive) x String x Lista(String) x Lista(Element)
% Descripción: Obtiene los elementos que cumplan un patrón de la lista de drives.
% Método: n/a
getElementsFromDrives(Drives, Pattern, [H | T], Elements) :-
  getDriveByLetter(Drives, H, Drive),
  getElementsFromDrive(Drive, Pattern, T, Elements).

% Dominio: Lista(Drive) x Element x Lista(String) x Lista(Drives)
% Descripción: Agrega un elemento a la lista de drives en la ruta.
% Método: n/a
addElementToDrives(Drives, Element, [H | T], NewDrives) :-
  getDriveByLetter(Drives, H, Drive),
  addElementToDrive(Drive, Element, T, NewDrive),
  setDrivesByLetter(Drives, NewDrive, H, NewDrives).

% Dominio: Lista(Drive) x Lista(Element) x Lista(String) x Lista(Drive)
% Descripción: Agrega una lista de elementos a la lista de drives en la ruta.
% Método: n/a
addElementsToDrives(Drives, Elements, [H | T], NewDrives) :-
  getDriveByLetter(Drives, H, Drive),
  addElementsToDrive(Drive, Elements, T, NewDrive),
  setDrivesByLetter(Drives, NewDrive, H, NewDrives).

% Dominio: Lista(Drive) x String x Lista(string) x Lista(Drive)
% Descripción: Filtra los elementos que cumplan un patrón en la ruta de la lista de drives.
% Método: 
filterElementsFromDrives(Drives, Pattern, [H | T], NewDrives) :-
  getDriveByLetter(Drives, H, Drive),
  filterElementsFromDrive(Drive, Pattern, T, NewDrive),
  setDrivesByLetter(Drives, NewDrive, H, NewDrives).

% Dominio: Lista(Drive) x String x Drive
% Descripción: Obtiene un drive de la lista de drives a partir de su letra
% Método: n/a
getDriveByLetter([[Letter, Name, Capacity, Content] | _], Letter, Drive) :-
  drive(Letter, Name, Capacity, Content, Drive).

getDriveByLetter([_ | T], Letter, Drive) :-
  getDriveByLetter(T, Letter, Drive).


% Dominio: Lista(Drive) x Lista(string)
% Descripción: Obtiene las letras de todos los drives de la lista.
% Método: 
getDrivesLetters([], []) :- !.

getDrivesLetters([FirstElement | Rest], [Letter | NewList]) :-
  getDriveLetter(FirstElement, Letter),
  getDrivesLetters(Rest, NewList).

% Dominio: Lista(Drive) X Drive X String X Lista(Drive)
% Descripción: Reemplaza un drive de la lista a partir de su letra
% Método: 
setDrivesByLetter([], _, _, []):- !.

setDrivesByLetter([[Letter, _, _, _] | Rest], NewDrive, Letter, [NewDrive | NewDrives]):-
  setDrivesByLetter(Rest, NewDrive, Letter, NewDrives).

setDrivesByLetter([FirstDrive | Rest], NewDrive, Letter, [FirstDrive | NewDrives]) :-
  setDrivesByLetter(Rest, NewDrive, Letter, NewDrives).

% Dominio: Lista(Drive) x Lista(String)
% Descripción: Confirma que una ruta exista en la lista de drives.
% Método: 
checkRouteExistsDrives(Drives, [H | T]) :-
  getDriveByLetter(Drives, H, Drive),
  checkRouteExistsDrive(Drive, T).

% Dominio: Drive X String X String X List(String) X Drive
% Descripción: Renombra el elemento del drive en la ruta indicada
% Método: n/a
renameElementFromDrive(Drive, Original, NewName, [], NewDrive) :-
  getDriveContent(Drive, Content),
  renameElementFromContent(Content, Original, NewName, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

renameElementFromDrive(Drive, Original, NewName, [H | T], NewDrive) :-
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  renameElementFromFolder(Folder, Original, NewName, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

% Dominio: Drives X String X String X List(String) X Drives
% Descripción: Renombra el elemento de los drives en la ruta indicada
% Método: n/a
renameElementFromDrives(Drives, Original, NewName, [H | T], NewDrives) :-
  getDriveByLetter(Drives, H, Drive),
  renameElementFromDrive(Drive, Original, NewName, T, NewDrive),
  setDrivesByLetter(Drives, NewDrive, H, NewDrives).

% Dominio: Drive X List(String) X List(String) X String
% Descripción: Obtiene el string de los elementos en la ruta indicada
% Método: n/a
getStringFromDrive(Drive, Parameters, [], String) :-
  getDriveContent(Drive, Content),
  getStringFromContent(Content, Parameters, String).

getStringFromDrive(Drive, Parameters, [H | T], String) :-
  getDriveContent(Drive, Content),
  getElementByName(Content, H, InnerFolder),
  getStringFromFolder(InnerFolder, Parameters, T, String).

% Dominio: Drives X Lista(String) X Lista(String) X String
% Descripción: Obtiene el string de los elementos en la ruta indicada
% Método: n/a
getStringFromDrives(Drives, Parameters, [H | T], String) :-
  getDriveByLetter(Drives, H, Drive),
  getStringFromDrive(Drive, Parameters, T, String).

% Dominio: Drives X Char X String X Drives
% Descripción: Formatea el drive con la letra, y cambia su nombre
% Método: n/a
formatDrive(Drives, Letter, Name, NewDrives) :-
  getDriveByLetter(Drives, Letter, Drive),
  drive(_, _, Capacity, _, Drive),
  drive(Letter, Name, Capacity, [], NewDrive),
  setDrivesByLetter(Drives, NewDrive, Letter, NewDrives).