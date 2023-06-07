
:- module(element, [isFile/1, matchPattern/2, getElementType/2, getElementName/2,
  getElementsFromFolder/4, checkRouteExistsFolder/2, clearPath/2,
  addElementToFolder/4, addElementsToFolder/4, filterElementsFromFolder/4]).

:- use_module(content).
:- use_module(element).

listaVacia([]).
partesLista([H | T], H, T).

element(Type, Name, Extra, Content, Security, CDate, MDate, Passkey ,[Type, Name, Extra, Content, Security, CDate, MDate, Passkey]).

isFile("file") :- true.
isFile(_) :- false.

equal(A, A) :- true.

% Dominio: Lista(Átomo) x Lista(Átomo)
% Descripción: Compara si dos listas de átomos son iguales, considerando comodines
% Método: Recursión.
matchText([], []).

matchText([Char | Word], [Char | Pattern]) :-
  matchText(Word, Pattern).

matchText(_, [*]).


% Dominio: Elemento x String
% Descripción: Compara si el nombre de un elemento cumple un patrón.
% Método: n/a
matchPattern(Element, Pattern) :-
  getElementType(Element, Type),
  isFile(Type),
  getElementName(Element, Name),
  split_string(Name, ".", "", [Filename, Extension]),
  split_string(Pattern, ".", "", [FilenamePattern, ExtensionPattern]),
  atom_chars(FilenamePattern, FilenamePatternChars),
  atom_chars(Filename, FilenameChars),
  print(FilenameChars),
  print(FilenamePatternChars),
  matchText(FilenameChars, FilenamePatternChars),
  atom_chars(ExtensionPattern, ExtensionPatternChars),
  atom_chars(Extension, ExtensionChars),
  matchText(ExtensionChars, ExtensionPatternChars).

matchPattern(Element, Pattern) :-
  getElementType(Element, Type),
  \+ isFile(Type),
  getElementName(Element, Name),
  split_string(Name, ".", "", [Filename]),
  split_string(Pattern, ".", "", [FilenamePattern]),
  atom_chars(FilenamePattern, FilenamePatternChars),
  atom_chars(Filename, FilenameChars),
  matchText(FilenameChars, FilenamePatternChars).

% Dominio: Elemento x string
% Descripción: Entrega el tipo de un elemento
% Método: n/a
getElementType(Element, Type) :-
  element(Type, _, _, _, _, _, _, _, Element).

% Dominio: Elemento x String
% Descripción: Entrega el nombre de un elemento.
% Método: n/a
getElementName(Element, Name) :-
  element(_, Name, _, _, _, _, _, _, Element).

% Dominio: Elemento x (Lista(Elemento) or String)
% Descripción: Obtiene el contenido de un elemento
% Método: n/a
getElementContent(Element, Content) :-
  element(_, _, _, Content, _, _, _, _, Element).

% Dominio: Elemento x (Lista(Elemento) or string) x Elemento
% Descripción: Reemplaza los contenidos de un elemento
% Método: n/a
setElementContent(Element, NewContent, NewElement) :-
  element(Type, Name, Extra, _, Security, CDate, MDate, Passkey, Element),
  element(Type, Name, Extra, NewContent, Security, CDate, MDate, Passkey, NewElement).

% Dominio: Elemento x String x List(String) x Lista(Element)
% Descripción: Obtiene los elementos de una carpeta que cumplan un patrón a partir de su ruta.
% Método: Recursión
getElementsFromFolder(Element, Pattern, Route, Elements) :-
  listaVacia(Route),
  getElementContent(Element, Content),
  getElementsFromContent(Content, Pattern, Elements).

getElementsFromFolder(Element, Pattern, [H | T], Elements) :-
  getElementContent(Element, Content),
  getElementByName(Content, H, InnerElement),
  getElementsFromFolder(InnerElement, Pattern, T, Elements).

% Dominio: Element x List(String)
% Descripción: Confirma si una ruta existe dentro del elemento
% Método: Recursión
checkRouteExistsFolder(_, []).

checkRouteExistsFolder(Element, [H | T]) :-
  \+ isFile(Element),
  getElementContent(Element, Content),
  getElementByName(Content, H, InnerElement),
  checkRouteExistsFolder(InnerElement, T).

% Dominio: Lista(String) x Lista(String)
% Descripción: Elimina los elementos vacíos y/o "." de una ruta
% Método: Backtracking
clearPath([], []) :- !.

clearPath([H | T], [ H | NewPath]) :-
  \+ equal(H, "."),
  \+ equal(H, ""),
  clearPath(T, NewPath).

clearPath([_ | T], NewPath) :-
  clearPath(T, NewPath).

% Dominio: Element x Element x Lista(string) x Element
% Descripción: Agrega un elemento a los contenidos de un elemento a partir de la ruta.
% Método: Recursión
addElementToFolder(Folder, Element, Route, NewFolder) :-
  listaVacia(Route),
  % Then
  getElementContent(Folder, Content),
  addElementToContent(Content, Element, NewContent),
  setElementContent(Folder, NewContent, NewFolder).

addElementToFolder(Folder, Element, Route, NewFolder) :-
  partesLista(Route, H, T),
  getElementContent(Folder, Content),
  getElementByName(Content, H, InnerFolder),
  addElementToFolder(InnerFolder, Element, T, NewInnerFolder),
  setContentByName(Content, NewInnerFolder, NewContent),
  setElementContent(Folder, NewContent, NewFolder).

% Dominio: Elemento x Lista(Elemento) x Ruta x Elemento
% Descripción: Agrega una lista de elementos a los contenidos de un elemento
% Método: recursión
addElementsToFolder(Folder, Elements, Route, NewFolder) :-
  listaVacia(Route),
  getElementContent(Folder, Content),
  addElementsToContent(Content, Elements, NewContent),
  setElementContent(Content, NewContent, NewFolder).

addElementsToFolder(Folder, Elements, Route, NewFolder) :-
  partesLista(Route, H, T),
  getElementContent(Folder, Content),
  getElementByName(Content, H, InnerFolder),
  addElementsToFolder(InnerFolder, Elements, T, NewInnerFolder),
  setContentByName(Content, NewInnerFolder, NewContent),
  setElementContent(Folder, NewContent, NewFolder).

% Dominio: Elemento x String x Lista(String) x Elemento
% Descripción: Elimina los elementos que cumplan un patrón de los contenidos del elemento
% Método: Recursión
filterElementsFromFolder(Folder, Pattern, Route, NewFolder) :-
  listaVacia(Route),
  getElementContent(Folder, Content),
  filterElementsFromContent(Content, Pattern, NewContent),
  setElementContent(Folder, NewContent, NewFolder).

filterElementsFromFolder(Folder, Pattern, Route, NewFolder) :-
  partesLista(Route, H, T),
  getElementContent(Folder, Content),
  getElementByName(Content, H, InnerFolder),
  filterElementsFromFolder(InnerFolder, Pattern, T, NewInnerFolder),
  setContentByName(Content, NewInnerFolder, NewContent),
  setElementContent(Folder, NewContent, NewFolder).

