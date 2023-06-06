
:- module(element, [isFile/1, matchPattern/2, getElementType/2, getElementName/2,
  getElementsFromFolder/4, checkRouteExistsFolder/2, clearPath/2,
  addElementToFolder/4, addElementsToFolder/4, filterElementsFromFolder/4]).

:- use_module(content).
:- use_module(element).

listaVacia([]).

element(Type, Name, Extra, Content, Security, CDate, MDate, Passkey ,[Type, Name, Extra, Content, Security, CDate, MDate, Passkey]).
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

