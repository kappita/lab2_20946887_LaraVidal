:- module(content, [getContentNames/2, getElementByName/3, getElementsFromContent/3,
  setContentByName/3, addElementToContent/3, addElementsToContent/3,
  filterElementsFromContent/3]).

:- use_module(element).

% Dominio: Lista (Elementos) x Lista(Strings)
% Descripción: Obtiene una lista con el nombre de todos los elementos
% Método: Backtracking
getContentNames([], []) :- !.

getContentNames([FirstElement | Rest], [Name | NewList]) :-
  getElementName(FirstElement, Name),
  getContentNames(Rest, NewList).


% Dominio: Lista(Elemento) x String x Elemento
% Descripción: Obtiene el elemento a partir de su nombre
% Método: n/a
getElementByName([], _, _) :- false.

getElementByName([[Type, Name, Extra, Content, Security, CDate, Mdate, Passkey] | _ ], Name, [Type, Name, Extra, Content, Security, CDate, Mdate, Passkey]).

getElementByName([ _ | T], Name, Element) :-
  getElementByName(T, Name, Element).


% Dominio: Lista(Elementos) x String x Lista(Elementos)
% Descripción: Obtiene todos los elementos que cumplan un patrón
% Método: Backtracking
getElementsFromContent([], _, []):- !.

getElementsFromContent([Element | Rest], Pattern, [Element | NewList]) :-
  matchPattern(Element, Pattern),
  getElementsFromContent(Rest, Pattern, NewList).

getElementsFromContent([_ | Rest], Pattern, NewList) :-
  getElementsFromContent(Rest, Pattern, NewList).

% Dominio: Lista(Elementos) x Elemento x Lista(Elementos)
% Descripción: Reemplaza el elemento que tenga el mismo nombre que el elemento nuevo.
% Método: Backtracking
setContentByName([], _, []) :- !.

setContentByName([[_, Name, _, _, _, _, _, _] | Rest], [Type, Name, Extra, Content, Security, CDate, MDate, Passkey], [[Type, Name, Extra, Content, Security, CDate, MDate, Passkey] | NewContent]) :-
  setContentByName(Rest, [Type, Name, Extra, Content, Security, CDate, MDate, Passkey], NewContent).

setContentByName([FirstElement | Rest], NewElement, [FirstElement | NewContent]):-
  setContentByName(Rest, NewElement, NewContent).

% Dominio: Lista(Elementos) x Elemento x Lista(Elementos)
% Descripción: Agrega el elemento a lista de elementos, según si es archivo o carpeta
% Método: n/a
addElementToContent(Content, Element, NewContent) :-
  isFile(Element),
  getElementName(Element, Name),
  getContentNames(Content, Names),
  member(Name, Names),
  setContentByName(Content, Element, NewContent).

addElementToContent(Content, Element, NewContent) :-
  isFile(Element),
  append([Element], Content, NewContent).

addElementToContent(Content, Element, NewContent) :-
  getElementName(Element, Name),
  getContentNames(Content, Names),
  \+ member(Name, Names),
  append([Element], Content, NewContent).

% Dominio: Lista(Elemento) x Elemento x Lista(Elemento).
% Descripción: Función genérica que agrega a una lista o reemplaza si existe un elemento con el mismo nombre
% Método: n/a
addOrReplaceToContent(Content, Element, NewContent) :-
  getElementName(Element, Name),
  getContentNames(Content, Names),
  member(Name, Names),
  setContentByName(Content, Element, NewContent).

addOrReplaceToContent(Content, Element, NewContent) :-
  append([Element], Content, NewContent).

% Dominio: Lista(Elementos) x Lista(Elementos) x Lista(Elementos)
% Descripción: Agrega una lista de elementos, reemplazando si ya existe un elemento con el nombre
% Método: Recursión
addElementsToContent(Content, [], Content):- !.

addElementsToContent(Content, [Element | Rest], NewContent) :-
  addOrReplaceToContent(Content, Element, NewInnerContent),
  addElementsToContent(NewInnerContent, Rest, NewContent).

% Dominio: Lista(Elementos) x string x Lista(Elementos)
% Descripción: Elimina los elementos que cumplan cierto patrón.
% Método: Backtracking
filterElementsFromContent([], _, []):- !.

filterElementsFromContent([Element | Rest], Pattern, [Element | NewList]) :-
  \+ matchPattern(Element, Pattern),
  filterElementsFromContent(Rest, Pattern, NewList).

filterElementsFromContent([_ | Rest], Pattern, NewList) :-
  filterElementsFromContent(Rest, Pattern, NewList).