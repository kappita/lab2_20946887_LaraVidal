
:- module(element, [isFile/1, matchPattern/2, getElementType/2, getElementName/2,
  getElementsFromFolder/4, checkRouteExistsFolder/2, clearPath/2,
  addElementToFolder/4, addElementsToFolder/4, filterElementsFromFolder/4]).

:- use_module(content).
:- use_module(element).

element(Type, Name, Extra, Content, Security, CDate, MDate, Passkey ,[Type, Name, Extra, Content, Security, CDate, MDate, Passkey]).
getElementType(Element, Type) :-
  element(Type, _, _, _, _, _, _, _, Element).


getElementName(Element, Name) :-
  element(_, Name, _, _, _, _, _, _, Element).

getElementContent(Element, Content) :-
  element(_, _, _, Content, _, _, _, _, Element).

setElementContent(Element, NewContent, NewElement) :-
  element(Type, Name, Extra, _, Security, CDate, MDate, Passkey, Element),
  element(Type, Name, Extra, NewContent, Security, CDate, MDate, Passkey, NewElement).

getElementsFromFolder(Element, Pattern, Route, Elements) :-
  listaVacia(Route),
  getElementContent(Element, Content),
  getElementsFromContent(Content, Pattern, Elements).

getElementsFromFolder(Element, Pattern, [H | T], Elements) :-
  getElementContent(Element, Content),
  getElementByName(Content, H, InnerElement),
  getElementsFromFolder(InnerElement, Pattern, T, Elements).


