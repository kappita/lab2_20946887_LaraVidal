:- module(content, [getContentNames/2, getElementByName/3, getElementsFromContent/3,
  setContentByName/3, addElementToContent/3, addElementsToContent/3,
  filterElementsFromContent/3]).

:- use_module(element).

getContentNames([], []) :- !.

getContentNames([FirstElement | Rest], [Name | NewList]) :-
  getElementName(FirstElement, Name),
  getContentNames(Rest, NewList).



getElementByName([], _, _) :- false.

getElementByName([[Type, Name, Extra, Content, Security, CDate, Mdate, Passkey] | _ ], Name, [Type, Name, Extra, Content, Security, CDate, Mdate, Passkey]).

getElementByName([ _ | T], Name, Element) :-
  getElementByName(T, Name, Element).


getElementsFromContent([], _, []):- !.

getElementsFromContent([Element | Rest], Pattern, [Element | NewList]) :-
  matchPattern(Element, Pattern),
  getElementsFromContent(Rest, Pattern, NewList).

getElementsFromContent([_ | Rest], Pattern, NewList) :-
  getElementsFromContent(Rest, Pattern, NewList).

setContentByName([], _, []) :- !.

setContentByName([[_, Name, _, _, _, _, _, _] | Rest], [Type, Name, Extra, Content, Security, CDate, MDate, Passkey], [[Type, Name, Extra, Content, Security, CDate, MDate, Passkey] | NewContent]) :-
  setContentByName(Rest, [Type, Name, Extra, Content, Security, CDate, MDate, Passkey], NewContent).

setContentByName([FirstElement | Rest], NewElement, [FirstElement | NewContent]):-
  setContentByName(Rest, NewElement, NewContent).