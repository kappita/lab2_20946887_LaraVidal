listaVacia([]) :- true.
listaVacia([_ | _]) :- false.

isFile("file") :- true.
isFile(_) :- false.

partesLista([H | T], H, T).

cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash, [Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash]).
drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).
folder(Type, Name, Author, Content, Security, CDate, MDate, [Type, Name, Author, Content, Security, CDate, MDate, Password]).
file(Type, Name, Ext, Content, Security, CDate, MDate, [Type, Name, Ext, Content, Security, CDate, MDate, Password]).
element(Type, Name, Extra, Content, Security, CDate, MDate, [Type, Name, Extra, Content, Security, CDate, MDate]).
content(Contents, Contents).

%--------- SYSTEM---------%

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
  cSystem(Name, Users, Drives, ActualU, ActualD, Route, CDate, Date, Trash, NewSystem).

setSystemTrash(System, Trash, NewSystem):-
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, _, _, System),
  %get_current_date_time(Date),
  cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, Date, Trash, NewSystem).

% ----- DRIVE ---------

getDriveLetter(Drive, Letter):-
  drive(Letter, _, _, _, Drive).

getDriveName(Drive, Name) :-
  drive(_, Name, _, _, Drive).

getDriveContent(Drive, Content) :-
  drive(_, _, _, Content, Drive).

setDriveContent(Drive, NewContent, NewDrive) :-
  drive(Letter, Name, Capacity, _, Drive),
  drive(Letter, Name, Capacity, NewContent, NewDrive).



getDrivesLetters([], []) :- !.

getDrivesLetters([FirstElement | Rest], [Letter | NewList]) :-
  getDriveLetter(FirstElement, Letter),
  getDrivesLetters(Rest, NewList).

setDrivesByLetter([], _, _, []):- !.

setDrivesByLetter([[Letter, _, _, _] | Rest], NewDrive, Letter, [NewDrive | NewDrives]):-
  setDrivesByLetter(Rest, NewDrive, Letter, NewDrives).

setDrivesByLetter([FirstDrive | Rest], NewDrive, Letter, [FirstDrive | NewDrives]) :-
  setDrivesByLetter(Rest, NewDrive, Letter, NewDrives).

addFolderToDrive(Drive, Element, Route, NewDrive) :-
  listaVacia(Route) ->
  getDriveContent(Drive, Content),
  addFolderToContent(Content, Element, NewContent),
  setDriveContent(Drive, NewContent, NewDrive);


  partesLista(Route, H, T),
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  addFolderToFolder(Folder, Element, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).

addFileToDrive(Drive, Element, Route, NewDrive) :-
  % If the route is empty
  listaVacia(Route) ->
  % then
  getDriveContent(Drive, Content),
  addFileToContent(Content, Element, NewContent),
  setDriveContent(Drive, NewContent, NewDrive);

  % else
  partesLista(Route, H, T),
  getDriveContent(Drive, Content),
  getElementByName(Content, H, Folder),
  addFolderToFolder(Folder, Element, T, NewFolder),
  setContentByName(Content, NewFolder, NewContent),
  setDriveContent(Drive, NewContent, NewDrive).


%---------- CONTENT ------------


%--- Funciones
% Dominio: String X System
% Descripción: Entrega un sistema con el nombre entregado
% Método: n/a
system(Name, NewSystem) :-
  cSystem(Name, [], [], [], [], [], "hora", "hora", [], NewSystem).

% Dominio: System X String X String or Char X int X System
% Descripción: Agrega un drive al sistema la información entregada
% Método: n/a
systemAddDrive(System, _, Letter, _, NewSystem) :-
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(Letter, Letters),
  setSystemDrives(System, Drives, NewSystem).

systemAddDrive(System, Name, Letter, Capacity, NewSystem) :-
  getSystemDrives(System, Drives),
  append([[Letter, Name, Capacity, []]], Drives, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Registra un usuario en el sistema
% Método: n/a
systemRegister(System, User, NewSystem) :-
  getSystemUsers(System, Users),
  \+ member(User, Users),
  append([User], Users, NewUsers),
  setSystemUsers(System, NewUsers, NewSystem).

systemRegister(System, _, System).

% Dominio: System X String X System
% Descripción: Inicia sesión en el sistema con el usuario entregado
% Método: n/a
systemLogin(System, User, NewSystem) :-
  getSystemUsers(System, Users),
  member(User, Users),
  setSystemActualU(System, User, NewSystem).

systemLogin(System, _, System).

% Dominio: System X System
% Descripción: Cierra la sesión del usuario actual en el sistema
% Método: 
systemLogout(System, NewSystem) :-
  getSystemActualU(System, User),
  \+ listaVacia(User),
  setSystemActualU(System, [], NewSystem).

% Dominio: System X String or Char, System
% Descripción: Selecciona el drive a ser utilizado en el sistema
% Método: n/a
systemSwitchDrive(System, Letter, NewSystem) :-
  getSystemActualU(System, U),
  \+ listaVacia(U),
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(Letter, Letters),
  setSystemActualD(System, [Letter], NewSystemMid),
  setSystemActualR(NewSystemMid, [Letter], NewSystem).

% Dominio: System X String X System
% Descripción: Añade una carpeta en el sistema con el nombre entregado, en la ruta actual
% Método: n/a
systemMkdir(System, Name, NewSystem) :-
  getSystemActualU(System, U),
  getSystemActualR(System, Route),
  element("folder", Name, U, [], [], "Date", "Date", 0, NewElement),
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, NewElement, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).


