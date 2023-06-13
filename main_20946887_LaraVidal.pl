:- use_module(system_tda_20946887_LaraVidal).
:- use_module(content_20946887_LaraVidal).
:- use_module(element_20946887_LaraVidal).
:- use_module(drive_20946887_LaraVidal).
:- use_module(datestring_20946887_LaraVidal).

% Reglas
listaVacia([]).
equal(A, A).

% Dominio: String, List(String)
% Descripción: Transforma un string en ruta
% Método: n/a
getTargetPath(Path, [Drive | Rest]) :-
  split_string(Path, "/", "", [H | Rest]),
  atom_chars(H, [Letter | _]),
  atom_string(Letter, Drive).

%--- Función para crear archivo
% No está dentro de los requisitos del enunciado, pero es necesario para el predicado systemAddFile

% Dominio: String X String X File
% Descripción: Crea un elemento del tipo "file"
% Método: n/a
file(Name, Content, File) :-
  string_lower(Name, LName),
  string_lower(Content, LContent),
  getDateString(Date),
  element("file", LName, [], LContent, Date, Date, 12345678902345678912345678, File).

%--- Funciones
% Dominio: String X System
% Descripción: Entrega un sistema con el nombre entregado
% Método: n/a
system(Name, NewSystem) :-
  string_lower(Name, LName),
  getDateString(Date),
  cSystem(LName, [], [], [], [], [], Date, Date, [], NewSystem).

% Dominio: System X String X String or Char X int X System
% Descripción: Agrega un drive al sistema la información entregada
% Método: n/a
systemAddDrive(System, Letter, _, _, NewSystem) :-
  atom_chars(Letter, [_ | []]),
  string_lower(Letter, NewLetter),
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(NewLetter, Letters),
  setSystemDrives(System, Drives, NewSystem).

systemAddDrive(System, Letter, Name, Capacity, NewSystem) :-
  atom_string(Letter, StringL),
  string_lower(StringL, LowerLetter),
  string_lower(Name, LowerName),
  getSystemDrives(System, Drives),
  append([[LowerLetter, LowerName, Capacity, []]], Drives, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

systemAddDrive(System, Letter, Name, Capacity, NewSystem) :-
  atom_string(Letter, StringL),
  string_lower(StringL, LowerLetter),
  string_lower(Name, LowerName),
  getSystemDrives(System, Drives),
  append([[LowerLetter, LowerName, Capacity, []]], Drives, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Registra un usuario en el sistema
% Método: n/a
systemRegister(System, User, NewSystem) :-
  string_lower(User, LUser),
  getSystemUsers(System, Users),
  \+ member(LUser, Users),
  append([LUser], Users, NewUsers),
  setSystemUsers(System, NewUsers, NewSystem).

systemRegister(System, _, System).

% Dominio: System X String X System
% Descripción: Inicia sesión en el sistema con el usuario entregado
% Método: n/a
systemLogin(System, User, NewSystem) :-
  string_lower(User, LUser),
  getSystemUsers(System, Users),
  member(LUser, Users),
  getSystemActualU(System, []),
  setSystemActualU(System, LUser, NewSystem).

systemLogin(System, User, System):-
  string_lower(User, LUser),
  getSystemActualU(System, User),
  equal(LUser, User).

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
  atom_string(Letter, SLetter),
  string_lower(SLetter, LLetter),
  getSystemActualU(System, U),
  \+ listaVacia(U),
  getSystemDrives(System, Drives),
  getDrivesLetters(Drives, Letters),
  member(LLetter, Letters),
  setSystemActualD(System, [LLetter], NewSystemMid),
  setSystemActualR(NewSystemMid, [LLetter], NewSystem).

% Dominio: System X String X System
% Descripción: Añade una carpeta en el sistema con el nombre entregado, en la ruta actual
% Método: n/a
systemMkdir(System, Name, NewSystem) :-
  string_lower(Name, LName),
  getSystemActualU(System, U),
  getSystemActualR(System, Route),
  element("folder", LName, U, [], "Date", "Date", 12345678901234567890, NewElement),
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, NewElement, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Cambia la ruta actual del sistema a partir de la ruta entregada
% Método: n/a
systemCd(System, Path, NewSystem) :-
  string_lower(Path, LPath),
  equal(LPath, "/"),
  getSystemActualD(System, D),
  setSystemActualR(System, D, NewSystem).

systemCd(System, Path, NewSystem) :-
  string_lower(Path, LPath),
  split_string(LPath, "/", "", PathList),
  clearPath(PathList, CleanPathList),
  setSystemPath(System, CleanPathList, NewSystem).

% Dominio: System X File X System
% Descripción: Agrega un archivo al sistema en la ruta actual.
% Método: n/a
systemAddFile(System, File, NewSystem) :-
  getSystemActualR(System, Route),
  getSystemDrives(System, Drives),
  addElementToDrives(Drives, File, Route, NewDrives),
  setSystemDrives(System, NewDrives, NewSystem).

% Dominio: System X String X System
% Descripción: Elimina todos elementos que cumplan el patrón entregado, en la ruta actual del sistema
% Método: n/a
systemDel(System, Pattern, NewSystem) :-
  string_lower(Pattern, LPattern),
  getSystemActualR(System, Route),
  getElementsFromSystem(System, LPattern, Route, Elements),
  filterElementsFromSystem(System, LPattern, Route, NewInnerSystem),
  sendToTrash(NewInnerSystem, Elements, Route, NewSystem).

% Dominio: System X String X String X System
% Descripción: Copia todos los elementos que cumplan el patrón en la ruta actual del sistema, a la ruta entregada.
% Método: n/a
systemCopy(System, Pattern, Target, NewSystem) :-
  string_lower(Pattern, LPattern),
  string_lower(Target, LTarget),
  getSystemActualR(System, Route),
  getElementsFromSystem(System, LPattern, Route, Elements),
  getTargetPath(LTarget, TargetList),
  clearPath(TargetList, CleanTargetList),
  checkRouteExistsSystem(System, CleanTargetList),
  addElementsToSystem(System, Elements, CleanTargetList, NewSystem).

% Dominio: System X String X String X System
% Descripción: Mueve todos los elementos que cumplan el patrón en la ruta actual del sistema, a la ruta entregada.
% Método: n/a
systemMove(System, Pattern, Target, NewSystem) :-
  string_lower(Pattern, LPattern),
  string_lower(Target, LTarget),
  getSystemActualR(System, Route),
  getElementsFromSystem(System, LPattern, Route, Elements),
  \+ listaVacia(Elements),
  filterElementsFromSystem(System, LPattern, Route, NewInnerSystem),
  getTargetPath(LTarget, TargetList),
  clearPath(TargetList, CleanTargetList),
  addElementsToSystem(NewInnerSystem, Elements, CleanTargetList, NewSystem).

% Dominio: System X String X String X System
% Descripción: Renombra un elemento
% Método: n/a
systemRen(System, Name, NewName, NewSystem) :-
  string_lower(Name, LName),
  string_lower(NewName, LNewName),
  getSystemActualR(System, Route),
  renameElementFromSystem(System, LName, LNewName, Route, NewSystem).

% Dominio: System X List(String) X String
% Descripción: Entrega un texto formateado con los elementos
% Método: n/a
systemDir(System, Params, String) :-
  getSystemActualR(System, Route),
  getStringFromSystem(System, Params, Route, String).

% Dominio: System X Char X String X System
% Descripción: Formatea el drive con la letra entregada
% Método: n/a
systemFormat(System, Letter, Name, NewSystem) :-
  string_lower(Letter, LLetter),
  string_lower(Name, LName),
  formatSystem(System, LLetter, LName, NewSystem).

% Dominio: System X String X String X System
% Descripción: Encripta el elemento con el nombre indicado, con la contraseña entregada
% Método: n/a
systemEncrypt(System, Password, Path, NewSystem) :-
  getSystemActualR(System, Route),
  string_lower(Password, LPassword),
  string_lower(Path, LPath),
  encryptSystem(System, LPassword, LPath, Route, NewSystem).

% Dominio: System X String X String X System
% Descripción: Desncripta el elemento con el nombre indicado, con la contraseña entregada
% Método: n/a
systemDecrypt(System, Password, Path, NewSystem) :-
  getSystemActualR(System, Route),
  string_lower(Password, LPassword),
  string_lower(Path, LPath),
  decryptSystem(System, LPassword, LPath, Route, NewSystem).

% Dominio: System X String X String X String
% Descripción: Entrega la posición del string en los elementos seleccionados
% Método: n/a
systemGrep(_, _, _, String) :-
  String = "".

% Dominio: System X String
% Descripción: Muestra los elementos de la papelera
% método: n/a
systemViewTrash(System, String) :-
  getSystemTrash(System, Trash),
  showTrash(Trash, String).

% Dominio: System X String X System
% Descripción: Restaura los elementos que cumplan el patrón
% Método: n/a
systemRestore(System, Pattern, NewSystem) :-
  getSystemTrash(System, Trash),
  getElementsFromTrash(Trash, Pattern, Elements),
  filterTrash(Trash, Pattern, Filtered),
  setSystemTrash(System, Filtered, NewInnerSystem),
  restoreTrash(NewInnerSystem, Elements, NewSystem).