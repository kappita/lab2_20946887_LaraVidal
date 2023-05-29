cSystem(Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash, [Name, Users, Drives, ActualU, ActualD, ActualR, CDate, MDate, Trash]).
drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).

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