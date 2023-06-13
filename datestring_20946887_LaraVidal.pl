:- module(datestring_20946887_LaraVidal, [getDateString/1]).


% Dominio: date x int x int x int
% Descripción: Obtiene fecha
% Método: n/a
getDate(date(Year, Month, Day), Year, Month, Day).

% Dominio: time x int x int x int 
% Descripción: Obtiene hora
% Método: n/a
getHour(time(Hour, Minute, FloatSecond), Hour, Minute, Second) :-
  floor(FloatSecond, Second).

% Dominio: DateTime
% Descripción: Obtiene fecha y hora 
% Método: n/a
getCurrentDateTime(DateTime) :-
  get_time(Timestamp),
  stamp_date_time(Timestamp, DateTime, local).

% Dominio: Date X Time
% Descripción: Separa la fecha y la hora 
% Método: n/a
getCurrentDateTime(Date, Time) :-
  getCurrentDateTime(DateTime),
  date_time_value(date, DateTime, Date),
  date_time_value(time, DateTime, Time).

% Dominio: String
% Descripción: Obtiene un string formateado con la fecha y hora
% Método: n/a
getDateString(String) :-
  getCurrentDateTime(Date, Time),
  getDate(Date, Year, Month, Day),
  getHour(Time, Hour, Minute, Second),
  format(atom(String), "~d/~d/~d ~d:~d:~d", [Year, Month, Day, Hour, Minute, Second]).