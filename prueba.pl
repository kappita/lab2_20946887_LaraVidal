:- use_module(main).
% Caso que debe retornar true:
% Creando un sistema, con el disco C, dos usuarios: “user1” y “user2”, 
% se hace login con "user1”, se utiliza el disco "C", se crea la carpeta “folder1”, 
% “folder2”, se cambia al directorio actual “folder1", 
% se crea la carpeta "folder11" dentro de "folder1", 
% se hace logout del usuario "user1", se hace login con “user2”, 
% se crea el archivo "foo.txt" dentro de “folder1”, se acceder a la carpeta c:/folder2, 
% se crea el archivo "ejemplo.txt" dentro de c:/folder2
system("newSystem", S1), systemAddDrive(S1, "C", "OS", 10000000000, S2), systemRegister(S2, "user1", S3), systemRegister(S3, "user2", S4), systemLogin(S4, "user1", S5), systemSwitchDrive(S5, "C", S6), systemMkdir(S6, "folder1", S7), systemMkdir(S7, "folder2", S8), systemCd(S8, "folder1", S9), systemMkdir(S9, "folder11", S10), systemLogout(S10, S11), systemLogin(S11, "user2", S12), file( "foo.txt", "hello world", F1), systemAddFile(S12, F1, S13), systemCd(S13, "/folder2", S14),  file( "ejemplo.txt", "otro archivo", F2), systemAddFile(S14, F2, S15).

% Casos que deben retornar false:
% si se intenta añadir una unidad con una letra que ya existe
system("newSystem", S1), systemRegister(S1, "user1", S2), systemRegister(S2, "user1", S3).

% si se intenta hacer login con otra sesión ya iniciada por este usuario u otro
system("newSystem", S1), systemRegister(S1, "user1", S2), systemRegister(S2, "user2", S3), systemLogin(S3, "user1", S4), systemLogin(S4, "user2", S5).

% si se intenta usar una unidad inexistente
system("newSystem", S1), systemRegister(S1, "user1", S2), systemLogin(S2, "user1", S3), systemSwitchDrive(S3, "K", S4).


% Casos estudiante
% Caso falla pues cambia el drive sin iniciar sesión
% Crea sistema, añade C, Cambia a drive C
system("sistema", S1), systemAddDrive(S1, "C", "Disco 1", 1, S2), systemSwitchDrive(S2, "C", S3).

% Caso falla porque cambia el drive después de cerrar sesión
% Crea sistema, añade drive, registra 3 usuarios, inciia sesión, añade más drives, cambia al drive e, cierra sesión
% y cambia de drive
system("SISTEMA", S1), systemAddDrive(S1, "C", "DISCO1", 420, S2), systemRegister(S2, "Kappita", S3), systemRegister(S3, "Gonzalo", S4), systemRegister(S4, "Juan", S5), systemLogin(S5, "Kappa", S6),
systemAddDrive(S6, "D", "Disco2", 4123, S7), systemAddDrive(S7, "E", "Disco3", 41234, S8), systemSwitchDrive(S8, "E", S9), systemLogout(S9, S10),
systemSwitchDrive(S10, "E", S11).

% Retomamos el caso anterior, ahora falla porque crea una carpeta con nombre ya existente
%Crea sistema, añade drives, registra usuarios, cambia drives y agrega carpetas, finalmente crea una carpeta con nombre ya existente
system("SISTEMA", S1), systemAddDrive(S1, "C", "DISCO1", 420, S2), systemRegister(S2, "Kappita", S3), systemRegister(S3, "Gonzalo", S4), systemRegister(S4, "Juan", S5), systemLogin(S5, "Kappa", S6),
systemAddDrive(S6, "D", "Disco2", 4123, S7), systemAddDrive(S7, "E", "Disco3", 41234, S8), systemSwitchDrive(S8, "E", S9), systemLogout(S9, S10),
systemLogin(S10, "Gonzalo", S11), systemMkdir(S11, "Carpeta1", S12), systemSwitchDrive(S12, "C", S13), systemMkdir(S13, "Carpeta1", S14),
systemLogout(S14, S15), systemLogin(S15, "Kappa", S16), systemMkdir(S16, "Carpeta1", S17).

% Retomamos caso anterior
% Crea sistema, añade drive, registra 3 usuarios, añade 2 drives más, cambia al drive e, cierra sesión, vuelve a iniciar sesión,
% crea carpeta, cambia drive, crea carpeta,  cierra sesión, inicia sesión, crea carpeta, entra en la carpeta, crea carpeta,
% entra a la carpeta, crea archivo y lo añade, copia archivos, vuelve a la raíz, crea 4 archivos y los añade, copia, mueve y cambia
% de drive, mueve nuevamente los archivos, cabia de drive, entra a Carpeta1, usa dir en los contenidos, usa dir mostrando archivos
% ocultos, encripta, desencripta, encripta, desencripta con contraseña incorrecta, desencripta con contraseña correcta,
% Viaja una carpeta atrás, encripta una carpeta, muestra los contenidos encriptados, viaja una carpeta atrás y desencripta la carpeta
% Renombra 3 archivos y formatea 3 drives
system("SISTEMA", S1), systemAddDrive(S1, "Disco 1", "C", 420, S2), systemRegister(S2, "Kappita", S3), systemRegister(S3, "Gonzalo", S4), systemRegister(S4, "Juan", S5), systemLogin(S5, "Kappita", S6),
systemAddDrive(S6, "Disco2", "D", 4123, S7), systemAddDrive(S7, "Disco 3", "E", 41234, S8), systemSwitchDrive(S8, "E", S9), systemLogout(S9, S10),
systemLogin(S10, "Gonzalo", S11), systemMkdir(S11, "Carpeta1", S12), systemSwitchDrive(S12, "C", S13), systemMkdir(S13, "Carpeta1", S14),
systemLogout(S14, S15), systemLogin(S15, "Kappita", S16), systemMkdir(S16, "Carpeta2", S17), systemCd(S17, "/Carpeta2", S18),
systemMkdir(S18, "Carpeta3", S19), systemCd(S19, "Carpeta3/", S20), file("Archivo1.txt", "Esto es contenido jaja", F1),
systemAddFile(S20, F1, S21), systemCopy(S21, "*.txt", "E:/", S22), systemCopy(S22, "Archivo1.*", "E:/Carpeta1", S23),
systemCd(S23, "/", S24), file("Archivo1.pdf", "Esto es un pdf", F2), file("Archivo2.txt", "Esto es un archivo de texto", F3),
file("Archivo3.xlsx", "Este es un archivo con un formato raro", F4), file(".Archivo4.txt", "Este archivo es secreto", F5),
systemAddFile(S24, F2, S25), systemAddFile(S25, F3, S26), systemAddFile(S26, F4, S27), systemAddFile(S27, F5, S28),
systemCopy(S28, "*.txt", "D:/", S29), systemMove(S29, "*", "D:/", S30), systemSwitchDrive(S30, "D", S31),
systemMove(S31, "*.txt", "E:/Carpeta1", S32), systemMove(S32, "Archivo*.*", "E:/Carpeta1", S33),
systemSwitchDrive(S33, "E", S34), systemCd(S34, "Carpeta1", S35), systemDir(S35, [], Texto1),
systemDir(S35, ["/a"], Texto2), systemEncrypt(S35, "Contraseña", "Archivo3.*", S36),
systemDecrypt(S36, "Contra", "Archivo3.*", S37), systemDecrypt(S37, "Contraseña", "Archivo3.*", S38),
systemEncrypt(S38, "ContraseñaNueva", "Archivo3.*", S39), systemDecrypt(S39, "ContraseñaNueva", "Archivo3.*", S40),
systemCd(S40, "..", S41), systemEncrypt(S41, "LiesOfP", "Carpeta1", S42), systemCd(S42, "/Carpeta1", S43),
systemDir(S43, [], Texto3), systemCd(S43, "..", S44), systemDecrypt(S44, "LiesOfP", "Carpeta1", S45),
systemCd(S45, "Carpeta1/", S46), systemDir(S46, [], Texto4), systemRen(S46, "Archivo3.xlsx", "Archivo33.xlsx", S47),
systemRen(S47, "Archivo1.pdf", "Archivo11.pdf", S48), systemRen(S48, "Archivo2.txt", "Archivo22.txt", S49),
systemDel(S49, "Archivo3.xslx", S50), systemDel(S49, "*.txt", S51), systemDel(S49, "*", S52),
systemFormat(S52, "C", "NuevoC", S53), systemFormat(S53, "D", "NuevoD", S54), systemFormat(S54, "E", "NuevoE", S55).