:-	dynamic
			idioma/1,					% Idioma en el que se forma
			modo/1,                 	% Modo de juego: pvp o pve
			reparto/1,              	% Modo en el que se reparten las fichas
			empieza/1,              	% Indica que jugador empezará
			puntuacion/2,           	% Guarda la puntuación asociada con cada jugador
			empezado/1,             	% Indica si hay una partida en progreso (0: no, 1: si)
			ronda_inicial/1, 			% Jugador que empieza la partida: player_1 o player_2
			siguiente_ronda/1,			% Jugador que tiene el turno: player_1, player_2 o end (partida finalizada)
			historial_puntuaciones/3,	% Guarda el historial de puntuaciones de los jugadores y si ha ganado o perdido
			diccionario/1,				% Guarda el diccionario de palabras segun el idioma de la partida
			tablero/1,					% Guarda el tablero de juego
			fichas_jugador/2.			% Guarda las fichas de cada jugador

% Opciones de configuración
:- assertz(idioma(es)).					% Idioma por defecto: EspaÑol
:- assertz(modo(pve)).	            	% Modo de juego por defecto: pve
:- assertz(reparto(aleatorio)).	    	% Modo en el que se reparten las fichas por defecto: aleatorio
:- assertz(empieza(0)).		    		% Modo de inicio de partida: normal
:- assertz(ronda_inicial(player)).		% Jugador que empieza la partida: player
:- assertz(siguiente_ronda(end)).		% Jugador que tiene el turno: player, maquina, player_1, player_2 o end (partida finalizada)

:- assertz(empezado(0)).				% Indica que la partida todavia no ha empezado




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DICCIONARIO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cargar_diccionario(+L) 
% tiene éxito si el diccionario de palabras en el idioma Idioma se carga correctamente. El diccionario debe estar en un archivo de texto
% con el nombre 'words.Idioma.txt' y debe contener una palabra por línea. Si el archivo no existe o no se puede abrir, la llamada termina en error.
% Tambien carga las letras, su puntuacion y la cantidad de estas en el predicado dinamico char_puntos_apariciones(Letra,Puntos,Cantidad)
cargar_diccionario(L):- 
	atomic_list_concat(['./diccionarios/words.', L, '.txt'], Diccionario),  	% Crear el nombre del archivo
	open(Diccionario, read, Stream, [encoding(utf8)]),							% Abrir el archivo en modo lectura con codificación UTF-8
	obtener_lineas(Stream, Lineas),
	close(Stream), !,
	retractall(diccionario(_)),													% Limpiar el diccionario actual	
	retractall(char_puntos(_,_)),						
	asserta(diccionario(Lineas)).												% Cargar el nuevo diccionario

cargar_diccionario(_):- throw('No se ha podido cargar el diccionario').

% cargar_letras(+L) tiene éxito si el diccionario de letras en el idioma Idioma se carga correctamente. El diccionario debe estar en un archivo de texto
cargar_letras(L):- 
	atomic_list_concat(['./letras/letras_', L, '.pl'], Caracteres),	% Crear el nombre del archivo
	consult(Caracteres).											% Cargar las letras validas, su puntuación y la cantidad de estas

cargar_letras(_):- throw('No se ha podido cargar el diccionario de letras').


% obtener_lineas(+Stream,-Lineas) 
% tiene éxito si Lineas es una lista de palabras leídas desde el flujo Stream. Cada palabra se considera una línea del archivo.
obtener_lineas(Stream, []) :-
    at_end_of_stream(Stream), !.

obtener_lineas(Stream, [Palabra|Resto]) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Palabra, Codes),
    obtener_lineas(Stream, Resto).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONFIGURACION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%==========Predicados para comprobar que las nuevas opciones son correctas============
opcionesIdioma(eus):-!.
opcionesIdioma(es):- !.
opcionesIdioma(en):- !.
opcionesIdioma(_):- throw('No existe ese idioma').


opcionesModo(pve):- !.
opcionesModo(pvp):- !.
opcionesModo(_):- throw('No existe ese modo de juego').

opcionesReparto(aleatorio):- !.
opcionesReparto(manual):- !.
opcionesReparto(_):- throw('No existe ese modo de reparto').

opcionesEmpieza(0):- !.
opcionesEmpieza(1):- !.
opcionesEmpieza(_):- throw('No existe ese modo de inicio').

% ver_opcion(+O) 
% muestra el valor establecido en el apartado de configuración O. Si el apartado de configuración O no existe, la llamada termina en error.
ver_opcion(idioma):- idioma(A), write(A), !.
ver_opcion(modo):- modo(A), write(A), !.
ver_opcion(reparto):- reparto(A), write(A), !.
ver_opcion(empieza):- empieza(A), write(A), !.
ver_opcion(_):- throw('Error esa opcion no existe').

% establecer_opcion(+O,+V)
% Si no hay ninguna partida iniciada, establece el apartado de configuración O al valor V. Si hay una partida iniciada, el apartado 
% de configuración O no existe o bien si el valor V no se corresponde con el apartado de configuración O, entonces la llamada termina en error.
establecer_opcion(_,_):- empezado(1), throw('No se pueden cambiar las opciones de configuracion mientras hay una partida en curso').
establecer_opcion(idioma,A):- empezado(0), opcionesIdioma(A), retract(idioma(_)), asserta(idioma(A)), cargar_diccionario(A), cargar_letras(A), !.
establecer_opcion(modo,A):- empezado(0), opcionesModo(A), retract(modo(_)), asserta(modo(A)),  cargar_diccionario(A), cargar_letras(A), !.
establecer_opcion(reparto,A):- empezado(0),  opcionesReparto(A), retract(reparto(_)), asserta(reparto(A)),  cargar_diccionario(A), cargar_letras(A), !.
establecer_opcion(empieza,A):- empezado(0), opcionesEmpieza(A), retract(empieza(_)), asserta(empieza(A)),  cargar_diccionario(A), cargar_letras(A), !.
establecer_opcion(_,_):- throw('No existe el apartado de configuracion especificado').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TABLERO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% crear_tablero()
% crear_tablero tiene éxito siempre y crea una matriz de 15x15 que representa el tablero de juego, donde cada celda puede contener un valor 
% especial o estar vacía.
crear_tablero :-
    crear_tablero_base(TableroVacio),	
	retractall(tablero(_)), asserta(tablero(TableroVacio)),									% Crear el tablero vacío
    celdas_especiales(PosEspeciales),										% Obtener las posiciones de las celdas especiales
    insertar_celdas_especiales(PosEspeciales),
	mostrar_tablero(). 				% Guardar el tablero creado

% crear_tablero_base(-B) 
% tiene éxito si B es una matriz de 15x15 que representa el tablero de juego, donde cada celda está vacía (representada por el identificador '---').
crear_tablero_base(B):-
	length(Row, 15), maplist(=(' --- '), Row), 			% Crea una fila con N elementos vacíos
    length(B, 15), maplist(=(Row), B).    				% Repite esa fila N veces para crear la matriz

% celdas_especiales(+C) 
% tiene éxito si C es una lista de celdas especiales, donde cada celda especial es una tupla (F,C,T) que indica la fila F, la columna C y 
% el tipo T de la celda especial. Hay cuatro tipos de celdas especiales:
% 	1. TP (Triple Palabra): multiplica por 3 la puntuación de la palabra formada en esa celda.
% 	2. DP (Doble Palabra): multiplica por 2 la puntuación de la palabra formada en esa celda.
% 	3. TL (Triple Letra): multiplica por 3 la puntuación de la letra colocada en esa celda.
% 	4. DL (Doble Letra): multiplica por 2 la puntuación de la letra colocada en esa celda.
celdas_especiales([
    (0, 0, ' TP  '), (0, 7, ' TP  '), (0,14, ' TP  '),
    (7, 0, ' TP  '), (7,14, ' TP  '),
    (14, 0,' TP  '), (14,7,' TP  '), (14,14,' TP  '),

    (1, 1, ' DP  '), (2, 2, ' DP  '), (3, 3, ' DP  '), (4, 4, ' DP  '), 
	(1, 13,' DP  '), (2, 12,' DP  '), (3, 11,' DP  '), (4, 10,' DP  '),
	(10,10,' DP  '), (11,11,' DP  '), (12,12,' DP  '), (13,13,' DP  '),
	(10,4, ' DP  '), (11,3, ' DP  '), (12,2, ' DP  '), (13,1, ' DP  '),

    (1, 5, ' TL  '), (1, 9, ' TL  '), 
	(5, 1, ' TL  '), (5, 5, ' TL  '), (5, 9, ' TL  '), (5, 13, ' TL  '), 
	(9, 1, ' TL  '), (9, 5, ' TL  '), (9, 9, ' TL  '), (9, 13, ' TL  '), 
	(9, 5, ' TL  '), (9, 9, ' TL  '),

    (0, 3, ' DL  '), (0, 11, ' DL  '), 
	(2, 6, ' DL  '), (2, 8, ' DL  '), 
	(3, 0, ' DL  '), (3, 7, ' DL  '), (3,14, ' DL  '),
	(6, 2, ' DL  '), (6, 6, ' DL  '), (6,8, ' DL  '), (6,12, ' DL  '),
	(7, 3, ' DL  '), (7, 11, ' DL  '), 
	(8, 2, ' DL  '), (8, 6, ' DL  '), (8,8, ' DL  '), (8,12, ' DL  '),
	(11, 0, ' DL  '), (11, 7, ' DL  '), (11,14, ' DL  '),
	(12, 6, ' DL  '), (12, 8, ' DL  '), 
	(14, 3, ' DL  '), (14, 11, ' DL  '),

	(7, 7, '  *  ')
]).

% insertar_celdas_especiales(+C,+BoardIn,-BoardOut) 
% tiene éxito si BoardOut es el tablero BoardIn con las celdas especiales de C insertadas.
insertar_celdas_especiales([]).
insertar_celdas_especiales([(F,C,V)|T]) :-
    set_cell(F, C, V),
    insertar_celdas_especiales(T).

% set_cell(+F,+C,+V) 
% tiene éxito si BoardOut es el tablero BoardIn con la celda (F,C) reemplazada por el valor V.
set_cell(F, C, V) :-
	tablero(B),

    % Extraer la fila en posición F
    append(TopRows, [OldRow|BottomRows], B),
    length(TopRows, F),

    % Reemplazar la columna C en esa fila
    reemplazar_celda(C, OldRow, V, NewRow),

    % Reinsertar la nueva fila en la matriz
    append(TopRows, [NewRow|BottomRows], BoardOut),
	retractall(tablero(_)), asserta(tablero(BoardOut)).


% reemplazar_celda(+C,+RowIn,+NewValue,-RowOut) tiene éxito si RowOut es la fila RowIn con la celda C reemplazada por NewValue.
reemplazar_celda(C, RowIn, NewValue, RowOut) :-
    append(Left, [_|Right], RowIn),
    length(Left, C),
    append(Left, [NewValue|Right], RowOut).

%get_cell(+F,+C,-R) dadas la fila y columna F y C devolvera el caracter en R que se encuentre en esa posicion en el tablero B
get_cell(F,C,R):- 
	tablero(B),
	X is F+1,
	nth1(X,B,Fila),
	Y is C+1,
	nth1(Y,Fila,R).

	
% mostrar_tablero tiene éxito siempre y escribe el contenido de una matriz (lista de listas) de tamaño 15 en pantalla
mostrar_tablero:-
	tablero(B),
	length(B,15),
	maplist(same_length(B),B),
	!,
	W is 6*15+1,
	length(L,W),
	maplist(=('-'),L),
	atom_chars(S,L),
	maplist(mostrar_fila(S),B),
	write(S),
	nl.
	
mostrar_tablero:- throw('El tablero no es de tamaño 15x15').

% mostrar_fila (+S,+R) tiene éxito siempre, y escribe en pantalla el contenido de la lista R tras escribir S
mostrar_fila(S,R):- write(S), nl, write('|'), maplist(mostrar_item, R), nl .

% mostrar_item(+C) tiene éxito siempre y escribe su valor en pantalla
mostrar_item(C):- atom_chars(C,L), length(L,X), X is 5, write(C), write('|').
mostrar_item(C):- atom_chars(C,L), length(L,X), X is 1, write('  '), write(C), write('  '), write('|').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUGABILIDAD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iniciar_partida(+J) (modo persona vs maquina) da inicio a una nueva partida del jugador J con la configuración actual. Si ya había una partida iniciada, 
% entonces la llamada termina en error.

iniciar_partida(_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_):- modo(pvp), throw('Modo de juego incorrecto, se esperaba pve').
iniciar_partida(player):- 
	empezado(0), retractall(empezado(_)), asserta(empezado(1)), 					% Comprobamos que no haya una partida iniciada y la iniciamos
	modo(pve),																		% Comprobamos que el modo de juego es pve
	idioma(L), cargar_diccionario(L), cargar_letras(L),								% Cargamos el diccionario y las letras
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores	
	asserta(puntuacion(player, 0)),	asserta(puntuacion(maquina, 0)),				% Inicializamos la puntuación del jugador 1 y el jugador 2 (la máquina) a 0
	(
		ronda_inicial(player) -> retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player));		% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(siguiente_ronda(_)), asserta(siguiente_ronda(maquina))									% Comprobamos que la máquina empieza la partida y lo asignamos
	),
	crear_tablero. %mostrar_tablero.											% Creamos el tablero y lo mostramos por pantalla	

% iniciar_partida(+J1,+J2) (modo persona vs persona) da inicio a una nueva partida de los jugadores J1 y J2 con la configuración actual. Si ya había una 
% partida iniciada, entonces la llamada termina en error.

iniciar_partida(_,_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_,_):- modo(pve), throw('Modo de juego incorrecto, se esperaba pvp').
iniciar_partida(player_1, player_2):- 
	empezado(0), retractall(empezado(_)), asserta(empezado(1)),						% Comprobamos que no haya una partida iniciada y la iniciamos
	modo(pvp), 																		% Comprobamos que el modo de juego es pvp
	idioma(L), cargar_diccionario(L), cargar_letras(L),								% Cargamos el diccionario y las letras
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores
	asserta(puntuacion(player_1 , 0)), asserta(puntuacion(player_2, 0)),			% Inicializamos la puntuación del jgador 1 y el jugador 2 a 0
	(
		ronda_inicial(player_1) -> retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player_1));		% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player_2))										% Comprobamos que el jugador 2 empieza la partida y lo asignamos
	),
	crear_tablero, mostrar_tablero.											% Creamos el tablero y lo mostramos por pantalla	

% Si hay una partida iniciada, abandonar_partida(+J) da la partida por perdida para el jugador J. Si no hay ninguna partida iniciada o bien el jugador J 
% no está jugando, entonces la llamada termina en error.

abandonar_partida(_):- empezado(0), throw('No hay ninguna partida iniciada').
abandonar_partida(J):- \+member(J, [player, player_1, player_2, maquina]), throw('El jugador no está jugando').
abandonar_partida(J):- 
	empezado(1), retractall(empezado(_)), asserta(empezado(0)),						% Comprobamos que hay una partida iniciada y la terminamos
	retractall(siguiente_ronda(_)), asserta(siguiente_ronda(end)), 					% Indicamos que la siguiente ronda es el final de la partida
	puntuacion(I,P), assertz(historial_puntuaciones(I,P)), 							% Añadimos el historial de puntuaciones del jugador
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores						 
	(
		modo(pvp) ->
			otro_jugador(J, Ganador),
			format('El jugador ~w ha ganado la partida.~n', [Ganador]),				% Mostramos quién gana
			puntuacion(Ganador, P1), puntuacion(J, P2),								% Obtenemos la puntuación de ambos jugadores
			assertz(historial_puntuaciones(Ganador, P1, w)),						% Añadimos el historial de puntuaciones del jugador ganador
			assertz(historial_puntuaciones(J, P2, l))								% Añadimos el historial de puntuaciones del jugador perdedor
		;
			writeln('La máquina ha ganado la partida.'),
			puntuacion(maquina, P1), puntuacion(player, P2),						% Obtenemos la puntuación de ambos jugadores
			assertz(historial_puntuaciones(maquina, P1, w)),						% Añadimos el historial de puntuaciones del jugador ganador
			assertz(historial_puntuaciones(player, P2, l))							% Añadimos el historial de puntuaciones del jugador perdedor
	),
	(
		empieza(0) -> retractall(ronda_inicial(_)), asserta(ronda_inicial(player_1));							% Comprobamos que el modo de juego es normal y asignamos el inicio de la partida al jugador 1
		empieza(1), ronda_inicial(player_1) -> retractall(ronda_inicial(_)), asserta(ronda_inicial(player_2));	% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 2
		retractall(ronda_inicial(_)), asserta(ronda_inicial(player_1))											% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 1
	).

% otro_jugador(+J,+O) tiene éxito si O es el jugador que le toca jugar contra J. Si J no es un jugador válido, entonces la llamada termina en error.
otro_jugador(player_1, player_2):- !.
otro_jugador(player_2, player_1):- !.
otro_jugador(player, maquina):- !.
otro_jugador(maquina, player):- !.
otro_jugador(_, _):- throw('El jugador no es válido').


%FORMAR_PALABRA(+J,+O,+F,+C,+P)
%  Si hay una partida iniciada y es el turno del jugador J, formar_palabra(+J,+O,+F,+C,+P) introduce la palabra P en orientación O (horizontal o vertical) 
% desde la fila F y la columna C y suma los puntos correspondientes al jugador J. Si no hay una partida iniciada, no es el turno del jugador J, la palabra 
% P no encaja en orientación O desde la fila F y la columna C o bien el jugador J no dispone de las fichas necesarias para formar la palabra P, entonces la 
% llamada finaliza en error.

formar_palabra(_,_,_,_,_):- empezado(0), throw('No hay ninguna partida iniciada').
%formar_palabra(J,_,_,_,_):- siguiente_ronda(P), P=\=J, throw('No es el turno del jugador J').
%formar_palabra(_,_,_,_,P):- 
%	\+validar_palabra_fichas(P), throw('No dispone de las fichas necesarias para formar la palabra').
formar_palabra(J,O,F,C,P):- 
	empezado(1),
	siguiente_ronda(J),
	atom_chars(P,L), 
	length(L,X), 
	(
		O = h -> comprobar_limites(C,X), comprobar_limites(F,0);
		O = v -> comprobar_limites(C,0), comprobar_limites(F,X)
	),
	comprobar_si_encaja(J,O,F,C,L,1,0),
	usa_letra(O,F,C,L), 
	actualizar_tablero(O,F,C,L),
	mostrar_tablero.

% ctualizar_tablero(+O,+F,+C,+B,+L) dada una lista de caracteres los escribe en el tablero B en la posiocion (F,C) en la orientacion O
actualizar_tablero(_,_,_,[]).
actualizar_tablero(h,F,C,[H|T]):- set_cell(F,C,H), X is C+1, actualizar_tablero(h,F,X,T).
actualizar_tablero(v,F,C,[H|T]):- set_cell(F,C,H), X is F+1, actualizar_tablero(v,X,C,T).

% usa_letra(O,F,C,L) comprueba que en la posicion (F,C) del tablero B haya al menos una ocurrencia de alguna de las letras que aparecen en L 
% en la posicion correspondiente
usa_letra(h,F,C,[H|T]):-
	(
		get_cell(F,C,H)->true;
		get_cell(F,C,'  *  ') -> true;
		X is C+1, usa_letra(h,F,X,T)
	).
usa_letra(v,F,C,[H|T]):-
	(
		get_cell(F,C,H)->true;
		get_cell(F,C,'  *  ') -> true;
		X is F+1, usa_letra(v,X,C,T)
	).

% comprobar_limites(P, L) comprueba que se puede escribir en la posicion P teniendo en cuenta que se va a desplazar L veces
comprobar_limites(P, L):- P >= 0, A is P+L, A<16.

% comprobar_si_encaja(+J,+O,+F,+C,+B,+L,+M,+P) comprueba si la palabra L encaja en el tablero B en la posicion (F,C) y devuelve la puntuacion obtenida
comprobar_si_encaja(J,_,_,_,[],M,P):- 
	P_palabra is M*P, 
	puntuacion(J, P_total), 
	Puntuacion is P_palabra+P_total, 
	retractall(puntuacion(J,_)),
	asserta(puntuacion(J,Puntuacion)),
	writeln(Puntuacion).
comprobar_si_encaja(J,h,F,C,[H|T],M,P):- 
	get_cell(F,C,H), 
	X is C+1, 
	char_puntos_apariciones(H,Puntos,_),
	P2 is P+Puntos,
	comprobar_si_encaja(J,h,F,X,T,M,P2).
comprobar_si_encaja(J,h,F,C,[H|T],M,P):- 
	get_cell(F,C,Z), 
	celdas_posibles(L), 
	member(Z,L),
	char_puntos_apariciones(H,Puntos,_),
	multiplicador_letra(Z,Mul_letra),
	multiplicador_palabra(Z,Mul_palabra),
	P2 is Puntos*Mul_letra,
	P3 is P+P2,
	write(Z ),writeln(Mul_letra),
	X is C+1, 
	M2 is max(M,Mul_palabra),
	comprobar_si_encaja(J,h,F,X,T,M2,P3).
comprobar_si_encaja(J,v,F,C,[H|T],M,P):- 
	get_cell(F,C,H), 
	X is F+1, 
	char_puntos_apariciones(H,Puntos,_),
	P2 is P+Puntos,
	comprobar_si_encaja(J,h,X,C,T,M,P2).
comprobar_si_encaja(v,F,C,[H|T],M,P):- 
	get_cell(F,C,Z), 
	celdas_posibles(L), 
	member(Z,L), 
	char_puntos_apariciones(H,Puntos,_),
	multiplicador_letra(Z,Mul_letra),
	multiplicador_palabra(Z,Mul_palabra),
	P2 is Puntos*Mul_letra,
	P3 is P+P2,
	X is F+1, 
	M2 is max(M,Mul_palabra),
	comprobar_si_encaja(h,X,C,T,M2,P3).


multiplicador_letra(' DL  ',2).
multiplicador_letra(' TL  ',3).
multiplicador_letra(_,1).


multiplicador_palabra(' DP  ',2).
multiplicador_palabra(' TP  ',3).
multiplicador_palabra(_,1).


celdas_posibles([' --- ', ' DL  ', ' TL  ', ' DP  ', ' TP  ', '  *  ']).


% Si hay una partida iniciada y el jugador J acaba de formar una palabra o bien la partida acaba de iniciarse, asignar_fichas(+J,+F) entrega al jugador J las 
% fichas F. En el caso del modo de juego persona vs máquina, el jugador máquina será identificado mediante ‘ordenador’. Si no hay una partida iniciada, el 
% jugador J no acaba de formar una palabra o no acaba de iniciarse la partida, las fichas F no forman parte de las que faltan por repartir o bien no son el 
% número de fichas que le faltan al jugador J (a excepción de que las fichas que falten por repartir sean menos de las que necesita el jugador J), entonces la 
% llamada finaliza en error.


% Si hay una partida iniciada y el jugador J participa en ella, mostrar_fichas(+J) muestra por pantalla las fichas de las que dispone el jugador J. En el caso
% del modo de juego persona vs máquina, el jugador máquina será identificado mediante ‘ordenador’. Si no hay una partida iniciada o bien el jugador J no 
% participa en ella, entonces la llamada finaliza en error.


%  Si hay una partida iniciada, mostrar_puntuación muestra la puntuación de ambos jugadores. Si no hay una partida iniciada, entonces la llamada termina en error.

mostrar_puntuacion:- empezado(0), throw('No hay ninguna partida iniciada').
mostrar_puntuacion:- 
	empezado(1),																% Comprobamos que hay una partida iniciada
	(
		modo(pvp) -> puntuacion(player, P1), puntuacion(maquina, P2);			% Comprobamos que el modo de juego es pvp y asignamos la puntuación del jugador 1 y el jugador 2 (la máquina)
		puntuacion(player_1, P1), puntuacion(player_2, P2)						% Comprobamos que el modo de juego es pvp y asignamos la puntuación del jugador 1 y el jugador 2
	),
	format('Puntuación del jugador 1: ~w~n', [P1]), format('Puntuación del jugador 2: ~w~n', [P2]). 	
	
% Si hay una partida iniciada, ver_resumen muestra un resumen de la partida actual que incluye:
%   a) Configuración de la partida.
%   b) Resumen de las palabras formadas, los puntos obtenidos con cada una y las fichas disponibles en cada turno.
% Si no hay una partida iniciada, entonces la llamada termina en error.

%TO-DO: Apartado B!! Resumen de las palabras formadas, los puntos obtenidos con cada una y las fichas disponibles en cada turno.
ver_resumen:- empezado(0), throw('No hay ninguna partida iniciada').
ver_resumen:- 
	empezado(1),																													% Comprobamos que hay una partida iniciada
	opcionesEmpieza(E), opcionesModo(M), opcionesReparto(R), opcionesIdioma(I),														% Obtenemos las opciones de configuración
	format('Modo de juego: ~w~n', [M]), format('Idioma: ~w~n', [I]), format('Reparto: ~w~n', [R]), format('Empieza: ~w~n', [E]) 	% Mostramos las opciones de configuración
	.

% ver_historial(+J) muestra el historial del jugador J: número de victorias y derrotas, puntuación máxima y puntuación media.
ver_historial(J):- var(J), !, throw('Debe especificar un jugador').
ver_historial(J):- \+member(J, [player, player_1, player_2, maquina]), !, throw('El jugador no es válido').
ver_historial(J):- findall(P,
	(historial_puntuaciones(J,P,l)),	
	L), length(L, NL), 						% Obtiene el número de partidas perdidas
	findall(P,
	(historial_puntuaciones(J,P,w)), 
	W), length(W, NW),						% Obtiene el número de partidas ganadas
	append(L,W,R),							% Une las listas de partidas ganadas y perdidas
	max_list(R, Max),						% Obtiene la puntuación máxima
	mean_list(R, Mean),						% Obtiene la puntuación media
	format('El jugador ~w ha ganado ~w partidas y ha perdido ~w partidas.~n Su puntuación máxima es ~w y la puntuación media es ~w~n', [J, NW, NL, Max, Mean]).

% mean_list(+L,-M) tiene éxito si M es la media de los elementos de la lista L.
mean_list(L, M) :- 
	sum_list(L, Sum), 
	length(L, N), 
	N > 0, 
	M is Sum / N.
	
	
% ver_ranking muestra dos listas de jugadores: en la primera, junto a cada nombre de jugador aparece su número y porcentaje de partidas ganadas, y los jugadores 
% aparecen ordenados de manera descendente según el porcentaje de victorias; en la segunda, junto a cada nombre de jugador aparece su puntuación máxima y media, 
% y los jugadores aparecen ordenados de manera descendente según su puntuación media




% jugar_A

% jugar_B
% jugar_maquina tiene éxito si el jugador es la máquina y se le asigna el turno. Si el jugador no es la máquina, entonces la llamada termina en error.
jugar_maquina:- 
	siguiente_ronda(maquina), 							% Si el jugador es la máquina, se le asigna el turno
	tablero(B), 										% Se obtiene el tablero actual
	fichas_jugador(maquina, Fichas), 					% Se obtiene la lista de letras disponibles para la máquina
	generar_palabra_aleatoria(Fichas, Palabra), 		% Se genera una palabra aleatoria
	obtener_posicion_aleatoria(Palabra, B, Posicion), 	% Se obtiene una posición aleatoria en el tablero
	formar_palabra(maquina, h, Posicion, Palabra),		% Se forma la palabra en el tablero
	siguiente_ronda(jugador).							% Se asigna el turno al jugador

jugar_maquina:- throw('El jugador no es la máquina').

% calcular_puntos

% validar_palabra(+P) tiene éxito si Palabra es una palabra válida en el idioma actual. Si la palabra no es válida, la llamada termina en error.
validar_palabra(P):- 
	diccionario(D), 
	member(P, D), !.
validar_palabra(_):- throw('La palabra no existe en el diccionario').

% validar_palabraCompuesta(+PalabraTablero,+PalabraJugada) tiene éxito si la composicion de PalabraTablero y PalabraJugada es una palabra válida en el idioma actual.
% Si la palabra no es válida, la llamada termina en error.
validar_palabraCompuesta(PalabraTablero, PalabraJugada):- 
	diccionario(Diccionario), 
	atom_concat(PalabraTablero, PalabraJugada, PalabraFinal),
	member(PalabraFinal, Diccionario), !.
validar_palabraCompuesta(_, _):- throw('La palabra compuesta no existe en el diccionario').

% validar_fichas (tiene las fichas necesarias para formar la palabra)


