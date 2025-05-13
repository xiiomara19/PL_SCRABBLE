:-	dynamic
			idioma/1,					% Idioma en el que se forma
			modo/1,                 	% Modo de juego: pvp o pve
			reparto/1,              	% Modo en el que se reparten las fichas
			empieza/1,              	% Indica que jugador empezará
			puntuacion/2,           	% Guarda la puntuación asociada con cada jugador
			empezado/1,             	% Indica si hay una partida en progreso (0: no, 1: si)
			initial_round/1, 			% Jugador que empieza la partida: player_1 o player_2
			next_round/1,				% Jugador que tiene el turno: player_1, player_2 o end (partida finalizada)
			historial_puntuaciones/3.	% Guarda el historial de puntuaciones de los jugadores

% Opciones de configuración
:- assertz(idioma(es)).					% Idioma por defecto: EspaÑol
:- assertz(modo(pve)).	            	% Modo de juego por defecto: pve
:- assertz(reparto(aleatorio)).	    	% Modo en el que se reparten las fichas por defecto: aleatorio
:- assertz(empieza(0)).		    		% Modo de inicio de partida: normal
:- assertz(initial_round(player)).		% Jugador que empieza la partida: player
:- assertz(next_round(end)).			% Jugador que tiene el turno: player, maquina, player_1, player_2 o end (partida finalizada)

:- assertz(puntuacion(player, 0)).  	% Indica con que puntuación empezara el jugador 1: 0
:- assertz(puntuacion(maquina, 0)).  	% Indica con que puntuación empezara el jugador 2 (la máquina): 0
:- assertz(empezado(0)).				% Indica que la partida todavia no ha empezado

:-assertz(historial_puntuaciones(player,0,w)).	% Historial de puntuaciones del jugador 1 y si ha ganado o perdido
:-assertz(historial_puntuaciones(maquina,0,l)).	% Historial de puntuaciones del jugador 2 (la máquina) y si ha ganado o perdido



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONFIGURACION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%==========Predicados para comprobar que las nuevas opciones son correctas============
opcionesIdioma(eu).
opcionesIdioma(es).
opcionesIdioma(en).
opcionesIdioma(_):- throw('No existe ese idioma').


opcionesModo(pve).
opcionesModo(pvp).
opcionesModo(_):- throw('No existe ese modo de juego').

opcionesReparto(aleatorio).
opcionesReparto(manual).
opcionesReparto(_):- throw('No existe ese modo de reparto').

opcionesEmpieza(0).
opcionesEmpieza(1).
opcionesEmpieza(_):- throw('No existe ese modo de inicio').

% ver_opcion(+O) muestra el valor establecido en el apartado de configuración O. Si el apartado de configuración O no existe, la llamada termina en error.
ver_opcion(idioma):- idioma(A), write(A), !.
ver_opcion(modo):- modo(A), write(A), !.
ver_opcion(reparto):- reparto(A), write(A), !.
ver_opcion(empieza):- empieza(A), write(A), !.
ver_opcion(_):- throw('Error esa opcion no existe').

% Si no hay ninguna partida iniciada, establecer_opcion(+O,+V) establece el apartado de configuración O al valor V. Si hay una partida iniciada, el apartado 
% de configuración O no existe o bien si el valor V no se corresponde con el apartado de configuración O, entonces la llamada termina en error.
establecer_opcion(_,_):- empezado(1), throw('No se pueden cambiar las opciones de configuracion mientras hay una partida en curso').
establecer_opcion(idioma,A):- empezado(0), opcionesIdioma(A), retract(idioma(_)), asserta(idioma(A)), !.
establecer_opcion(modo,A):- empezado(0), opcionesModo(A), retract(modo(_)), asserta(modo(A)), !.
establecer_opcion(reparto,A):- empezado(0),  opcionesReparto(A), retract(reparto(_)), asserta(reparto(A)), !.
establecer_opcion(empieza,A):- empezado(0), opcionesEmpieza(A), retract(empieza(_)), asserta(empieza(A)), !.
establecer_opcion(_,_):- throw('No existe el apartado de configuracion especificado').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TABLERO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% crear_tablero(-TableroFinal) tiene éxito si TableroFinal es una matriz de 15x15 que representa el tablero de juego, donde cada celda puede contener un valor 
% especial o estar vacía.
crear_tablero(TableroFinal) :-
    crear_tablero_base(TableroVacio),				% Crear el tablero vacío
    celdas_especiales(PosEspeciales),		% Obtener las posiciones de las celdas especiales
    insertar_celdas_especiales(PosEspeciales, TableroVacio, TableroFinal).

% crear_tablero_base(-B) tiene éxito si B es una matriz de 15x15 que representa el tablero de juego, donde cada celda está vacía (representada por el identificador 'SC').
crear_tablero_base(B):-
	length(Row, 15), maplist(=('(''SC'', )'), Row), 	% Crea una fila con N elementos vacíos
    length(B, 15), maplist(=(Row), B).    				% Repite esa fila N veces para crear la matriz

% celdas_especiales(+C) tiene éxito si C es una lista de celdas especiales, donde cada celda especial es una tupla (F,C,T) que indica la fila F, la columna C y 
% el tipo T de la celda especial. Hay cuatro tipos de celdas especiales:
% 	1. TP (Triple Palabra): multiplica por 3 la puntuación de la palabra formada en esa celda.
% 	2. DP (Doble Palabra): multiplica por 2 la puntuación de la palabra formada en esa celda.
% 	3. TL (Triple Letra): multiplica por 3 la puntuación de la letra colocada en esa celda.
% 	4. DL (Doble Letra): multiplica por 2 la puntuación de la letra colocada en esa celda.
celdas_especiales([
    (0, 0, '(''TP'', )'), (0, 7, '(''TP'', )'), (0,14, '(''TP'', )'),
    (7, 0, '(''TP'', )'), (7,14, '(''TP'', )'),
    (14, 0,'(''TP'', )'), (14,7,'(''TP'', )'), (14,14,'(''TP'', )'),

    (1, 1, '(''DP'', )'), (2, 2, '(''DP'', )'), (3, 3, '(''DP'', )'),
    (4, 4, '(''DP'', )'), (10,10,'(''DP'', )'), (11,11,'(''DP'', )'),
    (12,12,'(''DP'', )'), (13,13,'(''DP'', )'),

    (1, 5, '(''TL'', )'), (5, 1, '(''TL'', )'), (5, 5, '(''TL'', )'), (5, 9, '(''TL'', )'), (9, 5, '(''TL'', )'),

    (0, 3, '(''DL'', )'), (2, 6, '(''DL'', )'), (3, 0, '(''DL'', )'), (3, 7, '(''DL'', )'), (3,14, '(''DL'', )')
]).

% insertar_celdas_especiales(+C,+BoardIn,-BoardOut) tiene éxito si BoardOut es el tablero BoardIn con las celdas especiales de C insertadas.
insertar_celdas_especiales([], Board, Board).
insertar_celdas_especiales([(F,C,V)|T], BoardIn, BoardOut) :-
    set_cell(F, C, V, BoardIn, BoardTemp),
    insertar_celdas_especiales(T, BoardTemp, BoardOut).

% set_cell(+F,+C,+V,+BoardIn,-BoardOut) tiene éxito si BoardOut es el tablero BoardIn con la celda (F,C) reemplazada por el valor V.
set_cell(F, C, V, BoardIn, BoardOut) :-
    % Extraer la fila en posición F
    append(TopRows, [OldRow|BottomRows], BoardIn),
    length(TopRows, F),

    % Reemplazar la columna C en esa fila
    reemplazar_celda(C, OldRow, V, NewRow),

    % Reinsertar la nueva fila en la matriz
    append(TopRows, [NewRow|BottomRows], BoardOut).


reemplazar_celda(C, RowIn, NewValue, RowOut) :-
    append(Left, [_|Right], RowIn),
    length(Left, C),
    append(Left, [NewValue|Right], RowOut).
	
% mostrar_tablero(+B) tiene éxito si B es una matriz (lista de listas) de tamaño 15, y escribe su contenido en pantalla
mostrar_tablero(B):-
		length(B,15),
		maplist(same_length(B),B),
		!,
		W is 9*15+1,
		length(L,W),
		maplist(=('-'),L),
		atom_chars(S,L),
		maplist(mostrar_fila(S),B),
		write(S),
		nl.
	
mostrar_tablero(_):- throw('El tablero no es de tamaño 15x15').

% mostrar_fila (+S,+R) tiene éxito siempre, y escribe en pantalla el contenido de la lista R tras escribir S
mostrar_fila(S,R):- write(S), nl, write('|'), maplist(mostrar_item, R), nl .

% mostrar_item(+C) tiene éxito siempre y escribe su valor en pantalla
mostrar_item(C):- write(C), write('|').

% actualizar_tablero


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUGABILIDAD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iniciar_partida(+J) (modo persona vs maquina) da inicio a una nueva partida del jugador J con la configuración actual. Si ya había una partida iniciada, 
% entonces la llamada termina en error.

iniciar_partida(_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_):- modo(pvp), throw('Modo de juego incorrecto, se esperaba pve').
iniciar_partida(player):- 
	empezado(0), retractall(empezado(_)), asserta(empezado(1)), 					% Comprobamos que no haya una partida iniciada y la iniciamos
	modo(pve),																		% Comprobamos que el modo de juego es pve
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores	
	asserta(puntuacion(player, 0)),	asserta(puntuacion(maquina, 0)),				% Inicializamos la puntuación del jugador 1 y el jugador 2 (la máquina) a 0
	(
		initial_round(player) -> retractall(next_round(_)), asserta(next_round(player));		% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(next_round(_)), asserta(next_round(maquina))									% Comprobamos que la máquina empieza la partida y lo asignamos
	),
	crear_tablero(B), mostrar_tablero(B).											% Creamos el tablero y lo mostramos por pantalla	

% iniciar_partida(+J1,+J2) (modo persona vs persona) da inicio a una nueva partida de los jugadores J1 y J2 con la configuración actual. Si ya había una 
% partida iniciada, entonces la llamada termina en error.

iniciar_partida(_,_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_,_):- modo(pve), throw('Modo de juego incorrecto, se esperaba pvp').
iniciar_partida(player_1, player_2):- 
	empezado(0), retractall(empezado(_)), asserta(empezado(1)),						% Comprobamos que no haya una partida iniciada y la iniciamos
	modo(pvp), 																		% Comprobamos que el modo de juego es pvp
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores
	asserta(puntuacion(player_1 , 0)), asserta(puntuacion(player_2, 0)),			% Inicializamos la puntuación del jgador 1 y el jugador 2 a 0
	(
		initial_round(player_1) -> retractall(next_round(_)), asserta(next_round(player_1));		% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(next_round(_)), asserta(next_round(player_2))										% Comprobamos que el jugador 2 empieza la partida y lo asignamos
	),
	crear_tablero(B), mostrar_tablero(B).											% Creamos el tablero y lo mostramos por pantalla	

% Si hay una partida iniciada, abandonar_partida(+J) da la partida por perdida para el jugador J. Si no hay ninguna partida iniciada o bien el jugador J 
% no está jugando, entonces la llamada termina en error.

abandonar_partida(_):- empezado(0), throw('No hay ninguna partida iniciada').
abandonar_partida(J):- \+member(J, [player, player_1, player_2, maquina]), throw('El jugador no está jugando').
abandonar_partida(J):- 
	empezado(1), retractall(empezado(_)), asserta(empezado(0)),						% Comprobamos que hay una partida iniciada y la terminamos
	retractall(next_round(_)), asserta(next_round(end)), 							% Indicamos que la siguiente ronda es el final de la partida
	puntuacion(I,P), assertz(historial_puntuaciones(I,P)), 							% Añadimos el historial de puntuaciones del jugador
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores						 
	(
		modo(pvp) ->
			otro_jugador(J, Ganador),
			format('El jugador ~w ha ganado la partida.~n', [Ganador])				% Mostramos quién gana
		;
			writeln('La máquina ha ganado la partida.')
	),
	(
		empieza(0) -> retractall(initial_round(_)), asserta(initial_round(player_1));							% Comprobamos que el modo de juego es normal y asignamos el inicio de la partida al jugador 1
		empieza(1), initial_round(player_1) -> retractall(initial_round(_)), asserta(initial_round(player_2));	% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 2
		retractall(initial_round(_)), asserta(initial_round(player_1))											% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 1
	).

otro_jugador(player_1, player_2).
otro_jugador(player_2, player_1).
otro_jugador(player, maquina).
otro_jugador(maquina, player).

%  Si hay una partida iniciada y es el turno del jugador J, formar_palabra(+J,+O,+F,+C,+P) introduce la palabra P en orientación O (horizontal o vertical) 
% desde la fila F y la columna C y suma los puntos correspondientes al jugador J. Si no hay una partida iniciada, no es el turno del jugador J, la palabra 
% P no encaja en orientación O desde la fila F y la columna C o bien el jugador J no dispone de las fichas necesarias para formar la palabra P, entonces la 
% llamada finaliza en error.


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
		puntuacion(player_1, P1), puntuacion(player_2, P2),						% Comprobamos que el modo de juego es pvp y asignamos la puntuación del jugador 1 y el jugador 2
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
	format('Modo de juego: ~w~n', [M]), format('Idioma: ~w~n', [I]), format('Reparto: ~w~n', [R]), format('Empieza: ~w~n', [E]). 	% Mostramos las opciones de configuración


% ver_historial(+J) muestra el historial del jugador J: número de victorias y derrotas, puntuación máxima y puntuación media.


% ver_ranking muestra dos listas de jugadores: en la primera, junto a cada nombre de jugador aparece su número y porcentaje de partidas ganadas, y los jugadores 
% aparecen ordenados de manera descendente según el porcentaje de victorias; en la segunda, junto a cada nombre de jugador aparece su puntuación máxima y media, 
% y los jugadores aparecen ordenados de manera descendente según su puntuación media




% jugar_A
% jugar_B
% jugar_maquina (MAS ADELANTE)

% calcular_puntos

% validar_palabra (existe la palabra en el diccionario)
% validar_palabraCompuesta (se añade "palabra" nueva a una palabra existente y se comprueba si existe)

% validar_fichas (tiene las fichas necesarias para formar la palabra)
% validar_posicion (la palabra encaja en el tablero)



