:- consult('diccionario.pl').					% Cargar el diccionario de palabras
:- consult('configuracion.pl').					% Cargar la configuración del juego
:- consult('tablero.pl').						% Cargar el tablero de juego
:- consult('logica.pl').						% Carga los predicados encargados de la logica interna del juego

:-	dynamic
			puntuacion/2,           	% Guarda la puntuación asociada con cada jugador
			empezado/1,             	% Indica si hay una partida en progreso (0: no, 1: si)
			ronda_inicial/1, 			% Jugador que empieza la partida: player_1 o player_2
			siguiente_ronda/1,			% Jugador que tiene el turno: player_1, player_2 o end (partida finalizada)
			historial_puntuaciones/3,	% Guarda el historial de puntuaciones de los jugadores y si ha ganado o perdido		
			fichas_jugador/2.			% Guarda las fichas de cada jugador

% Opciones de configuración iniciales del juego
:- assertz(ronda_inicial(player)).		% Jugador que empieza la partida: player
:- assertz(siguiente_ronda(end)).		% Jugador que tiene el turno: player, maquina, player_1, player_2 o end (partida finalizada)
:- assertz(empezado(0)).				% Indica que la partida todavia no ha empezado



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARTIDA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iniciar_partida(+J) (modo persona vs maquina) da inicio a una nueva partida del jugador J con la configuración actual. Si ya había una partida iniciada, 
% entonces la llamada termina en error.

iniciar_partida(_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_):- modo(pvp), throw('Modo de juego incorrecto, se esperaba pve').
iniciar_partida(player):- 
	empezado(0), retractall(empezado(_)), asserta(empezado(1)), 					% Comprobamos que no haya una partida iniciada y la iniciamos
	modo(pve),																		% Comprobamos que el modo de juego es pve
	idioma(L), cargar_diccionario(L),												% Cargamos el diccionario y las letras
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
	idioma(L), cargar_diccionario(L), 												% Cargamos el diccionario y las letras
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

% validar_fichas (tiene las fichas necesarias para formar la palabra)


