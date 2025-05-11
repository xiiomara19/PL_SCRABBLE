:-	dynamic
			idioma/1,		% Idioma en el que se forma
			modo/1,                 % Modo de juego: pvp o pve
			reparto/1,              % Modo en el que se reparten las fichas
			empieza/1,              % Indica que jugador empezará

			puntuacion/2,           % Guarda la puntuación asociada con cada jugador
			empezado/1.             % Indica si hay una partida en progreso (0: no, 1: si)

% Opciones de configuración
:- assertz(idioma(español)).			% Idioma por defecto: Español
:- assertz(modo(pve)).	            	% Modo de juego por defecto: pve
:- assertz(reparto(aleatorio)).	    	% Modo en el que se reparten las fichas por defecto: aleatorio
:- assertz(empieza(0)).		    		% Modo de inicio de partida: normal
:- assertz(initial_round('player_1')).	% Jugador que empieza la partida: player_1
:- assertz(next_round(end)).			% Jugador que tiene el turno: player_1, player_2 o end (partida finalizada)

:- assertz(puntuacion('player_1', 0)).  % Indica con que puntuación empezara el jugador 1: 0
:- assertz(puntuacion(maquina, 0)).  % Indica con que puntuación empezara la máquina: 0
:- assertz(empezado(0)).			% Indica que la partida todavia no ha empezado

%==========Predicados para comprobar que las nuevas opciones son correctas============
opcionesIdioma(euskera).
opcionesIdioma(español).
opcionesIdioma(ingles).
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
establecer_opcion(O,_):- throw('No existe el apartado de configuracion especificado').

% iniciar_partida(+J) (modo persona vs maquina) da inicio a una nueva partida del jugador J con la configuración actual. Si ya había una partida iniciada, 
% entonces la llamada termina en error.

%TO-DO: ver lo del modo de juego y cambiar el initial_round al otro si el modo de juego es 0
iniciar_partida(_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_):- modo(pvp), throw('Modo de juego incorrecto, se esperaba pve').
iniciar_partida(J):- empezado(0), modo(pve), J =  /= '_maquina', retractall(empezado(_)), asserta(empezado(1)), retractall(empieza(_)), asserta(empieza(1)), 
			retractall(puntuacion(J, _)), retractall(puntuacion('_maquina', _)) asserta(puntuacion(J , 0)), asserta(puntuacion('_maquina', 0)).

% iniciar_partida(+J1,+J2) (modo persona vs persona) da inicio a una nueva partida de los jugadores J1 y J2 con la configuración actual. Si ya había una 
% partida iniciada, entonces la llamada termina en error.

%TO-DO: ver lo del modo de juego y cambiar el initial_round al otro si el modo de juego es 0
iniciar_partida(_,_):- empezado(1), throw('Ya hay una partida iniciada').
iniciar_partida(_,_):- modo(pve), throw('Modo de juego incorrecto, se esperaba pvp').
iniciar_partida(J1, J2):- empezado(0), modo(pvp), retractall(empezado(_)), asserta(empezado(1)), retractall(empieza(_)), asserta(empieza(1)), 
			retractall(puntuacion(J1, _)), retractall(puntuacion(J2, _)) asserta(puntuacion(J1 , 0)), asserta(puntuacion(J2, 0)).

% Si hay una partida iniciada, abandonar_partida(+J) da la partida por perdida para el jugador J. Si no hay ninguna partida iniciada o bien el jugador J 
% no está jugando, entonces la llamada termina en error.
abandonar_partida(_):- empezado(0), throw('No hay ninguna partida iniciada').
abandonar_partida(J):- empezado(1),  next_round(end), retract(puntuacion(player_1, P)), retract(puntuacion(maquina, _)), asserta(puntuacion(maquina, P)), 
			retractall(empezado(_)), asserta(empezado(0)), !, write('El jugador se ha rendido').
abandonar_partida(J):- empezado(1), next_round(end), retract(puntuacion(player_1, P)), retract(puntuacion(player_2, _)), asserta(puntuacion(player_2, P)),
			retractall(empezado(_)), asserta(empezado(0)), !, write('El jugador se ha rendido').
abandonar_partida(_):- throw('El jugador no está jugando').

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

% Si hay una partida iniciada, ver_resumen muestra un resumen de la partida actual que incluye:
%   a) Configuración de la partida.
%   b) Resumen de las palabras formadas, los puntos obtenidos con cada una y las fichas disponibles en cada turno.
% Si no hay una partida iniciada, entonces la llamada termina en error.

% ver_historial(+J) muestra el historial del jugador J: número de victorias y derrotas, puntuación máxima y puntuación media.

% ver_ranking muestra dos listas de jugadores: en la primera, junto a cada nombre de jugador aparece su número y porcentaje de partidas ganadas, y los jugadores 
% aparecen ordenados de manera descendente según el porcentaje de victorias; en la segunda, junto a cada nombre de jugador aparece su puntuación máxima y media, 
% y los jugadores aparecen ordenados de manera descendente según su puntuación media

