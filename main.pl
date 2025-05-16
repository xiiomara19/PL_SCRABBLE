:- set_prolog_flag(encoding, utf8). 		% Establecer la codificación UTF-8 para la entrada y salida

:- consult('diccionario.pl').					% Cargar el diccionario de palabras
:- consult('configuracion.pl').					% Cargar la configuración del juego
:- consult('tablero.pl').						% Cargar el tablero de juego
:- consult('palabra.pl').						% Carga los predicados encargados de comprobar las palabras
:- consult('ordenador.pl').						% Cargar los predicados encargados de gestionar los jugadores

:-	dynamic
			puntuacion/2,           	% Guarda la puntuación asociada con cada jugador
			empezado/1,             	% Indica si hay una partida en progreso (0: no, 1: si)
			ronda_inicial/1, 			% Jugador que empieza la partida: player_1 o player_2
			siguiente_ronda/1,			% Jugador que tiene el turno: player_1, player_2 o end (partida finalizada)
			historial_puntuaciones/3,	% Guarda el historial de puntuaciones de los jugadores y si ha ganado o perdido		
			fichas_jugador/2.			% Guarda el nombre del jugador y sus fichas

% Opciones de configuración iniciales del juego
:- assertz(ronda_inicial(player_1)).		% Jugador que empieza la partida: player
:- assertz(siguiente_ronda(end)).		% Jugador que tiene el turno: player, ordenador, player_1, player_2 o end (partida finalizada)
:- assertz(empezado(0)).				% Indica que la partida todavia no ha empezado



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARTIDA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iniciar_partida(+J) (modo persona vs ordenador) da inicio a una nueva partida del jugador J con la configuración actual. Si ya había una partida iniciada, 
% entonces la llamada termina en error.
iniciar_partida(_):- empezado(1), !, throw('Ya hay una partida iniciada').
iniciar_partida(_):- modo(pvp), !, throw('Modo de juego incorrecto, se esperaba pve').
iniciar_partida(player):- 
	empezado(0),  																	% Comprobamos que no haya una partida iniciada
	modo(pve),																		% Comprobamos que el modo de juego es pve
	idioma(L), cargar_diccionario(L),												% Cargamos el diccionario y las letras
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores	
	(
		ronda_inicial(player) -> retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player));		% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(siguiente_ronda(_)), asserta(siguiente_ronda(ordenador))									% Comprobamos que la máquina empieza la partida y lo asignamos
	),
	crear_tablero,																% Creamos el tablero y lo mostramos por pantalla	
	inicializar_fichas(player), inicializar_fichas(ordenador),					% Asignamos las fichas al jugador y la maquina y las mostramos por pantalla
	asserta(puntuacion(player, 0)),	asserta(puntuacion(ordenador, 0)),			% Inicializamos la puntuación del jugador 1 y el jugador 2 (la máquina) a 0
	retractall(empezado(_)), asserta(empezado(1)),								% Iniciamos la partida
	mostrar_fichas(player), mostrar_fichas(ordenador).							% Mostramos las fichas de ambos jugadores

% iniciar_partida(+J1,+J2) (modo persona vs persona) da inicio a una nueva partida de los jugadores J1 y J2 con la configuración actual. Si ya había una 
% partida iniciada, entonces la llamada termina en error.

iniciar_partida(_,_):- empezado(1), !, throw('Ya hay una partida iniciada').
iniciar_partida(_,_):- modo(pve), !, throw('Modo de juego incorrecto, se esperaba pvp').
iniciar_partida(player_1, player_2):- 
	empezado(0), 																	% Comprobamos que no haya una partida iniciada
	modo(pvp), 																		% Comprobamos que el modo de juego es pvp
	idioma(L), cargar_diccionario(L), 												% Cargamos el diccionario y las letras
	retractall(puntuacion(_, _)), 													% Retractamos la puntuación de los jugadores
	(
		ronda_inicial(player_1) -> retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player_1));			% Comprobamos que el jugador 1 empieza la partida y lo asignamos
		retractall(siguiente_ronda(_)), asserta(siguiente_ronda(player_2))										% Comprobamos que el jugador 2 empieza la partida y lo asignamos
	),
	crear_tablero,																% Creamos el tablero y lo mostramos por pantalla	
	inicializar_fichas(player_1), inicializar_fichas(player_2),					% Asignamos las fichas al jugador 1 y al jugador 2 y las mostramos por pantalla
	asserta(puntuacion(player_1 , 0)), asserta(puntuacion(player_2, 0)),		% Inicializamos la puntuación del jgador 1 y el jugador 2 a 0
	retractall(empezado(_)), asserta(empezado(1)),								% Iniciamos la partida
	mostrar_fichas(player_1), mostrar_fichas(player_2).							% Mostramos las fichas de ambos jugadores		


% jugar_jugador(+O,+F,+C,+P)
% tiene éxito si es un jugador y es su turno. Se realizarán las validaciones de la palabra introducida y se mostrarán en el tablero. 
% Si el jugador no es el jugador 1, entonces la llamada termina en error.
jugar_jugador(_,_,_,_):- 
	siguiente_ronda(J), \+member(J,[player, player_1, player_2]), throw('No es el turno de este jugador').	% Comprobamos que el jugador 1 tiene el turno
jugar_jugador(O,F,C,P):- 
	siguiente_ronda(J), member(J,[player,player_1, player_2]),	
	formar_palabra(J,O,F,C,P),
	fichas_jugador(J,Fichas),
	length(Fichas,N),
	Rest is 7-N,
	asignar_fichas(J,Rest),
	pasar_turno(J),
	(
		%modo(pve) -> jugar_ordenador;		% Si el modo de juego es pve, se pasa el turno a la máquina
		true	
	).

% Si hay una partida iniciada, abandonar_partida(+J) da la partida por perdida para el jugador J. Si no hay ninguna partida iniciada o bien el jugador J 
% no está jugando, entonces la llamada termina en error.

abandonar_partida(_):- empezado(0), !, throw('No hay ninguna partida iniciada').
abandonar_partida(J):- \+member(J, [player, player_1, player_2, ordenador]), !, throw('El jugador no está jugando').
abandonar_partida(J):- \+siguiente_ronda(J), !, throw('No es el turno del jugador J').	% Comprobamos que el jugador tiene el turno
abandonar_partida(J):- 
	empezado(1), retractall(empezado(_)), asserta(empezado(0)),							% Comprobamos que hay una partida iniciada y la terminamos
	siguiente_ronda(J),		 															% Comprobamos que el jugador J tiene el turno
	retractall(siguiente_ronda(_)), asserta(siguiente_ronda(end)), 						% Indicamos que la siguiente ronda es el final de la partida					 
	(
		modo(pvp) ->
			otro_jugador(J, Ganador),
			format('El jugador ~w ha ganado la partida.~n', [Ganador]),					% Mostramos quién gana
			puntuacion(Ganador, P1), puntuacion(J, P2),									% Obtenemos la puntuación de ambos jugadores
			assertz(historial_puntuaciones(Ganador, P1, w)),							% Añadimos el historial de puntuaciones del jugador ganador
			assertz(historial_puntuaciones(J, P2, l)),									% Añadimos el historial de puntuaciones del jugador perdedor
			retractall(fichas_jugador(J,_)), 
			retractall(fichas_jugador(Ganador,_))										% Retractamos las fichas de los jugadores
		;
			modo(pve), writeln('El ordenador ha ganado la partida.'),
			puntuacion(ordenador, P1), puntuacion(player, P2),						% Obtenemos la puntuación de ambos jugadores
			assertz(historial_puntuaciones(ordenador, P1, w)),						% Añadimos el historial de puntuaciones del jugador ganador
			assertz(historial_puntuaciones(player, P2, l)),							% Añadimos el historial de puntuaciones del jugador perdedor
			retractall(fichas_jugador(player,_)),
			retractall(fichas_jugador(ordenador,_))									% Retractamos las fichas de los jugadores
	),
	retractall(puntuacion(_, _)), 														% Retractamos la puntuación de los jugadores	
	(
		empieza(0) -> retractall(ronda_inicial(_)), asserta(ronda_inicial(player_1));							% Comprobamos que el modo de juego es normal y asignamos el inicio de la partida al jugador 1
		empieza(1), ronda_inicial(player_1) -> retractall(ronda_inicial(_)), asserta(ronda_inicial(player_2));	% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 2
		retractall(ronda_inicial(_)), asserta(ronda_inicial(player_1))											% Comprobamos que el modo de juego es alterno y asignamos el inicio de la partida al jugador 1
	).

% otro_jugador(+J,+O) tiene éxito si O es el jugador que le toca jugar contra J. Si J no es un jugador válido, entonces la llamada termina en error.
otro_jugador(player_1, player_2):- !.
otro_jugador(player_2, player_1):- !.
otro_jugador(player, ordenador):- !.
otro_jugador(ordenador, player):- !.
otro_jugador(_, _):- throw('El jugador no es válido').

% acabar_partida
% Si hay una partida iniciada, acabar_partida da la partida por finalizada y muestra el jugador que ha ganado.
% Si no hay una partida iniciada, entonces la llamada termina en error.
acabar_partida():-
	empieza(0), !, throw('No hay ninguna partida iniciada').

acabar_partida():-
	puntuacion(_,P1),
	puntuacion(_,P2),
	PM is min(P1,P2),
	puntuacion(JM,PM),
	retractall(siguiente_ronda(_)), asserta(siguiente_ronda(JM)),
	abandonar_partida(JM).

% pasar_turno(+J)
% Si hay una partida iniciada y es el turno del jugador  J,  pasar_turno(+J)  pasa el turno al siguiente jugador. Si no hay una partida iniciada o bien no es 
% el turno del jugador J, entonces la llamada finaliza en error.
pasar_turno(_):- empezado(0), !, throw('No hay ninguna partida iniciada').
pasar_turno(J):-
	(
		modo(pvp), \+siguiente_ronda(J) -> throw('El jugador no es válido. No es el turo de ese jugador');		% Comprobamos que el modo de juego es pvp y el jugador es válido
		modo(pve), \+siguiente_ronda(J), throw('El jugador no es válido. No es el turo de ese jugador')		% Comprobamos que el modo de juego es pve y el jugador es válido
	), !.
pasar_turno(J):- 
	empezado(1),
	siguiente_ronda(J),		
	(
		modo(pvp) -> 
			otro_jugador(J, Siguiente), 
			retractall(siguiente_ronda(_)), asserta(siguiente_ronda(Siguiente)),									% Comprobamos que el modo de juego es pvp y pasamos el turno al siguiente jugador
			format('Ahora es el turno del jugador ~w~n',[Siguiente]);												% Mostramos el turno del siguiente jugador
		modo(pve) -> 
			retractall(siguiente_ronda(_)), asserta(siguiente_ronda(ordenador)),									% Comprobamos que el modo de juego es pve y pasamos el turno a la máquina
			writeln('Ahora es el turno del ordenador')																% Mostramos el turno del siguiente jugador
	).


% asignar_fichas(+J,+F)
% Si hay una partida iniciada y el jugador J acaba de formar una palabra o bien la partida acaba de iniciarse, asignar_fichas(+J,+F) entrega al jugador J las 
% fichas F. En el caso del modo de juego persona vs máquina, el jugador máquina será identificado mediante ‘ordenador’. Si no hay una partida iniciada, el 
% jugador J no acaba de formar una palabra o no acaba de iniciarse la partida, las fichas F no forman parte de las que faltan por repartir o bien no son el 
% número de fichas que le faltan al jugador J (a excepción de que las fichas que falten por repartir sean menos de las que necesita el jugador J), entonces la 
% llamada finaliza en error.

asignar_fichas(_,_):- empezado(0), !, throw('No hay ninguna partida iniciada').
asignar_fichas(J,_):- 
	(
		modo(pvp), \+siguiente_ronda(J), ! -> throw('El jugador no es válido');	% Comprobamos que el modo de juego es pvp y el jugador es válido
		modo(pve), \+siguiente_ronda(J), ! -> throw('El jugador no es válido')		% Comprobamos que el modo de juego es pve y el jugador es válido
	).
asignar_fichas(_,0):- write('El jugador no necesita fichas'), !.		% Si el jugador no necesita fichas, mostramos un mensaje
asignar_fichas(_,F):- F>7, throw('El número de fichas no es válido').	% Comprobamos que el número de fichas es válido

asignar_fichas(J,F):- 
	(
		reparto(aleatorio) -> obtener_fichas(F, J, Fichas);						% Comprobamos que el modo de reparto es aleatorio y asignamos las fichas automáticamente
		reparto(manual) -> pedir_fichas_manual(F, J, Fichas)						% Comprobamos que el modo de reparto es manual y asignamos las fichas manualmente
	), 
	retractall(fichas_jugador(J,_)), asserta(fichas_jugador(J,Fichas)),
	mostrar_fichas(J),
	(
		Fichas=[] -> 
			puntuacion(J,P1), otro_jugador(J,J2), puntuacion(J2,P2),
			(																					% Si no quedan más fichas, el jugador pierde
				P1<P2 -> abandonar_partida(J);
				P1>P2, retractall(siguiente_ronda(_)), asserta(siguiente_ronda(J2)), abandonar_partida(J2)
			);												
		true
	).

% inicializar_fichas(+J) tiene éxito si inicializa las fichas del jugador J a 7 letras aleatorias.
inicializar_fichas(J) :-
    (
		reparto(aleatorio) -> obtener_fichas(7, J, Fichas);					% Comprobamos que el modo de reparto es aleatorio y asignamos las fichas automáticamente	
		reparto(manual) -> pedir_fichas_manual(7, J, Fichas)						% Comprobamos que el modo de reparto es manual y asignamos las fichas manualmente
	),
	retractall(fichas_jugador(J,_)), asserta(fichas_jugador(J,Fichas)).

% pedir_fichas_manual(+F,+Fichas)
%  tiene éxito si pide al jugador J que elija las fichas que quiere. Si el número de letras a repartir es mayor que 7, entonces la llamada termina en error.
pedir_fichas_manual(F,J,L):- 
	(
		fichas_jugador(J, Fichas) -> true;									% Obtenemos las letras disponibles y las mezclamos aleatoriamente
		Fichas = []
	), 	
	bolsa_letras(B),
    format('Fichas disponibles en la bolsa: ~w~n', [B]),
	format('Selecciona ~w letras separadas por comas (por ejemplo: a,b,c,...):~n', [F]),
    read_line_to_string(user_input, Input),
    split_string(Input, ",", " ", LS),
    maplist(string_lower, LS, LString),
	maplist(atom_string, LP, LString),
    length(LP, N),
    ( 
		N =\= F -> writeln('Número incorrecto de letras.'), pedir_fichas_manual(F,J,L); 
		\+letras_validas(LP,B) ->
        	writeln('Has elegido letras que no están disponibles en la bolsa.'),
			pedir_fichas_manual(F,J,L); 
		true
    ),
	maplist(actualizar_letra_usada, LP),										% Actualizamos la bolsa de letras	
	append(Fichas, LP, L). 												% Obtenemos las letras al jugador.


% obtener_fichas(+F,+J,-L) 
% tiene éxito si L es una lista de F letras aleatorias que le faltan al jugador J. Si el número de letras a repartir es mayor que 7,
% entonces la llamada termina en error. Si el número de letras a repartir es 0, entonces L es una lista vacía.
obtener_fichas(F, _, _):- F>8, !, throw('El número de letras no es válido').		% Comprobamos que el número de letras a repartir es válido
obtener_fichas(0, _, []):- !.												% Si el número de letras a repartir es 0, entonces L es una lista vacía
obtener_fichas(F, J, L):-
	F>0, F<8,
	(
		fichas_jugador(J, Fichas) -> true;									% Obtenemos las letras disponibles y las mezclamos aleatoriamente
		Fichas = []
	), 											
	bolsa_letras(B),
	(
		B = [] -> write('No hay más letras disponibles'), L is Fichas; 
		random_permutation(B, Perm), 										% Mezclamos las letras de la bolsa
		length(LP, F),														% Obtenemos las letras que le faltan al jugador
		append(LP, _, Perm),
		maplist(actualizar_letra_usada, LP),								% Actualizamos la bolsa de letras	
		append(Fichas, LP, L) 												% Obtenemos las letras al jugador
	).			

% mostrar_fichas(+J)
% Si hay una partida iniciada y el jugador J participa en ella, mostrar_fichas(+J) muestra por pantalla las fichas de las que dispone el jugador J. En el caso
% del modo de juego persona vs máquina, el jugador máquina será identificado mediante ‘ordenador’. Si no hay una partida iniciada o bien el jugador J no 
% participa en ella, entonces la llamada finaliza en error.
mostrar_fichas(_):- empezado(0), throw('No hay ninguna partida iniciada').
mostrar_fichas(J):- 
	(
		modo(pve), member(J, [player, ordenador]) -> fichas_jugador(J,F);	% Comprobamos que el modo de juego es pve y asignamos la lista de letras del jugador
		modo(pvp), member(J, [player_1, player_2]) -> fichas_jugador(J,F)	% Comprobamos que el modo de juego es pvp y asignamos la lista de letras del jugador
			
	),
	(
		F = [] -> throw('El jugador no tiene fichas');						% Notificamos que el jugador no tiene fichas
		format('Fichas del jugador ~w: ~w~n', [J, F])						% Mostramos las fichas del jugador	
	).						


%  Si hay una partida iniciada, mostrar_puntuación muestra la puntuación de ambos jugadores. Si no hay una partida iniciada, entonces la llamada termina en error.
mostrar_puntuacion:- empezado(0), throw('No hay ninguna partida iniciada').
mostrar_puntuacion:- 
	empezado(1),																% Comprobamos que hay una partida iniciada
	(
		modo(pve) -> puntuacion(player, P1), puntuacion(ordenador, P2);			% Comprobamos que el modo de juego es pvp y asignamos la puntuación del jugador 1 y el jugador 2 (la máquina)
		puntuacion(player_1, P1), puntuacion(player_2, P2)						% Comprobamos que el modo de juego es pvp y asignamos la puntuación del jugador 1 y el jugador 2
	),
	format('Puntuación del jugador 1: ~w~n', [P1]), format('Puntuación del jugador 2: ~w~n', [P2]). 	
	
% Si hay una partida iniciada, ver_resumen muestra un resumen de la partida actual que incluye:
%   a) Configuración de la partida.
%   b) Resumen de las palabras formadas, los puntos obtenidos con cada una y las fichas disponibles en cada turno.
% Si no hay una partida iniciada, entonces la llamada termina en error.
ver_resumen:- empezado(0), throw('No hay ninguna partida iniciada').
ver_resumen:- 
	empezado(1),																													% Comprobamos que hay una partida iniciada
	empieza(E), modo(M), reparto(R), idioma(I),																						% Obtenemos las opciones de configuración
	writeln('Configuracion de la partida:'), 																	% Mostramos la configuración de la partida
			format('Modo de juego	 : ~w~n', [M]), 
			format('Idioma           : ~w~n', [I]), 
			format('Reparto          : ~w~n', [R]), 
			format('Empieza          : ~w~n', [E]), 	% Mostramos las opciones de configuración														% Obtenemos el resumen del turno
	nl, writeln('Resumen por turno:'), 																	
	forall(
        resumen_turno(J, W, P, F),
        (																									% Mostramos el resumen del turno
            format('Jugador           : ~w~n', [J]),
            format('Palabra           : ~w~n', [W]),
            format('Puntos obtenidos  : ~w~n', [P]),
            format('Fichas disponibles: ~w~n~n', [F])
        )
    ).
% ver_historial(+J) muestra el historial del jugador J: número de victorias y derrotas, puntuación máxima y puntuación media.
ver_historial(J):- var(J), !, throw('Debe especificar un jugador').
ver_historial(J):- \+member(J, [player, player_1, player_2, ordenador]), !, throw('El jugador no es válido').
ver_historial(J):- findall(P,
	(historial_puntuaciones(J,P,l)),	
	L), length(L, NL), 						% Obtiene el número de partidas perdidas
	findall(P,
	(historial_puntuaciones(J,P,w)), 
	W), length(W, NW),						% Obtiene el número de partidas ganadas
	append(L,W,R),							% Une las listas de partidas ganadas y perdidas
	max_list(R, Max),						% Obtiene la puntuación máxima
	mean_list(R, Mean),						% Obtiene la puntuación media
	format('El jugador ~w ha ganado ~w partidas y ha perdido ~w partidas.~n Su puntuación máxima es ~w y la puntuación media es ~2f%~n', [J, NW, NL, Max, Mean]).

% mean_list(+L,-M) tiene éxito si M es la media de los elementos de la lista L.
mean_list(L, M) :- 
	sum_list(L, Sum), 
	length(L, N), 
	N > 0, 
	M is Sum / N.
	
	
% ver_ranking muestra dos listas de jugadores: 
%	1. junto a cada nombre de jugador aparece su número y porcentaje de partidas ganadas, y los jugadores aparecen ordenados de manera descendente según el porcentaje de victorias 
%	2. junto a cada nombre de jugador aparece su puntuación máxima y media, y los jugadores aparecen ordenados de manera descendente según su puntuación media
ver_ranking:- 
	findall(J, historial_puntuaciones(J, _, _), Jugadores),				% Obtiene la lista de jugadores repetidos
	sort(Jugadores, JSinRepetir),								% Elimina los jugadores repetidos
	findall(est(J,W,L,P), 
	(
		member(J, JSinRepetir),
		findall(P, historial_puntuaciones(J, P, w), LW),			% Obtiene la lista de jugadores ganados
		findall(P,historial_puntuaciones(J, P, l), LL),				% Obtiene la lista de jugadores perdidos
		length(LW, W), length(LL, L),								% Obtiene el número de partidas ganadas y perdidas
		T is W + L, 
		(
			T > 0 -> P is (W * 100) / T; 
			P is 0
		)
	),
	EstadisticasV),											% Obtiene una lista de estadísticas de los jugadores
	sort(4, @>=, EstadisticasV, RVictorias),					% Ordena la lista de jugadores por el porcentaje de victorias

	writeln('Numero y porcentaje de victorias ordenados:'),	% Primera lista
    forall(member(est(J, W, L, P), RVictorias),
        format('Jugador: ~w, Ganadas: ~w, Perdidas: ~w, Porcentaje: ~2f%~n', [J, W, L, P])),

	findall(est(J, MAX, MED),
	(
		member(J, Jugadores),
		findall(P, historial_puntuaciones(J, P, _), Puntuaciones),
		max_list(Puntuaciones, MAX),
		mean_list(Puntuaciones, MED)
	),
	EstadisticasP),

    sort(3, @>=, EstadisticasP, RPuntuacion),
		
	nl, writeln('Puntuacion maxima y media ordenados:'),	% Segunda lista
	forall(member(est(J, MAX, MED), RPuntuacion),
		format('Jugador: ~w, Maxima: ~w, Media: ~2f~n', [J, MAX, MED])).
