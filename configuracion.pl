:-	dynamic
			idioma/1,					% Idioma en el que se forma
			modo/1,                 	% Modo de juego: pvp o pve
			reparto/1,              	% Modo en el que se reparten las fichas
			empieza/1.              	% Indica que jugador empezará

% Opciones de configuración iniciales
:- assertz(idioma(es)).					% Idioma por defecto: EspaÑol
:- assertz(modo(pve)).	            	% Modo de juego por defecto: pve
:- assertz(reparto(aleatorio)).	    	% Modo en el que se reparten las fichas por defecto: aleatorio
:- assertz(empieza(0)).		    		% Modo de inicio de partida: normal

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
establecer_opcion(idioma,A):- empezado(0), opcionesIdioma(A), retract(idioma(_)), asserta(idioma(A)), !.
establecer_opcion(modo,A):- empezado(0), opcionesModo(A), retract(modo(_)), asserta(modo(A)), !.
establecer_opcion(reparto,A):- empezado(0),  opcionesReparto(A), retract(reparto(_)), asserta(reparto(A)), !.
establecer_opcion(empieza,A):- empezado(0), opcionesEmpieza(A), retract(empieza(_)), asserta(empieza(A)), !.
establecer_opcion(_,_):- throw('No existe el apartado de configuracion especificado').
