:-	dynamic
			resumen_turno/4.



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
    validar_palabra(P),
	empezado(1),
	%siguiente_ronda(J),
    fichas_jugador(J,Fichas),
	atom_chars(P,L),
	length(L,X),
    usa_letra(O,F,C,L), 
	(
		O = h -> comprobar_limites(C,X), comprobar_limites(F,0);
		O = v -> comprobar_limites(C,0), comprobar_limites(F,X)
	),
    letras_en_tablero(O,F,C,X,R),
    eliminar_si_posible(L,R,I),
    eliminar_si_posible(I,Fichas,I2), 
    length(I2,0),
    eliminar_si_posible(Fichas,I,Fichas_restantes),
	comprobar_si_encaja(J,O,F,C,L,1,0,Puntuacion_final), 
	retract(fichas_jugador(J,_)),
	retract(fichas_jugador(J,FichasRestantes)),
	actualizar_tablero(O,F,C,L),
	mostrar_tablero,
    write('Palabra formada: '), writeln(P),
    write('Puntuacion obtenida: '), writeln(Puntuacion_final),
    write('Fichas disponibles: '), writeln(Fichas_restantes),
    asserta(resumen_turno(J,P,Puntuacion_final,Fichas_restantes)),
	length(Fichas_restantes,N),
	R is 7-N,
	asignar_fichas(J,R).


eliminar_si_posible(Lista, Sub, Resultado) :-
    foldl(eliminar_si_existe, Sub, Lista, Resultado).

eliminar_si_existe(Elem, Lista, Resultado) :-
    ( select(Elem, Lista, Resultado) -> true
    ; Resultado = Lista ).


%letras_en_tablero(+O,+F,+C,+L,?R)
% Recorre L posiciones del tablero desde (F,C) en la orientacion o y guarda 
% todas las letras que se encuentren en R
letras_en_tablero(h,F,C,L,R):-
    get_cell(F,C,Z),
    X is C+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	\+member(Z,Celdas),
    letras_en_tablero(h,F,X,L2,R2),
    R = [Z|R2].

letras_en_tablero(h,F,C,L,R):-
    get_cell(F,C,Z),
    X is C+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	member(Z,Celdas),
    letras_en_tablero(h,F,X,L2,R).

letras_en_tablero(v,F,C,L,R):-
    get_cell(F,C,Z),
    X is F+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	\+member(Z,Celdas),
    letras_en_tablero(v,X,C,L2,R2),
    R = [Z|R2].

letras_en_tablero(v,F,C,L,R):-
    get_cell(F,C,Z),
    X is F+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	member(Z,Celdas),
    letras_en_tablero(v,X,C,L2,R).

letras_en_tablero(_,_,_,0,[]).



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

% comprobar_si_encaja(+J,+O,+F,+C,+L,+M,+P) comprueba si la palabra L encaja en el tablero B en la posicion (F,C) y devuelve la puntuacion obtenida en P,
% se le suman los puntos al posicionar cada letra, en M guarda el multiplicador de la palabra
comprobar_si_encaja(J,_,_,_,[],M,P,P_palabra):- 
	P_palabra is M*P, 
	puntuacion(J, P_total), 
	Puntuacion is P_palabra+P_total, 
	retractall(puntuacion(J,_)),
	asserta(puntuacion(J,Puntuacion)).

% si se encuentra el caracter en la posicion correspondiente sumar puntos y seguir con el resto de la palabra
comprobar_si_encaja(J,h,F,C,[H|T],M,P,Puntuacion_final):- 
	get_cell(F,C,H),
	X is C+1, 
	char_puntos_apariciones(H,Puntos,_),
	P2 is P+Puntos,
	comprobar_si_encaja(J,h,F,X,T,M,P2,Puntuacion_final).
comprobar_si_encaja(J,v,F,C,[H|T],M,P,Puntuacion_final):- 
	get_cell(F,C,H), 
	X is F+1, 
	char_puntos_apariciones(H,Puntos,_),
	P2 is P+Puntos,
	comprobar_si_encaja(J,v,X,C,T,M,P2,Puntuacion_final).

% Si es una celda vacia comprobar los multiplicadores de dicha celda y seguir con el resto de la palabra
comprobar_si_encaja(J,h,F,C,[H|T],M,P,Puntuacion_final):- 
	get_cell(F,C,Z), 
	celdas_posibles(L), 
	member(Z,L),
	char_puntos_apariciones(H,Puntos,_),
	multiplicador_letra(Z,Mul_letra),
	multiplicador_palabra(Z,Mul_palabra),
	P2 is Puntos*Mul_letra,
	P3 is P+P2,
	X is C+1, 
	M2 is M*Mul_palabra,
	comprobar_si_encaja(J,h,F,X,T,M2,P3,Puntuacion_final).
comprobar_si_encaja(J,v,F,C,[H|T],M,P,Puntuacion_final):- 
	get_cell(F,C,Z),
	celdas_posibles(L), 
	member(Z,L), 
	char_puntos_apariciones(H,Puntos,_),
	multiplicador_letra(Z,Mul_letra),
	multiplicador_palabra(Z,Mul_palabra),
	P2 is Puntos*Mul_letra,
	P3 is P+P2,
	X is F+1, 
	M2 is M*Mul_palabra,
	comprobar_si_encaja(J,v,X,C,T,M2,P3,Puntuacion_final).


% multiplicador_letra(?C,?V)
% dada una posible celda devuelve el multiplicador a aplicar a una letra
multiplicador_letra(' DL  ',2).
multiplicador_letra(' TL  ',3).
multiplicador_letra(_,1).

% multiplicador_palabra(?C,?V)
% dada una posible celda devuelve el multiplicador a aplicar a una palabra
multiplicador_palabra(' DP  ',2).
multiplicador_palabra(' TP  ',3).
multiplicador_palabra(_,1).

% celdas_posibles(-L)
% devuelve una lista de las celdas que se encuentran en el tablero por defecto
celdas_posibles([' --- ', ' DL  ', ' TL  ', ' DP  ', ' TP  ', '  *  ']).