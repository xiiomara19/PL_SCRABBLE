
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
	%siguiente_ronda(J),
	atom_chars(P,L), 
	length(L,X),
    usa_letra(O,F,C,L), 
	(
		O = h -> comprobar_limites(C,X), comprobar_limites(F,0);
		O = v -> comprobar_limites(C,0), comprobar_limites(F,X)
	),
    pillar_letras(O,F,C,X,R),
    write(R),
	comprobar_si_encaja(J,O,F,C,L,1,0), 
	actualizar_tablero(O,F,C,L),
	mostrar_tablero.


pillar_letras(h,F,C,L,R):-
    get_cell(F,C,Z),
    X is C+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	\+member(Z,Celdas),
    pillar_letras(h,F,X,L2,R2),
    R = [Z|R2].

pillar_letras(h,F,C,L,R):-
    get_cell(F,C,Z),
    X is C+1,
    L > 0,
    L2 is L-1,
    pillar_letras(h,F,X,L2,R).

pillar_letras(v,F,C,L,R):-
    get_cell(F,C,Z),
    X is F+1,
    L > 0,
    L2 is L-1,
    celdas_posibles(Celdas), 
	\+member(Z,Celdas),
    pillar_letras(v,X,C,L2,R2),
    R = [Z|R2].

pillar_letras(v,F,C,L,R):-
    get_cell(F,C,Z),
    X is F+1,
    L > 0,
    L2 is L-1,
    pillar_letras(v,X,C,L2,R).

pillar_letras(_,_,_,0,[]).




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
	X is C+1, 
	M2 is M*Mul_palabra,
	comprobar_si_encaja(J,h,F,X,T,M2,P3).

comprobar_si_encaja(J,v,F,C,[H|T],M,P):- 
	get_cell(F,C,H), 
	X is F+1, 
	char_puntos_apariciones(H,Puntos,_),
	P2 is P+Puntos,
	comprobar_si_encaja(J,v,X,C,T,M,P2).

comprobar_si_encaja(J,v,F,C,[H|T],M,P):- 
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
	comprobar_si_encaja(J,v,X,C,T,M2,P3).


multiplicador_letra(' DL  ',2).
multiplicador_letra(' TL  ',3).
multiplicador_letra(_,1).


multiplicador_palabra(' DP  ',2).
multiplicador_palabra(' TP  ',3).
multiplicador_palabra(_,1).


celdas_posibles([' --- ', ' DL  ', ' TL  ', ' DP  ', ' TP  ', '  *  ']).






















% comprobar_prefijo(+O,+F,+C,R) dada posicion F,C y una orientacion (izquierda o arriba dependiendo de la orientacion)
% miarará si existe algun prefijo en la tabla y lo devolvera en R

%Llega a la celda vacia
comprobar_prefijo(h,F,C,[]):-
	C >= 0,
	get_cell(F,C,T),
	celdas_posibles(L), 
	member(T,L), !.

comprobar_prefijo(h,F,C,R):-
	C >= 0,
	get_cell(F,C,T),
	X is C-1,
	comprobar_prefijo(h,F,X,T2),
    R = [T|T2],!.    

comprobar_prefijo(_,_,C,[]):- C<0,!.



% comprobar_sufijo(+O,+F,+C,L,R) dada una palabra, su posicion F,C y su orientacion miarará si hay que concatenarla con algun sufijo
% (derecha o abajo dependiendo de la orientacion) y devolvera la nueva palabra en R