% jugar_ordenador tiene éxito si el jugador es la máquina y se le asigna el turno. Si el jugador no es la máquina, entonces la llamada termina en error.
jugar_ordenador:- \+siguiente_ronda(ordenador),! ,throw('El jugador no es la máquina').
jugar_ordenador:- 
	siguiente_ronda(ordenador), 						% Si el jugador es la máquina, se le asigna el turno
	tablero(B), 										% Se obtiene el tablero actual
	fichas_jugador(ordenador, Fichas), 					% Se obtiene la lista de letras disponibles para la máquina
	generar_palabra_aleatoria(Fichas, P), 		% Se genera una palabra aleatoria
	obtener_posicion_aleatoria(P, B, Posicion), 	% Se obtiene una posición aleatoria en el tablero
	formar_palabra(ordenador, h, Posicion, P),	% Se forma la palabra en el tablero
	pasar_turno(ordenador).								% Se asigna el turno al jugador

% generar_palabra_aleatoria(+Fichas,-P) 
% tiene éxito si P es una palabra aleatoria formada por las letras de la lista Fichas. Si no hay letras disponibles, entonces la llamada termina en error.
generar_palabra_aleatoria(Fichas, P):- 
	setof(W, palabra_valida(Fichas, W), Palabras),  % obtiene todas las posibles
    random_member(P, Palabras).

% palabra_valida(+Fichas,-P)
% tiene éxito si P es una palabra válida formada por las letras de la lista Fichas. Si no hay letras disponibles, entonces la llamada termina en error.
palabra_valida(Fichas, P):- 
	diccionario(P), 
	atom_chars(P, LC),
	es_valida(LC, Fichas).

% es_valida(+L,+F)
% tiene éxito si la lista de letras L puede formarse con las letras de la lista F. Si no hay letras disponibles, entonces la llamada termina en error.
es_valida([], _):- !.	% Si la lista de letras está vacía, entonces es válida
es_valida([H|T], F):- 
	select(H, F, R),	% Se selecciona la letra H de la lista de letras F
	(
		H = '$' -> true;	% Si la letra es un dolar, entonces es válida
		member(H, F) -> true;	% Si la letra está en la lista de letras, entonces es válida
		throw('La letra no es válida')	% Si la letra no está en la lista de letras, entonces no es válida
	),
	es_valida(T, R).	% Se comprueba si el resto de letras son válidas


jugar_maquina([],Fichas_restantes):-
	nth1(1,Fichas_restantes,E,Fichas_restantes2),
	jugar_maquina([E],Fichas_restantes2),!.


jugar_maquina(Fichas_uso, Fichas_restantes):-
	findall(Perm, permutation(Fichas_uso, Perm), Permutaciones),
	tablero(B),
	member(Fila, B),
    member(Elem, Fila),
	celdas_posibles(L),
    \+member(Elem,L),

	añadir_a_cada_sublista(Elem,Permutaciones,R),
	listas_a_atomos(R,R2),
	writeln(R2),

	between(0, 15, F),
    between(0, 15, C),

	(
		intentar(once(include(formar_palabra(ordenador,v,F,C),R2,Resultado))),
		\+intentar(once(include(formar_palabra(ordenador,v,F,C),R2,Resultado)))->
		!,
		nth1(1,Fichas_restantes,E,Fichas_restantes2),
		append(Fichas_uso,[E],Fichas_uso2),
		jugar_maquina(Fichas_uso2,Fichas_restantes2)
	).


intentar(Goal) :-
    catch((call(Goal), !), _, fail), write('A').

añadir_a_cada_sublista(Elem, ListaDeListas, Resultado) :-
    maplist(agregar_elemento(Elem), ListaDeListas, Resultado).

agregar_elemento(Elem, Sublista, NuevaSublista) :-
    append(Sublista, [Elem], NuevaSublista).

listas_a_atomos(Listas, Atomos) :-
    maplist(atomic_list_concat, Listas, Atomos).