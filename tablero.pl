:- dynamic
			tablero/1,					% Guarda el tablero de juego

% crear_tablero 
% tiene éxito siempre y crea una matriz de 15x15 que representa el tablero de juego, donde cada celda puede contener un valor 
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

% celdas_especiales(-C) 
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