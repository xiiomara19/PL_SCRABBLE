:-	dynamic
			board_size/1,			% Tamaño del tablero
			symbol/2,				% Símbolos que usan los jugadores
			board/1,				% Tablero de juego
			next_round/1.			% Jugador que tiene el turno: player_A, player_B o end (partida finalizada)

:- assertz(board_size(3)).			% Tamaño por defecto: 3
:- assertz(symbol(player_A,'X')).	% Símbolo del jugador A por defecto: X
:- assertz(symbol(player_B,'O')).	% Símbolo del jugador B por defecto: O
:- assertz(next_round(end)).		% Siguiente turno por defecto: partida finalizada


% create_board(+N,-B) tiene éxito si B es una matriz (lista de listas) de tamaño N

create_board(N,B):-
		length(B,N),
		maplist(same_length(B),B).


%%% write_board(+B,+N) tiene éxito si B es una matriz (lista de listas) de tamaño N, y escribe su contenido en pantalla

write_board(B,N):-
		length(B,N),
		maplist(same_length(B),B),
		!,
		W is 4*N+1,
		length(L,W),
		maplist(=('-'),L),
		atom_chars(S,L),
		maplist(write_row(S),B),
		write(S),
		nl.
write_board(N,B):-
		throw(board_size_error(N,B)).

% write_row(+S,+R) tiene éxito siempre, y escribe en pantalla el contenido de la lista R tras escribir S

write_row(_, []):- write('|'), nl.
write_row(S, [_|T]):-
    write('|'),
    write(S),
    write_row(S, T).


% write_cell(+C) tiene éxito si C es un valor de celda correcto (celda vacía, 'X' o 'O'), y escribe su valor en pantalla

write_cell(C):- C=='X', write(C).
write_cell(C):- C=='O', write(C).


% start_new_game tiene éxito siempre y establece el inicio de una nueva partida

start_new_game:- asserta(board_size(4)), board_size(B), write_board(_,B).


% play_A(+R,+C) tiene éxito si el R y C son índices de fila y columna válidos, si la celda (R,C) está vacía y si el siguiente turno es del jugador A
%	Actualiza el tablero, comprueba si la partida ha finalizado y/o cambia el turno

%play_A(R,C):-


% play_B(+R,+C) tiene éxito si el R y C son índices de fila y columna válidos, si la celda (R,C) está vacía y si el siguiente turno es del jugador B
%	Actualiza el tablero, comprueba si la partida ha finalizado y/o cambia el turno

%play_B(R,C):-


% update_round(+B,+CR,+NR) tiene éxito siempre, comprueba si la partida ha finalizado y/o cambia el turno de CR a NR

%update_round(B,CR,NR):-


% is_win(+B,+N,+S) tiene éxito si hay una línea con el símbolo S en el tablero B de tamaño N

%is_win(B,N,S):-


% write_next_round(+P) tiene éxito si P es un jugador, y escribe por pantalla quien tiene el siguiente turno

write_next_round(P):- P==player_A, write("Turno del jugador A").
write_next_round(P):- P==player_B, write("Turno del jugador B").


% write_winner(+P) tiene éxito si P es un jugador, y escribe por pantalla que ha ganado la partida

write_winner(P):- P==player_A, write("The winner is player A").
write_winner(P):- P==player_B, write("The winner is player B").


% set_board_size(+N) tiene éxito si N es un número mayor o igual que 2 y no hay partida iniciada,
%	actualizando el tamaño del tablero a N

set_board_size(N):- asserta(board_size(N)).

% set_player_symbol(+P,+S) tiene éxito si P es un jugador válido, S es un símbolo válido y no hay una partida iniciada,
%	actualizando el tamaño del tablero a N.

%set_player_symbol(P,S):-