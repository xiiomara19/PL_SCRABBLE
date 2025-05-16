:- dynamic
        diccionario/1,					% Guarda el diccionario de palabras segun el idioma de la partida
        char_puntos_apariciones/3.

% cargar_diccionario(+L) 
% tiene éxito si el diccionario de palabras y letras en el idioma Idioma se carga correctamente. El diccionario esta compuesto por dos archivos de texto:
% 	1. El archivo de palabras, que se encuentra en la carpeta 'palabras' y tiene el nombre 'words.Idioma.txt'. 
% 	2. El archivo de letras, puntuacion y cantidad, que se encuentra en la carpeta 'letras' y tiene el nombre 'letras_Idioma.pl'. Este lo carga en el predicado 
%		dinamico char_puntos_apariciones(Letra,Puntos,Cantidad)
% Si el archivo no existe o no se puede abrir, la llamada termina en error.
cargar_diccionario(L):- 
	atomic_list_concat(['./diccionario/palabras/words.', L, '.txt'], Diccionario),  	% Crear el nombre del archivo de palabras
	open(Diccionario, read, Stream, [encoding(utf8)]),						% Abrir el archivo en modo lectura con codificación UTF-8
	obtener_lineas(Stream, Lineas),
	close(Stream), !,
	retractall(diccionario(_)),												% Limpiar el diccionario actual	
	retractall(char_puntos_apariciones(_,_,_)),						
	asserta(diccionario(Lineas)),											% Cargar el archivo de palabras
	atomic_list_concat(['./diccionario/letras/letras_', L, '.pl'], Caracteres),			% Crear el nombre del archivo de letras
	consult(Caracteres).													% Cargar el archivo de letras

cargar_diccionario(_):- throw('No se ha podido cargar el diccionario').


% obtener_lineas(+Stream,-Lineas) 
% tiene éxito si Lineas es una lista de palabras leídas desde el flujo Stream. Cada palabra se considera una línea del archivo.
obtener_lineas(Stream, []) :-
    at_end_of_stream(Stream), !.

obtener_lineas(Stream, [Palabra|Resto]) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Palabra, Codes),
    obtener_lineas(Stream, Resto).


% bolsa_letras(-B)
% Genera la bolsa completa de letras, repitiendo cada letra según su cantidad
bolsa_letras(B) :-
    findall(L, letra_repetida(L), LetrasRepetidas),
    flatten(LetrasRepetidas, B).

% letra_repetida(-LR)
% Por cada letra en el diccionario, genera una lista con esa letra repetida N veces
letra_repetida(LR) :-
    char_puntos_apariciones(L, _, C),
    length(LR, C),
    maplist(=(L), LR).

% actualizar_letra_usada(+L)
% Actualiza la cantidad de letras disponibles en la bolsa de letras al usar una letra
actualizar_letra_usada(L) :-
    char_puntos_apariciones(L, P, C),
    C > 0,
    retract(char_puntos_apariciones(L, P, C)),
    C1 is C - 1,
    ( C1 > 0 -> asserta(char_puntos_apariciones(L, P, C1)) ; true ).

% letras_validas(+L,+B)
% Comprueba si la lista de letras L se encuentra disponible en la bolsa de letras B.
letras_validas([], _):- !.
letras_validas([L|R], Bolsa) :-
    select(L,Bolsa, Resto),
    letras_validas(R, Resto).