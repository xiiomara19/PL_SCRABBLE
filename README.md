# PL_SCRABBLE

**Autores:** Asier Aguilar y Xiomara Cáceres

## Objetivos

- Desarrollar una aplicación en **Prolog** completamente funcional que implemente un juego de mesa sobre palabras.
- Poner en práctica los conocimientos adquiridos a lo largo de la asignatura, que incluyen, entre otros, los siguientes aspectos:
  - Recursividad.
  - Listas, expresiones aritméticas y expresiones atómicas (o *strings*).
  - Cortes.
  - Entrada/salida y ficheros.
  - Predicados all-solutions e iteradores.
  - Predicados dinámicos

## Predicados a realizar

### main.pl

- [x] iniciar_partida(+J)
- [x] iniciar_partida(+J1,+J2)
- [x] abandonar_partida(+J)
- [x] otro_jugador(+J,+O)
- [x] pasar_turno(+J)
- [x] asignar_fichas(+J,+F)
- [x] asignar_fichas_auto(+J,+F)
- [x] asignar_fichas_manual(+J,+F)
- [x] inicializar_fichas(+J)
- [x] obtener_fichas(+F,+J,-L)
- [x] mostrar_fichas(+J)
- [x] mostrar_puntuación
- [x] ver_resumen
- [x] ver_historial(+J)
- [x] mean_list(+L,-M)
- [x] ver_ranking
- [ ] jugar_A
- [ ] jugar_B
- [ ] jugar_maquina
- [x] validar_palabra(+P)
- [ ] validar_fichas_palabra

### configuracion.pl

- [x] ver_opcion(+O)
- [x] establecer_opcion(+O,+V)

### diccionario.pl

- [x] cargar_diccionario(+L)
- [x] obtener_lineas(+S,-L)
- [x] bolsa_letras(-B)
- [x] letra_repetida(-LR)
- [x] actualizar_letra_usada(+L)
- [x] letras_validas(+L,+B)

### tablero.pl

- [x] crear_tablero
- [x] crear_tablero_base(-B)
- [x] celdas_especiales(-C)
- [x] insertar_celdas_especiales(+C,+BoardIn,-BoardOut)
- [x] set_cell(+F,+C,+V) 
- [x] reemplazar_celda(+C,+RowIn,+NewValue,-RowOut)
- [x] get_cell(+F,+C,+B,-R)
- [x] mostrar_tablero
- [x] mostrar_fila (+S,+R)
- [x] mostrar_item(+C)
- [x] actualizar_tablero(+O,+F,+C,+L)

### palabra.pl

- [x] formar_palabra(+J,+O,+F,+C,+P)
- [x] escribir_final_ronda(+P,+Puntos,+F)
- [x] puede_escribir(+O,+F,+C,+L,+Fichas,-Fichas_restantes)
- [x] restar_comodines(+A,+F,-R)
- [x] eliminar_si_posible(+L,+S,-R)
- [x] eliminar_si_existe(+E,+L,-R)
- [x] contar_apariciones(+E,+L,-C)
- [x] letras_en_tablero(+O,+F,+C,+L,?R)
- [x] usa_letra(O,F,C,B,L)
- [x] comprobar_limites(+P,+L)
- [x] comprobar_si_encaja(+J,+O,+F,+C,+L,+M,+P)
- [x] multiplicador_letra(?C,?V)
- [x] multiplicador_palabra(?C,?V)
- [x] celdas_posibles(-L)

## TAREAS PENDIENTES

- Reordenar proyecto
- Añadir mensajes de error
- Hacer retract de todo al finalizar
- Jueguen los jugadores (play_A, play_B, play_maquina)
