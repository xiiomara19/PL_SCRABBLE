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
- [ ] otro_jugador(+J,+O)
- [ ] formar_palabra(+J,+O,+F,+C,+P)
- [ ] actualizar_tablero(+O,+F,+C,+B,+L)
- [ ] usa_letra(O,F,C,B,L)
- [ ] comprobar_limites(P, L)
- [ ] comprobar_si_encaja(+O,+F,+C,+B,+L)
- [ ] asignar_fichas(+J,+F)
- [ ] mostrar_fichas(+J)
- [ ] mostrar_puntuación
- [ ] ver_resumen
- [ ] ver_historial(+J)
- [ ] mean_list(+L,-M)
- [ ] ver_ranking
- [ ] jugar_A
- [ ] jugar_B
- [ ] jugar_maquina
- [ ] calcular_puntos
- [ ] validar_palabra(+P)
- [ ] validar_fichas
- [ ] validar_posicion

### configuracion.pl

- [x] ver_opcion(+O)
- [x] establecer_opcion(+O,+V)

### diccionario.pl

- [x] cargar_diccionario(+L)
- [x] obtener_lineas(+S,-L)

### tablero.pl

- [x] crear_tablero
- [x] crear_tablero_base(-B)
- [x] celdas_especiales(-C)
- [x] insertar_celdas_especiales(+C,+BoardIn,-BoardOut)
- [ ] set_cell(+F,+C,+V,+BoardIn,-BoardOut)
- [ ] reemplazar_celda(+C,+RowIn,+NewValue,-RowOut)
- [ ] get_cell(+F,+C,+B,-R)
- [x] mostrar_tablero
- [x] mostrar_fila (+S,+R)
- [x] mostrar_item(+C)