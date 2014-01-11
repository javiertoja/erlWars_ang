-module(erlwars_app_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([%%%test1/1, 
         %%%test2/1,
         testStart/1,
         testInfo/1,
         testRObtenerPartidas/1,
         testRPartida1/1,
         testRPartida2/1,
         testHandler/1,
         testPCreaPartida/1,
         testPJoinPartida/1,
         testPLeavePartida/1,
         testTCrearTablero/1,
         testTCrearTablero2/1,
         testTCrearTablero3/1,
         testTCrearTablero4/1,
         testTCrearTablero5/1,
         testTPosicionarNaves/1,
         testTDisparoNave/1]).
 
all() -> [%%%test1,
          %%%test2,
          testStart,
          testInfo,
          testRObtenerPartidas,
          testRPartida1,
          testRPartida2,
          testHandler,
          testPCreaPartida,
          testPJoinPartida,
          testPLeavePartida,
          testTCrearTablero,
          testTCrearTablero2,
          testTCrearTablero3,
          testTCrearTablero4,
          testTCrearTablero5,
          testTPosicionarNaves,
          testTDisparoNave].
%%% 
%%%test1(_Config) ->
%%%1 = 1.
 
%%%test2(_Config) ->
%%%A = 5,
%%%1/A.

%%% *************************************************************************
%%% Test que comprueba que se puede iniciar un nuevo weboscket y finalizarlo
%%% *************************************************************************
testStart(_Config) ->
  {ok,0,undefined_state} = ws_handler:websocket_init(0, 0, 0),
  ok = ws_handler:websocket_terminate(0,0,0).



%%% *************************************************************************
%%% Probamos a solicitar información al websocket de manera no válida.
%%% *************************************************************************
testInfo(_Config) ->
  {error,0,0} = ws_handler:websocket_info(0, 0, 0).


%%% *************************************************************************
%%% Probamos a intentar manejar un mensaje no válido.
%%% *************************************************************************
testHandler(_Config) ->
  {error,0,0} = ws_handler:websocket_handle("EstoEsUnMensajeNoValido", 0, 0).


%%% --- TEST ROUTER ---

%%% *************************************************************************
%%% Probamos a recuperar las partidas sin haber creado ninguna.
%%% *************************************************************************
testRObtenerPartidas(_Config) ->
  [] = router:obtener_partidas().

%%% *************************************************************************
%%% Probamos a crear 1 partida y recuperarla
%%% *************************************************************************
testRPartida1(_Config) ->
  router:unirse_a_partida("TestNick","TestPartida" ),
  ["TestPartida@partidaDisponible"] = router:obtener_partidas().

%%% *************************************************************************
%%% Probamos a crear 2 partidas y unir a 4 jugadores .
%%% *************************************************************************
testRPartida2(_Config) ->
  router:unirse_a_partida("TestNick","TestPartida" ),
  ["TestPartida@partidaDisponible"] = router:obtener_partidas(),
  router:unirse_a_partida("TestNick2","TestPartida" ),
  router:unirse_a_partida("TestNick3","TestPartida" ),
  router:unirse_a_partida("TestNick4","TestPartida" ),
  ["TestPartida@partidaDisponible"] = router:obtener_partidas(),
  router:unirse_a_partida("TestNick1_1","TestPartida1" ),
  ["TestPartida1@partidaDisponible","TestPartida@partidaDisponible"] = router:obtener_partidas().

%%% --- TEST PARTIDA ---

%%% *************************************************************************
%%% Probamos a crear 1 partida desde el módulo de partidas.
%%% *************************************************************************
testPCreaPartida(_Config) ->
  partida:start("PartidaPrueba", "PartidaOwner", self()).

%%% *************************************************************************
%%% Probamos a crear 1 partida y unir a varios jugadores.
%%% *************************************************************************
testPJoinPartida(_Config) ->
  Partida = partida:start("PartidaPrueba", "PartidaOwner", self()),
  Cliente1 = list_to_pid("<0.60.0>"),
  Cliente2 = list_to_pid("<0.61.0>"),
  Cliente3 = list_to_pid("<0.62.0>"),
  partida:join(Partida, "PartidaMember1", Cliente1),
  partida:join(Partida, "PartidaMember2", Cliente2),
  partida:join(Partida, "PartidaMember3", Cliente3).

%%% *************************************************************************
%%% Probamos a crear 1 partida y unir a varios jugadores y luego hacer que 1 
%%% abandone.
%%% *************************************************************************
testPLeavePartida(_Config) ->
  Partida = partida:start("PartidaPrueba", "PartidaOwner", self()),
  Cliente1 = list_to_pid("<0.60.0>"),
  Cliente2 = list_to_pid("<0.61.0>"),
  Cliente3 = list_to_pid("<0.62.0>"),
  partida:join(Partida, "PartidaMember1", Cliente1),
  partida:join(Partida, "PartidaMember2", Cliente2),
  partida:join(Partida, "PartidaMember3", Cliente3),
  {dejarPartida,"PartidaMember3"} = partida:abandonar_partida(Partida, "PartidaMember3"),
  {dejarPartida,"PartidaMember2"} = partida:abandonar_partida(Partida, "PartidaMember2"),
  {dejarPartida,"PartidaMember1"} = partida:abandonar_partida(Partida, "PartidaMember1").

%%% --- TEST TABLERO ---

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero.
%%% *************************************************************************
testTCrearTablero(_Config) ->
  tablero:start_link().

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y comprobar que se inicializa vacio.
%%% *************************************************************************
testTCrearTablero2(_Config) ->
  Tablero = tablero:start_link(),
  tablero:get_posiciones(Tablero).

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y comprobar el valor de todas las celdas.
%%% *************************************************************************
testTCrearTablero3(_Config) ->
  Tablero = tablero:start_link(),
  tablero_x(Tablero, 10 , 10).

tablero_x(Tablero, X, Y) ->
  {ok,vacio} = tablero:get_resultado(Tablero, X, Y),
  io:format("~p~n",[tablero:get_resultado(Tablero, X, Y)]),
  case X of
    1 ->
      tablero_x(Tablero, 10 , Y -1);
    _Else ->
      tablero_y(Tablero, X-1 , Y)
  end.

tablero_y(Tablero, X, Y) ->
  case Y of
    1 -> 
      ok;
    _Else ->
      tablero_x(Tablero,X , Y)
  end.

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y ver que no hay naves posicionadas
%%% *************************************************************************
testTCrearTablero4(_Config) ->
  Tablero = tablero:start_link(),
  false = tablero:estan_naves_posicionadas(Tablero).

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y ver que esta todo destruido.
%%% *************************************************************************
testTCrearTablero5(_Config) ->
  Tablero = tablero:start_link(),
  true = tablero:todo_destruido(Tablero).

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y posicionamos naves.
%%% *************************************************************************
testTPosicionarNaves(_Config) ->
  Tablero = tablero:start_link(),
  tablero:posicionar_nave(Tablero, 1, 1),
  tablero:posicionar_nave(Tablero, 2, 2),
  tablero:posicionar_nave(Tablero, 3, 3),
  false = tablero:todo_destruido(Tablero),
  true = tablero:estan_naves_posicionadas(Tablero).

%%% *************************************************************************
%%% Probamos a crear un nuevo tablero y disparar a naves posicionadas
%%% *************************************************************************
testTDisparoNave(_Config) ->
  Tablero = tablero:start_link(),
  tablero:posicionar_nave(Tablero, 1, 1),
  tablero:posicionar_nave(Tablero, 2, 2),
  tablero:posicionar_nave(Tablero, 3, 3),
  false = tablero:todo_destruido(Tablero),
  true = tablero:estan_naves_posicionadas(Tablero),
  nave = tablero:disparo(Tablero, 1, 1),
  nave = tablero:disparo(Tablero, 2, 2),
  nave = tablero:disparo(Tablero, 3, 3),
  vacio = tablero:disparo(Tablero, 4, 4).





