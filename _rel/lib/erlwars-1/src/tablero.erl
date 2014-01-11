%% -*- coding: utf-8 -*-

%%%-------------------------------------------------------------------
%%% @author Alejandro Pernas Pan <alejandro.pernas@udc.es>
%%% @author Laura Otero Méndez <inslom01@udc.es﻿>
%%% @author Javier Toja Alamancos <insjta00@udc.es﻿>
%%% @author Sergio Cores Acha <sergio.cores@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%% Este módulo implementa el tablero individual para un jugador de erlwars. En este
%%% tablero se ubican las naves del jugador. Cada posición del tablero admite 4 estados.
%%% ==Estados de las celdas del tablero==
%%% <ul>
%%% <li><em>?VACIO</em>: La celda del tablero está vacía y no alberga ninguna nave.</li>
%%% <li><em>?NAVE</em>: La celda del tablero representa una nave del jugador que aún no ha sido abatida.</li>
%%% <li><em>?NAVEALCANZADA</em>: La celda del tablero representa una nave del jugador que ha sido abatida.</li>
%%% <li><em>?NAVE</em>: La celda del tablero representa una nave del jugador que aún no ha sido abatida.</li>
%%% <li><em>?NAVE</em>: Otro jugador ha disparado a la posición, representada por la celda, pero no existía ninguna nave ubicada en ella.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(tablero).
-include("erlwars.hrl").

-export([start_link/0, posicionar_nave/3, disparo/3, get_resultado/3, estan_naves_posicionadas/1, todo_destruido/1]).
-export([get_posiciones/1]).

-spec tablero:posicionar_nave(Tablero::pid(), X::integer(), Y::integer()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite posicionar una nave en el tablero del jugador, pasado como parámetro, en las coordenadas (Y,X).
%%% Para ello envía un mensaje con la información necesaria al tablero del jugador.
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>ok</em>: Retorna siempre ?OK (predefinido al átomo ok).</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
posicionar_nave(Tablero, X, Y) ->
  Tablero ! {self(), {posicionar, X, Y}},
  ?OK.

-spec tablero:disparo(Tablero::pid(), X::integer(), Y::integer()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite disparar a una posición, en el tablero del jugador pasado como parámetro, en las coordenadas (Y,X).
%%% Para ello envía un mensaje con la información necesaria al tablero del jugador.
%%% ==Valor de retorno==
%%% Esta función retorna ?VACIO, ?NAVE, ?NAVEALCANZADA, ?DISPARADO o ?ERROR, en función del estado en el que se encuentre
%%% el tablero del jugador.
%%% @end
%%%-------------------------------------------------------------------
disparo(Tablero, X, Y) ->
  Tablero ! {self(), {disparo, X, Y}},
  receive
    ?VACIO         -> ?VACIO;
    ?NAVE          -> ?NAVE;
    ?NAVEALCANZADA -> ?NAVEALCANZADA;
    ?DISPARADO     -> ?DISPARADO;
    _              -> ?ERROR
  end.

-spec tablero:get_resultado(Tablero::pid(), X::integer(), Y::integer()) -> tuple(atom(), atom()) | atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite obtener el estado actual de una celda del tablero.
%%% ==Parámetros==
%%% <ul>
%%% <li><em>X,Y</em>: Coordenadas de la celda del tablero.</li>
%%% <li><em>Tablero</em>: Pid del proceso tablero objetivo.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% Esta función retorna:
%%% {ok,?VACIO}, {ok,?NAVE}, {ok,?NAVEALCANZADA}, {ok,?DISPARADO} o ?ERROR, en función del estado en el que se encuentre
%%% la celda del tablero del jugador.
%%% @end
%%%-------------------------------------------------------------------
get_resultado(Tablero, X, Y) ->
  get_dato(get_posiciones(Tablero), X, Y).

-spec tablero:get_posiciones(Tablero::pid()) -> dict().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función devuelve un diccionario con el estado de las posiciones del tablero pasado como parámetro.
%%% ==Parámetros==
%%% <ul>
%%% <li><em>Tablero</em>: Pid del proceso tablero objetivo.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% Un diccionario cuya clave es la tupla {X,Y} (coordenada de una celda del tablero) y cuyo valor es uno de
%%% los estados en el que se puede encontrar la celda: ?VACIO, ?NAVE, ?NAVEALCANZADA, ?DISPARADO.
%%% @end
%%%-------------------------------------------------------------------
get_posiciones(Tablero) ->
  Tablero ! {self(), get},
  receive
    {Tablero, Posiciones} ->
      Posiciones
  end.

-spec tablero:estan_naves_posicionadas(Tablero::pid()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite saber si un jugador ha terminado de colocar sus naves en el tablero.
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>true</em>: Si el jugador ha terminado de posicionar todas sus naves en el tablero.</li>
%%% <li><em>false</em>: En caso contrario.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
estan_naves_posicionadas(Tablero) ->
  Filtrados = dict:filter(fun(_,V) -> V == ?NAVE end, get_posiciones(Tablero)),
  io:format("Filtrados~p~n",[dict:to_list(Filtrados)]),
  dict:size(Filtrados) == ?NUMNAVES.

-spec tablero:todo_destruido(Tablero::pid()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite saber si todas las naves de un determinado tablero han sido alcanzadas.
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>true</em>: Si todas las naves han sido destruidas.</li>
%%% <li><em>false</em>: En caso contrario.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
todo_destruido(Tablero) ->
  dict:size(dict:filter(fun(_,V) -> V == ?NAVE end, get_posiciones(Tablero))) == 0.

-spec tablero:start_link() -> pid().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función crea un nuevo proceso tablero.
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>Pid</em>: El pid del nuevo proceso creado.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
start_link() ->
  spawn_link(fun() -> init() end).

%% Crea el tablero 10x10.
%% Posiciones desde {1,1} hasta {10, 10}.
%% Usa diccionario con Clave={x,y}.
%% http://www.erlang.org/doc/man/dict.html
init_tablero(0, _Columnas, Posiciones) ->
  Posiciones;
init_tablero(Filas, Columnas, Posiciones) ->
  case Columnas > 0 of
    true ->
      init_tablero(Filas, Columnas - 1, dict:store({Filas, Columnas}, ?VACIO, Posiciones));
    _ ->
      init_tablero(Filas - 1, ?TAMANO, dict:store({Filas, Columnas}, ?VACIO, Posiciones))
  end.

init() ->
  put(numeroNavesAlcanzadas, 0),
  loop(init_tablero(?TAMANO, ?TAMANO, dict:new())).

loop(Posiciones) ->
  receive
    {_From, {posicionar, X, Y}} ->
      loop(set_dato(Posiciones, ?NAVE, X, Y));
    {From, {disparo, X, Y}} ->
      case get_dato(Posiciones, X, Y) of
        {?OK, ?VACIO} ->
          From ! ?VACIO,
          loop(set_dato(Posiciones, ?DISPARADO, X, Y));
        {?OK, ?NAVE} ->
          From ! ?NAVE,
          loop(set_dato(Posiciones, ?NAVEALCANZADA, X, Y));
        {?OK, ?NAVEALCANZADA} ->
          From ! ?NAVEALCANZADA,
          loop(Posiciones);
        {?OK, ?DISPARADO} ->
          From ! ?DISPARADO,
          loop(Posiciones);
        XXX ->
          io:format("~nError(tablero): Disparo no válido. ~p~n", [XXX]),
          loop(Posiciones)
      end;
    {From, get} ->
      From ! {self(), Posiciones},
      loop(Posiciones);
    _ ->
      loop(Posiciones)
  end.

set_dato(Posiciones, Dato, X, Y) ->
  dict:store({X,Y}, Dato, Posiciones).

get_dato(Posiciones, X, Y) ->
  dict:find({X,Y}, Posiciones).
