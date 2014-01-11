%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Alejandro Pernas Pan <alejandro.pernas@udc.es>
%%% @author Laura Otero Méndez <inslom01@udc.es﻿>
%%% @author Javier Toja Alamancos <insjta00@udc.es﻿>
%%% @author Sergio Cores Acha <sergio.cores@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%% Este módulo implementa el router del juego erlwars. El servidor web establecerá
%%% conexiones con el router para obtener la lista de partidas del juego, crear
%%% partidas y asociar clientes con partidas.
%%% @end
%%%-------------------------------------------------------------------
-module(router).

-include("erlwars.hrl").

-export([start/0, unirse_a_partida/2, obtener_partidas/0]).

-define(OBTENER_PARTIDAS_ROUTER, obtenerPartidas).
-define(UNIRSE_A_PARTIDA_ROUTER, unirseAPartida).

-type unirse_a_partida_return_type()::{atom(), pid()}|{atom(),atom()}.

-spec router:start() -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función inicia y registra el proceso router de erlwars.
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>ok</em>: Retorna siempre ?OK (predefinido al átomo ok).</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
start() ->
    spawn(fun() -> init() end),
    ?OK.

-spec router:unirse_a_partida(Nick::string(), NombrePartida::string()) -> unirse_a_partida_return_type()|atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite a un cliente unirse a una partida enviando un mensaje al router. Si no existe ninguna
%%% partida con el nombre especificado se creará.
%%% ==Parámetros==
%%% <ul>
%%% <li><em>Nick</em>: Cadena de caracteres que contiene el nombre que utilizará el jugador en la partida.</li>
%%% <li><em>NombrePartida</em>: Nombre de la partida que se desea crear o a la que se desea unirse.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% El pid asociado a la partida o ?PARTIDACERRADA (en caso de que el estado de la partida no permita unirse en
%%% ese momento).
%%% @end
%%%-------------------------------------------------------------------
unirse_a_partida(Nick, NombrePartida) ->
    io:format("[Router:unirse_a_partida]: Nick: ~p Partida:~p~n",[Nick, NombrePartida]),
    ?ROUTER ! {self(), {?UNIRSE_A_PARTIDA_ROUTER, Nick, NombrePartida}},
    receive
        {?ROUTER, ?ERROR}  -> ?PARTIDACERRADA;
        {?ROUTER, Partida} -> Partida
    end.

-spec router:obtener_partidas() -> list(string()).
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función obtiene el nombre y estado de las partidas existentes actualmente.
%%% ==Valor de retorno==
%%% Una lista cuyos elementos son cadenas de caracteres. Cada elemento es de la forma "nombrePartida@Estado".
%%% El estado de una partida es: partidaNoDisponible | partidaDisponible.
%%% @end
%%%-------------------------------------------------------------------
obtener_partidas() ->
    io:format("[Router:obtenerPartidas]: Obteniendo partidas~n"),
    ?ROUTER ! {self(), ?OBTENER_PARTIDAS_ROUTER},
    receive
        {?ROUTER, NombreYEstadoPartidas} -> NombreYEstadoPartidas
    end.

%% Inicialización del router.
%% Registra el nombre del proceso.
%% Queda a la espera de mensajes.

-spec router:init() -> no_return().
init() ->
    register(?ROUTER, self()),
    loop(dict:new(), sets:new()).

-spec loop(Partidas::dict(), ConjuntoClientes::set()) -> no_return().
loop(Partidas, ConjuntoClientes) ->
    receive
        {Origen, {?UNIRSE_A_PARTIDA_ROUTER, Nick, NombrePartida}} ->
            case dict:find(NombrePartida, Partidas) of
                {ok, {Partida, EstadoPartida}} ->
                    case EstadoPartida of
                        ?PARTIDA_NO_DISPONIBLE ->
                            Origen ! {?ROUTER, ?ERROR};
                        ?PARTIDA_DISPONIBLE ->
                            case partida:join(Partida, Nick, Origen) of
                                ?OK ->
                                    Origen ! {?ROUTER, Partida};
                                _ ->
                                    Origen ! {?ROUTER, ?ERROR}
                            end
                    end,
                    loop(Partidas, ConjuntoClientes);
                error -> 
                    Origen ! {?ROUTER, PidNuevaPartida = partida:start(NombrePartida, Nick, Origen)},
                    io:format("Partida Creada: ~p ~p ~n",[NombrePartida, ?PARTIDA_DISPONIBLE]),
                    PartidasActualizado   = dict:store(NombrePartida, {Origen, ?PARTIDA_DISPONIBLE}, Partidas),
                    ListaClientes         = sets:to_list(ConjuntoClientes),
                    NombreYEstadoPartidas = obtenerEstadoPartidas(PartidasActualizado, dict:fetch_keys(PartidasActualizado), []),
                    lists:map(fun(Pid) -> Pid ! {?ESTADO_PARTIDA_MODIFICADO, make_ref(), NombreYEstadoPartidas} end, ListaClientes),
                    loop(dict:store(NombrePartida, {PidNuevaPartida, ?PARTIDA_DISPONIBLE}, Partidas), sets:add_element(Origen, ConjuntoClientes))
            end;
        %% Cuando se recibe se envía a los clientes el nuevo listado de partidas.
        {Origen, ?ESTADO_PARTIDA_MODIFICADO, NombrePartidaObjetivo, NuevoEstadoPartida} ->
            io:format("Partida Modificada: ~p ~p ~n",[NombrePartidaObjetivo, NuevoEstadoPartida]),
            PartidasActualizado   = dict:store(NombrePartidaObjetivo, {Origen, NuevoEstadoPartida}, Partidas),
            ListaClientes         = sets:to_list(ConjuntoClientes),
            NombreYEstadoPartidas = obtenerEstadoPartidas(PartidasActualizado, dict:fetch_keys(PartidasActualizado), []),
            lists:map(fun(Pid) -> Pid ! {?ESTADO_PARTIDA_MODIFICADO, make_ref(), NombreYEstadoPartidas} end, ListaClientes),
            loop(PartidasActualizado, ConjuntoClientes);
        {_Origen, ?ELIMINAR_PARTIDA, NombrePartida} ->
            io:format("Partida Eliminada: ~p ~n",[NombrePartida]),
            PartidasActualizado = dict:erase(NombrePartida, Partidas),
            loop(PartidasActualizado, ConjuntoClientes);
        {Origen, ?OBTENER_PARTIDAS_ROUTER} ->
            io:format("Obtener partidas: ~p ~n",[Origen]),
            NombreYEstadoPartidas = obtenerEstadoPartidas(Partidas, dict:fetch_keys(Partidas), []),
            io:format("NombreYEstadoPartidas: ~p~n",[NombreYEstadoPartidas]),
            Origen ! {?ROUTER, NombreYEstadoPartidas},
            loop(Partidas, sets:add_element(Origen, ConjuntoClientes));
        M ->
            io:format("[Router:loop]: Ignorando mensaje desconocido [~p]~n", [M]),
            loop(Partidas, ConjuntoClientes)
    end.

obtenerEstadoPartidas(_Partidas, [], ListaNombrePartidaYEstado) ->
    ListaNombrePartidaYEstado;
obtenerEstadoPartidas(Partidas, [NombrePartida|NombresPartidas], ListaNombrePartidaYEstado) ->
    {_, EstadoPartida} = dict:fetch(NombrePartida, Partidas),
    obtenerEstadoPartidas(Partidas,
                          NombresPartidas,
                          [NombrePartida++"@"++atom_to_list(EstadoPartida)|ListaNombrePartidaYEstado]).
    
