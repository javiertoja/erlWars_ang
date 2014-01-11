%% -*- coding: utf-8 -*-

%%%-------------------------------------------------------------------
%%% @author Alejandro Pernas Pan <alejandro.pernas@udc.es>
%%% @author Laura Otero Méndez <inslom01@udc.es﻿>
%%% @author Javier Toja Alamancos <insjta00@udc.es﻿>
%%% @author Sergio Cores Acha <sergio.cores@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%% Este módulo representa una partida en curso del juego erlwars. Una partida se
%%% compone de jugadores, cada uno de los cuales tiene un tablero asignado. La partida
%%% también contiene un tablero maestro, para mantener información general sobre la misma.
%%% @end
%%%-------------------------------------------------------------------
-module(partida).
-include("erlwars.hrl").

-export([start/3, disparar/4, posicionar_nave/4, join/3, abandonar_partida/2]).

-define(POSICION_SIN_DISPARAR, -1).

-spec partida:start(NombrePartida::string(), Propietario::string(), PidCliente::pid()) -> pid().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función crea un nuevo proceso partida.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>NombrePartida</em>: El nombre asignado a la partida.</li>
%%% <li><em>Propietario</em>: Nick de la persona que crea la partida.</li>
%%% <li><em>PidCliente</em>: Pid del proceso que crea la partida.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>Pid</em>: Pid del nuevo proceso.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
start(NombrePartida, Propietario, PidCliente) ->
    spawn(fun() -> init(NombrePartida, Propietario, PidCliente) end).

init(NombrePartida, Propietario, PidCliente) ->
    put(colaJugadores, queue:in({Propietario, PidCliente}, queue:new())),
    %% Tablero maestro en diccionario de proceso.
    put(tableroMaestro, init_tablero_maestro(?TAMANO, ?TAMANO, dict:new())),
    loop1(NombrePartida,
          dict:store(Propietario, tablero:start_link(), dict:new())).

init_tablero_maestro(0, _Columnas, TableroMaestro) ->
    TableroMaestro;
init_tablero_maestro(Filas, Columnas, TableroMaestro) ->
    case Columnas > 0 of
        true ->
            init_tablero_maestro(Filas, Columnas - 1, dict:store({Filas, Columnas}, ?POSICION_SIN_DISPARAR, TableroMaestro));
        _ ->
            init_tablero_maestro(Filas - 1, ?TAMANO, dict:store({Filas, Columnas}, ?POSICION_SIN_DISPARAR, TableroMaestro))
    end.

-spec partida:join(Partida::pid(), Nick::string(), PidCliente::pid()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite a un jugador unirse a una partida.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Partida</em>: El Pid de la partida a la que el jugador desea unirse.</li>
%%% <li><em>Nick</em>: Nick del jugador que desea unirse a la partida.</li>
%%% <li><em>PidCliente</em>: Pid del cliente que desea unirse a la partida.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>ok:</em>: En caso de que el jugador sea integrado correctamente en la partida.</li>
%%% <li><em>error:</em>: En otro caso.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
join(Partida, Nick, PidCliente) ->
    Partida ! {self(), PidCliente, {join, Nick}},
    receive
        ?PARTIDAABIERTA -> ?OK;
        _               -> ?ERROR
    end.

-spec partida:abandonar_partida(Partida::pid(), Nick::string()) -> no_return().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite informar a la partida que un jugador ha abandonado, ya sea voluntariamente
%%% o por desconexion
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Partida</em>: El Pid de la partida a la que se informa.</li>
%%% <li><em>Nick</em>: El Nick del jugador que deja la partida.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
abandonar_partida(Partida, Nick) ->
    Partida ! {dejarPartida, Nick}.

-spec partida:posicionar_nave(Partida::pid(), Nick::string(), X::integer(), Y::integer()) -> no_return().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite a un jugador posicionar su nave en su tablero de la partida.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Partida</em>: El Pid de la partida en la que se desea posicionar la nave.</li>
%%% <li><em>Nick</em>: Nick del jugador cuya nave es posicionada en el tablero de la partida.</li>
%%% <li><em>X,Y</em>: Coordenadas de la posición en el tablero en la que se desea posicionar la nave.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
posicionar_nave(Partida, Nick, X, Y) ->
    Partida ! {self(), {posicionarNave, Nick, X, Y}}.
    
-spec partida:disparar(Partida::pid(), Nick::string(), X::integer(), Y::integer()) -> integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función permite disparar a la posición especificada de los tableros de otros jugadores.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Partida</em>: El Pid de la partida en la que se desea disparar.</li>
%%% <li><em>Nick</em>: Nick de la persona que dispara en la partida.</li>
%%% <li><em>X,Y</em>: Coordenadas de la posición en la que se desea disparar.</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>Número de aciertos:</em>: .</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
disparar(Partida, Nick, X, Y) ->
    Partida ! {self(), {disparo, Nick, X, Y}},
    receive
        {numDisparos, NumAciertos} ->
            NumAciertos
    end.

%% Se permite unirse a la partida y posicionar las naves
%% No hay timeout, en cuanto se unan dos jugadores se pasa a loop2
loop1(NombrePartida, Tableros) ->
    io:format("loop1~n"),
    receive
        {PidJoin, From, {join, Nick}} ->
            PidJoin ! ?PARTIDAABIERTA,
            case dict:find(Nick, Tableros) of
                error ->
                    NuevosTableros = dict:store(Nick, tablero:start_link(), Tableros),
                    
                    case dict:size(NuevosTableros) > 1 of
                        true ->
                            put(colaJugadores, queue:in({Nick,From}, get(colaJugadores))), %
                            loop2(NombrePartida, NuevosTableros);
                        _ ->
                            loop1(NombrePartida, NuevosTableros)
                    end;
                {ok, _Tablero} ->
                    case dict:size(Tableros) > 1 of
                        true ->
                            loop2(NombrePartida, Tableros);
                        _ ->
                            loop1(NombrePartida, Tableros)
                    end
            end;
        {_From, {posicionarNave, Nick, X, Y}} ->
            case dict:find(Nick, Tableros) of
                {ok, Tablero} ->
                    tablero:posicionar_nave(Tablero, X, Y);
                _ ->
                    ?ERROR
            end,
            loop1(NombrePartida, Tableros);
        {dejarPartida, Nick} ->
            TablerosNuevos = dict:erase(Nick,Tableros),
            ColaJugadoresNueva = queue:filter(fun({NickJugador, _PidJugador}) -> 
                                                    NickJugador /= Nick
                                              end, get(colaJugadores)),
            put(colaJugadores, ColaJugadoresNueva),
            case dict:size(TablerosNuevos) of
                0 ->
                    finPartida(NombrePartida);
                _ ->
                    loop1(NombrePartida, TablerosNuevos)
            end;
        _ ->
            loop1(NombrePartida, Tableros)
    end.

%% se permite unirse a la partida y posicionar las naves
%% si no hay más mensajes en el buzón, cuando el timeout expira, se pasa a loop3
loop2(NombrePartida, Tableros) ->
    io:format("loop2 ~n"),

    receive
        {PidJoin, From, {join, Nick}} ->
            
            PidJoin ! ?PARTIDAABIERTA,
            case dict:find(Nick,Tableros) of
                {ok, _Tablero} ->
                    loop2(NombrePartida, Tableros);
                _ ->
                    put(colaJugadores, queue:in({Nick,From}, get(colaJugadores))),
                    loop2(NombrePartida, dict:store(Nick, tablero:start_link(), Tableros))
            end;
        {_From, {posicionarNave, Nick, X, Y}} ->
            case dict:find(Nick,Tableros) of
                {ok,Tablero} ->
                    tablero:posicionar_nave(Tablero, X, Y),
                    case todos_posicionados(dict:to_list(Tableros)) of
                        true ->
                            ?ROUTER ! {self(), ?ESTADO_PARTIDA_MODIFICADO, NombrePartida, ?PARTIDA_NO_DISPONIBLE},
                            loop4(NombrePartida, Tableros);
                        _ ->
                            loop2(NombrePartida, Tableros)
                    end;
                _ ->
                    ?ERROR
            end,
            loop2(NombrePartida, Tableros);
        {dejarPartida, Nick} ->
            TablerosNuevos = dict:erase(Nick,Tableros),
            ColaJugadoresNueva = queue:filter(fun({NickJugador, _PidJugador}) -> 
                                                    NickJugador /= Nick 
                                              end, get(colaJugadores)),
            put(colaJugadores, ColaJugadoresNueva),
            case dict:size(TablerosNuevos) of
                0 ->
                    finPartida(NombrePartida);
                1 ->
                    loop1(NombrePartida, TablerosNuevos);
                _ ->
                    loop2(NombrePartida, TablerosNuevos)
            end;
        _ ->
            loop2(NombrePartida, Tableros)
            %% A value of 0 in the timeout means check the message buffer first and if it is empty execute the following code. 
    after ?TIMEOUTPARTIDA ->
            ?ROUTER ! {self(), ?ESTADO_PARTIDA_MODIFICADO, NombrePartida, ?PARTIDA_NO_DISPONIBLE},
            loop3(NombrePartida, Tableros)
    end.

%% No se permite unirse a la partida. Se permite posicionar las naves
%% no hay timeout
loop3(NombrePartida, Tableros) ->
    io:format("loop3~n"),
    receive
        {PidJoin, _From, {join, _Nick}} ->
            PidJoin ! ?PARTIDACERRADA,
            loop3(NombrePartida, Tableros);
        {_From, {posicionarNave, Nick, X, Y}} ->
            case dict:find(Nick,Tableros) of
                {ok,Tablero} ->
                    tablero:posicionar_nave(Tablero, X, Y),
                    case todos_posicionados(dict:to_list(Tableros)) of
                        true ->
                            loop4(NombrePartida, Tableros);
                        _ ->
                            loop3(NombrePartida, Tableros)
                    end;
                _ ->
                    io:format("loop3 ?ERROR~n"),
                    ?ERROR
            end;
        {dejarPartida, Nick} ->
            TablerosNuevos = dict:erase(Nick,Tableros),
            ColaJugadoresNueva = queue:filter(fun({NickJugador, _PidJugador}) -> 
                                                    NickJugador /= Nick 
                                              end, get(colaJugadores)),
            put(colaJugadores, ColaJugadoresNueva),
            case dict:size(TablerosNuevos) of
                0 ->
                    finPartida(NombrePartida);
                1 ->
                    loop1(NombrePartida, TablerosNuevos);
                _ ->
                    loop3(NombrePartida, TablerosNuevos)
            end;
        _ ->
            loop3(NombrePartida, Tableros)
    end.

todos_posicionados([]) ->
    true;
todos_posicionados([{_Nick,Tablero}|Lista]) ->
    case tablero:estan_naves_posicionadas(Tablero) of
        true ->
            io:format("Todos posicionados~n"),
            todos_posicionados(Lista);
        _ ->
            io:format("Alguno NO posicionado~n"),
            false
    end.

%% No se permite unirse a la partida ni posicionar las naves.
%% Ya deben estar todas posicionadas
%% Se permite disparar antes de que acabe el timeout
%% Hay timeout después del cual saltará el turno al siguiente jugador
loop4(NombrePartida, Tableros) ->
    io:format("loop4 ~n"),

    Cola = get(colaJugadores),
    
    case queue:len(Cola) of
        1 ->
            {{value, {_Nick1, Pid}}, _NuevaCola} = queue:out(Cola),
            Pid ! {?CAMPEON, make_ref(), "Noraboa"},
            finPartida(NombrePartida);
        _ ->
            case queue:out(get(colaJugadores)) of
                {{value, {Nick1, Pid}}, NuevaCola} ->
                    put(colaJugadores, queue:in({Nick1, Pid}, NuevaCola)),
                    Pid ! {turno, make_ref(), "turno"},
                   loop5({Nick1, Pid}, NombrePartida, Tableros);
                {empty, _Cola} ->
                    finPartida(NombrePartida)
            end
    end.


%% Bucle para tratar con las acciones del que tiene el turno.
loop5({Nick,Pid}, NombrePartida, Tableros) ->
    io:format("loop5~n"),
    receive
        {PidJoin, _From, {join, _Nick}} ->
            PidJoin ! ?PARTIDACERRADA,
            loop5({Nick,Pid}, NombrePartida, Tableros);
        {Pid, {disparo, Nick, X, Y}} ->
            case dict:find(Nick, Tableros) of
                {ok, _Tablero} ->
                    Pid ! {numDisparos, disparo(Nick, Tableros, X, Y)},
                    loop4(NombrePartida, Tableros);
                _ ->
                    io:format("Error: No existe el tablero del nick ~p~n", [Nick]),
                    loop4(NombrePartida, Tableros)
            end;
        {dejarPartida, Nick} ->
            TablerosNuevos = dict:erase(Nick,Tableros),
            ColaJugadoresNueva = queue:filter(fun({NickJugador, _PidJugador}) -> 
                                                    NickJugador /= Nick 
                                              end, get(colaJugadores)),
            put(colaJugadores, ColaJugadoresNueva),
            case dict:size(TablerosNuevos) of
                0 ->
                    finPartida(NombrePartida);
                1 ->
                    loop1(NombrePartida, TablerosNuevos);
                _ ->
                    loop4(NombrePartida, TablerosNuevos)
            end;
        _ ->
            loop5({Nick, Pid}, NombrePartida, Tableros)
    after ?TIMEOUTTURNO ->
            Pid ! {turno, make_ref(), noturno},
            loop4(NombrePartida, Tableros)		
    end.

finPartida(NombrePartida) ->
    ?ROUTER ! {self(), ?ELIMINAR_PARTIDA, NombrePartida}.

disparo(Nick, Tableros, X, Y) ->
    NavesAlcanzadas = disparo_aux(Nick, dict:to_list(Tableros), X, Y, 0),
    put(tableroMaestro, dict:store({X, Y}, NavesAlcanzadas, get(tableroMaestro))),
    notificacion_disparo(Nick, X, Y, NavesAlcanzadas, Tableros),
    notificar_perdedores(Nick, dict:to_list(Tableros), X, Y),
    NavesAlcanzadas.

disparo_aux(_Nick, [], _X, _Y, NumAciertos) ->
    NumAciertos;

disparo_aux(Nick, [{Clave, _Valor}|T], X, Y, NumAciertos) when Nick == Clave ->
    disparo_aux(Nick, T, X, Y, NumAciertos);

disparo_aux(Nick, [{_Clave, Valor}|T], X, Y, NumAciertos) ->
    case tablero:disparo(Valor, X, Y) of
        ?NAVE ->
            disparo_aux(Nick, T, X, Y, NumAciertos + 1);
        _ ->
            disparo_aux(Nick, T, X, Y, NumAciertos)
    end.

notificar_perdedores(_Nick, [], _X, _Y) ->
    ?OK;

notificar_perdedores(Nick, [{Clave, _Valor}|T], X, Y) when Nick == Clave ->
    notificar_perdedores(Nick, T, X, Y);

notificar_perdedores(Nick, [{Clave, Valor}|T], X, Y) ->
    case tablero:todo_destruido(Valor) of
        true ->
            Lista = queue:to_list(get(colaJugadores)),
            case lists:filter(fun({NickJugador, _PidJugador}) -> NickJugador == Clave end, Lista) of
                [] ->
                    ?OK;
                [{N, P}|_Tail] ->
                    put(colaJugadores, queue:from_list(lists:delete({N, P}, Lista))),
                    P ! {?PERDEDOR, make_ref(), "Perdiches!!!"}
            end,
            notificar_perdedores(Nick, T, X, Y);
        _ ->
            notificar_perdedores(Nick, T, X, Y)
    end.

notificacion_disparo(Nick, X, Y, NavesAlcanzadas, Tableros) ->
    Lista = queue:to_list(get(colaJugadores)),
    notificacion_disparo_aux(Nick, X, Y, NavesAlcanzadas, Lista, Tableros).

notificacion_disparo_aux(_Nick, _X, _Y, _NavesAlcanzadas, [], _Tableros) -> ok;

notificacion_disparo_aux(NickDisparo, X, Y, NavesAlcanzadas, [{NickJugador, PidJugador}|Lista], Tableros) ->
    io:format("Naves Alcanzadas:~p   PidJugador:~p ~n",[NavesAlcanzadas, PidJugador]),
    if
        NickJugador == NickDisparo ->
            Mensaje = "notificacionDisparado&"++integer_to_list(NavesAlcanzadas)++"&"++integer_to_list(X)++"&"++integer_to_list(Y)++"&false&true",
            io:format("Mensaje:~p~n",[Mensaje]),
            PidJugador ! {notificacionDisparado, make_ref(), Mensaje};
        true ->

            case tablero:get_resultado(dict:fetch(NickJugador, Tableros), X, Y) of
                {ok,?NAVEALCANZADA} ->
                    Mensaje = "notificacionDisparado&"++integer_to_list(NavesAlcanzadas)++"&"++integer_to_list(X)++"&"++integer_to_list(Y)++"&true&false",
                    io:format("Mensaje:~p~n",[Mensaje]),
                    PidJugador ! {notificacionDisparado, make_ref(), Mensaje};
                _ ->
                    Mensaje = "notificacionDisparado&"++integer_to_list(NavesAlcanzadas)++"&"++integer_to_list(X)++"&"++integer_to_list(Y)++"&false&false",
                    io:format("Mensaje:~p~n",[Mensaje]),
                    PidJugador ! {notificacionDisparado, make_ref(), Mensaje}
            end
    end,
    notificacion_disparo_aux(NickDisparo, X, Y, NavesAlcanzadas, Lista, Tableros).
  
