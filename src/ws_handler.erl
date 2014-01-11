%% -*- coding: utf-8 -*-

%%%-------------------------------------------------------------------
%%% @author Alejandro Pernas Pan <alejandro.pernas@udc.es>
%%% @author Laura Otero Méndez <inslom01@udc.es﻿>
%%% @author Javier Toja Alamancos <insjta00@udc.es﻿>
%%% @author Sergio Cores Acha <sergio.cores@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%% Este módulo implementa el servidor web al que se conecta el cliente web durante el juego erlwars.
%%% Utiliza cowboy y el protocolo websocket para la comunicación entre el cliente y el servidor. Está
%%% basado en el behaviour cowboy_websocket_handler proporcionado por cowboy.
%%% ==Información adicional==
%%% <ul>
%%% <li>[https://github.com/extend/cowboy/blob/master/manual/cowboy_websocket_handler.md]</li>
%%% <li>[https://github.com/extend/cowboy/blob/master/manual/cowboy_req.md]</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------

-module(ws_handler).
-behaviour(cowboy_websocket_handler).

%% El behaviour está documentado en https://github.com/extend/cowboy/blob/master/manual/cowboy_websocket_handler.md

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("erlwars.hrl").

-define(PARTIDA_CREADA(NombrePartida, Nick),     ["partidaCreada", "&", NombrePartida, "&", Nick]).
-define(UNIDO_PARTIDA(NombrePartida, Nick),      ["unidoAPartida", "&", NombrePartida, "&", Nick]).
-define(LISTO_PARA_EMPEZAR(NombrePartida, Nick), ["ok", "&", NombrePartida, "&", Nick]).
-define(NAVE_POSICIONADA(NombrePartida, Nick),   ["ok", "&", NombrePartida, "&", Nick]).
-define(DISPARO_EFECTUADO(),                     ["disparoEfectuado", "", "", "&", "7"]).
-define(TURNO_ACTIVADO,                          ["turno"]).
-define(TURNO_DESACTIVADO,                       ["noturno"]).


-define(SOLICITAR_PARTIDAS_RECV, "partidas").
-define(CREAR_PARTIDA_RECV, "crearPartida").
-define(UNIRSE_A_PARTIDA_RECV, "unirseAPartida").
-define(LISTO_PARA_EMPEZAR_RECV, "listoParaEmpezar").
-define(POSICIONAR_NAVE_RECV, "posicionarNave").
-define(DISPARAR_RECV, "disparo").

%% Siempre se invoca.
%% Actualiza el protocolo a cowboy_websocket.
%% _Req = cowboy_req:req()
%% https://github.com/extend/cowboy/blob/master/manual/cowboy_req.md
%% cowboy_req module provides functions to access, manipulate and respond to requests.
-spec ws_handler:init(tuple(TransportName::atom(), ProtocolName::atom()), _Req::cowboy_req:req(), _Opts::any()) -> tuple(atom(), atom(), atom()).
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función actualiza el protocolo a cowboy_websocket.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>TransportName</em>: El atom tcp.</li>
%%% <li><em>ProtocolName</em>: El atom http.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li>Retorna la tupla {upgrade, protocol, cowboy_websocket}.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

%% Siempre se invoca.
%% Invocada antes de cambiar al protocolo Websocket.
%% Sirve para registrar este proceso como un manejador de eventos.
%% Si el proceso router no está ejecutando, lo inicia.
-spec ws_handler:websocket_init(TransportName::atom(), Req::cowboy_req:req(), _Opts::any()) -> tuple(atom(), cowboy_req:req(), atom()).
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función inicializa el estado para esta sesión.
%%% Es invocada antes de que la actualización a WebSocket ocurra. Puede ser usada para negociar extensiones del
%%% protocolo websocket con el cliente.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>TransportName</em>: El atom tcp.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li>Retorna la tupla {ok, Req, undefined_state}.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
websocket_init(_TransportName, Req, _Opts) ->
  case whereis(?ROUTER) of
    undefined ->
      router:start();
    _ ->
      ok
  end,
  {?OK, Req, undefined_state}.

%% Se invoca cero o más veces.
%% Maneja los datos recibidos por la conexión websocket.
%% Se invoca cada vez que se reciben datos del cliente web.
-spec ws_handler:websocket_handle(tuple(Text::atom(), Msg::binary()), Req::cowboy_req:req(), State::any()) -> tuple(Reply::atom(), tuple(Text::atom(), Content::binary()), Req::cowboy_req:req(), State::any()).
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función maneja los datos recibidos desde la conexión WebSocket. Se invoca cada vez que se reciben datos
%%% por la conexión.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Text</em>: El atom text.</li>
%%% <li><em>Msg</em>: El mensaje recibido en binario.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% <li><em>Req</em>: any().</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>Reply</em>: El atom reply.</li>
%%% <li><em>Content</em>: El contenido de la respuesta en binario.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% <li><em>State</em>: any().</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
websocket_handle({text, Msg}, Req, State) ->
    io:format("PID HANDLER ~p~n", [self()]),
    case ContenidoMsg = binary_to_list(Msg) of
        ?SOLICITAR_PARTIDAS_RECV ->
            {reply, {text, list_to_binary([ContenidoMsg|lists:map(fun (X) -> "&"++X end, ?ROUTER:obtener_partidas())])}, Req, State};
        M ->
            case string:tokens(M, ",") of
                [?CREAR_PARTIDA_RECV, Nick, NombrePartida] ->
                    %% Diccionario de proceso: Clave = PID Handler ; Valor = PID partida asociada con el handler.
                    put(self(), ?ROUTER:unirse_a_partida(Nick, NombrePartida)),
                    put("Nick", Nick),
                    io:format("[ws_handler:websocket_handle]: Partida:[~p] Nick: [~p] Partida: [Creada]~n", [NombrePartida, Nick]),
                    {reply, {text, list_to_binary(?PARTIDA_CREADA(NombrePartida, Nick))}, Req, State};
                [?UNIRSE_A_PARTIDA_RECV, Nick, NombrePartida] ->
                    case PartidaObjetivo = ?ROUTER:unirse_a_partida(Nick, NombrePartida) of
                        ?PARTIDACERRADA ->
                            {reply, {text, list_to_binary([atom_to_list(PartidaObjetivo)])}, Req, State};
                        _ ->
                            %% Diccionario de proceso: Clave = PID Handler ; Valor = PID partida asociada con el handler.
                            put(self(), PartidaObjetivo),
                            put("Nick", Nick),
                            {reply, {text, list_to_binary(?UNIDO_PARTIDA(NombrePartida, Nick))}, Req, State}
                    end;
                [?LISTO_PARA_EMPEZAR_RECV, Nick, NombrePartida] ->
                    io:format("[ws_handler:websocket_handle]: Partida:[~p]   Nick: [~p] Estado: [Listo para empezar]~n", [NombrePartida, Nick]),
                    {reply, {text, list_to_binary(?LISTO_PARA_EMPEZAR(NombrePartida, Nick))}, Req, State};
                [?POSICIONAR_NAVE_RECV, Nick, NombrePartida, X, Y] ->
                    case PidPartida = get(self()) of
                        undefined ->
                            io:format("[ws_handler:websocket_handle]: El ID del cliente no se encuentra en el diccionario de proceso~n"),
                            {reply, {text, list_to_binary([atom_to_list(?ERROR)])}, Req, State};
                        _ ->
                            partida:posicionar_nave(PidPartida, Nick, list_to_integer(X), list_to_integer(Y)),
                            io:format("[ws_handler:websocket_handle]: Partida:[~p] Nick: [~p] Nave en: [~p,~p]~n", [NombrePartida, Nick, list_to_integer(X), list_to_integer(Y)]),
                            {reply, {text, list_to_binary(?NAVE_POSICIONADA(NombrePartida, Nick))}, Req, State}
                    end;
                [?DISPARAR_RECV, Nick, NombrePartida, X, Y] ->
                    case PidPartida = get(self()) of
                        undefined ->
                            io:format("[ws_handler:websocket_handle]: El ID del cliente no se encuentra en el diccionario de proceso~n"),
                            {reply, {text, list_to_binary([atom_to_list(?ERROR)])}, Req, State};
                        _ ->
                            partida:disparar(PidPartida, Nick, list_to_integer(X), list_to_integer(Y)),
                            io:format("[ws_handler:websocket_handle]: Partida:[~p] Nick: [~p] Disparo en: [~p,~p]~n", [NombrePartida, Nick, list_to_integer(X), list_to_integer(Y)]),
                            {reply, {text, list_to_binary(?DISPARO_EFECTUADO())}, Req, State}
                    end;
                AccionNoPermitida ->
                    io:format("[ws_handler:websocket_handle]: Rebicida acción no permitida ~p~n", [AccionNoPermitida]),
                    {reply, {text, list_to_binary([atom_to_list(?ERROR)])}, Req, State}
            end
    end;
websocket_handle(MensajeDesconocido, Req, State) ->
  io:format("[ws_handler:websocket_handle]: Rebicido mensaje desconocido ~p~n", [MensajeDesconocido]),
  {?ERROR, Req, State}.

%% 1 handler recibe join por WS
%% 2 handler envía join a Router
%% 3 Router busca a partida e pásalle o PID do handler
%% 4 Router devolve PID de partida ó handler
%% e despois diso o handler envía as peticións directamente á Partida
%% Se invoca cero o más veces.
%% Cada vez que se recibe un mensaje Erlang.
-spec ws_handler:websocket_info(tuple(TipoMensaje::atom(), Ref::reference(), Msg::any()), Req::cowboy_req:req(), State::any()) -> tuple(Reply::atom(), tuple(Text::atom(), Content::binary()), Req::cowboy_req:req(), State::any()).
%%%-------------------------------------------------------------------
%%% @doc
%%% Esta función se invoca cada vez que este proceso (servidor web) recibe un mensaje Erlang.
%%% En concreto, se emplea para reenviar los mensajes Erlang, recibidos por este proceso, al
%%% cliente web. Recepción de mensajes asíncronos y reenvío.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>TipoMensaje</em>: Identifica el tipo de mensaje que se enviará al cliente web.</li>
%%% <li><em>Ref</em>: Referencia que permite identificar el mensaje concreto.</li>
%%% <li><em>Msg</em>: any().</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% <li><em>State</em>: any().</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li><em>Reply</em>: El atom reply.</li>
%%% <li><em>Content</em>: El contenido de la respuesta en binario.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% <li><em>State</em>: any().</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
websocket_info({turno, _Ref, ?NO_TURNO}, Req, State) ->
  io:format("Enviando turno al cliente web~n"),
  {reply, {text, list_to_binary(?TURNO_DESACTIVADO)}, Req, State};
websocket_info({turno, _Ref, _Msg}, Req, State) ->
  io:format("Enviando turno al cliente web~n"),
  {reply, {text, list_to_binary(?TURNO_ACTIVADO)}, Req, State};
websocket_info({?CAMPEON, _Ref, _Msg}, Req, State) ->
  io:format("Enviando \"campeon\" al cliente web~n"),
  {reply, {text, list_to_binary([atom_to_list(?CAMPEON)])}, Req, State};
websocket_info({?PERDEDOR, _Ref, _Msg}, Req, State) ->
  io:format("Enviando \"perdedor\" cliente web~n"),
  {reply, {text, list_to_binary([atom_to_list(?PERDEDOR)])}, Req, State};
websocket_info({notificacionDisparado, _Ref, Msg}, Req, State) ->
  io:format("Enviando ~p cliente web~n", [Msg]),
  {reply, {text, list_to_binary([Msg])}, Req, State};
websocket_info({?ESTADO_PARTIDA_MODIFICADO, _Ref, Msg}, Req, State) ->
  io:format("Enviando nuevo listado de partidas al cliente web: ~p~n", [Msg]),
  {reply, {text, list_to_binary([?SOLICITAR_PARTIDAS_RECV|lists:map(fun (X) -> "&"++X end, Msg)])}, Req, State};
websocket_info(Info, Req, State) ->
  io:format("Error en el servidor: ~p~n",[Info]),
  {?ERROR, Req, State}.

%% Cierra la conexión y termina el proceso.
-spec ws_handler:websocket_terminate(_Reason::tuple(), _Req::cowboy_req:req(), _State::any()) -> atom().
%%%-------------------------------------------------------------------
%%% @doc
%%% Cierra la conexión y termina el proceso.
%%% ==Parámetros de entrada==
%%% <ul>
%%% <li><em>Reason</em>: Razón por la que se termina la conexión.</li>
%%% <li><em>Req</em>: cowboy_req:req().</li>
%%% <li><em>State</em>: any().</li>
%%% </ul>
%%% ==Valor de retorno==
%%% <ul>
%%% <li>Retorna el átomo ok.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
websocket_terminate(Reason, _Req, _State) ->
  io:format("Terminando Proceso, Razón: ~p~n",[Reason]),
  case PidPartida = get(self()) of
    undefined ->
        io:format("[ws_handler:websocket_handle]: El ID del cliente no se encuentra en el diccionario de proceso~n");
    _ ->
      case Nick = get("Nick") of
        undefined ->
            io:format("[ws_handler:websocket_handle]: El Nick del cliente no se encuentra en el diccionario de proceso~n");
        _ ->
          partida:abandonar_partida(PidPartida,Nick)
      end
  end,  
  ?OK.
