%% @private
-module(erlwars_app).
-behaviour(application).

%% API.
%% Funciones que hay que implementar para el behaviour "application".
-export([start/2]).
-export([stop/1]).

%% API.
%% Función para iniciar la aplicación.
start(_Type, _Args) ->
	%% {URIHost, list({URIPath, Handler, Opts})}
	Dispatch = cowboy_router:compile([
	  %% Rutas del host.
		{'_', [
			{"/", cowboy_static, {priv_file, erlwars, "index.html"}},
			%% Se corresponde con la URL: http:localhost:8080/websocket
			{"/websocket", ws_handler, []},
			%% [...] representa el resto del path
			%% cowboy_static sirve TODOS los ficheros del directorio erlwars/priv/static
			{"/images/[...]", cowboy_static, {priv_dir, erlwars, "images"}}
		]}
	]),
	%% Empieza a escuchar conexiones HTTP.
	%% Pool de 100 procesos "acceptor", cada uno de los cuales acepta infinitas conexiones.
	%% Los procesos acceptor tienen como única función aceptar conexiones.
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]).
	%% Inicia el árbol de supervisión.
	%%	websocket_sup:start_link().

%% Función para finalizar la aplicación.
stop(_State) ->
	ok.
