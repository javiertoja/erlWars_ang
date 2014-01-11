-define(ROUTER, router).
-define(TAMANO, 10).
-define(NUMNAVES, 3).

-define(TIMEOUTPARTIDA, 60000).
-define(TIMEOUTTURNO, 60000).

-define(PARTIDACERRADA, partidaCerrada).
-define(PARTIDAABIERTA, partidaAbierta).
-define(CAMPEON, campeon).
-define(PERDEDOR, perdedor).
-define(NO_TURNO, noturno).

%% Estados de las celdas del tablero.
-define(VACIO, vacio).
-define(NAVE, nave).
-define(NAVEALCANZADA, naveAlcanzada).
-define(DISPARADO, disparado).

-define(DISPARO_REPETIDO, disparoRepetido).

% Representación de salidas erróneas/exitosas.
-define(OK, ok).
-define(ERROR, error).

-define(PARTIDA_DISPONIBLE, partidaDisponible).
-define(PARTIDA_NO_DISPONIBLE, partidaNoDisponible).
-define(ESTADO_PARTIDA_MODIFICADO, estadoPartidaModificado).
-define(ELIMINAR_PARTIDA, eliminarPartida).
