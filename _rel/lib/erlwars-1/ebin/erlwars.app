{application, erlwars, [
	{description, "Juego ERL Wars."},
	{vsn, "1"},
	{modules, [erlwars_app, router, tablero, ws_handler, partida]},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		cowboy
	]},
	{mod, {erlwars_app, []}},
	{env, []}
]}.
