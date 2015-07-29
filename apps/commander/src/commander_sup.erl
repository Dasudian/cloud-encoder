-module(commander_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    MaxRestart = 5,
	MaxTime = 30,
	DispatchSpecs = {dispatch,
		{commander_dispatch, start_link, []},
		permanent,
		60000,
		worker,
		[commander_dispatch]
	},

	% application gen server specs
	ServerSpecs = {commander_srv,
		{commander_srv, start_link, []},
		permanent, 60000, worker, [commander_srv]
	},
	% spawn
	{ok, {{one_for_one, MaxRestart, MaxTime}, [DispatchSpecs, ServerSpecs]}}.

