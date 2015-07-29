-module(commander_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
    commander_sup:start_link().

stop(_State) ->
    ok.
