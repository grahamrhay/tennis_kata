-module(tennis_kata_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    tennis_kata_sup:start_link().

stop(_State) ->
    ok.
