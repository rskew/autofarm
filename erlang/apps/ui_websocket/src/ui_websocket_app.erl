-module(ui_websocket_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", ui_websocket_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(hello,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),
    ui_websocket_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(hello).
