-module(frontend_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, User, Password} = load_basic_auth_credentials(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/authorize", frontend_server_authorization_handler, #{user => User, password => Password, ws_path => <<"/ws">>}},
            {"/ws", frontend_server_websocket_handler, #{token => <<User/binary, ":", Password/binary>>, authorized => false}},
            {"/", cowboy_static, {priv_file, frontend_server, "index.html"}},
            {"/[...]", cowboy_static, {priv_dir, frontend_server, ""}}
    ]}]),
    Port = list_to_integer(os:getenv("AUTOFARM_FRONTEND_SERVER_PORT")),
    {ok, _} = cowboy:start_clear(hello,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Frontend server started on port ~p~n", [Port]),
    frontend_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(hello).

load_basic_auth_credentials() ->
    {ok, PasswordFile} = file:open(os:getenv("AUTOFARM_FRONTEND_SERVER_BASIC_AUTH_CREDENTIALS_FILE"), [read]),
    {ok, Line} = file:read_line(PasswordFile),
    file:close(PasswordFile),
    [User, PasswordNL] = re:split(Line, ":"),
    Password = string:trim(PasswordNL, trailing, "\n"),
    {ok, User, Password}.
