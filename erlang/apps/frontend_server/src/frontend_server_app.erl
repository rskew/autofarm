-module(frontend_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, User, Password} = load_basic_auth_credentials(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/authorize", frontend_server_authorization_handler, #{user => User, password => Password, ws_url => <<"ws://localhost:8081/ws">>}},
            {"/ws", frontend_server_websocket_handler, #{token => << User/binary, ":", Password/binary >>, authorized => false}},
            %{"/", cowboy_static, {priv_file, frontend_server, "static/assets/index.html"}},
            %{"/[...]", cowboy_static, {priv_dir, frontend_server, "static/assets"}}
            {"/", cowboy_static, {priv_file, frontend_server,     "../../../../frontend/dist/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, frontend_server, "../../../../../../frontend/dist"}}
    ]}]),
    {ok, _} = cowboy:start_clear(hello,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),
    frontend_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(hello).

load_basic_auth_credentials() ->
    {ok, PasswordFile} = file:open(os:getenv("FRONTEND_SERVER_BASIC_AUTH_CREDENTIALS_FILE"), [read]),
    {ok, Line} = file:read_line(PasswordFile),
    file:close(PasswordFile),
    [User, PasswordNL] = re:split(Line, ":"),
    Password = string:trim(PasswordNL, trailing, "\n"),
    {ok, User, Password}.
