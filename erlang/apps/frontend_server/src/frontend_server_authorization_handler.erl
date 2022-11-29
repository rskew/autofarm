-module(frontend_server_authorization_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

is_authorized(Req, State=#{user := User, password := Password}) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Password} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

to_json(Req, State=#{user := User, password := Password, ws_url := WsUrl}) ->
    {jsone:encode(#{ws_url => WsUrl, session_token => << User/binary, ":", Password/binary >>}), Req, State}.
