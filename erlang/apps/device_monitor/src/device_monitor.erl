-module(device_monitor).

-export([empty_parser_state/0, parse_packet/3, format_command/2, format_raw_message/1]).

empty_parser_state() ->
    <<>>.

parse_packet(ForwardMessagesTo, Packet, PartialMessage) ->
    %io:format("Received tcp packet ~p~n", [Packet]),
    Packet1 = <<PartialMessage/binary, Packet/binary>>,
    Messages1 = binary:split(Packet1, <<0>>, [global]),
    NewPartialMessage = lists:last(Messages1), % This is <<>> if packet ends in <<0>>
    Messages2 = lists:droplast(Messages1),
    %io:format("Received messages ~p~n", [Messages2]),
    HandleMessage =
        fun(Message) ->
            case jsone:try_decode(Message) of
                {ok, MessageMap, <<>>} ->
                    gen_statem:cast(ForwardMessagesTo, {message, MessageMap});
                {error, _} ->
                    io:format("Couldn't decode message ~p~n", [Message])
            end
        end,
    lists:map(HandleMessage, Messages2),
    NewPartialMessage.

format_command(Command, ArgsMap) ->
    [jsone:encode([Command, ArgsMap]), <<0>>].

format_raw_message(Message) ->
    [Message, <<0>>].
