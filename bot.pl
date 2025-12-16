#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).

main :-
    format(user_error, 'Bot starting... Waiting 15 seconds for server.~n', []),
    sleep(15),
    client('localhost', 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        bot(Stream),
        close(Stream)
    ).

bot(Stream) :-
    % Register
    format(Stream, '~n', []), flush_output(Stream),
    format(Stream, 'ai_boris2~n', []), flush_output(Stream),
    format(Stream, 'SUPER SKUF~n', []), flush_output(Stream),
    format(Stream, '8~n', []), flush_output(Stream),
    format(Stream, '1~n', []), flush_output(Stream),
    format(Stream, '1~n', []), flush_output(Stream),
    sleep(2),

    format(user_error, 'Bot registered as "ai_boris2". Game loop started.~n', []),

    repeat,
        format(Stream, 'look~n', []), flush_output(Stream),
        sleep(2),
        read_response(Stream, Response),

        format(user_error, 'Server response:~n~s~n', [Response]),

        (   parse_players(Response, Players),
            Players \= [],
            random_member(Target, Players),
            Target \= "ai_boris2",
            (   random(1, 10) > 5 ->
                format(Stream, 'kill ~w~n', [Target]), flush_output(Stream),
                format(user_error, 'Bot attacks: ~w~n', [Target]),
                sleep(5)
            ;   random(1, 10) > 7 ->
                format(Stream, 'heal ~w~n', [Target]), flush_output(Stream),
                format(user_error, 'Bot heals: ~w~n', [Target]),
                sleep(5)
            ;   true
            )
        ;   true
        ),

        random(1, 101, Action),
        (   Action < 30 ->
            random_move_command(MoveCmd),
            format(Stream, '~w~n', [MoveCmd]), flush_output(Stream),
            format(user_error, 'Bot moves: ~w~n', [MoveCmd]),
            sleep(3)
        ;   Action < 50 ->
            format(Stream, 'yell I AM BOT! HA-HA-HA!~n', []), flush_output(Stream),
            format(user_error, 'Bot yells!~n', []),
            sleep(2)
        ;   Action < 70 ->
            format(Stream, 'yell WHO IS DEAD? I RESURRECT!~n', []), flush_output(Stream),
            sleep(2),
            known_players(Known),
            Known \= [],
            random_member(Potential, Known),
            Potential \= "ai_boris2",
            format(Stream, 'resurrect ~w~n', [Potential]), flush_output(Stream),
            format(user_error, 'Bot tries to resurrect: ~w~n', [Potential]),
            sleep(5)
        ;   Action < 90 ->
            random_say_command(SayCmd),
            format(Stream, '~w~n', [SayCmd]), flush_output(Stream),
            format(user_error, 'Bot says: ~w~n', [SayCmd]),
            sleep(2)
        ;   format(Stream, 'inventory~n', []), flush_output(Stream),
            format(user_error, 'Bot checks inventory~n', []),
            sleep(2)
        ),

        random(3, 15, Pause),
        sleep(Pause),

        fail.

% Helpers

random_move_command(Cmd) :-
    random(1, 5, Dir),
    (   Dir = 1 -> Cmd = north
    ;   Dir = 2 -> Cmd = south
    ;   Dir = 3 -> Cmd = east
    ;   Dir = 4 -> Cmd = west
    ).

random_say_command(Cmd) :-
    random(1, 7, Phrase),
    (   Phrase = 1 -> Cmd = 'say Hello everyone!'
    ;   Phrase = 2 -> Cmd = 'say I am here!'
    ;   Phrase = 3 -> Cmd = 'say Who wants to fight?'
    ;   Phrase = 4 -> Cmd = 'say I am bored...'
    ;   Phrase = 5 -> Cmd = 'say Let us play!'
    ;   Phrase = 6 -> Cmd = 'say Bot is online'
    ).

parse_players(Response, Players) :-
    split_string(Response, "\n", "", Lines),
    member(Line, Lines),
    sub_string(Line, _, _, _, "Игроки здесь:"),
    split_string(Line, ":", "", [_|Rest]),
    atomic_list_concat(Rest, ":", Full),
    split_string(Full, ", ", " ", RawList),
    include(dif(""), RawList, Players),
    !.
parse_players(_, []).


read_response(Stream, Response) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file -> Response = ""
    ;   Line == "" -> Response = ""
    ;   read_response(Stream, Rest),
        atomic_list_concat([Line, "\n", Rest], Response)
    ).

known_players(["player1", "player2", "qqq", "rrr", "yyy", "Andrey", "Vlad"]).

