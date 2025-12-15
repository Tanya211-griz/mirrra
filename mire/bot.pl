#!/usr/bin/env swipl

:- initialization(main,main).
:- use_module(library(socket)).
:- use_module(library(random)).

main:-
    sleep(15),  % Ждем 15 секунд пока сервер запустится
    client(localhost,3333).

client(Host,Port):-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream,[]),
        bot(Stream),
        close(Stream)).

bot(Stream):-
    % Пустая строка для первого промпта
    format(Stream,'~s~n',""),
    flush_output(Stream),

    % Имя бота
    format(Stream,'~s~n',"ai_boris2"),
    flush_output(Stream),

    % Описание
    format(Stream,'~s~n',"SUPER SKUF"),
    flush_output(Stream),

    % Статы (сила, интеллект, восприятие)
    format(Stream,'~s~n',"8"),
    flush_output(Stream),
    format(Stream,'~s~n',"1"),
    flush_output(Stream),
    format(Stream,'~s~n',"1"),
    flush_output(Stream),

    % Ждем немного чтобы сервер обработал статы
    sleep(2),

    % Бесконечный цикл действий
    repeat,
        % Случайное движение
        (   maybe(1,2) ->
            (maybe(1,2) ->
                Command = "north" ;
                Command = "south"
            )
        ;   (maybe(1,2) ->
                Command = "east" ;
                Command = "west"
            )
        ),
        format(Stream,'~s~n',Command),
        flush_output(Stream),

        % Ждем 10 секунд
        sleep(10),

        % Кричим
        format(Stream,'~s~n',"yell NINININININININI"),
        flush_output(Stream),

        % Небольшая пауза
        sleep(2),

        % Осматриваемся
        format(Stream,'~s~n',"look"),
        flush_output(Stream),

        sleep(10),

        fail.  % Зацикливаем repeat

% Функция для чтения ответов от сервера (опционально)
read_stream(Stream,[H|T]):-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream,H),
    read_stream(Stream,T).
read_stream(_,[]).