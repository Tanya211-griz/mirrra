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
    format(Stream,'~s~n',"8"),    % Высокая сила для убийств
    flush_output(Stream),
    format(Stream,'~s~n',"1"),
    flush_output(Stream),
    format(Stream,'~s~n',"1"),
    flush_output(Stream),

    % Ждем немного чтобы сервер обработал статы
    sleep(2),

    % Основной игровой цикл
    repeat,
        % Получаем информацию о комнате
        format(Stream,'~s~n',"look"),
        flush_output(Stream),
        sleep(2),
        read_response(Stream, Response),

        % Парсим ответ для поиска игроков
        (   parse_players(Response, Players),
            Players \= [] ->
            % Игроки найдены, выбираем случайного для атаки
            random_member(Target, Players),
            Target \= "ai_boris2",  % Не атакуем сами себя
            (   random(1, 10) > 5 ->  % 50% шанс атаковать
                atomic_list_concat(['kill', Target], ' ', Command),
                format(Stream,'~s~n',Command),
                flush_output(Stream),
                format('Бот атакует ~w~n',[Target]),
                sleep(5)
            ;   % 50% шанс исцелить
                random(1, 10) > 7 ->  % 30% шанс исцелить
                atomic_list_concat(['heal', Target], ' ', Command),
                format(Stream,'~s~n',Command),
                flush_output(Stream),
                format('Бот исцеляет ~w~n',[Target]),
                sleep(5)
            ;   true
            )
        ;   true
        ),

        % Случайное действие
        random(1, 101, Action),
        (   Action < 30 ->  % 30% шанс движения
            random_move_command(MoveCmd),
            format(Stream,'~s~n',MoveCmd),
            flush_output(Stream),
            format('Бот двигается: ~w~n',[MoveCmd]),
            sleep(3)
        ;   Action < 50 ->  % 20% шанс крикнуть
            format(Stream,'~s~n',"yell Я БОТ! БУ-ГА-ГА!"),
            flush_output(Stream),
            format('Бот кричит~n',[]),
            sleep(2)
        ;   Action < 70 ->  % 20% шанс воскресить (если есть мертвые)
            format(Stream,'~s~n',"yell КТО УМЕР? Я ВОСКРЕШУ!"),
            flush_output(Stream),
            sleep(2),
            % Попробуем воскресить случайного игрока из известных
            known_players(KnownPlayers),
            KnownPlayers \= [],
            random_member(PotentialTarget, KnownPlayers),
            PotentialTarget \= "ai_boris2",
            atomic_list_concat(['resurrect', PotentialTarget], ' ', ResurrectCmd),
            format(Stream,'~s~n',ResurrectCmd),
            flush_output(Stream),
            format('Бот пытается воскресить ~w~n',[PotentialTarget]),
            sleep(5)
        ;   Action < 90 ->  % 20% шанс сказать что-то
            random_say_command(SayCmd),
            format(Stream,'~s~n',SayCmd),
            flush_output(Stream),
            format('Бот говорит: ~w~n',[SayCmd]),
            sleep(2)
        ;   % 10% шанс проверить инвентарь
            format(Stream,'~s~n',"inventory"),
            flush_output(Stream),
            format('Бот проверяет инвентарь~n',[]),
            sleep(2)
        ),

        % Случайная пауза между действиями
        random(3, 15, Pause),
        sleep(Pause),

        fail.  % Зацикливаем repeat

% Генератор случайных команд движения
random_move_command(Cmd) :-
    random(1, 5, Dir),
    (   Dir = 1 -> Cmd = "north"
    ;   Dir = 2 -> Cmd = "south"
    ;   Dir = 3 -> Cmd = "east"
    ;   Cmd = "west"
    ).

% Генератор случайных фраз для say
random_say_command(Cmd) :-
    random(1, 7, Phrase),
    (   Phrase = 1 -> Cmd = "say Привет всем!"
    ;   Phrase = 2 -> Cmd = "say Я здесь!"
    ;   Phrase = 3 -> Cmd = "say Кто хочет подраться?"
    ;   Phrase = 4 -> Cmd = "say Мне скучно..."
    ;   Phrase = 5 -> Cmd = "say Давайте играть!"
    ;   Cmd = "say Бот онлайн"
    ).

% Парсинг списка игроков из ответа look
parse_players(Response, Players) :-
    split_string(Response, "\n", "", Lines),
    member(Line, Lines),
    sub_string(Line, _, _, _, "Игроки здесь:"),
    split_string(Line, ":", "", Parts),
    length(Parts, 2),
    nth1(2, Parts, PlayersStr),
    split_string(PlayersStr, ", ", " ", PlayersList),
    exclude(=\="", PlayersList, Players), !.
parse_players(_, []).

% Чтение ответа от сервера
read_response(Stream, Response) :-
    read_line_to_string(Stream, Line),
    (   Line = end_of_file ->
        Response = ""
    ;   read_response(Stream, Rest),
        atomic_list_concat([Line, "\n", Rest], Response)
    ).

% Список известных игроков (можно расширять)
known_players(["player1", "player2", "qqq", "rrr", "yyy"]).

% Функция для случайной проверки (для совместимости)
maybe(A,B) :- random(1, B+1, X), X =< A.