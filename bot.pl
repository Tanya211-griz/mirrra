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
        tcp_connect(Host:Port, Stream, [encoding(utf8)]),
        bot(Stream),
        close(Stream)
    ).

bot(Stream) :-
    % Устанавливаем UTF-8 кодировку
    set_stream(Stream, encoding(utf8)),
    
    % Регистрация
    format(Stream, '~n', []), flush_output(Stream),
    sleep(0.5),
    
    format(Stream, 'ai_boris2~n', []), flush_output(Stream),
    sleep(0.5),
    
    format(Stream, 'SUPER SKUF~n', []), flush_output(Stream),
    sleep(0.5),
    
    format(Stream, '8~n', []), flush_output(Stream),
    sleep(0.5),
    
    format(Stream, '1~n', []), flush_output(Stream),
    sleep(0.5),
    
    format(Stream, '1~n', []), flush_output(Stream),
    sleep(2),
    
    format(user_error, 'Bot registered as "ai_boris2". Game loop started.~n', []),
    
    % Основной игровой цикл
    game_loop(Stream).

% Основной игровой цикл
game_loop(Stream) :-
    repeat,
        % Выполняем команду look
        format(Stream, 'look~n', []), 
        flush_output(Stream),
        sleep(1),
        
        % Читаем ответ (простой способ - читаем несколько строк)
        catch(read_response_simple(Stream, Response), _, Response = ""),
        
        format(user_error, 'Server response:~n~s~n---~n', [Response]),

        % Парсим игроков и взаимодействуем
        (   parse_players(Response, Players),
            Players \= [],
            random_member(Target, Players),
            Target \= "ai_boris2" ->
            
            random(1, 10, RandAction),
            (   RandAction > 7 ->
                format(Stream, 'kill ~w~n', [Target]), 
                flush_output(Stream),
                format(user_error, 'Bot attacks: ~w~n', [Target]),
                sleep(2),
                catch(read_response_simple(Stream, _), _, true)
                
            ;   RandAction > 5 ->
                format(Stream, 'heal ~w~n', [Target]), 
                flush_output(Stream),
                format(user_error, 'Bot heals: ~w~n', [Target]),
                sleep(2),
                catch(read_response_simple(Stream, _), _, true)
                
            ;   true
            )
        ;   true
        ),

        % Случайное действие
        random(1, 101, Action),
        (   Action < 25 ->
            random_move_command(MoveCmd),
            format(Stream, '~w~n', [MoveCmd]), 
            flush_output(Stream),
            format(user_error, 'Bot moves: ~w~n', [MoveCmd]),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true)
            
        ;   Action < 40 ->
            format(Stream, 'yell I AM BOT! HA-HA-HA!~n', []), 
            flush_output(Stream),
            format(user_error, 'Bot yells!~n', []),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true)
            
        ;   Action < 55 ->
            format(Stream, 'yell WHO IS DEAD? I RESURRECT!~n', []), 
            flush_output(Stream),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true),
            
            known_players(Known),
            Known \= [],
            random_member(Potential, Known),
            Potential \= "ai_boris2",
            format(Stream, 'resurrect ~w~n', [Potential]), 
            flush_output(Stream),
            format(user_error, 'Bot tries to resurrect: ~w~n', [Potential]),
            sleep(2),
            catch(read_response_simple(Stream, _), _, true)
            
        ;   Action < 70 ->
            random_say_command(SayCmd),
            format(Stream, '~w~n', [SayCmd]), 
            flush_output(Stream),
            format(user_error, 'Bot says: ~w~n', [SayCmd]),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true)
            
        ;   Action < 85 ->
            format(Stream, 'inventory~n', []), 
            flush_output(Stream),
            format(user_error, 'Bot checks inventory~n', []),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true)
            
        ;   format(Stream, 'who~n', []), 
            flush_output(Stream),
            format(user_error, 'Bot checks who is online~n', []),
            sleep(1),
            catch(read_response_simple(Stream, _), _, true)
        ),

        % Пауза между циклами
        random(4, 10, Pause),
        sleep(Pause),

        fail.

% Простое чтение ответа - читаем строки пока не истечёт таймаут или не найдём промпт
read_response_simple(Stream, Response) :-
    read_response_lines(Stream, 20, [], Lines),  % Читаем максимум 20 строк
    atomic_list_concat(Lines, "\n", Response).

read_response_lines(_, 0, Acc, Acc) :- !.  % Достигли лимита строк

read_response_lines(Stream, N, Acc, Result) :-
    % Пытаемся прочитать строку с таймаутом
    catch(
        call_with_time_limit(0.5, read_line_to_string(Stream, Line)),
        _,
        Line = timeout
    ),
    (   Line == timeout ->
        % Таймаут - возвращаем что накопили
        Result = Acc
    ;   Line == end_of_file ->
        % Конец потока
        Result = Acc
    ;   sub_string(Line, _, _, _, ">") ->
        % Нашли промпт - убираем его и возвращаем результат
        (   sub_string(Line, Before, _, _, ">"),
            Before \= "" ->
            append(Acc, [Before], NewAcc),
            Result = NewAcc
        ;   Result = Acc
        )
    ;   % Обычная строка - добавляем и продолжаем
        append(Acc, [Line], NewAcc),
        N1 is N - 1,
        read_response_lines(Stream, N1, NewAcc, Result)
    ).

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

known_players(["player1", "player2", "qqq", "rrr", "yyy", "Andrey", "Vlad"]).