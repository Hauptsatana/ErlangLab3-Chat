%% Задание - написать приложение-чат

%% Пользователи хранятся в списке

%% Основной процесс сервера - loop(Users) ->
%% Подключение пользователя к серверу - connect(User, Server)
%% Отключение пользователя от сервера - disconnect(User, Server)
%% Отправка сообщения - send(Msg, User, Server)

%% Принятые сообщения сервер распечатывает, он также оповещает о подключении/отключении пользователя

%% Домой:
%% * Каждый клиент запущен в самомстоятельном процессе
%% * Процесс сервера необходимо зарегистрировать с именем chat_server

-module(chat).
-export([loop/1, connect/2, disconnect/2, send/3]).

loop(Users) ->
    receive
        {connect, User} -> 
            case lists:member(User, Users) of 
                true -> 
                    io:format("User with name \"~s\" already exists~n", [User]),
                    loop(Users);
                false ->
                    io:format("!! ~s connected~n", [User]),
                    loop([User | Users])
             end;
        {disconnect, User} ->
            case lists:member(User, Users) of
                true ->
                    io:format("!! ~s diconnected~n", [User]),
                    loop(lists:delete(User, Users));
                false ->
                    io:format("No user with name \"~s\"~n", [User]),
                    loop(Users)
             end;
        {send, User, Message} ->
            case lists:member(User, Users) of
                true -> io:format("~s: ~s~n", [User, Message]);
                false -> io:format("No user with name \"~s\"~n", [User])
             end,
             loop(Users)
    end.
    
connect(User, Server) ->
    Server ! {connect, User}.
    
disconnect(User, Server) ->
    Server ! {disconnect, User}.
    
send(User, Server, Message) ->
    Server ! {send, User, Message}.
      
       