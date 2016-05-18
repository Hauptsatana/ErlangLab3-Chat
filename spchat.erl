%% Задание - написать приложение-чат

%% Пользователи хранятся в списке

%% Основной процесс сервера - loop(Users) ->
%% Подключение пользователя к серверу - connect(User, Server)
%% Отключение пользователя от сервера - disconnect(User, Server)
%% Отправка сообщения - send(Msg, User, Server)

%% Принятые сообщения сервер распечатывает, он также оповещает о подключении/отключении пользователя

%% Домой:
%% * Каждый клиент запущен в самостоятельном процессе
%% * Процесс сервера необходимо зарегистрировать с именем chat_server

-module(spchat).
-export([start_server/0, loop_server/1, start_client/1, loop_client/1, register/2, disconnect/1, send/2]).

start_server() ->
    ServerPID = spawn(?MODULE, loop_server, [maps:new()]),
    ServerPID.

loop_server(Users) ->
    receive
        {register, UserPID, UserName} ->
            case lists:member(UserName, maps:values(Users)) of 
                true ->
                    UserPID ! {name_already_exists},
                    loop_server(Users);
                false ->
                    UserPID ! {ok},
                    io:format("!! \"~s\" connected~n", [UserName]),
                    loop_server(maps:put(UserPID, UserName, Users))
             end;
        {disconnect, UserPID} ->
            case maps:is_key(UserPID, Users) of
                true ->
                    io:format("!! \"~s\" disconnected~n", [maps:get(UserPID, Users)]),
                    loop_server(maps:remove(UserPID, Users));
                false ->
                    io:format("No user with pid ~p~n", [UserPID]),
                    loop_server(Users)
             end;
        {send, UserPID, Message} ->
            case maps:is_key(UserPID, Users) of
                true -> io:format("~s: ~s~n", [maps:get(UserPID, Users), Message]);
                false -> io:format("No user with pid ~p~n", [UserPID])
             end,
             loop_server(Users)
    end.

start_client(ServerPID) ->
    UserPID = spawn(?MODULE, loop_client, [ServerPID]),
    UserPID.

loop_client(ServerPID) ->
    receive
        {register, UserName} ->
            ServerPID ! {register, self(), UserName},
            receive 
                {ok} -> io:format("Your Username is \"~s\" since now.~n", [UserName]);
                {name_already_exists} -> io:format("Username \"~s\" is already taken", [UserName])
            end,
            loop_client(ServerPID);
        {send, Msg} -> 
            ServerPID ! {send, self(), Msg},
            loop_client(ServerPID);
        {disconnect} ->
            ServerPID ! {disconnect, self()}
    end.

register(UserPID, UserName) ->
    UserPID ! {register, UserName}.
    
disconnect(UserPID) ->
    UserPID ! {disconnect}.
    
send(UserPID, Message) ->
    UserPID ! {send, Message}.
      
       