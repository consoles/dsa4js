% 创建一个基本的OTP服务器，可以把消息记录到文件中。

-module(t6).
-behavior(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 启动服务器
start() ->
    gen_server:start_link({local, t6}, ?MODULE, [], []).

% 停止服务器
stop() ->
    gen_server:stop(t6).

% 初始化服务器
init([]) ->
    {ok, {}}.

% 处理同步请求
handle_call({log, Message}, _From, State) ->
    log(Message),
    {reply, ok, State}.

% 处理异步请求
handle_cast({log, Message}, State) ->
    log(Message),
    {noreply, State}.

% 处理信息
handle_info(_Info, State) ->
    {noreply, State}.

% 在关闭服务器之前清理
terminate(_Reason, _State) ->
    ok.

% 更改代码时调用
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% 将消息记录到文件中
log(Message) ->
    {ok, File} = file:open("log.txt", [append]),
    io:fwrite(File, "~s~n", [Message]),
    file:close(File).

% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(t6).
% {ok,t6}
% 2> t6:start().
% {ok,<0.87.0>}
% 3> gen_server:call(t6, {log, "Hello World!"}).
% ok
% 4> gen_server:call(t6, {log, "Hello World2!"}).
% ok
% 5> gen_server:call(t6, {log, "Hello World3!"}).
% ok
% 6>
% 观察当前目录下的 log.txt
% gen_server:call(t6, {log, "Hello World!"}). 表示向 t6 进程发送一个同步请求，这个请求包含一个带有 log 原子和一个字符串 "Hello World!" 的元组。
% 在 gen_server OTP 模板中，handle_call/3 函数被用来处理同步请求。在这个例子中，我们在 handle_call/3 函数中定义了 {log, Message} 模式匹配，以获取 log 原子和消息字符串。然后，我们调用了 log(Message) 函数，将消息记录到文件中，并返回一个响应原子 ok 给调用方。

% 发送异步请求
% gen_server:cast(t6, {log, "sdfds"}).
