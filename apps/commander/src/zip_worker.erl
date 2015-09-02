-module(zip_worker).
-behaviour(gen_server).
-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%%-record(state, {dispatcher, path, code, ftp, dir}).
-record(state, {dispatcher, path, code, dir, config, m3u8File}).

%%start_link(DispatcherPid, Path, Code, TgtFtp, Dir) ->
%%    gen_server:start_link(?MODULE, [DispatcherPid, Path, Code, TgtFtp, Dir], []).


start_link(DispatcherPid, Path, Code, Dir, Config, M3u8File) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Path, Code, Dir, Config, M3u8File], []).


%%init([DispatcherPid, Path, Code, TgtFtp, Dir]) ->
%%    self() ! start,
%%    {ok, #state{dispatcher = DispatcherPid,
%%        path = Path,
%%        code = Code,
%%        ftp = TgtFtp,
%%        dir = Dir}}.

init([DispatcherPid, Path, Code, Dir, Config, M3u8File]) ->
    self() ! start,
    {ok, #state{dispatcher = DispatcherPid,
        config = Config,
        m3u8File = M3u8File,
        path = Path,
        code = Code,
        dir = Dir}}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{config = Config, path = _Path, code = Code, dir = Dir, m3u8File = M3u8File}) ->
    Filename = Code ++ ".zip",
    Cwd = Dir ++ "/" ++ Code,
    {ok, ZipFile} = zip:zip(filename:join(Cwd, Filename), [Code], [{cwd, Cwd}]),
    commander_dispatch:zip_complete(S#state.dispatcher, ZipFile, M3u8File, Code, Config),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
