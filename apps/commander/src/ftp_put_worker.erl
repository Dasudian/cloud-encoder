-module(ftp_put_worker).
-behaviour(gen_server).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {dispatcher, file, dir, code, ftp_info}).

start_link(DispatcherPid, File, Dir, Code, FtpInfo) ->
    gen_server:start_link(?MODULE, [DispatcherPid, File, Dir, Code, FtpInfo], []).

init([DispatcherPid, File, Dir, Code, FtpInfo]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
				file = File,
				dir = Dir,
				code = Code,
				ftp_info = FtpInfo}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{file=File, dir=Dir, code=Code, ftp_info=FtpInfo}) ->
	{User, Passwd, Ip, Path} = commander_lib:parse_ftp_addr(FtpInfo),
	[LocalFile|_] = lists:reverse(string:tokens(File, "/")),
	{ok, Pid} = inets:start(ftpc, [{host, Ip}]),
	ftp:user(Pid, User, Passwd),
	ftp:cd(Pid, Path),
	ftp:lcd(Pid, Dir), 
	ftp:send(Pid, LocalFile),
	inets:stop(ftpc, Pid),
	os:cmd("cd "++Dir++" && rm -rf "++Code++" "++LocalFile++" "++Code++".ts"),
    commander_dispatch:ftp_put_complete(S#state.dispatcher, Code),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
