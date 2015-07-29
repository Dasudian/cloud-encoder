-module(ftp_get_worker).
-behaviour(gen_server).
-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {dispatcher, ftp_info, code, pros, drm, dir}).

start_link(DispatcherPid, FtpInfo, Code, Profiles, WithDrm, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, FtpInfo, Code, Profiles, WithDrm, Dir], []).

init([DispatcherPid, FtpInfo, Code, Profiles, WithDrm, Dir]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
				ftp_info = FtpInfo,
				code = Code,
				pros = Profiles,
				drm = WithDrm,
				dir = Dir}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{ftp_info=FtpInfo, code=Code, pros=Profiles, drm=WithDrm, dir=Dir}) ->
	{SrcFtp, TgtFtp, KeyFtp} = FtpInfo,
	{User, Passwd, Ip, Path} = commander_lib:parse_ftp_addr(SrcFtp),
	[File| _] = lists:reverse(Path),
	RemoteDir = filename:join(Path -- [File]),
	LocalFile = Code++".ts",
	{ok, Pid} = inets:start(ftpc, [{host, Ip}]),
	ftp:user(Pid, User, Passwd),
	ftp:cd(Pid, RemoteDir),
	ftp:lcd(Pid, Dir),
	ftp:recv(Pid, File, LocalFile),
	inets:stop(ftpc, Pid),
	MediaDir = filename:join(Dir, Code),
	os:cmd("mkdir "++MediaDir),
	write_index(MediaDir, Code, commander_lib:select_ts(Profiles)),
    commander_dispatch:ftp_get_complete(S#state.dispatcher, filename:join(Dir, LocalFile), Code, Profiles, WithDrm, {TgtFtp, KeyFtp}),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


write_index(Path, Code, Profiles) ->
	Ratio_Bitrates = extract_ratios(Profiles),
	[write_index(Path, Code, RB, ["#EXTM3U\n"]) || RB <- Ratio_Bitrates].

write_index(Path, Code, {Ratio, []}, Con) ->
	file:write_file(filename:join(Path, Code++"_"++Ratio++".m3u8"), Con);
write_index(Path, Code, {Ratio, [H|T]}, Con) ->
	NewCon = Con++"#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH="++
	H ++ "\n" ++ Code ++ "_" ++ Ratio ++ "_" ++ H ++ ".m3u8\n",
	write_index(Path, Code, {Ratio, T}, NewCon).

extract_ratios(Profiles) ->
	extract_ratios(Profiles, []).
extract_ratios([], Out) ->
	Out;
extract_ratios([H|T], Out) ->
	[Ratio, BitRate] = string:tokens(H, "_"),
	case proplists:lookup(Ratio, Out) of
		none ->
			extract_ratios(T, [{Ratio, [BitRate]}|Out]);
		{Ratio, BRList} ->
			extract_ratios(T, lists:keyreplace(Ratio, 1, Out, {Ratio, [BitRate|BRList]}))
	end.

