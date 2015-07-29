-module(crypt_worker).
-behaviour(gen_server).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {dispatcher, path, code, profile, ftp_info}).

start_link(DispatcherPid, Path, Code, Profiles, FtpInfo) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Path, Code, Profiles, FtpInfo], []).

init([DispatcherPid, Path, Code, Profile, FtpInfo]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
				path = Path,
				code = Code,
				profile = Profile,
				ftp_info = FtpInfo}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{path=Path, code=Code, profile=Profiles, ftp_info=FtpInfo}) ->
	{Key, _IV} = encrypt(Path),
	KeyFile = gen_key_file(Path, Code, Key),
	{TgtFtp, KeyFtp} = FtpInfo,
	ftp_key_file(Path, KeyFtp, KeyFile),
	write_m3u8(Path, Code, commander_lib:select_ts(Profiles)),
	commander_dispatch:encrypt_complete(S#state.dispatcher, Path, Code, TgtFtp),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encrypt(Dir) ->
	Key = crypto:rand_bytes(16),
	IV = crypto:rand_bytes(16),
	KeyStr = commander_lib:bin_to_hexstr(Key),
	IVStr = commander_lib:bin_to_hexstr(IV),
	encrypt_ts(Dir, KeyStr, IVStr),
	{Key, IV}.

encrypt_ts(Dir, Key, IV) ->
    case file:list_dir(Dir) of
        {ok, []} ->
			empty;
        {ok, Files} ->
			Cmd = filename:join(commander_lib:priv(), "encrypt"),
			encrypt_many(Dir, Key, IV, Files, Cmd)
    end.
%% Pops an item from the queue and runs it.
encrypt_many(_Dir, _Key, _IV, [], _Cmd) ->
	ok;
encrypt_many(Dir, Key, IV, [H|T], Cmd) ->
	case filename:extension(H) of
		".ts" ->
			os:cmd("cd "++Dir++" && "++Cmd++" "++H++" "++Key++" "++IV);
		_ ->
			notts
	end,
	encrypt_many(Dir, Key, IV, T, Cmd).

gen_key_file(Path, Code, Key) ->
	Filename = Code ++ ".key",
	Name = filename:join(Path, Filename),
	file:write_file(Name, Key),
	Filename.

ftp_key_file(Dir, FtpAddr, KeyFile) ->
	{User, Passwd, Ip, Path} = commander_lib:parse_ftp_addr(FtpAddr),
	{ok, Pid} = inets:start(ftpc, [{host, Ip}]),
	ftp:user(Pid, User, Passwd),
	ftp:cd(Pid, Path),
	ftp:lcd(Pid, Dir),
	ftp:send(Pid, KeyFile),
	inets:stop(ftpc, Pid),
	os:cmd("rm -f "++filename:join(Dir, KeyFile)).

write_m3u8(Dir, Code, Profiles) ->
	[write_m3u8_file(Dir, Code, Pro) || Pro <- Profiles].

write_m3u8_file(Dir, Code, Profile) ->
	{ok, Original} = file:read_file(filename:join(Dir, Code++"_"++Profile++".m3u8")),
	<<Head:33/binary, End/binary>> = Original,
	New = binary_to_list(Head) ++
	"#EXT-X-KEY:METHOD=AES-128,URI=\""++Code++".key\""++
	"\n"++binary_to_list(End),
	file:write_file(filename:join(Dir, Code++"_"++Profile++"_cryto.m3u8"), New),
	os:cmd("cd "++Dir++" && mv -f "++Code++"_"++Profile++"_cryto.m3u8 "++Code++"_"++Profile++".m3u8").
