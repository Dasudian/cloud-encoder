%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2015 上午8:44
%%%-------------------------------------------------------------------
-module(riakcs_put_worker).

%% API

%% API
-behaviour(gen_server).
-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {dispatcher, riakcs_info, code, pros, drm, encryption_key, dir}).

start_link(DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir], []).

init([DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir]) ->
    io:format("~n riakcs ~n"),
    self() ! start,
    {ok, #state{dispatcher = DispatcherPid,
        riakcs_info = RiakcsInfo,
        code = Code,
        pros = Profiles,
        drm = WithDrm,
        encryption_key = Encryption_Key,
        dir = Dir}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{riakcs_info = RiakcsInfo, code = Code, pros = Profiles, drm = WithDrm, encryption_key = Encryption_Key, dir = Dir}) ->
    {{ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT}, Bucket, File_Key} = RiakcsInfo,
    Aws_config = lib_riakcs:riakcs_init(ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT),
    LocalFile = Code ++ ".ts",
    MediaDir = filename:join(Dir, Code),
    os:cmd("mkdir " ++ MediaDir),
    io:format("~nLocalFile:~p~n", [LocalFile]),
    io:format("~nconfig:~p~n", [Aws_config]),
    lib_riakcs:select(Bucket, File_Key, Aws_config),
    io:format("~n download end ~n"),
    write_index(MediaDir, Code, commander_lib:select_ts(Profiles)),
    %%commander_dispatch:ftp_get_complete(S#state.dispatcher, filename:join(Dir, LocalFile), Code, Profiles, WithDrm, {TgtFtp, KeyFtp}),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


write_index(Path, Code, Profiles) ->
    Ratio_Bitrates = extract_ratios(Profiles),
    [write_index(Path, Code, RB, ["#EXTM3U\n"]) || RB <- Ratio_Bitrates].

write_index(Path, Code, {Ratio, []}, Con) ->
    file:write_file(filename:join(Path, Code ++ "_" ++ Ratio ++ ".m3u8"), Con);
write_index(Path, Code, {Ratio, [H | T]}, Con) ->
    NewCon = Con ++ "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=" ++
        H ++ "\n" ++ Code ++ "_" ++ Ratio ++ "_" ++ H ++ ".m3u8\n",
    write_index(Path, Code, {Ratio, T}, NewCon).

extract_ratios(Profiles) ->
    extract_ratios(Profiles, []).
extract_ratios([], Out) ->
    Out;
extract_ratios([H | T], Out) ->
    [Ratio, BitRate] = string:tokens(H, "_"),
    case proplists:lookup(Ratio, Out) of
        none ->
            extract_ratios(T, [{Ratio, [BitRate]} | Out]);
        {Ratio, BRList} ->
            extract_ratios(T, lists:keyreplace(Ratio, 1, Out, {Ratio, [BitRate | BRList]}))
    end.


