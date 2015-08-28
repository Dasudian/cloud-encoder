%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2015 上午8:43
%%%-------------------------------------------------------------------
-module(riakcs_get_worker).

%% API
-behaviour(gen_server).
-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).



-record(state, {dispatcher, riakcs_info, code, pros, drm, encryption_key, dir}).



-type(config() :: {{string(), string(), inet:ip4_address(), inet:port_number()}, string(), string()}).
-export_type([config/0]).


start_link(DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir], []).

init([DispatcherPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir]) ->
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
    MediaDir = filename:join(Dir, Code),
    EncodeDir = filename:join(MediaDir, Code),
    file:set_cwd(Dir),
    os:cmd("mkdir " ++ MediaDir),
    os:cmd("mkdir " ++ EncodeDir),
    Props = lib_riakcs:select(Bucket, File_Key, Aws_config),
    M3u8File = write_index(MediaDir, Code, Profiles),
    case string:tokens(proplists:get_value(content_type, Props), "/") of
        [] ->
            Filename = filename:join(MediaDir, Code);
        [_, VideoType] ->
            Filename = filename:join(MediaDir, Code ++ "." ++ VideoType)
    end,
    file:write_file(Filename, proplists:get_value(content, Props)),
    commander_dispatch:riakcs_get_complete(S#state.dispatcher, Filename, Code, Profiles, WithDrm, Encryption_Key, RiakcsInfo, M3u8File),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


write_index(Path, Code, Profiles) ->
    write_index(Path, Code, Profiles, ["#EXTM3U\n"]).

write_index(Path, Code, [], Con) ->
    M3u8File = filename:join(Path, Code ++ ".m3u8"),
    ok = file:write_file(M3u8File, Con),
    M3u8File;
write_index(Path, Code, [H | T], Con) ->
    New_H = lib_util:to_list(H),
    New_code = lib_util:to_list(Code),
    NewCon = Con ++ "#EXT-X-STREAM-INF:PROGRAM-ID = 1, BANDWIDTH = " ++
        New_H ++ "\n" ++ New_code ++ "/" ++ New_H ++ "/" ++ New_code ++ "_" ++ New_H ++ ".m3u8\n",
    write_index(Path, Code, T, NewCon).

