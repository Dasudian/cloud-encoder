%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2015 上午8:44
%%%-------------------------------------------------------------------
-module(riakcs_put_worker).
-behaviour(gen_server).
-define(SIZE, 1024 * 1024 * 5). %% 注意：最小为5M

-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-record(state, {dispatcher, zip, m3u8, dir, code, config}).

start_link(DispatcherPid, Zip, M3u8File, Dir, Code, Config) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Zip, M3u8File, Dir, Code, Config], []).

init([DispatcherPid, Zip, M3u8File, Dir, Code, Config]) ->
    self() ! start,
    {ok, #state{dispatcher = DispatcherPid,
        zip = Zip,
        m3u8 = M3u8File,
        dir = Dir,
        code = Code,
        config = Config}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{zip = Zip, m3u8 = M3u8, dir = Dir, code = Code, config = Config}) ->
    {{ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT}, Bucket, OldKey} = Config,
    Aws_config = lib_riakcs:riakcs_init(ACCESS_KEY_ID, SECRET_ACCESS_KEY, S3_HOST, S3_PORT),
    ZipName = filename:basename(Zip),
    M3u8Name = filename:basename(M3u8),
    BasePath = filename:rootname(OldKey),
    {Zipkey, M3u8Key} = get_key(ZipName, M3u8Name, BasePath),
%%     {ok, M3u8Bin} = file:read_file(M3u8),
%%     lib_riakcs:insert(Bucket, M3u8Key, M3u8Bin, [{"Content-Type", "application/x-mpegURL"}], Aws_config),
    upload_file(M3u8, "application/x-mpegURL", Bucket, M3u8Key, Aws_config),
%%     {ok, ZipBin} = file:read_file(Zip),
%%     lib_riakcs:insert(Bucket, Zipkey, ZipBin, [{"Content-Type", "application/zip"}], Aws_config),
    upload_file(Zip, "application/zip", Bucket, Zipkey, Aws_config),
    os:cmd("cd " ++ Dir ++ " && rm -rf " ++ Code),
    commander_dispatch:riakcs_put_complete(S#state.dispatcher, Code, Bucket, Zipkey, M3u8Key),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_key(ZipName, M3u8Name, Basepath) ->
    ZipKey = Basepath ++ "/media_source/" ++ ZipName,
    M3u8Key = Basepath ++ "/m3u8_source/" ++ M3u8Name,
    {ZipKey, M3u8Key}.



upload_file(Filepath, FileType, Bucket, Key, Config) ->
    UploadId = lib_riakcs:initiate_upload(Bucket, Key, FileType, Config),
    {ok, IoDevice} = file:open(Filepath, [read, raw, binary]),
    ok = upload(IoDevice, 0, 1, Bucket, Key, UploadId, Config, []),
    ok = file:close(IoDevice).




upload(IoDevice, ReadIndex, UploadIndex, Bucket, Key, Uploadid, Config, EtagList) ->
    case file:pread(IoDevice, ReadIndex * ?SIZE, ?SIZE) of
        {ok, Data} ->
            NewEtagList = lib_riakcs:upload_part(Bucket, Key, Uploadid, UploadIndex, Data, Config, EtagList),
            upload(IoDevice, ReadIndex + 1, UploadIndex + 1, Bucket, Key, Uploadid, Config, NewEtagList);
        eof ->
            lib_riakcs:complete_upload(Bucket, Key, Uploadid, Config, lists:reverse(EtagList)),
            ok
    end.