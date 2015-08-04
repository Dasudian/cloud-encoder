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
    {ok, M3u8Bin} = file:read_file(M3u8),
    lib_riakcs:insert(Bucket, M3u8Key, M3u8Bin, [{"Content-Type", "application/x-mpegURL"}], Aws_config),
    {ok, ZipBin} = file:read_file(Zip),
    lib_riakcs:insert(Bucket, Zipkey, ZipBin, [{"Content-Type", "application/zip"}], Aws_config),
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
