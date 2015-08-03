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
    os:cmd("mkdir " ++ MediaDir),
    Props = lib_riakcs:select(Bucket, File_Key, Aws_config),
    ["video", VideoType] = string:tokens(proplists:get_value(content_type, Props), "/"),
    write_index(MediaDir, Code, commander_lib:select_ts(Profiles)),
    Filename = filename:join(MediaDir, Code ++ "." ++ VideoType),
    file:write_file(Filename, proplists:get_value(content, Props)),
    commander_dispatch:riakcs_get_complete(S#state.dispatcher, RiakcsInfo, Filename, Code, Profiles, WithDrm, Encryption_Key),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


write_index(Path, Code, Profiles) ->
    [write_index(Path, Code, RB, ["#EXTM3U\n"]) || RB <- Profiles].

write_index(Path, Code, Profile, Con) ->
    file:write_file(filename:join(Path, Code ++ "_" ++ Profile ++ ".m3u8"), Con).

