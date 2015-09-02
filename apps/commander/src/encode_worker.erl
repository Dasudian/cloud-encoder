-module(encode_worker).
-behaviour(gen_server).
%%-export([start_link/8]).
-export([start_link/10]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%-record(state, {dispatcher, file, code, profile, pros, drm, ftp, dir}).
-record(state, {dispatcher, file, code, profile, pros, drm, encryption_key, config, m3u8File, dir}).

%%start_link(DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir) ->
%%    gen_server:start_link(?MODULE, [DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir], []).

start_link(DispatcherPid, FileDir, Code, Profile, Profiles, WithDrm, Encryption_Key, Config, M3u8File, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, FileDir, Code, Profile, Profiles, WithDrm, Encryption_Key, Config, M3u8File, Dir], []).

%init([DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir]) ->
%    self() ! start,
%    {ok, #state{dispatcher = DispatcherPid,
%        file = File,
%        code = Code,
%        profile = Profile,
%        pros = Profiles,
%        drm = WithDrm,
%        ftp = TgtFtp,
%        dir = Dir}}.


init([DispatcherPid, FileDir, Code, Profile, Profiles, WithDrm, Encryption_Key, Config, M3u8File, Dir]) ->
    self() ! start,
    {ok, #state{dispatcher = DispatcherPid,
        config = Config,
        m3u8File = M3u8File,
        file = FileDir,
        code = Code,
        profile = Profile,
        pros = Profiles,
        drm = WithDrm,
        encryption_key = Encryption_Key,
        dir = Dir}}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(start, S = #state{dispatcher = Dispatcher, config = Config, m3u8File = M3u8File, file = File, code = Code, profile = Profile, pros = Profiles, drm = WithDrm, encryption_key = Encryption_key, dir = Dir}) ->
    EnCmd = filename:join(commander_lib:priv(), Profile), %eg. 720000
    Path = filename:join(Dir, Code ++ "/" ++ Code ++ "/" ++ Profile),
    file:make_dir(Path),
    case filelib:is_file(Path) of
        true ->
            ok;
        false ->
            file:make_dir(Path)
    end,
    os:cmd("cd " ++ Path ++ " && " ++ EnCmd ++ " " ++ File ++ " " ++ Code),
    commander_dispatch:encode_complete(Dispatcher, Path, Code, Profile, Profiles, WithDrm, Encryption_key, Config, M3u8File),
    {stop, normal, S}.


%handle_info(start, S = #state{file = File, code = Code, profile = Profile, pros = Profiles, drm = WithDrm, ftp = TgtFtp, dir = Dir}) ->
%    EnCmd = filename:join(commander_lib:priv(), Profile), %eg. ts_16t9_150k
%    Path = filename:join(Dir, Code),
%    case filelib:is_file(Path) of
%        true ->
%            ok;
%        false ->
%            os:cmd("mkdir " ++ Path)
%    end,
%    os:cmd("cd " ++ Path ++ " && " ++ EnCmd ++ " " ++ File ++ " " ++ Code),
%    commander_dispatch:encode_complete(S#state.dispatcher, Path, Code, Profile, Profiles, WithDrm, TgtFtp),
%    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


