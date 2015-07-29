-module(encode_worker).
-behaviour(gen_server).
-export([start_link/8]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {dispatcher, file, code, profile, pros, drm, ftp, dir}).

start_link(DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir], []).

init([DispatcherPid, File, Code, Profile, Profiles, WithDrm, TgtFtp, Dir]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
                file = File,
				code = Code,
				profile = Profile,
				pros = Profiles,
				drm = WithDrm,
				ftp = TgtFtp,
				dir = Dir}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{file=File, code=Code, profile=Profile, pros=Profiles, drm=WithDrm, ftp=TgtFtp, dir=Dir}) ->
	EnCmd = filename:join(commander_lib:priv(), Profile), %eg. ts_16t9_150k
	Path = filename:join(Dir, Code),
	case filelib:is_file(Path) of 
		true ->
			ok;
		false ->
			os:cmd("mkdir "++Path)
	end,
	os:cmd("cd "++Path++" && "++EnCmd++" "++File++" "++Code),
    commander_dispatch:encode_complete(S#state.dispatcher, Path, Code, Profile, Profiles, WithDrm, TgtFtp),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


