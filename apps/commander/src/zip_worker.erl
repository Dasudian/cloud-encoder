-module(zip_worker).
-behaviour(gen_server).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {dispatcher, path, code, ftp, dir}).

start_link(DispatcherPid, Path, Code, TgtFtp, Dir) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Path, Code, TgtFtp, Dir], []).

init([DispatcherPid, Path, Code, TgtFtp, Dir]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
                path = Path,
				code = Code,
				ftp = TgtFtp,
				dir = Dir}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{path=_Path, code=Code, ftp=TgtFtp, dir=Dir}) ->
	Filename = Code++".zip",
	file:set_cwd(Dir),
	{ok, ZipFile} = zip:zip(Filename, [Code], [{cwd, Dir}]),
	commander_dispatch:zip_complete(S#state.dispatcher, filename:join(Dir,ZipFile), Code, TgtFtp),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
