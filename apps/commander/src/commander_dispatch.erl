-module(commander_dispatch).
-behaviour(gen_fsm).
-export([start_link/0,
    encode/5,
    encode/6,
    ftp_get_complete/6,
    riakcs_get_complete/8,
    encode_complete/7,
    encode_complete/9,
    encrypt_complete/4,
    zip_complete/5,
    ftp_put_complete/2,
    riakcs_put_complete/5]).
-export([init/1, dispatching/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(ENC_POOL, encode_pool).
-define(FTP_GET_POOL, ftp_get_pool).
-define(FTP_PUT_POOL, ftp_put_pool).
-define(RIAKCS_GET_POOL, riakcs_get_pool).
-define(RIAKCS_PUT_POOL, riakcs_put_pool).
-define(ZIP_POOL, zip_pool).
-define(CRYPT_POOL, crypt_pool).
%-define(PROFILES, ["ts_16t9_158000", "ts_16t9_248000", "ts_16t9_448000", "ts_4t3_648000", "ts_4t3_848000", "ts_4t3_1248000"]).
-define(PROFILES, ["ts_16t9_158000", "ts_16t9_248000", "ts_16t9_448000", "ts_16t9_648000", "ts_16t9_848000", "ts_16t9_1248000",
    "ts_4t3_158000", "ts_4t3_248000", "ts_4t3_448000", "ts_4t3_648000", "ts_4t3_848000", "ts_4t3_1248000"]).
-define(TOTAL_PROFILE, 12).

-record(data, {work_dir, refs, srv_pid}).

%%% PUBLIC API
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

encode(SrvPid, FtpInfo, Code, Profile, WithDrm) ->  %%{SrcFtp, TargetFtp, KeyFtp}}, The code of media, WithDrm - 0/1
    Profiles =
        case Profile of
            [] ->
                ?PROFILES;
            Profile ->
                Profile
        end,
    gen_fsm:send_event(?MODULE, {encode, SrvPid, FtpInfo, Code, Profiles, WithDrm}).

encode(SrvPid, RiakcsInfo, Code, Profile, WithDrm, Encryption_Key) ->
    Profiles =
        case Profile of
            [] ->
                ?PROFILES;
            Profile ->
                Profile
        end,
    gen_fsm:send_event(?MODULE, {encode, SrvPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key}).


ftp_get_complete(Pid, Path, Code, Profiles, WithDrm, TgtFtp) ->
    gen_fsm:send_all_state_event(Pid, {ftp_get_complete, Path, Code, Profiles, WithDrm, TgtFtp}).

ftp_put_complete(Pid, Code) ->
    gen_fsm:send_all_state_event(Pid, {ftp_put_complete, Code}).

-spec riakcs_get_complete(pid(), string(), string(), [string()], boolean(), string(), riakcs_get_worker:config(), string()) -> ok.
riakcs_get_complete(DispatcherPid, FileDir, Code, Profiles, WithDrm, Encryption_Key, Config, M3u8File) ->
    gen_fsm:send_all_state_event(DispatcherPid, {riakcs_get_complete, FileDir, Code, Profiles, WithDrm, Encryption_Key, Config, M3u8File}).


riakcs_put_complete(DispatcherPid, Code, Bucket, Zipkey, M3u8Key) ->
    gen_fsm:send_all_state_event(DispatcherPid, {riakcs_put_complete, Code, Bucket, Zipkey, M3u8Key}).

encode_complete(Pid, Path, Code, Profile, Profiles, WithDrm, TgtFtp) ->
    gen_fsm:send_all_state_event(Pid, {encode_complete, Path, Code, Profile, Profiles, WithDrm, TgtFtp}).

encode_complete(Pid, Path, Code, Profile, Profiles, WithDrm, Encryption_key, Config, M3u8File) ->
    gen_fsm:send_all_state_event(Pid, {encode_complete, Path, Code, Profile, Profiles, WithDrm, Encryption_key, Config, M3u8File}).

encrypt_complete(Pid, Path, Code, TgtFtp) ->
    gen_fsm:send_all_state_event(Pid, {encrypt_complete, Path, Code, TgtFtp}).

%zip_complete(Pid, Zip, Code, TgtFtp) ->
%    gen_fsm:send_all_state_event(Pid, {zip_complete, Zip, Code, TgtFtp}).

zip_complete(Pid, Zip, M3u8File, Code, Config) ->
    gen_fsm:send_all_state_event(Pid, {zip_complete, Zip, M3u8File, Code, Config}).

%%% GEN_FSM
%% Two states: dispatching and listening
init([]) ->
    %% Move the get_env stuff to the supervisor's init.
    {ok, Dir} = application:get_env(work_dir),
    {ok, MaxQueue} = application:get_env(max_queue),
    ecpool:start_pool(?ENC_POOL, MaxQueue, {encode_worker, start_link, []}),
    %%ecpool:start_pool(?FTP_GET_POOL, MaxQueue, {ftp_get_worker, start_link, []}),
    %%ecpool:start_pool(?FTP_PUT_POOL, MaxQueue, {ftp_put_worker, start_link, []}),
    ecpool:start_pool(?RIAKCS_GET_POOL, MaxQueue, {riakcs_get_worker, start_link, []}),
    ecpool:start_pool(?RIAKCS_PUT_POOL, MaxQueue, {riakcs_put_worker, start_link, []}),
    ecpool:start_pool(?ZIP_POOL, MaxQueue, {zip_worker, start_link, []}),
    ecpool:start_pool(?CRYPT_POOL, MaxQueue, {crypt_worker, start_link, []}),
    {ok, dispatching, #data{work_dir = Dir, refs = []}}.

dispatching({encode, SrvPid, FtpInfo, Code, Profiles, WithDrm}, Data = #data{work_dir = Dir, refs = Refs}) ->
    ecpool:async_queue(?FTP_GET_POOL, [self(), FtpInfo, Code, Profiles, WithDrm, Dir]),
    {next_state, dispatching, Data#data{refs = [{Code, 0} | Refs], srv_pid = SrvPid}};
dispatching({encode, SrvPid, RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key}, Data = #data{work_dir = Dir, refs = Refs}) ->
    ecpool:async_queue(?RIAKCS_GET_POOL, [self(), RiakcsInfo, Code, Profiles, WithDrm, Encryption_Key, Dir]),
    {next_state, dispatching, Data#data{refs = [{Code, 0} | Refs], srv_pid = SrvPid}}.

handle_event({ftp_get_complete, Path, Code, Profiles, WithDrm, TgtFtp}, State, Data = #data{work_dir = Dir}) ->
    commander_lib:log("FTP get for Media: ~p finished at ~p", [Code, Path]),
    [ecpool:async_queue(?ENC_POOL, [self(), Path, Code, Profile, Profiles, WithDrm, TgtFtp, Dir]) || Profile <- Profiles],
    {next_state, State, Data};

handle_event({riakcs_get_complete, FileDir, Code, Profiles, WithDrm, Encryption_Key, Config, M3u8File}, State, Data = #data{work_dir = Dir}) ->
    commander_lib:log("riakcs get for Media: ~p finished at ~p", [Code, FileDir]),
    [ecpool:async_queue(?ENC_POOL, [self(), FileDir, Code, Profile, Profiles, WithDrm, Encryption_Key, Config, M3u8File, Dir]) || Profile <- Profiles],
    {next_state, State, Data};

handle_event({encode_complete, Path, Code, Profile, Profiles, WithDrm, TgtFtp}, State, Data = #data{work_dir = Dir, refs = Refs}) ->
    commander_lib:log("Encoding for Media: ~p finished at ~p with Profile ~p", [Code, Path, Profile]),
    {Code, Counts} = proplists:lookup(Code, Refs),
    NewRefs =
        if
            Counts + 1 < length(Profiles) ->
                lists:keyreplace(Code, 1, Refs, {Code, Counts + 1});
            true ->
                case WithDrm of
                    "0" ->
                        ecpool:async_queue(?ZIP_POOL, [self(), Path, Code, TgtFtp, Dir]);
                    "1" ->
                        ecpool:async_queue(?CRYPT_POOL, [self(), Path, Code, Profiles, TgtFtp])
                end,
                Refs -- [{Code, length(Profiles) - 1}]
        end,
    {next_state, State, Data#data{refs = NewRefs}};

handle_event({encode_complete, Path, Code, Profile, Profiles, WithDrm, Encryption_key, Config, M3u8File}, State, Data = #data{work_dir = Dir, refs = Refs}) ->
    commander_lib:log("Encoding for Media: ~p finished at ~p with Profile ~p", [Code, Path, Profile]),
    {Code, Counts} = proplists:lookup(Code, Refs),
    NewRefs =
        if
            Counts + 1 < length(Profiles) ->
                lists:keyreplace(Code, 1, Refs, {Code, Counts + 1});
            true ->
                case WithDrm of
                    false ->
                        ecpool:async_queue(?ZIP_POOL, [self(), Path, Code, Dir, Config, M3u8File]);
                    true ->
                        ecpool:async_queue(?CRYPT_POOL, [self(), Path, Code, Profiles, Encryption_key])
                end,
                Refs -- [{Code, length(Profiles) - 1}]
        end,
    {next_state, State, Data#data{refs = NewRefs}};


handle_event({encrypt_complete, Path, Code, TgtFtp}, State, Data = #data{work_dir = Dir}) ->
    commander_lib:log("Encryption for Media: ~p finished at ~p", [Code, Path]),
    ecpool:async_queue(?ZIP_POOL, [self(), Path, Code, TgtFtp, Dir]),
    {next_state, State, Data};


handle_event({zip_complete, Zip, Code, TgtFtp}, State, Data = #data{work_dir = Dir}) ->
    commander_lib:log("Zipping for Media: ~p finished at ~p", [Code, Zip]),
    ecpool:async_queue(?FTP_PUT_POOL, [self(), Zip, Dir, Code, TgtFtp]),
    {next_state, State, Data};


handle_event({zip_complete, Zip, M3u8File, Code, Config}, State, Data = #data{work_dir = Dir}) ->
    commander_lib:log("Zipping for Media: ~p finished zip at :~p  m3u8 file at:~p", [Code, Zip, M3u8File]),
    ecpool:async_queue(?RIAKCS_PUT_POOL, [self(), Zip, M3u8File, Dir, Code, Config]),
    {next_state, State, Data};

handle_event({ftp_put_complete, Code}, State, Data = #data{srv_pid = SrvPid}) ->
    commander_lib:log("All work done for the Media: ~p", [Code]),
    notify(SrvPid, {finish, Code}),
    {next_state, State, Data};



handle_event({riakcs_put_complete, Code, Bucket, Zipkey, M3u8Key}, State, Data = #data{srv_pid = SrvPid}) ->
    commander_lib:log("All work done for the Media: ~p", [Code]),
    notify(SrvPid, {finish, Code, Bucket, Zipkey, M3u8Key}),
    {next_state, State, Data}.


handle_sync_event(Event, _From, State, Data) ->
    commander_lib:log("Unexpected event: ~p~n", [Event]),
    {next_state, State, Data}.

handle_info(Event, State, Data) ->
    commander_lib:log("Unexpected info: ~p~n", [Event]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% PRIVATE 
notify(Pid, Msg) ->
    Pid ! Msg.
