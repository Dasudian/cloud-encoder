-module(commander_srv).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/0]).

-record(state, {channel, exchange, routing}).

% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([]) ->
%	Payload = <<48,"cmsmedia:0ttbesTV@118.97.96.4/20120612/601093.ts",38,"cmsmedia:0ttbesTV@118.97.96.4/20120612",
%	38,"cmsmedia:0ttbesTV@118.97.96.4/20120612",8,"bc000000",0,1>>,
%	{FtpInfo, Code, Profiles, WithDrm} = decode(Payload),
%	commander_dispatch:encode(self(), FtpInfo, Code, Profiles, WithDrm),
%
%	{ok, "OK"}. %debug
	{ok, MqHost} = application:get_env(mq_host),
	{ok, MqUser} = application:get_env(mq_user),
	{ok, MqPass} = application:get_env(mq_pass),
	{ok, MqExchange} = application:get_env(mq_exchange),
	{ok, MqQueue} = application:get_env(mq_queue),
	{ok, MqRoute} = application:get_env(mq_routing_key),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host=MqHost,
		   	username= list_to_binary(MqUser), password= list_to_binary(MqPass),
			auth_mechanisms=[fun amqp_auth_mechanisms:amqplain/3]}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	%%Exchange declare
    ExDeclare = #'exchange.declare'{exchange = list_to_binary(MqExchange), type = <<"topic">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExDeclare),
	%%Declare a queue
	DeclareQ_Sub = #'queue.declare'{queue = list_to_binary(MqQueue), durable=true},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, DeclareQ_Sub),

	DeclareQ_Pub = #'queue.declare'{queue = list_to_binary(MqQueue++"_finish"), durable=true},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, DeclareQ_Pub),

	BindingSub = #'queue.bind'{queue = list_to_binary(MqQueue), 
		exchange    = list_to_binary(MqExchange),
		routing_key = list_to_binary(MqRoute)},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, BindingSub),

	BindingPub = #'queue.bind'{queue = list_to_binary(MqQueue++"_finish"), 
		exchange    = list_to_binary(MqExchange),
		routing_key = list_to_binary(MqRoute++".finish")},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, BindingPub),
	%%Subscribe to encoding messages
	Sub = #'basic.consume'{queue = list_to_binary(MqQueue)},
	#'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:call(Channel, Sub),
	{ok, #state{channel=Channel, exchange=MqExchange, routing=MqRoute}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

%Receive subscribed messages
handle_info(#'basic.consume_ok'{}, State) ->
	{noreply, State};
handle_info(#'basic.cancel_ok'{}, _State) ->
	{stop, "subscription cancel"};
handle_info({#'basic.deliver'{delivery_tag = Tag}, Content}, State = #state{channel=Channel}) ->
	#amqp_msg{payload = Payload} = Content,
	{FtpInfo, Code, Profiles, WithDrm} = decode(Payload),
	%debug
	io:format("FTP: ~p~n Code: ~p~n Profiles: ~p~n DRM: ~p~n", [FtpInfo, Code, Profiles, WithDrm]),
	commander_dispatch:encode(self(), FtpInfo, Code, Profiles, WithDrm),
	amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	{noreply, State};
%infomation from dispatch for finishing the encoding work with msg: {finish, Code}
handle_info({finish, Code}, State = #state{channel=Channel, exchange=Exc, routing=Route}) -> 
	Publish = #'basic.publish'{exchange = list_to_binary(Exc), routing_key = list_to_binary(Route++".finish")},
	Props = #'P_basic'{delivery_mode = 2}, %%persistent message
	amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = list_to_binary(Code)}),
	commander_lib:log("~n~n~~~~~~~~~~Video: ~p Finish !!! ~~~~~~~~~~~n~n",[Code]),
	{noreply, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================
% |1byte|SourceFTP|1byte|TargetFTP|1byte|KeyFTP|1byte|Code|1byte|Profiles|Withdrm|
decode(Payload) ->
	<<SrcLengh:3/binary, L1/binary>> = Payload,
	{SLL, _} = string:to_integer(binary_to_list(SrcLengh)),
	<<SrcFtp:SLL/binary, TgtLength:3/binary, L2/binary>> = L1,
	{TLL, _} = string:to_integer(binary_to_list(TgtLength)),
	<<TgtFtp:TLL/binary, KeyLength:3/binary, L3/binary>> = L2,
	{KLL, _} = string:to_integer(binary_to_list(KeyLength)),
	<<KeyFtp:KLL/binary, CodeLength:2/binary, L4/binary>> = L3,
	{CLL, _} = string:to_integer(binary_to_list(CodeLength)),
	<<Code:CLL/binary, ProLength:3/binary, L5/binary>> = L4,
	{ProLL, _} = string:to_integer(binary_to_list(ProLength)),
	<<Pros:ProLL/binary, WithDrm/binary>> = L5,
	{{binary_to_list(SrcFtp), binary_to_list(TgtFtp), binary_to_list(KeyFtp)},
		binary_to_list(Code),
		decode_pros(binary_to_list(Pros)), binary_to_list(WithDrm)}.

% "16t9_150k+16t9_240k+...."
decode_pros(Pros) ->
	string:tokens(Pros, "+").
