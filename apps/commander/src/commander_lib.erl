-module(commander_lib).
-export([lib_dir/0, conf_val/2, priv/0, parse_ftp_addr/1, log/2, select_ts/1]).
-export([bin_to_hexstr/1,hexstr_to_bin/1]).
-include_lib("kernel/include/file.hrl").

-define(TS_FORMATSTR, "GMT ~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ").

lib_dir() ->
	Ebin = filename:dirname(code:which(?MODULE)),
	filename:join(filename:split(Ebin)--["ebin"]).
lib_dir(Dir) ->
	filename:join([lib_dir(), to_list(Dir)]).

conf_val(Key, Default) ->
	case application:get_env(hlsencoder, Key) of
		undefined -> Default;
		{ok, Value} -> Value
	end.

priv() ->
	lib_dir("priv").


parse_ftp_addr(FtpAddr) ->
	[U, P, I | Pa] = re:split(FtpAddr, "[:@/]", [{return, list}]),
	{U, P, I, Pa}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For binary Hex 
hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a+(N-10).

int(C) when $0 =< C, C =< $9 ->
	C - $0;
int(C) when $A =< C, C =< $F ->
	C - $A + 10;
int(C) when $a =< C, C =< $f ->
	C - $a + 10.

to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) -> 
	[];
list_to_hexstr([H|T]) ->
	to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
	list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
	list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
	[int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
	[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list({rsc_list, L}) -> L;
to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F) -> float_to_list(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log(Text, Data) ->
	 {{Y, M, D}, {H, Mm, S}} = calendar:local_time(),
	 io:format(?TS_FORMATSTR ++ " " ++ Text ++ "~n", 
		 [Y, M, D, H, Mm, S] ++ Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
select_ts(Profiles) ->
	select_ts(Profiles, []).
select_ts([], L) ->
	L;
select_ts([H|T], L) ->
	case re:run(H, "^ts_.*") of
		nomatch ->
			select_ts(T, L);
		_Match ->
			select_ts(T, [string:sub_string(H, 4) | L])
	end.
