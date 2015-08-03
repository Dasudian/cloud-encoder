%% @author Barco You <barco@dasudian.com>
%% @copyright 2015 Dasudian
%%
-module(lib_util).

-define(EPOCH_DIFF, 62167219200).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([utc_now/0, utc/1, epoch_utc_now/0, epoch_utc/1, seconds_now/0]).
-export([unique_key/0, hash_to_string/1]).
-export([to_atom/1, to_binary/1, to_integer/1, to_list/1]).
-export([proplists_delete/2]).
-export([md5_hex/1]).
-export([re_email/1]).
%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

utc_now() ->
    utc(erlang:now()).

utc(Now = {_, _, Micro}) ->
    {{Y, M, D}, {H, MM, S}} = calendar:now_to_universal_time(Now),
    iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0wZ", [Y, M, D, H, MM, S, trunc(Micro / 1000)])).

epoch_utc_now() ->
    erlang:now().

epoch_utc(Now) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)) - ?EPOCH_DIFF.

seconds_now() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

to_atom(X) when is_list(X) ->
    list_to_atom(X);
to_atom(X) when is_float(X) ->
    to_atom(float_to_list(X));
to_atom(X) when is_binary(X) ->
    to_atom(binary_to_list(X));
to_atom(X) when is_integer(X) ->
    to_atom(integer_to_list(X));
to_atom(X) when is_atom(X) ->
    X.

to_binary(X) when is_atom(X) ->
    to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) ->
    to_binary(integer_to_list(X));
to_binary(X) when is_float(X) ->
    float_to_binary(X);
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.

to_integer(X) when is_atom(X) ->
    to_integer(atom_to_list(X));
to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_float(X) ->
    to_integer(float_to_binary(X));
to_integer(X) when is_binary(X) ->
    binary_to_integer(X);
to_integer(X) when is_integer(X) ->
    X.


to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_float(X) ->
    float_to_list(X);
to_list(X) when is_list(X) ->
    X.

unique_key() ->
    {ok, Key} = flake_server:id(62),
    Key.

hash_to_string(HashBin) when is_binary(HashBin) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end,
        binary_to_list(HashBin))).

proplists_delete(Key, Proplists) when is_atom(Key) ->
    proplists:delete(Key, Proplists);
proplists_delete([], Proplists) ->
    Proplists;
proplists_delete([Key | KeyList], Proplists) ->
    proplists_delete(KeyList, proplists:delete(Key, Proplists)).


md5_hex(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).


re_email(Email) ->
    case re:run(Email, "^[a-z0-9]+([._\\-]*[a-z0-9])*@([a-z0-9]+[-a-z0-9]*[a-z0-9]+.){1,63}[a-z0-9]+$") of
        {match, _} ->
            true;
        nomatch ->
            false
    end.