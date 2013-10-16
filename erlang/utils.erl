-module(utils).

-define(encoding, latin1).

-export([guid/1, guid/2, guid/3, priv_dir/1]).
-export([md5/1, hmac_sha/3, is_exists/2]).
-export([bin/1, atom/1, bool/1, list/1, int/1, int/2, float/1, float/2, number/1, number/2]).
-export([cmd/2]).

guid(Len) -> guid(Len, fun(_)-> true end).
guid(Len, Fun)-> guid(Len, Fun, 10).
guid(_Len, _Fun, 0) -> {error, reach_max_guid_try_limit};
guid(Len, Fun, Step) when (Len>0) and (Len=<32) ->
    <<N:128>> = erlang:md5(erlang:term_to_binary({erlang:now(),self(),node(),Step})),
    Value = list_to_binary(string:substr(lists:flatten(io_lib:format("~-32.36.0B",[N])), 1, Len)),
    case Fun(Value) of
        false -> guid(Len,Fun, Step-1);
        _ -> {ok, Value}
    end.

md5(BinStr)->
    <<N:128>> = erlang:md5(BinStr),
    lists:flatten(io_lib:format("~32.16.0B",[N])).
    
hmac_sha(_Type, Secret, Data) ->
    <<Mac:160/integer>> = crypto:hmac(sha, ?MODULE:list(Secret), Data),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).
 
cmd(Command, Options) ->
    Port = open_port({spawn,Command}, Options),
    loop_cmd(Port, "").

loop_cmd(Port, Result) ->
    receive
        {Port,{exit_status, 0}} ->
            {ok, Result};
        {Port,{exit_status, Status}} ->
            {error, Status, Result};
        {Port, {data, Data}} ->
            loop_cmd(Port, Result ++ Data)
    end.

is_exists(Type, Object) when is_atom(Type)->
    case mnesia:dirty_read(Type, Object) of
        [_|_] -> true;
        [] -> false
    end.
    
%% @private Return the path to the priv/ directory of an application.
-spec priv_dir(atom()) -> string().
priv_dir(App) ->
	case code:priv_dir(App) of
		{error, bad_name} -> priv_dir_mod(App);
		Dir -> Dir
	end.

-spec priv_dir_mod(atom()) -> string().
priv_dir_mod(Mod) ->
	case code:which(Mod) of
		File when not is_list(File) -> "../priv";
		File -> filename:join([filename:dirname(File),"../priv"])
	end.

%----------------------------------------------------------------

bin(B) when is_binary(B)->B;
bin(B) when is_atom(B)->list_to_binary(atom_to_list(B));
bin(B) when is_integer(B)-> list_to_binary(integer_to_list(B));
bin(B)-> list_to_binary(list(B)).

atom(A) when is_atom(A)-> A;
atom(A) when is_list(A)-> list_to_atom(A);
atom(V) when is_binary(V) -> atom(V, ?encoding);
atom(A) -> atom(list(A)).
atom(V, Encoding) ->
  case catch(erlang:binary_to_existing_atom(V, Encoding)) of
    A when is_atom(A) -> A;
    _ -> erlang:binary_to_atom(V, Encoding)
  end.

list(A) when is_list(A)-> A;
list(A) when is_atom(A)-> atom_to_list(A);
list(A) when is_binary(A)-> binary_to_list(A);
list(A) when is_integer(A)-> integer_to_list(A);
list({A, B, C, D}) -> [integer_to_list(A), $. ,
                       integer_to_list(B), $. ,
                       integer_to_list(C), $. ,
                       integer_to_list(D)];
list({datetime,{{Y, M, D},{H, I, S}}}) ->
    io_lib:format("~p/~2.2.0w/~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [Y, M, D, H, I, S]);
list(A) -> io_lib:format("~p", [A]).


int(V) -> int(V, 0).
int(V, _) when is_integer(V) -> V;
int(V, _) when is_float(V) -> erlang:round(V);
int([], Default) -> Default;
int(V, Default) ->
    S = list(V),
    case string:to_integer(S) of
        {error, _} -> Default;
        {N,_ } -> N
    end.

float(V) -> float(V, 0.0).
float(V, _) when is_float(V) -> V;
float(V, _) when is_integer(V) -> V * 1.0;
float([], Default) -> Default;
float(V, Default) ->
    S = list(V),
    case string:to_float(S) of
        {error, _} -> Default;
        {N,_ } -> N
    end.

number(V)-> number(V, 0).
number(V, Default)->
    case float(V, not_float) of
        not_float -> int(V, Default);
        N when is_float(N) -> N
    end.

bool(V)->
    case atom(V) of
        true -> true;
        yes -> true;
        on -> true;
        _ -> false
    end.
