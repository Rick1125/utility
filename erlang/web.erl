-module(web).

-export([parse_qs/2]).
-export([json_encode/1]).

parse_qs(<<>>, _URLDecode) ->
  [];
parse_qs(Qs, URLDecode) ->
  Tokens = binary:split(Qs, <<"&">>, [global, trim]),
  [case binary:split(Token, <<"=">>) of
    [Token] -> {URLDecode(Token), true};
    [Name, Value] -> {URLDecode(Name), URLDecode(Value)}
  end || Token <- Tokens].

json_encode(Data) ->
  F = mochijson2:encoder([{handler, fun json_encoder_struct/1}, {utf8, true}]),
  F(Data).

-define(r(T), r(T)-> record_info(fields, T)).

?r(pool_diff);
r(_) -> undefined.

json_encoder_struct(Data) when is_tuple(Data) ->
  case element(1, Data) of
    Name when is_atom(Name)->
      case r(Name) of
        Def when is_list(Def) and (tuple_size(Data)=:=(length(Def)+1)) ->
          {_, L2} = lists:foldl(fun(K, {N, L})->
              case get({skip_json_element, Name, K}) of
                skip_json_element -> {N+1, L};
                _ -> {N+1, [{K, element(N, Data)} | L]}
              end
            end, {2, []}, Def),
          {struct, lists:reverse(L2)};
        _ -> tuple_to_list(Data)
      end;
    _ -> tuple_to_list(Data)
  end.
