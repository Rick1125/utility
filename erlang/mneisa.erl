-module(mnesia).

-compile(export_all).

%%Dirty Mnesia Foldl
dirty_foldl(F, Acc0, Table) ->
  dirty_foldl(F, Acc0, Table, mnesia:dirty_first(Table)).

dirty_foldl(_, Acc, _, '$end_of_table') ->
  Acc;
dirty_foldl(F, Acc, Table, Key) ->
  Acc2 = lists:foldl(F, Acc, mnesia:dirty_read(Table, Key)),
  dirty_foldl(F, Acc2, Table, mnesia:dirty_next(Table, Key)).

%%Dirty Mnesia Foreach
dirty_foreach(F, Table) ->
  dirty_foreach(F, Table, mnesia:dirty_first(Table)).

dirty_foreach(_, _, '$end_of_table') ->
  ok;
dirty_foreach(F, Table, Key) ->
  lists:foreach(F, mnesia:dirty_read(Table, Key)),
  dirty_foreach(F, Table, mnesia:dirty_next(Table, Key)).
