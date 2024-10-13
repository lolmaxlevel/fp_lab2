-module(hashmap_set).

-export([new/0, add_element/2]).

-include("hashmap_set.hrl").

-opaque set(Value) ::
#set{storage :: array:array(Value), length :: integer()}.

-export_type([set/1]).

new() ->
  #set{storage = array:new(?INIT_CAPACITY), length = 0}.

add_element(Value, #set{storage = Array, length = Length}) ->
  ResizedArray =
    case Length / array:size(Array) > ?LOAD_FACTOR of
      true ->
        grow_array(Array);
      false ->
        Array
    end,
  case put_element(Value, ResizedArray) of
    {changed_value, ReturnedArray} ->
      #set{storage = ReturnedArray, length = Length};
    {new_value, ReturnedArray} ->
      #set{storage = ReturnedArray, length = Length + 1}
  end.

put_element(Value, Array) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      {new_value, array:set(Position, Value, Array)};
    OldValue when OldValue == Value ->
      {changed_value, Array};
    OldValue when OldValue /= Value ->
      FreePosition = find_free_position(Position, Array),
      {new_value, array:set(FreePosition, Value, Array)}
  end.

find_free_position(Position, Array) ->
  Size = array:size(Array),
  WrappedPosition = Position rem Size,
  case array:get(WrappedPosition, Array) of
    undefined ->
      WrappedPosition;
    _ ->
      find_free_position(WrappedPosition + 1, Array)
  end.

grow_array(Array) ->
  NewSize = array:size(Array) * ?GROW_FACTOR,
  NewArray = array:new(NewSize),
  copy_array(Array, NewArray).

copy_array(Array, NewArray) ->
  Size = array:size(Array),
  copy_array(Array, NewArray, 0, Size).

copy_array(Array, NewArray, Index, Size) when Index < Size ->
  case array:get(Index, Array) of
    undefined ->
      copy_array(Array, NewArray, Index + 1, Size);
    Value ->
      {_, NewArray1} = put_element(Value, NewArray),
      io:format("NewArray1: ~p~n", [NewArray1]),
      copy_array(Array, NewArray1, Index + 1, Size)
  end;

copy_array(_, NewArray, _, _) ->
  NewArray.

calc_hash(Value, Array) ->
  erlang:phash2(Value, array:size(Array)).