-module(hashmap_set).

-export([]).

-include("hashmap_set.hrl").

-opaque set(Key) ::
    #map{storage :: array:array({Key, Value}), length :: integer()}.

-export_type([set/1]).

new() ->
    #map{storage = array:new(?INIT_CAPACITY), length = 0}.

add_element(Value, #map{storage = Array, length = Length}) ->
    ResizedArray =
        case Length / array:size(Array) > ?LOAD_FACTOR of
            true ->
                grow_array(Array);
            false ->
                Array
        end,
    case put_element(Value, ResizedArray) of
        {changed_value, ReturnedArray} ->
            #map{storage = ReturnedArray, length = Length};
        {new_value, ReturnedArray} ->
            #map{storage = ReturnedArray, length = Length + 1}
    end.

put_element(Value, #map{storage = Array, length = Length}) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      array:set(Position, Value, Array),
      {new_value, Array};
    OldValue when OldValue == Value ->
      {changed_value, Array};
    OldValue when OldValue /= Value->
      FreePosition = find_free_position(Position, Array),
      array:set(FreePosition, Value, Array),
      {new_value, Array}
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

calc_hash(Key, Array) ->
    erlang:phash2(Key, array:size(Array)).