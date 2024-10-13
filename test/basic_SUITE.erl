-module(basic_SUITE).

-include_lib("../src/hashmap_set.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([grow_test/1, add_element_test/1, remove_test/1, filter_test/1, to_list_test/1, from_list_test/1, map_test/1]).
all() ->
  [grow_test, add_element_test, remove_test, filter_test, to_list_test, from_list_test, map_test].

% add few elements to test adding
add_element_test(_) ->
  Set = hashmap_set:new(),
  Set1 = hashmap_set:add_element(1, Set),
  Set2 = hashmap_set:add_element(2, Set1),
  Set3 = hashmap_set:add_element(3, Set2),
  ?assertEqual(Set3#set.length, 3),
  ?assertEqual(hashmap_set:get_element(1, Set3), found),
  ?assertEqual(hashmap_set:get_element(2, Set3), found).


% add a lot of elements to test growing
grow_test(_) ->
  #set{storage = Array, length = Length} = add_1k_elements(hashmap_set:new(), 0),
  ?assertEqual(Length, 1000),
  ?assert(array:size(Array) >= 1000).

% add 1000 elements and remove 3 of them to test removing
remove_test(_) ->
  Set = add_1k_elements(hashmap_set:new(), 0),
  Set1 = hashmap_set:remove_element(1, Set),
  Set2 = hashmap_set:remove_element(2, Set1),
  Set3 = hashmap_set:remove_element(3, Set2),
  ?assertEqual(Set3#set.length, 997),
  ?assertEqual(hashmap_set:get_element(1, Set3), not_found),
  ?assertEqual(hashmap_set:get_element(2, Set3), not_found),
  ?assertEqual(hashmap_set:get_element(3, Set3), not_found),
  ?assertEqual(hashmap_set:get_element(4, Set3), found).

add_1k_elements(Set, Count) when Count < 1000 ->
  NewSet = hashmap_set:add_element(Count, Set),
  add_1k_elements(NewSet, Count + 1);

add_1k_elements(Set, _) ->
  Set.

filter_test(_) ->
  Set = add_1k_elements(hashmap_set:new(), 0),
  FilteredSet = hashmap_set:filter(fun(X) -> X > 100 end, Set),
  ?assertEqual(FilteredSet#set.length, 899),
  ?assertEqual(hashmap_set:get_element(1, FilteredSet), not_found),
  ?assertEqual(hashmap_set:get_element(52, FilteredSet), not_found),
  ?assertEqual(hashmap_set:get_element(100, FilteredSet), not_found),
  ?assertEqual(hashmap_set:get_element(101, FilteredSet), found).

to_list_test(_) ->
  Set = add_1k_elements(hashmap_set:new(), 0),
  List = hashmap_set:to_list(Set),
  ?assertEqual(length(List), 1000),
  ?assertEqual(lists:sort(List), lists:seq(0, 999)).

from_list_test(_) ->
  List = lists:seq(0, 999),
  Set = hashmap_set:from_list(List),
  ?assertEqual(Set#set.length, 1000),
  ?assertEqual(hashmap_set:get_element(1, Set), found),
  ?assertEqual(hashmap_set:get_element(555, Set), found),
  ?assertEqual(hashmap_set:get_element(321, Set), found).

map_test(_) ->
  Set = add_1k_elements(hashmap_set:new(), 0),
  NewSet = hashmap_set:map(fun(X) -> X + 1 end, Set),
  NewSetMultiple = hashmap_set:map(fun(X) -> X * 2 end, NewSet),
  ?assertEqual(NewSet#set.length, 1000),
  ?assertEqual(NewSetMultiple#set.length, 1000),
  ?assertEqual(lists:sort(hashmap_set:to_list(NewSet)), lists:seq(1, 1000)),
  ?assertEqual(lists:sort(hashmap_set:to_list(NewSetMultiple)), [X || X <- lists:seq(1, 2000), X rem 2 =:= 0]).
