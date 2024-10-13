-module(hashmap_set_unit_SUITE).

-include_lib("../src/hashmap_set.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([grow_test/1, add_element_test/1, remove_test/1, filter_test/1, to_list_test/1,
  from_list_test/1, map_test/1, foldl_test/1, foldr_test/1, is_associative_test/1, has_identity_element_test/1, merge_test/1]).

all() ->
  [grow_test, add_element_test, remove_test, filter_test, to_list_test, from_list_test,
    map_test, foldl_test, foldr_test, is_associative_test, has_identity_element_test, merge_test].

% add few elements to test adding
add_element_test(_) ->
  Set = hashmap_set:new(),
  Set1 = hashmap_set:add_element(1, Set),
  Set2 = hashmap_set:add_element(2, Set1),
  Set3 = hashmap_set:add_element(3, Set2),
  ?assertEqual(3, Set3#set.length),
  ?assertEqual(found, hashmap_set:get_element(1, Set3)),
  ?assertEqual(found, hashmap_set:get_element(2, Set3)).

% add a lot of elements to test growing
grow_test(_) ->
  #set{storage = Array, length = Length} = add_elements(hashmap_set:new(), 0),
  ?assertEqual(1000, Length),
  ?assert(array:size(Array) >= 1000).

% add 1000 elements and remove 3 of them to test removing
remove_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  Set1 = hashmap_set:remove_element(1, Set),
  Set2 = hashmap_set:remove_element(2, Set1),
  Set3 = hashmap_set:remove_element(3, Set2),
  ?assertEqual(997, Set3#set.length),
  ?assertEqual(not_found, hashmap_set:get_element(1, Set3)),
  ?assertEqual(not_found, hashmap_set:get_element(2, Set3)),
  ?assertEqual(not_found, hashmap_set:get_element(3, Set3)),
  ?assertEqual(found, hashmap_set:get_element(4, Set3)).

add_elements(Set, Count) when Count < 1000 ->
  NewSet = hashmap_set:add_element(Count, Set),
  add_elements(NewSet, Count + 1);
add_elements(Set, _) ->
  Set.

filter_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  FilteredSet = hashmap_set:filter(fun(X) -> X > 100 end, Set),
  ?assertEqual(899, FilteredSet#set.length),
  ?assertEqual(not_found, hashmap_set:get_element(1, FilteredSet)),
  ?assertEqual(not_found, hashmap_set:get_element(52, FilteredSet)),
  ?assertEqual(not_found, hashmap_set:get_element(100, FilteredSet)),
  ?assertEqual(found, hashmap_set:get_element(101, FilteredSet)).

to_list_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  List = hashmap_set:to_list(Set),
  ?assertEqual(1000, length(List)),
  ?assertEqual(lists:seq(0, 999), lists:sort(List)).

from_list_test(_) ->
  List = lists:seq(0, 999),
  Set = hashmap_set:from_list(List),
  ?assertEqual(1000, Set#set.length),
  ?assertEqual(found, hashmap_set:get_element(1, Set)),
  ?assertEqual(found, hashmap_set:get_element(555, Set)),
  ?assertEqual(found, hashmap_set:get_element(321, Set)).

map_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  NewSet = hashmap_set:map(fun(X) -> X + 1 end, Set),
  NewSetMultiple = hashmap_set:map(fun(X) -> X * 2 end, NewSet),
  ?assertEqual(1000, NewSet#set.length),
  ?assertEqual(1000, NewSetMultiple#set.length),
  ?assertEqual(lists:seq(1, 1000), lists:sort(hashmap_set:to_list(NewSet))),
  ?assertEqual([X || X <- lists:seq(1, 2000), X rem 2 =:= 0], lists:sort(hashmap_set:to_list(NewSetMultiple))).

foldl_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  MultSet = hashmap_set:add_element(2, hashmap_set:add_element(1, hashmap_set:new())),
  Sum = hashmap_set:foldl(fun(X, Acc) -> X + Acc end, 0, Set),
  Multiplied = hashmap_set:foldl(fun(X, Acc) -> X * Acc end, 1, Set),
  Multiplied2 = hashmap_set:foldl(fun(X, Acc) -> X * Acc end, 1, MultSet),
  ?assertEqual(499500, Sum),
  ?assertEqual(0, Multiplied),
  ?assertEqual(2, Multiplied2).

foldr_test(_) ->
  Set = add_elements(hashmap_set:new(), 0),
  MultSet = hashmap_set:add_element(2, hashmap_set:add_element(1, hashmap_set:new())),
  Sum = hashmap_set:foldr(fun(X, Acc) -> X + Acc end, 0, Set),
  Multiplied = hashmap_set:foldr(fun(X, Acc) -> X * Acc end, 1, Set),
  Multiplied2 = hashmap_set:foldr(fun(X, Acc) -> X * Acc end, 1, MultSet),
  ?assertEqual(499500, Sum),
  ?assertEqual(0, Multiplied),
  ?assertEqual(2, Multiplied2).

is_associative_test(_) ->
  A = hashmap_set:add_element(1, hashmap_set:new()),
  B = hashmap_set:add_element(2, hashmap_set:new()),
  AB = hashmap_set:add_element(2, A),
  ABC1 = hashmap_set:add_element(3, AB),
  BC = hashmap_set:add_element(3, B),
  ABC2 = hashmap_set:add_element(1, BC),
  ?assertEqual(ABC2, ABC1).

has_identity_element_test(_) ->
  EmptySet = hashmap_set:new(),
  A = hashmap_set:add_element(1, EmptySet),
  ?assertEqual(A, hashmap_set:add_element(1, hashmap_set:new())).

merge_test(_) ->
  Set1 = hashmap_set:from_list(lists:seq(1, 1000)),
  Set2 = hashmap_set:from_list(lists:seq(1001, 2000)),
  Merged = hashmap_set:merge(Set1, Set2),
  MergedWithSelf = hashmap_set:merge(Set1, Set1),
  ?assertEqual(2000, Merged#set.length),
  ?assertEqual(found, hashmap_set:get_element(1, Merged)),
  ?assertEqual(found, hashmap_set:get_element(1000, Merged)),
  ?assertEqual(found, hashmap_set:get_element(1001, Merged)),
%%  ?assertEqual(1000, MergedWithSelf#set.length),
  ?assertEqual(lists:seq(1, 1000), lists:sort(hashmap_set:to_list(MergedWithSelf))).