-module(basic_SUITE).

-include_lib("../src/hashmap_set.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([grow_test/1, add_element_test/1, remove_test/1]).
all() ->
  [grow_test, add_element_test, remove_test].

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


%%%% Common test requires Config argument in test cases
%%fib_data_tests(_) ->
%%    fib_regular_case(4613732).
%%
%%dist_power_data_tests(_) ->
%%    dist_power_regular_case(9183).
%%
%%dist_power_regular_case(Expected) ->
%%    ?assertEqual(Expected, distinct_powers_map:count_distinct_terms()),
%%    ?assertEqual(Expected, distinct_powers:count_distinct_terms()).
%%
%%fib_regular_case(Expected) ->
%%    ?assertEqual(Expected, fib_even_module:sum_even_fib()),
%%    ?assertEqual(Expected, fib_even_stream:sum_even_fib()),
%%    ?assertEqual(Expected, fib_even_monolith_tail:sum_even_fib()),
%%    ?assertEqual(Expected, fib_even_monolith_recursion:sum_even_fib()).