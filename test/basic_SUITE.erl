-module(basic_SUITE).

-include_lib("../src/hashmap_set.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([grow_test/1]).
all() ->
  [grow_test].

grow_test(_) ->
  #set{storage = Array, length = Length} = add_1k_elements(hashmap_set:new(), 0),
  ?assertEqual(Length, 1000),
  ?assert(array:size(Array) >= 1000).


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