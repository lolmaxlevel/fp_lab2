-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%-export([all/0]).
%%-export([fib_data_tests/1, dist_power_data_tests/1]).

%%all() ->
%%    [fib_data_tests, dist_power_data_tests].
%%
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