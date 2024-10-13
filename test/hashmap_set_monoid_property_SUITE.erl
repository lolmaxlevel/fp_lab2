-module(hashmap_set_monoid_property_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("../src/hashmap_set.hrl").

-define(SET_SIZE, 1000).

-export([all/0, identity_property_test/1, associativity_property_test/1,
  idempotence_property_test/1]).

all() ->
  [identity_property_test, associativity_property_test, idempotence_property_test].

identity_property_test(_) ->
  ?FORALL(Set, set(),
    begin
      EmptySet = hashmap_set:new(),
      ?assertEqual(Set, hashmap_set:merge(Set, EmptySet)),
      ?assertEqual(Set, hashmap_set:merge(EmptySet, Set))
    end).

associativity_property_test(_) ->
  ?FORALL({Set1, Set2, Set3}, {set(), set(), set()},
    begin
      Merged1 = hashmap_set:merge(Set1, hashmap_set:merge(Set2, Set3)),
      Merged2 = hashmap_set:merge(hashmap_set:merge(Set1, Set2), Set3),
      ?assertEqual(Merged1, Merged2)
    end).

idempotence_property_test(_) ->
  ?FORALL(Set, set(),
    begin
      Merged = hashmap_set:merge(Set, Set),
      ?assertEqual(Set, Merged)
    end).

set() ->
  ?LET(Elements, list(int()),
    lists:foldl(fun(E, Acc) -> hashmap_set:add_element(E, Acc) end, hashmap_set:new(), Elements)).
