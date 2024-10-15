# Лабораторная работа №2 по дисциплине "Функциональное Программирование"

## Терновский Илья, P3332

## Вариант: `Open Addressing Hashmap Set`

## Задание

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами
тестирования (unit testing, property-based testing).

## Требования:

1. Функции:
    - [X] добавление и удаление элементов;
    - [X] фильтрация;
    - [X] отображение (map);
    - [X] свертки (левая и правая);
    - [X] структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства
   моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют
   получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про
   экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Немного о реализации

Я немного поленился, поэтому был реализован Open Addressing HashSet, то есть это классический hashtable, в котором для
решения коллизий используется метод открытой адресации(линейное пробирование).

В качестве хэширующей функции была использована стандартная функция `erlang:phash2/2`, которая сразу берет остаток от
хэша.

Set имеет такой интерфейс:

```erlang
-export([new/0, add_element/2, remove_element/2, get_element/2,
  is_set/1, filter/2, from_list/1, to_list/1, map/2, foldl/3, foldr/3, merge/2]).

-record(set, {storage, length}).
```

Так же из особенностей есть то, что массив, который используется для хранения элементов динамически увеличивается, если
количество элементов превышает 75% от размера массива. **НО** он не уменьшается при удалении, что при неправильном
использовании может привести к утечкам памяти.

Функции **filter** и **map** были реализованы без приведения к листу, fold'ы наоборот, реализованы через приведение к
листу (для разнообразия).

Функции привидения к листу и получение из листа:
```erlang
from_list(List) ->
  lists:foldl(fun add_element/2, new(), List).

to_list(#set{storage = Array, length = _}) ->
  to_list(Array, 0, array:size(Array), []).

to_list(Array, Index, Size, Acc) when Index < Size ->
  case array:get(Index, Array) of
    undefined ->
      to_list(Array, Index + 1, Size, Acc);
    Value ->
      to_list(Array, Index + 1, Size, [Value | Acc])
  end;

to_list(_, _, _, Acc) ->
  Acc.
```

Добавление Элемента:
```erlang
add_element(Value, #set{storage = Array, length = Length}) ->
  case get_element(Value, #set{storage = Array, length = Length}) of
    found ->
      #set{storage = Array, length = Length}; % элемент уже существует, не добавляем
    not_found ->
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
      end
  end.
```

Удаление элемента:
```erlang
remove_element(Value, #set{storage = Array, length = Length}) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      #set{storage = Array, length = Length};
    Value ->
      NewArray = array:set(Position, undefined, Array),
      #set{storage = NewArray, length = Length - 1}
  end.
```

Фильтр:
```erlang
filter(Pred, #set{storage = Array, length = _}) ->
  filter(Pred, Array, 0, array:size(Array), new()).

filter(Pred, OldArray, Index, Size, NewSet) when Index < Size ->
  PredResult = Pred(array:get(Index, OldArray)),
  case array:get(Index, OldArray) of
    undefined ->
      filter(Pred, OldArray, Index + 1, Size, NewSet);
    Value when PredResult ->
      NewSet1 = add_element(Value, NewSet),
      filter(Pred, OldArray, Index + 1, Size, NewSet1);
    _ ->
      filter(Pred, OldArray, Index + 1, Size, NewSet)
  end;

filter(_, _, _, _, NewSet) ->
  NewSet.
```

map:
```erlang
map(Function, #set{storage = Array, length = _}) ->
  map(Function, Array, 0, array:size(Array), new()).

map(Function, OldArray, Index, Size, NewSet) when Index < Size ->
  case array:get(Index, OldArray) of
    undefined ->
      map(Function, OldArray, Index + 1, Size, NewSet);
    Value ->
      NewValue = Function(Value),
      NewSet1 = add_element(NewValue, NewSet),
      map(Function, OldArray, Index + 1, Size, NewSet1)
  end;

map(_, _, _, _, NewSet) ->
  NewSet.
```
Конкретные функции можно посмотреть в файле [hashmap_set.erl](src%2Fhashmap_set.erl).

## Тестирование

Тестирование производилось с помощью библиотеки `common test`(официальная, для юнит тестирования) и `proper`(
неофициальная для тестирования свойств).

Тесты можно найти в папке [test](test).

В рамках тестирования были протестированы все функции из интерфейса, а так же были протестированы свойства моноида.