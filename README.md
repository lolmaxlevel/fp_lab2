# Лабораторная работа №2 по дисциплине "Функциональное Программирование"

## Задание
Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Вариант: `Open Addressing Hashmap Set`

## Требования:

1. Функции:
   - [X] добавление и удаление элементов;
   - [X] фильтрация;
   - [X] отображение (map);
   - [X] свертки (левая и правая);
   - [X] структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.