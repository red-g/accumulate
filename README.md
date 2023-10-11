# accumulate
Accumulate provides the `Fold`, `Reduce`, and `Split` abstractions.
1. `Fold`
  * Use an input to accumulate over some state
  * Examples: `List.foldl`, `Dict.foldl`
2. `Reduce`
  * A `Fold` where the input and state share the same type
  * Examples: `sum`, `++`
3. `Split`
  * Extract an item from a collection, if it exists
  * Examples: `String.uncons`, `head :: tail`
  * This is mostly useful in the context of reducing over a collection
# extension
`Fold`, `Reduce`, and `Split` are already defined for built-in types, but you can easily extend them to your types with the `custom` function.
# examples
Cons to a list if the element exists:
```elm
maybeCons =
  Fold.maybe Fold.list

Fold.merge maybeCons [ 2, 3, 4 ] (Just 1)
-- [ 1, 2, 3, 4 ]
```
Get the minimum value of a list:
```elm
minimum =
  Reduce.listLeft (Reduce.min Sort.int)

minimum [ 8, 5, 7 ]
-- Just 5
```
