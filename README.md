Evaluate boolean expressions on sets.

https://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/

Operator (highest to lowest binding strength): not (!), and (&), or (|)

```
> cabal run boolean-expressions -- "x and y" x y
True

> cabal run boolean-expressions -- "x or y" x y
True

cabal run boolean-expressions -- "(x | !z)" y z
False

> cabal run boolean-expressions -- "x or y" x y
True

> cabal run boolean-expressions -- "not x and y" x y
False
```
