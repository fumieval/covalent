Covalent: heterogenous traversal
----

`Covalent` is an analogue of `Traversal` where its targets may be any type satisfying the specified constraint.

```haskell
class Covalent c a where
  traverseCommon :: Applicative f => (forall x. c x => x -> f x) -> a -> f a
```

An instance of `Covalent` can be generically derived as long as _all_ the fields satisfy the constraint:

```haskell
data Numeral = NumInt Int | NumDouble Double | NumPair Double Int deriving Generic

instance Covalent Show Numeral
instance Covalent Num Numeral
```

Using the `Covalen Num Numeral` instance, you can write a function that adds 1 regardless of the constructor.

```haskell
add1 :: Numeral -> Numeral
add1 = overCommon @Num (+1)
```