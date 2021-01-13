---
title: "Running example"
author: "Michał J. Gajda"
header-includes:
  - \usepackage{scalerel}
inline-code: "haskell"
---

The example input is here:
```{.haskell}
class Eq      a
   => Compare a where
  compare :: a -> a -> Ordering
  (>=)    :: a -> a -> Bool
```

With extra space to break layout:

```{.haskell}
class Eq      a
   => Compare a where


  compare :: a -> a -> Ordering
  (>=)    :: a -> a -> Bool
```

Another note on indent structure:
```{.haskell}
class Eq      a
   => Compare a where
   compare :: a -> a -> Ordering
   (>=)    :: a -> a -> Bool
```

Inline `<>` quote is here.

```{.haskell}
test :: forall a. a -> b -> c -> d -> e
test = do
  (>>=)
  x `elem` v
  (>=>)
  mempty
  bottom
  top
  not
  (|)
  (||)
  (|>)
  (>>)
  (>>>)
  (+1)
  (+ alpha)
  i  (//)
  <<
  <<<
  -<
  >-
  <-
  >=
  <=
  !=
  <->
  ->
  =>
  <>
  elem
  ~
  ~=
  mempty
  a
  b
  c
  d
  f <$> x <*> y
  f "stringexample"
  eps
  TCons
  bot
  undefined
  |=>
  |->
  \\
```
