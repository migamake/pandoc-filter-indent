---
title: "Running example"
author: "Michał J. Gajda"
header-includes:
  - \usepackage{scalerel}
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

Another note on indent strucutre:
```{.haskell}
class Eq      a
   => Compare a where
   compare :: a -> a -> Ordering
   (>=)    :: a -> a -> Bool
```

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
  <<
  <<<
  -<
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
  eps
  TCons
  bot
  undefined
  |=>
  |->
  \\
```
