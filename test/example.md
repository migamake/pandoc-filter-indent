---
title: "Running example"
author: "Michał J. Gajda"
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

