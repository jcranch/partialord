# partialord

A data structure supporting partial orders, and some helpful
associated routines.

## Other packages

An alternative package is already available, namely
  [partial-order](https://hackage.haskell.org/package/partial-order-0.2.0.0/docs/Data-PartialOrd.html).
Differences include:
* PartialOrd has a comparison valued in Maybe Ordering; we use a fresh
  type but provide pattern synonyms.
* Where types have several natural partial orderings, we provide
  newtypes rather than choosing one.
