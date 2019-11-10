# hw-koans [![CircleCI](https://circleci.com/gh/haskell-works/hw-koans/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-koans/tree/master)

Course for learn functional programming.

## Prerequisites

* `cabal-3.0.0.0` or above
* `ghc-8.6.5` or above

## Running

Run:

```bash
$ git clone git@github.com:haskell-works/hw-koans.git
$ cd hw-koans
$ cabal v2-test --enable-tests --test-show-details=direct
...
15 modules not enrolled
All enrolled 0 test modules succeeded
Test suite koan-test: PASS
...
```

The above output shows that there are `15` modules not enrolled.
This course is structure into modules, which are found under the
`koan/Koan` directory.

Each module defines the following a function:

```haskell
enrolled :: Bool
enrolled = False
```

You will need to enable the module by changing `enrolled` to `True`
then running the test command again:

```bash
$ cabal v2-test --enable-tests --test-show-details=direct
━━━ Check.Functor ━━━
  ✗ prop_fmap_void failed at test/Check/Functor.hs:43:3
    after 1 test and 1 shrink.

       ┏━━ test/Check/Functor.hs ━━━
    40 ┃ prop_fmap_void :: Property
    41 ┃ prop_fmap_void = property $ do
    42 ┃   mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
       ┃   │ Nothing
    43 ┃   K.void mb === P.void mb
       ┃   ^^^^^^^^^^^^^^^^^^^^^^^
       ┃   │ ━━━ Exception (ErrorCall) ━━━
       ┃   │ TODO: implement void
       ┃   │ CallStack (from HasCallStack):
       ┃   │   error, called at koan/Koan/Functor.hs:34:8 in hw-koans-0.1.0.0-inplace:Koan.Functor

    This failure can be reproduced by running:
    > recheck (Size 0) (Seed 16511336219962586096 11754670307026383675) prop_fmap_void
...
```

From the above you can see the failure `TODO: implement void` in the module `koan/Koan/Functor.hs`
file at line `34`.

Go to that location to find the function `void` and implement it by replacing `error "TODO: implement void"`
with a correct implementation:

```haskell
void :: Functor f => f a -> f ()
void = error "TODO: implement void"
```

Run the test command to check that you've correctly implemented the function then repeat for the
other failures until there are no more errors.

## IDE support

The project will build & test in Visual Studio Code with ⌘⇧V.

## List of modules

The following are modules in order of difficulty.

### Introductory

* `Koan.Start`
* `Koan.List`
* `Koan.Eq`
* `Koan.Ord`
* `Koan.Simple`

### Beginner

* `Koan.Functor`
* `Koan.Maybe`
* `Koan.Applicative`
* `Koan.Monad`
* `Koan.State`

## Acknowledgements

This course was inspired by the [Data 61 Functional Programming Course](https://github.com/data61/fp-course)
