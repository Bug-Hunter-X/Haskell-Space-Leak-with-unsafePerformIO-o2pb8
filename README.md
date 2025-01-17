# Haskell Space Leak Example

This repository demonstrates a space leak in Haskell caused by the misuse of `unsafePerformIO`. The program generates a very large list, and because `unsafePerformIO` is used, the garbage collector may not reclaim the memory occupied by this list after it is no longer needed, leading to a significant memory leak.  This is a subtle but crucial issue when dealing with side effects and memory management in Haskell.  The solution shows how to mitigate the problem using more idiomatic approaches.

## How to Reproduce

1. Clone this repository.
2. Compile and run `bug.hs` using a Haskell compiler (GHC is recommended): `ghc bug.hs && ./bug`
3. Observe the memory usage.  The program will likely consume a substantial amount of memory and might not release it after completion.  Running with `+RTS -s` will give you more detailed memory usage statistics.
4. Compare to the solution which showcases a memory-safe alternative.
