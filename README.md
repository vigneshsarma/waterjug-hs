# waterjug-hs

An update to the an older program I wrote in [python](https://github.com/vigneshsarma/water-jug). For a detailed explanation of the code you can check the post:

- https://hittaruki.info/post/water-jug-rewrite-with-haskell-part-i/

## Usage
**Note**: You will need [haskell stack](https://docs.haskellstack.org/en/stable/README/) installed before you can use this.

To solve the default problem you can run the following from the folder cloned the repo into.

``` batchfile
$ stack build && stack exec waterjug-hs-exe
Just [State (Jug 5 0) (Jug 3 0),State (Jug 5 5) (Jug 3 0),State (Jug 5 2) (Jug 3 3),State (Jug 5 2) (Jug 3 0),State (Jug 5 0) (Jug 3 2),State (Jug 5 5) (Jug 3 2),State (Jug 5 4) (Jug 3 3),State (Jug 5 4) (Jug 3 0)]
```

If you want to apply it on other problems, using the repl would be the easiest way.

``` batchfile
$ stack ghci
> solve $ newProblem 4 3 2 2
Nothing
>
> solve $ newProblem 5 3 4 0
Just [State (Jug 5 0) (Jug 3 0),State (Jug 5 5) (Jug 3 0),State (Jug 5 2) (Jug 3 3),State (Jug 5 2) (Jug 3 0),State (Jug 5 0) (Jug 3 2),State (Jug 5 5) (Jug 3 2),State (Jug 5 4) (Jug 3 3),State (Jug 5 4) (Jug 3 0)]
>
```
