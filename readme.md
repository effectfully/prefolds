# prefolds

With [`Control.Foldl`](https://hackage.haskell.org/package/foldl-1.2.1/docs/Control-Foldl.html) you can write

```
fold ((/) <$> sum <*> genericLength) [1..10^6]
```

and it'll stream.

With `prefolds` you can write

```
execute ((/) <$> take (10^7) sum <*> take (10^6) genericLength) [1..]
```

and it'll stream too.