# Generalized swap

I pushed a *pull request* to [Edward Kmett’s either package](https://hackage.haskell.org/package/either)
to implement two functions some guys was complaining not to find: `flipEither :: Either e a -> Either a e`
and `flipEitherT :: EitherT e m a -> EitherT a m e`.

When implementing the functions, I wondered: “Hey, flipping stuff is a pretty
common operation. Don’t we have an abstraction for that yet?”. I haven’t found any.

# Meet Swap

I decided to make a little typeclass to see what it’d be.

```haskell
class Swap s where
  swap :: s a b -> s b a

instance Swap (,) where
  swap (a,b) = (b,a)

instance Swap Either where
  swap = flipEither

-- let’s go wild and fooled
instance Swap Map where
  swap = fromList . fmap swap . toList
```

If you think that’s handy, I’ll write a little package with default instances
to make it live.

Happy hacking folks!
