# luminance 0.8 and existential quantification

It’s been a while I haven’t released anything on my blog. I just wrote a few changes for the latest
version of luminance, [luminance-0.8.2](https://hackage.haskell.org/package/luminance-0.8.2) and I
decided to write about it because I think those changes are interesting on a Haskell level.

## The problem

If you haven’t read the [changelog](https://hackage.haskell.org/package/luminance-0.8.2/changelog)
yet, I changed the `createProgram` function and the way it handles uniform interfaces. In
luminance < 0.8, you were provided with as many functions as there are uniform kinds. Up to now,
luminance supports two uniform kinds:

  * simple uniforms;
	* uniform block (UBO).

So you had two rank-2 functions like `forall a. (Uniform a) => Either String Natural ->
UniformInterface m (U a)` and `forall a. (UniformBlock a) => String ->
UniformInterface m (U (Region rw (UB a)))` to map whichever uniforms you wanted to.

The issue with that is that it requires to break the interface of `createProgram` each time we want
to add a new kind of uniform, and it’s also [a pretty hard to read function signature](https://hackage.haskell.org/package/luminance-0.7.2/docs/Graphics-Luminance-Shader-Program.html#v:createProgram)!

So… how does luminance-0.8 solve that?

## (Generalized) Algebraic data types, rank-2 and existential quantification

What is the only way we have to select uniforms? Names. Names can either be a `String` or a
`Natural` for explicit semantics. We could encode such a name using an algebraic data type:

```haskell
data UniformName
  = UniformName String
	| UniformSemantic Natural
	  deriving (Eq,Show)
```

That’s a good start. Though, we still have the problem of choosing the kind of uniform because we
still have several functions – one per kind. We could encode the kind of the uniform directly into
the name. After all, when we ask for a uniform mapping through a name, we require to know the kind.
So that kind of makes sense. Let’s change our `UniformName` type:

```haskell
data UniformName :: * -> * where
  UniformName :: String -> UniformName a
	UniformSemantic :: Natural -> UniformName a
	UniformBlockName :: String -> UniformName (Region rw (UB a))
```

That’s neat, but with that definition, we won’t go anywhere, because we’re too polymorphic. Indeed,
`UniformName "foo" :: UniformName a` can have any `a`. We need to put constraints on `a`. And that’s
where *GADTs* come in so handy! We can hide the constraints in the constructors and bring them into
scope when pattern matching. That’s a very neat feature of *GADTs*. So now, let’s add some
constraints to our constructors:

```haskell
data UniformName :: * -> * where
  UniformName :: (Uniform a) => String -> UniformName a
	UniformSemantic :: (Uniform a) => Natural -> UniformName a
	UniformBlockName :: (UniformBlock a) => String -> UniformName (Region rw (UB a))
```

Yes! Now, we can write a function that takes a `UniformName a`, pattern matches it and call the
appropriate function regarding the infered shape of `a`!

However, how do we forward the error? In older version of luminance, we were using `ProgramError`
and more especially two of its constructors: `InactiveUniform` and `InactiveUniformBlock`. We
need to shrink that to a single `InactiveUniform` constructor and find a way to store our
`UniformName`… But we can’t yet, because of the `a` parameter! So the idea is to hide it through
existential quantification!

```haskell
data SomeUniformName = forall a. SomeUniformName (UniformName a)

instance Eq SomeUniformName where
  -- …

instance Show SomeUniformName where
  -- …
```

And now we can store `SomeUniformName` in `InactiveUniform`. We won’t need to recover the type, we
just need the constructor and the carried name. By pattern matching, we can recover both those
information!

## Conclusion

Feel free to have a look at the new [`createProgram` function](https://hackage.haskell.org/package/luminance-0.8.1/docs/Graphics-Luminance-Shader-Program.html#v:createProgram).
As you will see, the type signature is easier to read and to work with! :)

Have fun, and keep the vibe!
