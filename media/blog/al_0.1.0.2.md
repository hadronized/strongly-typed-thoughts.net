# al patch

This is a very short article to make you notice that
[al](https://hackage.haskell.org/package/al) received two important changes:

  - I uploaded documentation (hourra!) ;
  - OpenAL paths will default to default installation on **Windows systems**.

I tested the latter with a Windows 64b:

    cabal update
    cabal install al

That’s all. You should try it out as well. If you have errors about OpenAL
libraries and/or header files, check whether OpenAL is correctly installed.
If the error comes up again, proceed as I said
[here](http://phaazon.blogspot.fr/2015/02/al-01-released.html#installing-al-1).

Also, I take people on linux feedback about installs ;).
