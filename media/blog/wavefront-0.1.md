I’ve been away from [luminance](https://hackage.haskell.org/package/luminance) for a few days
because I wanted to enhance the graphics world of Haskell. luminance might be interesting, if you
can’t use the art works of your artists, you won’t go any further for a real-world application. I
decided that I to write a parser/lexer to load 3D geometries from files. The
[Wavefront OBJ](https://en.wikipedia.org/wiki/Wavefront_.obj_file) is an old yet simple and
efficient way of encoding such objects. It supports materials, surfaces and a lot of other cool
stuff – I don’t cover them yet, though.

There’s a [package](http://hackage.haskell.org/package/obj) out there to do that, but it hasn’t been
updated since 2008 and has a lot of dependencies I don’t like (InfixApplicative, OpenGL,
OpenGLCheck, graphicsFormats, Codec-Image-Devil, and so on…). I like to keep things ultra simple and
lightweight. So here we go. [wavefront](http://hackage.haskell.org/package/wavefront).

Currently, my package only builds up a pure value you can do whatever you want with. Upload it to
the GPU, modify it, pretty print it, perform some physics on it. Whatever you want. The interface is
not frozen yet and I need to perform some benchmarks to see if I have to improve the
performance – the lexer is very simple and naive, I’d be amazed if the performance were that good
yet.

As always, feel free to contribute, and keep in mind that the package will move quickly along the
performance axis.
