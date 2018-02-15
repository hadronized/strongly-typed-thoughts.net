# al 0.1.1.2

## Changes

[al 0.1.1.2](http://hackage.haskell.org/package/al-0.1.1.2) was shipped. It
includes several improvements, among them:

  - fix CPU architecture issues ;
  - since version 0.1.1, the `stdcall` flag is available ;
  - `alcIsExtensionSupported` was renamed to `alcIsExtensionPresent` as the
    former just doesn’t exist – sorry for the typo.

The `stdcall` flag might be a great ally for people compiling on a 32-bit
Windows. For people on 64-bit Windows, the default is sufficient – and for
UNIX systems, you don’t have anything special to do.

I’m also looking for hackers to test the library on the most OS as possible. I
have issues with it (see [this](http://stackoverflow.com/questions/28829976/ffi-and-static-libraries-used-in-application))
and I’d like to hear from people. Even though I have that issue, al compiles
well and runs great in **ghci**, which is weird regarding the fact running an
application compiled with al silently crashes at startup.

## About paths…

That’s a nasty issue I don’t really know how to correctly fix. Up to now,
default OpenAL 1.1 SDK installation are detected for Windows. For people with
custom installation and other systems, **you have to pass the path of the SDK by
hand**. I know, it’s a pain in the ass, but I don’t want to depend on tools like
`pkg-config` as that tool is not available everywhere – getting it on Windows is
not that simple.
