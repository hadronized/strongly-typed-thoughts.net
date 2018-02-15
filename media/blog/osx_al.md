# Support for Mac users!

This will be a short announcement about
[al](https://hackage.haskell.org/package/al), a **Haskell** wrapper to the
**OpenAL** C library.

Currently, the wrapper has been successfully tested on Linux – at least it works
well on my *Archlinux* distro. I made a little program that reads from an *.ogg*
file and streams the PCM signal to the wrapper – see
[libvorbis](https://hackage.haskell.org/package/libvorbis) for further details.
I’ll release the program later on if I find the spare time.

The wrapper might also work on *Windows* as long as you have
[pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config/) installed.
I’d be very happy with feedback from people working on *Windows*. I don’t want
anyone be put apart with my packages.

However, I’ve never tested the wrapper on *Mac OS X*. I guessed it wouldn’t work
out of the box because *Mac OS X* doesn’t use regular libraries to compile and
link – that would have been too easy otherwise, and hell, *think different
right?* They use something called a [framework](https://developer.apple.com/library/mac/documentation/MacOSX/Conceptual/BPFrameworks/Concepts/WhatAreFrameworks.html).
It’s possible to include a framework in a **Haskell** project by fulfilling
the `frameworks` field in the *.cabal* file. I received a simple patch to do
that – [here](https://github.com/mtolly/al/commit/fef897083c89899b72c2e026a7831f9c48710b5c), and I merged it upstream.

Then, *Mac OS X* is now officially supported. The release version is the
[0.1.4](https://hackage.haskell.org/package/al-0.1.4).

# About stackage

There’s something else I’d like to discuss. Quickly after the first release of
[al](https://hackage.haskell.org/package/al-0.1.0.0), I decided to push it onto
[stackage](https://www.stackage.org). Unfortunately, there’s a problem with the
package and *Ubuntu*. For a very dark reason, *Ubuntu* doesn’t expose
anything when invoking `pkg-confg --cflags`, even if the files are there – on
*Ubuntu* they can be found in `/usr/include/AL`.

That’s very silly because I don’t want to hardcode the location – it might be
something else on other Linux distro. The problem might be related to the
**OpenAL** support in *Ubuntu* – the *.pc* file used by `pkg-config` might be
incomplete. So if you experience that kind of issue, you can fix it by passing
the path to your **OpenAL** headers:

    cabal install al --extra-include-dirs=/usr/include/AL

If **OpenAL** is installed somewhere else, consider using:

    find / -name al.h

I’ll do my best to quickly fix that issue.
