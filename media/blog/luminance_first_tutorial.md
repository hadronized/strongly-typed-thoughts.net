# Woah!

I’m very happy about people getting interested about my [luminance](http://hackage.haskell.org/package/luminance)
graphics framework. I haven’t received use case feedback yet, but I’m pretty confident I will sooner
or later.

In the waiting, I decided to write an *embedded tutorial*. It can be found
[here](http://hackage.haskell.org/package/luminance-0.1.1/docs/Graphics-Luminance.html).

That tutorial explains all the basic types of luminance – not all though, you’ll have to dig in the
documentation ;) – and describes how you should use it. I will try to add more documentation for
each modules in order to end up with a very well documented piece of software!

# Let’s sum up what you need

People on [reddit](https://www.reddit.com/r/haskell/comments/3lyxzc/luminance_01_released)
complain – they are right to – about the fact the samples just *“didn’t work*. They actually did,
but the errors were muted. I released
[luminance-0.1.1](http://hackage.haskell.org/package/luminance-samples-0.1.1) to fix that issue. Now
you’ll get the proper error messages.

The most common issue is when you try to run a sample without having the required hardware
implementation. luminance requires **OpenGL 4.5**. On Linux, you might need to use `primusrun` or
`optirun` if you have the **Optimus** technology. On Windows, I guess you have to allow the samples
to run on the dedicated *GPU*. And on Mac OSX… I have no idea; `primusrun` / `optirun`, I’d go.

Anyways, I’d like to thank all people who have/will tried/try the package. As always, I’ll keep you
informed about all the big steps I take about luminance. Keep the vibe!
