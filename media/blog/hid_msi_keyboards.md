# MSI keyboards

I have a [MSI GS60 Ghost Pro 2QE](http://www.mobiletechreview.com/notebooks/MSI-GS60-2QE-Ghost-Pro.htm)
I’m very proud of. It’s sexy and powerful. It comes with a fancy and
configurable backlit keyboard.

![](http://www.ittipexplorer.info/wp-content/uploads/2014/04/wpid-msigs60ghostpro-8-580-90.jpg)

There’s a tool called *SteelSeries Engine* for Windows we can use to change the
colors of the keyboard. It supports several features:

  - changing colors of three parts of the keyboard (*left*, *middle* and *right*) ;
  - changing modes (*normal*, *breathe*, *wave*, *demo*, *gaming*).

Unfortunately, that software doesn’t work on Linux, even with wine. I tried hard
to make it work and never actually found a way to run it. Then, I decided to
look for alternatives and… found nothing *working*.

Yesterday, I tried a **node.js-powered** tool called
[msi-keyboard](https://github.com/wearefractal/msi-keyboard). And it worked.
However, the interface and user interface was not my cup of tea. I decided to
dig in in order to understand how it works, and I decided to write my own tool
with a decent interface.

## HID access

The key idea is that such keyboards are just
[HID devices](https://en.wikipedia.org/wiki/Human_interface_device). As a
**Haskell** programmer, I looked for something that would grant me access to
such devices, but nothing was working. There’s a
[hidapi](https://hackage.haskell.org/package/hidapi) package, but it doesn’t
work and has bugs.

I didn’t give up though. I wrote my own **Haskell HID API** binding, called
[hid](https://hackage.haskell.org/package/hid). Several good things about it:

  - it does work ;
  - it’s simple ;
  - I lately wrote a software using it.

Feel free to install and use it!

## msi-kb-backlit

Then, I wrote another **Haskell** package,
[msi-kb-backlit](http://hackage.haskell.org/package/msi-kb-backlit). It might
require *super user rights* to work. If you’re not a **Haskeller**, you can
find installation details [here](https://github.com/phaazon/msi-kb-backlit).

**Note: if you use Archlinux, you can directly download msi-kb-backlit through
the AUR! Search for msi-kb-backlit with yaourt, or download
[the tarball](https://aur4.archlinux.org/packages/msi-kb-backlit).**

The software has an embedded documentation to help you tweak with colors and
modes. ;)

Feel free to use all those pieces of software. I made them with love for you
all!

Enjoy your week end, and keep the vibe!
