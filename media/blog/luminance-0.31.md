[luminance] has been having lots of activity lately. I’ve been working (hard) on making it to the
`1.0` release for weeks now and came to a very important realization: lots of changes have been
done and I’m still not ready to release it.

# Motivation

Lately, [luminance] has received a wide range of feature additions, bug fixes and design ideas. I’ve
been wanting to release all those changes as part of the `1.0` release but the thing is: among all
the changes, some have been around on the `master` branch for months without a proper release on
crates.io… because the `1.0` milestone was not reached. People have been moving towards [luminance]
more and more and some provided feedback about using [luminance]. Happy but indecisive about what to
do, I faced a dilemma:

  - Either wait and release everything as part of `1.0` but eventually _block_ people from using all
    the cool features of [luminance] because the last release on crates.io is months old.
  - Break the feature set into two releases: `1.0` for everything and `0.31` for all the new
    candies.

I have decided to go with the second option.

# luminance-0.31

At the time of writing this article, the [last luminance version] is `0.30` — just for the record,
that version is eleven months old, ~200 commits behind the current `master`, at the time of writing.
The very-soon-to-be version will thus be `0.31`. The feature set includes:

[luminance]: https://crates.io/crates/luminance
[last luminance version]: https://crates.io/crates/luminance/0.30.1
