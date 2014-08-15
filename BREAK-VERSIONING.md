[Semantic Versioning](http://semver.org/) is inadequate in practice. Why?

  1. It doesn't recognise the reality that **software breaks are common**, and **not all equal**. It should be possible to distinguish between a major and minor breakage.
  2. It doesn't recognise the reality of **psychological/marketing influences**. People strongly resist bumping major version numbers for every breaking change. And sometimes they want to indicate that a change is "significant", even if it's not breaking.
  3. There's **anarchy in <v1.0.0**. And (1,2) mean that code tends to linger in <v1.0.0 for too long.

Net effect of all this is that _in practice_ most people don't actually use SemVer correctly[1], making Semantic versions ambiguous and therefore unreliable.

[This post](http://www.jongleberry.com/semver-has-failed-us.html) by [Jonathan Ong](https://github.com/jonathanong) is great and mostly mirrors my own views on the subject, minus the objection to version qualifiers (which I think serve a necessary purpose despite the difficulties they can pose).

> [1] If people were using SemVer correctly, you'd be seeing a lot more code at version `37.y.z`. If you've ever released code with ANY breaking change without bumping your major  version number, you've technically violated the SemVer contract.

## Break Versioning (BreakVer)

As an alternative, I'm suggesting the following modification to SemVer which I'll be using for my own projects in future:

```
<major>.<minor>.<non-breaking>[-<optional-qualifier>]:
------------------------------------------------------
<major>        - Major breaking changes [or discretionary (qualitative/marketing)].
<minor>        - Minor breaking changes [or discretionary (qualitative/marketing)].
<non-breaking> - **Strictly** non-breaking changes.
```

This is a prescription that should be **comfortable to follow strictly**, so one that should be dependable in practice:

Bump             | We can infer                                       |
---------------- | -------------------------------------------------- |
`<non-breaking>` | Safe upgrade, always. Just do it.                  |
`<minor>`        | Might break code in a minor way, check changelog.  |
`<major>`        | Might break code in a major way, check changelog!! |

It also happens to be much simpler.

> Notice that since we care about the _maximum_ (not minimum) amount of damage a version could inflict, the allowance for discretionary major/minor bumping does not negatively impact our ability to make useful inferences.

#### I want to use BreakVer. Which number should I bump?

The prescription is simple and natural: Is it possible that your change (bug fix, new feature, whatever) could break ANYONE's code? If so, then a `<non-breaking>` bump is strictly out. Bump one of `<minor>` or `<major>`. Otherwise bump one of `<non-breaking>`, `<minor>`, or `<major>`. Choose whichever you think best characterizes the change. That's it!

> Note that even in the case of a bug fix, it's still possible for a change to be breaking if there exist people that may be relying on pre-fix faulty behaviour.

--------------------------------------------------------------------------------

Copyright &copy; 2012-2014 Peter Taoussanis. All rights reserved.