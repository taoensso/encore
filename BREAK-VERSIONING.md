Will try keep this quick+readable.

[Semantic Versioning](http://semver.org/) has some practical issues:

  1. The spec's too long.
  2. It doesn't recognise that **software breaks are common** and **not all equal**. It'd be nice to distinguish between a major and minor breakage.
  3. It doesn't recognise **psychological/marketing pressures**. People strongly resist bumping major version numbers for every breaking change. And sometimes they want to indicate that a change is somehow "significant" even when it's not breaking.
  4. There's **anarchy pre-v1.0.0**. And (2,3) mean that code tends to linger in pre-v1.0.0 for too long.

Net effect of these issues: most people don't actually use SemVer consistently[1], undermining the whole point of having a spec in the first place.

> [1] If people were using SemVer as per the spec, you'd be seeing a lot more code at version `37.y.z`. If you've ever released code with ANY breaking change without bumping your major version number, you've technically violated the spec and potentially surprised someone with a breaking update that they weren't expecting.

## Break Versioning (BreakVer)

As an alternative, I'm suggesting the following trivial scheme which I'll be using for my own projects in future:

```
<major>.<minor>.<non-breaking>[-<optional-qualifier>]:
------------------------------------------------------
<major>              - Major breaking changes [or discretionary "major non-breaking changes"]
<minor>              - Minor breaking changes [or discretionary "minor non-breaking changes"]
<non-breaking>       - **Strictly** (!!) NO breaking changes, ever
<optional-qualifier> - Tag-style qualifiers: -alpha1, -RC2, etc.
```

This is intented to be **comfortable to follow strictly**, so more likely to be **reliable in practice**:

Bump             | Can infer                                          |
---------------- | -------------------------------------------------- |
`<non-breaking>` | Safe upgrade, always - just do it                  |
`<minor>`        | Might break code in a minor way, check changelog   |
`<major>`        | Might break code in a major way, check changelog!! |

Notice that this scheme emphasizes the _maximum_ amount of damage a version update could inflict. There's only two types of version bumps: those that are **definitely safe**, and those that **require you to check the changelog**.

#### I want to use BreakVer. Which number should I bump?

Is it possible that your change (bug fix, new feature, whatever) could break ANYONE's code? If so, then a `<non-breaking>` bump is **strictly out**. Bump one of `<minor>` or `<major>` (your choice). Otherwise bump one of `<non-breaking>`, `<minor>`, or `<major>` (your choice).

> Note that even in the case of a bug fix, it's still possible for a change to be breaking if there exist people that may be relying on pre-fix faulty behaviour

--------------------------------------------------------------------------------

Copyright &copy; 2012-2015 Peter Taoussanis. All rights reserved.