> This project uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md) as of **Aug 16, 2014**.

## v1.10.0 / 2014 Sep 30

 * General housekeeping.
 * **DEPRECATED**: `nnil-keys?` -> `keys-nnil?`, `set-exp-backoff-timeout!`, `repeatedly*`, `first-nth`.
 * **NEW**: `zero-num?`, `pos-num?`, `nneg-num?`, `vec*`, `abs`, `qbench` now works in Cljs.
 * **CHANGE**: `qbench` can now compare multiple forms.


## v1.9.4 / 2014 Sep 24

 * **NEW**: Make `set*` public.
 * **FIX**: Cljs `parse-int`, `as-int` now correctly use decimal radix (@favila).


## v1.9.3 / 2014 Sep 23

 * **NEW**: `asserted` util.


## v1.9.2 / 2014 Sep 22

 * Temporary (?) workaround for http://goo.gl/3At6xz, http://goo.gl/gAqxad.


## v1.9.1 / 2014 Sep 17

 * **NEW**: Add `kw-identical?` util, use internally to fix a number of bugs caused by http://goo.gl/be8CGP.


## v1.9.0 / 2014 Sep 11

 * **BREAKING**: `swap-in!` extra arity (for `apply`) has been dropped.
 * **CHANGE**: `as-map` now returns nil->nil (previously threw on nils).
 * **CHANGE**: map transforms now return {} on nil inputs (previously returned nil).
 * **NEW**: Experimental `replace-in`, bulk `swap-in!`+`reset-in!`.


## v1.8.3 / 2014 Sep 10

 * **CHANGE**: Cljs `format` now treats undefined args as `nil`s.


## v1.8.2 / 2014 Sep 9

 * **NEW**: Add utils `nnil=`, `have`.


## v1.8.1 / 2014 Sep 7

 * **FIX**: https://github.com/ptaoussanis/timbre/issues/79.


## v1.8.0 / 2014 Sep 2

 * **BREAKING**: Cleaned up `ajax-lite` API for inclusion as public util in Sente.


## v1.7.3 / 2014 Sep 1

 * Add remaining cljs logging fns.
 * `uuid-str` now has an extra arity for trimming.


## v1.7.2 / 2014 Aug 31

 * Add `atom?` and key utils.


## v1.7.1 / 2014 Aug 21

 * Add some simple (Cljs) logging-level controls.


## v1.7.0 / 2014 July 4

 * Bump some dependencies.
 * **BREAKING**: drop `compiling-cljs?*`, `compiling-cljs?` macros in favor of `if-cljs`.


## v1.6.0 / 2014 May 8

 * **NEW**: `ajax-lite` now returns XHR instance on success.


## v1.5.1 / 2014 May 7

 * [#1] **FIX**: missing tools.reader dependency (@ducky427).
 * **NEW**: Add `qbench` macro.


## v1.5.0 / 2014 May 2

 * **NEW**: Validation utils.
 * **NEW**: Add `nvec?`.
 * **FIX**: `nblank-str?`.
 * **FIX**: `interleave-all`.


## v1.4.0 / 2014 Apr 17

 * **NEW**: Add `nblank-str?`.
 * **BREAKING**: Further simplify `swap-in!`, add support for arbitrary return values. See commit for details.


## v1.3.1 / 2014 Apr 10

 * **FIX** broken Clojure <1.5 support by using correct (tools.reader) edn reader (@kul).


## v1.3.0 / 2014 Apr 8

 * Housekeeping: `swap!` stuff.
 * Rewrote memoization utils for lock-free anti-contention (better performance under high contention).


## v1.2.1 / 2014 Apr 1

 * **FIX**: `ajax-lite` catch typo.
 * **FIX**: `ajax-lite` wrong (auto) response type for `text/html`.


## v1.2.0 / 2014 Mar 30

 * **NEW**: `compiling-cljs?` utils.
 * **FIX**: nix accidental `declare-remote` println.


## v1.1.0 / 2014 Mar 28

 * **NEW**: `swap-in!`, `reset-in!`, `dissoc-in`, `contains-in?`, `assoc-some`, `assoc-when` (all cross-platform).


## v1.0.0 / 2014 Mar 26

 * Minor housekeeping, **v1.0.0 release**.


## v0.9.9 / 2014 Mar 15

 * **NEW**: Add `swap!*`, `reset!*` fns (cross-platform).
 * **NEW**: Add `substr` fn (cross-platform).
 * **DEPRECATED**: `str-trunc` (`substr` is a more general form).


## v0.9.8 / 2014 Mar 10

 * **FIX**: `ajax-lite` broken response type detection.


## v0.9.7 / 2014 Mar 8

 * **NEW**: Add `pow` fn (cross-platform).
 * **NEW**: Add some Ring utils (clj only).


## v0.9.6 / 2014 Mar 7

 * **NEW**: Add `simple-date-format` util (clj only).


## v0.9.5 / 2014 Mar 6

 * **NEW**: Add clj-side `format` fn for easier cross-platform use.
 * **NEW**: Add clj(s)-side `nblank?` fns.
 * **NEW**: Add `round*` alias.
 * **NEW**: Add `now-udt-mock-fn`, `make-timestamp-fn`.


## v0.9.4 / 2014 Feb 28

 * **FIX**: Faulty `str-trunc` (clj). Regression from v0.9.1.
 * **NEW**: Add separate parse-/as- coercion fns.


## v0.9.3 / 2014 Feb 27

 * **FIX**: `as-map` post-condition: can return nil.


## v0.9.2 / 2014 Feb 26

 * **FIX**: project.clj to prevent unnecessary downstream deps.


## v0.9.1 / 2014 Feb 25

 * **FIX**: Faulty cljs utils: `ajax-lite`, `str-ends-with?`, `str-trunc`.


## v0.9.0 / 2014 Feb 24

 * **NEW**: Added simple Closure-based client Ajax util for use by Sente.


## v0.8.0 / 2014 Feb 23

 * **NEW**: Initial public release.
