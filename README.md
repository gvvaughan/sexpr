# Sexpr

Sexpr is free software, licensed under the GNU GPL.

Copyright (c) 2013-2014 Free Software Foundation, Inc.

**Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.**


## INTRODUCTION

Sexpr is a Lisp parser and evaluator implemented in Lua.

It will replace the toy Lisp implementation in [GNU Zile][] at some
point, and so is modelled after [Emacs Lisp][]. It does have a macro
facility and flexible enough design that tweaking it to implement, say,
a scheme environment would be quite straight forward.

Sexpr is written in Lua using [lua-stdlib][].  It uses [Specl][] to
check that it meets specifications, and has [LDoc][] comments that can
be used to generate API documentation.

 [emacs lisp]: http://www.emacswiki.org/emacs/EmacsLisp
 [gnu zile]:   http://gnu.org/s/zile
 [ldoc]:       http://stevedonovan.github.io/ldoc/
 [lua-stdlib]: http://rrthomas.github.io/lua-stdlib/
 [sexpr]:      http://github.com/gvvaughan/sexpr/
 [specl]:      http://gvvaughan.github.io/specl/


### Screenshots

Nothing much to see here, but screenshots are always popular:

```
$ ./sexpr
sexpr version 0  Copyright (C) 2014 Gary V. Vaughan
enter ':q' to exit
> _
```


## Source Layout

Run the [Sexpr][] REPL from the source tree with: `./sexpr`.  There's
nothing to configure or build first, although there's a `Makefile` for
running specification checks, and building documentation.

 * See file [AUTHORS][] for the names of maintainers past and present.
 * See file [COPYING][] for copying conditions.
 * See file [THANKS][] for a list of important infuences and contributors.
 * Directory [build-aux][] contains helper scripts used to build Sexpr.
 * Directory [lib][] contains the source code for Sexpr.
 * Directory [specs][] contains the Sexpr specifications for [Specl][]

 [authors]:   http://github.com/gvvaughan/sexpr/blob/master/AUTHORS
 [build-aux]: http://github.com/gvvaughan/sexpr/tree/master/build-aux
 [copying]:   http://github.com/gvvaughan/sexpr/blob/master/COPYING
 [lib]:       http://github.com/gvvaughan/sexpr/tree/master/lib/sexpr
 [specs]:     http://github.com/gvvaughan/sexpr/tree/master/specs
 [thanks]:    http://github.com/gvvaughan/sexpr/blob/master/THANKS


## Web Pages

 * Sexpr development is co-ordinated from the [Sexpr project page][sexpr]
   at [github][].

 [github]: http://github.com/


# REPORTING BUGS

If this distribution doesn't work for you, before you report the
problem, please try upgrading to the latest released version first, to
see whether your issue has been fixed already. If you can, please also
check whether the latest development sources for the next release still
exhibit the problem.

Please file bug reports, feature requests and patches as
[github issues][issues].

When you are ready to submit a report, first, please read [this][bugs].

Sexpr has a suite of [Specl][] specifications in the [specs] directory
of the source distribution, which you can run with:

```
    make check
```

If, when you report a bug, you can create a spec example that
demonstrates it, the maintainers will be most grateful, and it will
prevent them from accidentally reintroducing the bug in a subsequent
release.

 [bugs]:      http://www.chiark.greenend.org.uk/~sgtatham/bugs.html
 [issues]:    http://github.com/gvvaughan/sexpr/issues
