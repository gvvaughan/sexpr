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
 * See file [THANKS][] for a list of important infuences and
   contributors.

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

 * Sexpr development is co-ordinated from the [Sexpr project page][]
   at [github][].

 [sexpr project page]: http://github.com/gvvaughan/sexpr


## Official Release

We archive compressed tarballs of all [recent GNU Sexpr releases][releases].

Additionally, we sometimes upload compressed tarballs of
[unstable prereleases][alpha].

Official tarballs are supplied with a [GnuPG][] detached signature file
so that you can verify that the corresponding tarball is still the same
file that was released by the owner of its GPG key ID. First, be sure to
download both the .sig file and the corresponding release:

    wget http://ftpmirror.gnu.org/zile/zile-2.3.24.tar.gz
    wget http://ftpmirror.gnu.org/zile/zile-2.3.24.tar.gz.sig

then run a command like this:

    gpg --verify zile-2.3.24.tar.gz.sig

If that command fails because you don't have the required public key,
then run this command to import it:

    gpg --keyserver keys.gnupg.net --recv-keys 80EE4A00

and then rerun the `gpg --verify` command.

Generic instructions for how to build GNU Sexpr from a release tarball
are contained in the file [INSTALL][].

If you are missing any of the prerequisite libraries needed to
successfully build GNU Sexpr, the `configure` script will abort itself
and tell you right away.

 [alpha]: http://alpha.gnu.org/gnu/zile
 [gnupg]: http://www.gnupg.org/


## Development Sources

Sexpr development sources are maintained at the
[GNU Savannah git server][git]. You can fetch a read-only copy with
either:

    git clone git://git.sv.gnu.org/zile.git

or using the CVS pserver protocol:

    cvs -d:pserver:anonymous@pserver.git.sv.gnu.org:/srv/git/zile.git \
        co -d zile HEAD

If you are behind a firewall that blocks the git protocol, you can force
git to transparently rewrite all savannah references to use http:

    git config --global url.http://git.sv.gnu.org/r/.insteadof \
        git://git.sv.gnu.org/

When you are building GNU Sexpr from a git checkout, you first need to
run the `bootstrap` script to generate various files that are shipped in
release tarballs, but not checked in to git.

Normally, you just need to run `./bootstrap`, and it will either get
everything ready so that you can then run `./configure` as would for a
release tarball, or else tell you if your machine is missing some
packages that it needs in order to do that.

  [gitbrowser]: http://git.sv.gnu.org/cgit/zile.git


# REPORTING BUGS

If this distribution doesn't work for you, before you report the
problem, please try upgrading to the latest released version first, to
see whether your issue has been fixed already. If you can, please also
check whether the latest development sources for the next release still
exhibit the problem (see OBTAINING THE LATEST SOURCES above).

Please send bug reports, feature requests and patches to the
[bug mailing list], preferably, file them directly in the relevant
tracker at [GNU Savannah][zile project page].

When you are ready to submit a report, first, please read [this][bugs].

Sexpr has a suite of Lisp tests in the tests directory of the source
distribution, which you can run with:

    make check

If, when you report a bug, you can create a similar test that
demonstrates it, the maintainers will be most grateful, and it will
prevent them from accidentally reintroducing the bug in a subsequent
release.

 [bugs]:      http://www.chiark.greenend.org.uk/~sgtatham/bugs.html
 [bug-zile]:  mailto:bug-zile@gnu.org
