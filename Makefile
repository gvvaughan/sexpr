# Specl make rules.
# Written by Gary V. Vaughan, 2013
#
# Copyright (c) 2013-2014 Gary V. Vaughan
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

all:
	@echo 'No need to build anything, just run ./sexpr'

srcdir = .
abs_srcdir = `cd $(srcdir) && pwd`
check_local =

## ------------ ##
## Environment. ##
## ------------ ##

LUA_PATH  = ;

std_path = $(abs_srcdir)/lib/?.lua
SPECL_ENV  = LUA_PATH="$(std_path);$(LUA_PATH)"



## -------------- ##
## Documentation. ##
## -------------- ##


doc: doc-html

LDOC = ldoc

dist_doc_DATA +=					\
	$(srcdir)/doc/index.html			\
	$(srcdir)/doc/ldoc.css

dist_modules_DATA +=					\
	$(srcdir)/doc/modules/sexpr.lisp.html		\
	$(srcdir)/doc/modules/sexpr.primitive.html	\
	$(NOTHING_ELSE)

ldoc_DEPS =						\
	$(srcdir)/lib/sexpr/lisp.lua			\
	$(srcdir)/lib/sexpr/primitive.lua		\
	$(NOTHING_ELSE)

doc-html: $(dist_doc_DATA) $(dist_modules_DATA)

$(dist_doc_DATA) $(dist_classes_DATA) $(dist_modules_DATA): $(ldoc_DEPS)
	test -d "$(srcdir)/doc" || mkdir "$(srcdir)/doc"
	$(LDOC) -c build-aux/config.ld -d $(abs_srcdir)/doc .

## ------ ##
## Specs. ##
## ------ ##

specl_SPECS =						\
	$(srcdir)/specs/atom_spec.yaml			\
	$(srcdir)/specs/parser_spec.yaml		\
	$(srcdir)/specs/lisp_spec.yaml			\
	$(NOTHING_ELSE)


## ------ ##
## Specl. ##
## ------ ##

SPECL = specl

# Use 'make check V=1' for verbose output, or set SPECL_OPTS to
# pass alternative options to specl command.

SPECL_OPTS      =
SPECL_OPTS     += $(specl_verbose_$(V))
specl_verbose_  = $(specl_verbose_0)
specl_verbose_0 =
specl_verbose_1 = --verbose --formatter=report

check_local += specl-check-local
specl-check-local: $(specl_SPECS)
	$(SPECL_ENV) $(SPECL) $(SPECL_OPTS) $(specl_SPECS)


check: $(check_local)


## --------- ##
## LuaRocks. ##
## --------- ##

luarocks_config   = build-aux/luarocks-config.lua

set_LUA_BINDIR = LUA_BINDIR=`which $(LUA) |$(SED) 's|/[^/]*$$||'`
LUA_INCDIR = `cd $$LUA_BINDIR/../include && pwd`
LUA_LIBDIR = `cd $$LUA_BINDIR/../lib && pwd`

$(luarocks_config): Makefile.am
	@test -d build-aux || mkdir build-aux
	$(AM_V_GEN){						\
	  $(set_LUA_BINDIR);					\
	  echo 'rocks_trees = { "$(abs_srcdir)/luarocks" }';	\
	  echo 'variables = {';					\
	  echo '  LUA = "$(LUA)",';				\
	  echo '  LUA_BINDIR = "'$$LUA_BINDIR'",';		\
	  echo '  LUA_INCDIR = "'$(LUA_INCDIR)'",';		\
	  echo '  LUA_LIBDIR = "'$(LUA_LIBDIR)'",';		\
	  echo '}';						\
	} > '$@'
