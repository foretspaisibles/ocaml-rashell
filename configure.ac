### configure.ac -- Autoconf for Rashell

# Rashell (https://github.com/michipili/rashell)
# This file is part of Rashell
#
# Copyright © 2015 Michael Grünewald
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, with linking exceptions;
# either version 3 of the License, or (at your option) any later
# version. See COPYING file for details.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_INIT([meta/rashell.in])
AC_CONFIG_MACRO_DIR([Library/Autoconf])
AC_NEED_OCAML
AC_NEED_FINDLIB
AC_WITH_OCAML_SITE_LIB
AC_NEED_FINDLIB_PKG([broken])
AC_NEED_FINDLIB_PKG([lwt.unix])
AC_NEED_FINDLIB_PKG([lwt.ppx])
AC_NEED_FINDLIB_PKG([str])

AC_PATH_PROG_REQUIRE([AWK], [awk])
AC_PATH_PROG_REQUIRE([CP], [cp])
AC_PATH_PROG_REQUIRE([FIND], [find])
AC_PATH_PROG_REQUIRE([GREP], [grep])
AC_PATH_PROG_REQUIRE([ID], [id])
AC_PATH_PROG_REQUIRE([LN], [ln])
AC_PATH_PROG_REQUIRE([MV], [mv])
AC_PATH_PROG_REQUIRE([RM], [rm])
AC_PATH_PROG_REQUIRE([SED], [sed])

AC_CONFIG_FILES([Makefile.config src/rashell_Configuration.ml])
AC_OUTPUT

### End of file `configure.ac'
