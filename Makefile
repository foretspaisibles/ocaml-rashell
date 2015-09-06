### Makefile -- Project Rashell

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

PACKAGE=		rashell
VERSION=		0.1.0
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:src
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:lwt.unix
EXTERNAL+=		ocaml.findlib:lwt.ppx
EXTERNAL+=		ocaml.findlib:str

CONFIGURE+=		Makefile.config.in
CONFIGURE+=		src/rashell_Configuration.ml.in

.include "generic.project.mk"

### End of file `Makefile'
