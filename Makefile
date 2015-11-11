### Makefile -- Project Rashell

# Rashell (https://github.com/michipili/rashell)
# This file is part of Rashell
#
# Copyright © 2015 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PACKAGE=		rashell
VERSION=		0.2.1-current
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:src
MODULE+=		ocaml.prog:example
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:lwt.unix
EXTERNAL+=		ocaml.findlib:lwt.ppx
EXTERNAL+=		ocaml.findlib:mixture
EXTERNAL+=		ocaml.findlib:str
EXTERNAL+=		ocaml.findlib:atdgen

CONFIGURE+=		Makefile.config.in
CONFIGURE+=		src/rashell_Configuration.ml.in

.include "generic.project.mk"

### End of file `Makefile'
