### Makefile -- Rashell testsuite

# Rashell (https://github.com/michipili/rashell)
# This file is part of Rashell
#
# Copyright © 2015—2016 Michael Grünewald
#
# This file must be used under the terms of the MIT license.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at
# https://opensource.org/licenses/MIT

PROGRAM=		unit_testing

SRCS=			toolbox.ml
SRCS+=			testCommand.ml
SRCS+=			testMktemp.ml
SRCS+=			testPosix.ml
SRCS+=			testTimestamp.ml
SRCS+=			main.ml

OCAMLLFLAGS+=		-linkall

do-test: ${PROGRAM}
	${.ALLSRC:tA}

do-install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
