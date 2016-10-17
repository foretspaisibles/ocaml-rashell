### Makefile -- Rashell example

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

PROGRAM=		example

.if empty(WITH_DOCKER:Mno)
PROGRAM+=		example_docker
.endif

.if empty(WITH_GIT:Mno)
PROGRAM+=		example_git
.endif

install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
