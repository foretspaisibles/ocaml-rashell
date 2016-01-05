### Makefile -- Rashell library

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

LIBRARY=		rashell

SRCS=			rashell_Command.ml
SRCS+=			rashell_Timestamp.ml
SRCS+=			rashell_Configuration.ml
SRCS+=			rashell_Posix.ml
SRCS+=			rashell_Mktemp.ml
SRCS+=			rashell_Broken.ml

.if empty(WITH_DOCKER:Mno)
SRCS+=			rashell_Docker_t.ml
SRCS+=			rashell_Docker_j.ml
SRCS+=			rashell_Docker.ml
ATDGEN+=		rashell_Docker.atd
.endif

#
# Prepare ADT serialisers
#

.for atdgen in ${ATDGEN:.atd=}
${atdgen}_t.ml ${atdgen}_t.mli: ${atdgen}.atd
	atdgen -o ${.TARGET:C/_t.mli?//} -t ${.ALLSRC}

${atdgen}_j.ml ${atdgen}_j.ml: ${atdgen}.atd
	atdgen -o ${.TARGET:C/_j.mli?//} -j -j-std ${.ALLSRC}

CLEANFILES+=		${atdgen}_t.mli
CLEANFILES+=		${atdgen}_t.ml
CLEANFILES+=		${atdgen}_j.mli
CLEANFILES+=		${atdgen}_j.ml
.endfor


.if empty(WITH_GIT:Mno)
SRCS+=			rashell_Git.ml
.endif

.include "ocaml.lib.mk"

### End of file `Makefile'
