### release.mk -- Configuration for releases

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

.if !empty(THISMODULE:Mocaml.lib)
COMPILE=		byte_code
COMPILE+=		native_code
.endif

.if !empty(THISMODULE:Mocaml.prog)
COMPILE+=		native_code
.endif

### End of file `release.mk'
