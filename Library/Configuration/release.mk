### release.mk -- Configuration for releases

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

.if !empty(THISMODULE:Mocaml.lib)
COMPILE=		byte_code
COMPILE+=		native_code
.endif

.if !empty(THISMODULE:Mocaml.prog)
COMPILE+=		native_code
.endif

### End of file `release.mk'
