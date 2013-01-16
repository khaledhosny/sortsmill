# -*- coding: utf-8; python-indent: 2 -*-
#
# Free FontForge to dump a core image, regardless of the setting in
# one’s shell.
#
# Put ‘import ffContrib.allow_core’ in your user_init.py file.
#
# To use a core file, say something like ‘gdb fontforge core’
# or ‘gdb -c core fontforge’.
#
# NOTE: This module is unnecessary. You can instead just put
#
#   (setrlimit 'core #f #f)
#
# in your user-init.scm file.

import resource

resource.setrlimit(resource.RLIMIT_CORE, (-1, -1))
