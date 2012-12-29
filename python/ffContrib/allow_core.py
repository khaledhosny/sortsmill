# -*- coding: utf-8; python-indent: 2 -*-
#
# Free FontForge to dump a core image, regardless of the setting in
# one’s shell.
#
# Put ‘import ffContrib.allow_core’ in your user_init.py file.

import resource

resource.setrlimit(resource.RLIMIT_CORE, (-1, -1))
