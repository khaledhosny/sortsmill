# Contributed by Max Rabkin, 2008.
# Modified for Sorts Mill FontForge by Barry Schwartz, 2012.
#
# With this installed, Python exceptions not caught elsewhere will be
# reported through the fontforge user interface and not to stderr.

import sys
import traceback
import sortsmill.notices

# FIXME: Eliminate this dependency.
import sortsmill.ffcompat

def excepthook (*args):
    tb = ''.join (traceback.format_exception (*args))
    message = ''.join (traceback.format_exception_only (*args[:2])).strip ()

    sortsmill.notices.log_fontforge_warning (tb.replace ('%', '%%'))
    sortsmill.notices.post_fontforge_error ('Unhandled exception',
                                              message.replace ('%','%%'))

if sortsmill.ffcompat.hasUserInterface ():
    # only install the hook if this session has a UI.
    sys.excepthook = excepthook
