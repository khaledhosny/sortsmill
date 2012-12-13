# Contributed by Max Rabkin, 2008.
# Modified for Sorts Mill FontForge by Barry Schwartz, 2012.
#
# With this installed, Python exceptions not caught elsewhere will be
# reported through the fontforge user interface and not to stderr.

import sys
import traceback
import sortsmillff.notices

# FIXME: Eliminate this dependency.
import sortsmillff.ffcompat

def excepthook (*args):
    tb = ''.join (traceback.format_exception (*args))
    message = ''.join (traceback.format_exception_only (*args[:2])).strip ()

    sortsmillff.notices.log_fontforge_warning (tb.replace ('%', '%%'))
    sortsmillff.notices.post_fontforge_error ('Unhandled exception',
                                              message.replace ('%','%%'))

if sortsmillff.ffcompat.hasUserInterface ():
    # only install the hook if this session has a UI.
    sys.excepthook = excepthook
