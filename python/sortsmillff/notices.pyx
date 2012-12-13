# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

cimport sortsmillff.cython.const_pointers as constp

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

# FIXME: Put this stuff in a .pxd file.
cdef extern from "uiinterface.h":
  ctypedef struct ui_interface_t:
    void (*ierror) (constp.const_char_ptr fmt, ...)
    void (*post_error) (constp.const_char_ptr title, constp.const_char_ptr error, ...)
    void (*logwarning) (constp.const_char_ptr fmt, ...)
    void (*post_warning) (constp.const_char_ptr title, constp.const_char_ptr statement, ...)
    int (*ask) (constp.const_char_ptr title, constp.const_char_ptr *answers,
                int _def, int cancel, constp.const_char_ptr question, ...)
    int (*choose) (constp.const_char_ptr title, constp.const_char_ptr *answers,
                   int _def, int cancel, constp.const_char_ptr question, ...)
    int (*choose_multiple) (char *title, constp.const_char_ptr *choices, char *sel,
                            int cnt, char *buts[2], constp.const_char_ptr question, ...)
    char *(*ask_string) (constp.const_char_ptr title,
                         constp.const_char_ptr _def, constp.const_char_ptr question, ...)
    char *(*ask_password) (constp.const_char_ptr title,
                           constp.const_char_ptr _def, constp.const_char_ptr question, ...)
    char *(*open_file) (constp.const_char_ptr title, constp.const_char_ptr defaultfile,
                        constp.const_char_ptr initial_filter)
    char *(*saveas_file) (constp.const_char_ptr title, constp.const_char_ptr defaultfile,
                          constp.const_char_ptr initial_filter)
    void (*progress_start) (int delay, constp.const_char_ptr title, constp.const_char_ptr line1,
                            constp.const_char_ptr line2, int tot, int stages, bool has_stop)
    void (*progress_end) ()
    void (*progress_show) ()
    int (*progress_next) ()
    int (*progress_next_stage) ()
    int (*progress_increment) (int)
    void (*progress_change_line1) (constp.const_char_ptr )
    void (*progress_change_line2) (constp.const_char_ptr )
    void (*progress_pause) ()
    void (*progress_resume) ()
    void (*progress_change_stages) (int)
    void (*progress_change_total) (int)
    int  (*progress_reset) ()
    void (*allow_events) ()
    constp.const_char_ptr (*strid) (int)
    constp.const_char_ptr (*mslang) (int)
    int (*stroke_flags) ()

  ui_interface_t *ui_interface

def log_warning (message not None):
  try:
    msg = message.encode ('UTF-8')
  except:
    msg = message
  ui_interface.logwarning (msg)

def post_notice (title not None, message not None):
  try:
    titl = title.encode ('UTF-8')
  except:
    titl = title
  try:
    msg = message.encode ('UTF-8')
  except:
    msg = message
  ui_interface.post_warning (titl, msg)

def post_error (title not None, message not None):
  try:
    titl = title.encode ('UTF-8')
  except:
    titl = title
  try:
    msg = message.encode ('UTF-8')
  except:
    msg = message
  ui_interface.post_error (titl, msg)
