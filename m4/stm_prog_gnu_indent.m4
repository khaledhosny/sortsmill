# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 1

# StM_PROG_GNU_INDENT
# -------------
#
# Set GNU_INDENT to the path of the first GNU indent in the PATH, or
# to an empty string if GNU indent is not found. The result is cached
# in ac_cv_path_GNU_INDENT. The test may be overridden by setting
# GNU_INDENT or the cache variable.
#
AC_DEFUN([StM_PROG_GNU_INDENT],[
   StM_PATH_PROGS_CACHED_AND_PRECIOUS([GNU_INDENT],[GNU implementation of indent],
      [indent gindent],[
         # Run a command that gnu_indent, but neither mawk nor nawk, can execute.
         if LC_ALL=C LANG=C ${ac_path_GNU_INDENT} --gnu-style --ignore-profile 2>&1 > conftest.out <<EOF; then
int func(void){if(1){2;}}
EOF
            if diff conftest.out - 2>&1 > /dev/null <<EOF; then
int
func (void)
{
  if (1)
    {
      2;
    }
}
EOF
               ac_cv_path_GNU_INDENT=${ac_path_GNU_INDENT}
               ac_path_GNU_INDENT_found=:
            fi
         fi
         rm -f conftest.out
      ])
])
