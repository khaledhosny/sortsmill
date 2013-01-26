# Copyright (C) 2012, 2013 by Barry Schwartz
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#--------------------------------------------------------------------------

dist_man_MANS =
nodist_man_MANS =

DISTCLEANFILES = $(nodist_man_MANS)

info_TEXINFOS = sortsmill-tools.texi
sortsmill_tools_TEXINFOS = psMat.interface.texi	\
	ps_number.visible.texi

if THE_PROGRAMS

dist_man_MANS += sortsmill-lint.1 sortsmill-image.1 sortsmill-compare.1
nodist_man_MANS += smlint.1 smimage.1 smcompare.1

smlint.1: Makefile
	echo '.so man1/sortsmill-lint.1' > $@

smimage.1: Makefile
	echo '.so man1/sortsmill-image.1' > $@

smcompare.1: Makefile
	echo '.so man1/sortsmill-compare.1' > $@

if HAVE_GUI

dist_man_MANS += sortsmill-editor.1
nodist_man_MANS += smed.1

smed.1: Makefile
	echo '.so man1/sortsmill-editor.1' > $@

endif HAVE_GUI

endif THE_PROGRAMS
#--------------------------------------------------------------------------

