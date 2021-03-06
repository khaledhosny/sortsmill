# -*- tab-width: 4 -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# Copyright (C) 2000-2012 by George Williams
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

ACLOCAL_AMFLAGS = -I m4 -I m4_ff --install

include $(top_srcdir)/mk/layout.am

#--------------------------------------------------------------------------

SUBDIRS = mk lib inc auxiliary gutils

if HAVE_GUI
SUBDIRS += gdraw data
endif HAVE_GUI

SUBDIRS += fontforge guile

if THE_PROGRAMS
if HAVE_GUI
SUBDIRS += main
endif HAVE_GUI
endif THE_PROGRAMS

SUBDIRS += po doc htdocs

if THE_PROGRAMS
SUBDIRS += sfd2sxml
endif THE_PROGRAMS

if HAVE_PYTHON_API
SUBDIRS += python
endif HAVE_PYTHON_API

#if HAVE_FORTRAN_API
#SUBDIRS += fortran
#endif HAVE_FORTRAN_API

#if HAVE_PURE_API
#SUBDIRS += pure
#endif HAVE_PURE_API

if FONTTOOLS
SUBDIRS += fonttools
endif FONTTOOLS

if EXAMPLES
SUBDIRS += examples
endif EXAMPLES

SUBDIRS += tests

EXTRA_DIST = \
	m4/gnulib-cache.m4 INSTALL.git .dir-locals.el	\
	.indent.pro fontforge/.indent.pro fonttools/.indent.pro			\
	fortran/.indent.pro gdraw/.indent.pro guile/.indent.pro			\
	gutils/.indent.pro inc/.indent.pro								\
	inc/sortsmill/guile/.indent.pro inc/sortsmill/.indent.pro		\
	main/.indent.pro pure/.indent.pro tests/.indent.pro

uninstall-local:
	(cd data; $(MAKE) $(AM_MAKEFLAGS) uninstall)

distclean-local:
	rm -f config-summary
	if test '$(top_srcdir)' -ef '$(top_builddir)'; then			\
		:;														\
	else														\
		for dir in . inc inc/sortsmill							\
				inc/sortsmill/guile								\
				inc/sortsmill/guile/math						\
				auxiliary gutils gdraw fontforge pyhook main	\
				tests fonttools examples; do					\
             rm -f "$(top_builddir)/$${dir}/.indent.pro";		\
		done;													\
	fi

#--------------------------------------------------------------------------

extract-text:
	for dir in inc python; do \
		(cd "$${dir}"; $(MAKE) $(AM_MAKEFLAGS) extract-text); \
	done

#--------------------------------------------------------------------------

@includemk@ $(top_srcdir)/mk/xgettext_search.mk

XGETTEXT_OPTIONS = --keyword=H_ --from-code=utf-8 --language=C	\
--sort-by-file

# This potfile will be created in $(builddir).
MY_POTFILE = $(FF_SHORTCUTSDOMAIN).pot

$(MY_POTFILE): shortcuts-template

shortcuts-template:
	@echo ''
	@echo Creating the template file $(MY_POTFILE) ...
	@$(XGETTEXT) $(XGETTEXT_OPTIONS) --output=$(MY_POTFILE) $(xgettext_search_files)
	@echo Editing the file ...
	@$(SED) -i -e '1,4d' $(MY_POTFILE)
	@$(SED) -i -e "1i\
\# Menu shortcuts for $(PACKAGE_NAME).\n\
\# Copyright (C) 2000-2012 by George Williams.\n\
\# This file is distributed under the same license as the $(PACKAGE_NAME) package.\n\
\# The FontForge project, <https://github.com/fontforge/fontforge>, 2012." $(MY_POTFILE)
	@$(SED) -i -e 's|^\("Content-Type: text/plain; charset=\)CHARSET\(\\n"\)|\1UTF-8\2|' $(MY_POTFILE)
	@$(SED) -i -e '/^"Content-Transfer-Encoding: 8bit\\n"/a\
\
\#. This file allows you to change FontForge’s menu shortcuts. It is a gettext\
\#.  based system, which means it is locale dependent.\
\#.\
\#. These entries are formatted as follows:\
\#.  First we have the (English) name of the command as it appears in\
\#.      the menu.\
\#.  Then we have a "|" (which is used as a separator)\
\#.  Finally we have the shortcut for the command as a textual string.\
\#.\
\#. If you wish to change a shortcut simply provide its textual representation\
\#.  in the msgstr field. So if you want the shortcut for New Outline Window\
\#.  to be Ctl+O\
\#.        msgid "New Outline Window|Ctl+H"\
\#.        msgstr "Ctl+O"\
\#. (NOTE: You should NOT include "New Outline Window|" in your msgstr)\
\#. If you wish to remove a shortcut\
\#.        msgid "New Outline Window|Ctl+H"\
\#.        msgstr "No Shortcut"\
\#. If you wish to use a function key (or similar)\
\#.        msgid "New Outline Window|Ctl+H"\
\#.        msgstr "F2"\
\#. or\
\#.        msgid "New Outline Window|Ctl+H"\
\#.        msgstr "Backspace"\
\#.\
\#. You may apply one or several modifiers to shortcuts. FontForge recognizes:\
\#.   Ctl+   Shft+    Alt+   CapsLock+\
\#. Some keyboards have additional modifier keys, X maps them to a flag bit in\
\#.  the keyboard state mask of the XKeyEvent. If you want to use these modifiers\
\#.  you need to know what that bit is. FontForge will also recognize:\
\#.    Flag0x01+   Flag0x02+   Flag0x04+  Flag0x08+\
\#.    Flag0x10+   Flag0x20+   Flag0x40+  Flag0x80+\
\#. (I can’t come up with better names than these because in general\
\#.  I don’t know what they are myself).\
\#. The mac has two additional modifier keys: Command and Option\
\#.  X never gets the Command key (or rather the X application’s menubar gets\
\#.  it and client programs never see it) so we can’t use it.\
\#.  But clients can get at the Option key and FontForge recognizes\
\#.     Opt+\
\#.\
\#. So...\
\#.        msgid "New Outline Window|Ctl+H"\
\#.        msgstr "Ctl+Shft+Alt+Flag10+Opt+F2"\
\#. would be practically untypeable, but syntactically valid.\
\#.\
\#. When FF displays a shortcut in the menu it will use a similar syntax.\
\#. That may not be appropriate for non-English terminals. On a French system\
\#.  it might be better to show the Shift modifier as Majuscule. At the bottom of\
\#.  this file are several entries which are not used to set shortcuts but are\
\#.  used in displaying the shortcuts in the menu, thus\
\#.      #: ../gdraw/gmenu.c:120\
\#.      msgid "Shft+"\
\#.      msgstr "Maj+"\
\#.\
\#. Even though not obvious from this file, it is also possible to add names for\
\#.  special keys. So you could add:\
\#.      msgid "Escape"\
\#.      msgstr "Échappe"\
\#.      msgid "Delete"\
\#.      msgstr "Efface"\
\#. (or whatever is appropriate for keyboards)\
\#.\
\#. The English names accepted are those defined under XK_MISCELLANY in\
\#.  keysymdef.h of the X11 header files -- without the initial XK_\
\#.\
\#.\
\#. I am told that the use of "|" to provide contextual information in a\
\#. gettext string is non-standard. However it is documented in section\
\#. 10.2.6 of http://www.gnu.org/software/gettext/manual/html_mono/gettext.html\
\#. I need it, and I will use it.\
\#.\
\#. --------------------------------------------------------------------------\
' $(MY_POTFILE)
	@echo Done.

#--------------------------------------------------------------------------

CYCLO_SOURCEFILES = $(top_srcdir)/inc/*.[ch]					\
	$(top_srcdir)/auxiliary/*.[ch] $(top_srcdir)/gutils/*.[ch]	\
	$(top_srcdir)/gdraw/*.[ch] $(top_srcdir)/fontforge/*.[ch]

cyclo-tools.html: $(CYCLO_SOURCEFILES)
	$(PMCCABE) $(CYCLO_SOURCEFILES) \
		| sort -n -r \
		| $(AWK) -f $(top_srcdir)/config/pmccabe2html \
			-v lang=html \
			-v name="$(PACKAGE_NAME)" \
			-v vcurl="http://bitbucket.org/sortsmill/sortsmill-tools/raw/master/%FILENAME%" \
			-v url="http://bitbucket.org/sortsmill/sortsmill-tools" \
			-v css=$(top_srcdir)/config/pmccabe.css \
			-v cut_dir=$(top_srcdir)/ \
			> $@-tmp
	mv $@-tmp $@

#--------------------------------------------------------------------------

MAINTAINERCLEANFILES = \
  $(GITIGNORE_MAINTAINERCLEANFILES_TOPLEVEL) \
  $(GITIGNORE_MAINTAINERCLEANFILES_M4_LIBTOOL) \
  $(GITIGNORE_MAINTAINERCLEANFILES_MAKEFILE_IN)

-include $(top_srcdir)/git.mk
