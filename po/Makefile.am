# Copyright (C) 2000-2012 by George Williams
# Copyright (C) 2012 by Barry Schwartz
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

PO_FILES = ca.po el.po es.po it.po pl.po vi.po zh_TW.po de.po	\
	en.po en_GB.po fr.po ja.po ml.po ru.po uk.po zh_CN.po

MO_FILES = ca.mo el.mo es.mo it.mo pl.mo vi.mo zh_TW.mo de.mo	\
	en.mo en_GB.mo fr.mo ja.mo ml.mo ru.mo uk.mo zh_CN.mo

.po.mo:
	$(MSGFMT) --check --output-file=$@ $<

all-local: $(MO_FILES)

install-data-local:
	(for localefile in $(MO_FILES); do \
		locale=`echo "$${localefile}" | $(SED) -e 's/\.mo$$//'`; \
		$(MKDIR_P) "$(DESTDIR)$(localedir)/$${locale}/LC_MESSAGES"; \
		$(INSTALL_DATA) "$${localefile}" "$(DESTDIR)$(localedir)/$${locale}/LC_MESSAGES/$(FF_TEXTDOMAIN).mo"; \
	done)

uninstall-local:
	(for localefile in $(MO_FILES); do \
		locale=`echo "$${localefile}" | $(SED) -e 's/\.mo$$//'`; \
		$(MKDIR_P) "$(DESTDIR)$(localedir)/$${locale}/LC_MESSAGES"; \
		rm -f "$(DESTDIR)$(localedir)/$${locale}/LC_MESSAGES/$(FF_TEXTDOMAIN).mo"; \
	done)

EXTRA_DIST = $(PO_FILES)
MOSTLYCLEANFILES = $(MO_FILES)

#--------------------------------------------------------------------------

include $(top_builddir)/mk/xgettext_search.mk

XGETTEXT_OPTIONS = --keyword=_ --keyword=N_ --keyword=S_ --keyword=U_	\
--keyword=NU_ --keyword=P_:1,2 --add-comments=GT: --from-code=utf-8		\
--language=C --sort-by-file

# This potfile will be created in $(builddir). You can copy it to
# $(srcdir) if you do outside-of-source builds and want it in the
# source tree.
MY_POTFILE = $(FF_TEXTDOMAIN).pot

$(MY_POTFILE): potfile

potfile:
	@echo Creating $(MY_POTFILE)...
	@$(XGETTEXT) $(XGETTEXT_OPTIONS) --output=$(MY_POTFILE) $(xgettext_search_files)
	@$(SED) -i -e '1,4d' $(MY_POTFILE)
	@$(SED) -i -e "1i\
\# Template of user interface strings for $(PACKAGE_NAME), in US English.\n\
\# Copyright (C) 2000-2012 by George Williams.\n\
\# This file is distributed under the same license as the $(PACKAGE_NAME) package.\n\
\# $(PACKAGE_NAME), <$(PACKAGE_URL)>, 2012." $(MY_POTFILE)

#--------------------------------------------------------------------------
#
# An aid for making British English translation files. Few people will
# have use for this.

include $(top_srcdir)/mk/flags.am

EXTRA_PROGRAMS = toengb
toengb_SOURCES = toengb.c
toengb_LDADD = $(top_builddir)/lib/libgnu.la

DISTCLEANFILES = toengb

#--------------------------------------------------------------------------
