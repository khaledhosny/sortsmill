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

include $(top_srcdir)/mk/layout.am

noinst_HEADERS = config.h config-extra.h intl.h basics.h gmpy.h			\
	null_passthru.h xunistring.h chardata.h gdraw.h gfile.h ggadget.h	\
	gicons.h gimage.h gio.h gkeysym.h gprogress.h gresedit.h			\
	gresource.h gwidget.h ustring.h utype.h annotations_base.h			\
	ffthread.h precomputed_polyspline_data.h contour_interface.h

nobase_include_HEADERS = sortsmillff/attributes.h			\
	sortsmillff/null_passthru.h sortsmillff/xdie_on_null.h	\
	sortsmillff/xgc.h sortsmillff/gmp_gc.h					\
	sortsmillff/gmp_constants.h sortsmillff/linalg.h		\
	sortsmillff/xunicase.h sortsmillff/xuniconv.h			\
	sortsmillff/xuninorm.h sortsmillff/xunistr.h			\
	sortsmillff/xunistring.h sortsmillff/ps_number.h		\
	sortsmillff/brentroot.h sortsmillff/qbrentroot.h		\
	sortsmillff/rexp.h sortsmillff/polyspline.h

nobase_include_HEADERS += sortsmillff/guile/polyspline.h	\
	sortsmillff/guile/brentroot.h sortsmillff/guile/iconv.h	\
	sortsmillff/guile/gsl.h

#--------------------------------------------------------------------------

extract_text = $(PERL) -n -e											\
	'print $$_ if (s|^\s*(//\|[*])\s*$(1)--\s*(.*)\s*|\2\n|)'

$(top_srcdir)/doc/%.visible.texi: sortsmillff/%.h
	$(call extract_text,vis) $< > $@

extract-text: $(top_srcdir)/doc/ps_number.visible.texi

#--------------------------------------------------------------------------
