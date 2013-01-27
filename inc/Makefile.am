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
	gimage.h gio.h gkeysym.h gprogress.h gresedit.h gresource.h			\
	gwidget.h ustring.h utype.h annotations_base.h ffthread.h			\
	precomputed_polyspline_data.h contour_interface.h moveto_funcs.h	\
	invoke_funcs.h

nobase_include_HEADERS = sortsmill/attributes.h						\
	sortsmill/null_passthru.h sortsmill/xdie_on_null.h				\
	sortsmill/xgc.h sortsmill/gmp_gc.h sortsmill/gmp_constants.h	\
	sortsmill/linalg.h sortsmill/xunicase.h sortsmill/xuniconv.h	\
	sortsmill/xuninorm.h sortsmill/xunistr.h sortsmill/xunistring.h	\
	sortsmill/ps_number.h sortsmill/brentroot.h						\
	sortsmill/qbrentroot.h sortsmill/rexp.h sortsmill/polyspline.h	\
	sortsmill/usermenu.h sortsmill/fontforge_main.h

# FIXME: These headers need to be examined for relevance and for
# whether the functions they declare need to be moved into the
# auxiliary library. If they are declared here, they probably should
# be moved, rather than left in DLLs.
nobase_include_HEADERS += sortsmill/guile/polyspline.h				\
	sortsmill/guile/brentroot.h sortsmill/guile/iconv.h				\
	sortsmill/guile/gsl.h sortsmill/guile/usermenu.h				\
	sortsmill/guile/views.h sortsmill/guile/internal-structures.h	\
	sortsmill/guile/notices.h

nobase_include_HEADERS += sortsmill/guile/rnrs_conditions.h

if HAVE_PYTHON_API
nobase_include_HEADERS += sortsmill/__init__.pxd						\
	sortsmill/cython/__init__.pxd sortsmill/cython/const_pointers.pxd	\
	sortsmill/cython/brentroot.pxd										\
	sortsmill/cython/contour_interface.pxd sortsmill/cython/gmpy.pxi	\
	sortsmill/cython/guile.pxd sortsmill/cython/linalg.pxd				\
	sortsmill/cython/splinefont.pxd sortsmill/cython/xgc.pxd			\
	sortsmill/cython/config.pxi sortsmill/cython/gmpy.pxd				\
	sortsmill/cython/gsl.pxd sortsmill/cython/polyspline.pxd			\
	sortsmill/cython/usermenu.pxd
nobase_include_HEADERS += sortsmill/cython/internal/__init__.pxd	\
	sortsmill/cython/internal/__exec.pxd
endif HAVE_PYTHON_API

#--------------------------------------------------------------------------

extract_text = $(PERL) -n -e											\
	'print $$_ if (s|^\s*(//\|[*])\s*$(1)--\s*(.*)\s*|\2\n|)'

$(top_srcdir)/doc/%.visible.texi: sortsmill/%.h
	$(call extract_text,vis) $< > $@

extract-text: $(top_srcdir)/doc/ps_number.visible.texi

#--------------------------------------------------------------------------
