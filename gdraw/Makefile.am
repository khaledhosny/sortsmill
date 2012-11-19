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

include $(top_srcdir)/mk/flags.am

lib_LTLIBRARIES = libsortsmillff_gdraw.la

libsortsmillff_gdraw_la_SOURCES = choosericons.c ctlvalues.c			\
	drawboxborder.c gaskdlg.c gbuttons.c gcolor.c gcontainer.c			\
	gdraw.c gdrawbuildchars.c gdrawerror.c gdrawtxt.c gfilechooser.c	\
	gfiledlg.c ggadgets.c ggroupbox.c gimageclut.c gimagecvt.c			\
	gdrawgimage.c gkeysym.c glist.c gmenu.c gprogress.c gradio.c		\
	gresource.c gresourceimage.c gresedit.c gsavefiledlg.c				\
	gscrollbar.c gtabset.c gtextfield.c gtextinfo.c gwidgets.c			\
	gxdraw.c gxcdraw.c ghvbox.c gmatrixedit.c gdrawable.c gspacer.c		\
	xkeysyms_unicode.c colorP.h gdrawP.h gresourceP.h		\
	gxcdrawP.h fontP.h ggadgetP.h gwidgetP.h gxdrawP.h

libsortsmillff_gdraw_la_LIBADD =					\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/gutils/libsortsmillff_gutils.la	\
	$(top_builddir)/lib/libgnu.la
libsortsmillff_gdraw_la_LDFLAGS = $(call ldflags)
libsortsmillff_gdraw_la_DEPENDENCIES = Makefile $(libsortsmillff_gdraw_la_LIBADD)

#--------------------------------------------------------------------------
