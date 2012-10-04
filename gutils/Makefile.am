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

lib_LTLIBRARIES = libsortsmillff_gutils.la

#--------------------------------------------------------------------------

libsortsmillff_gutils_la_SOURCES = divisors.c fsys.c gcol.c gimage.c	\
	gimagereadbmp.c gimageread.c gimagereadgif.c gimagereadjpeg.c		\
	gimagereadpng.c gimagereadras.c gimagereadrgb.c gimagereadtiff.c	\
	gimagereadxbm.c gimagereadxpm.c gimagewritebmp.c					\
	gimagewritegimage.c gimagewritejpeg.c gimagewritepng.c				\
	gimagewritexbm.c gimagewritexpm.c gwwintl.c gio.c giofile.c			\
	giohosts.c giomime.c giothread.c giotrans.c gimagebmpP.h			\
	giofuncP.h gioP.h gioftp.c gioftpP.h

EXPORTED_GIOFTP_SYMBOLS = GIO_cancel GIO_dispatch GIO_init GIO_term

EXPORTED_FSYS_SYMBOLS = GFileGetUserConfigDir GFileGetUserCacheDir		\
	GFileGetUserDataDir GFileGetHomeDir GFileBuildName GFileBaseName	\
	GFileAppendFile GFileIsAbsolute GFileIsDir GFileExists				\
	GFileReadable GFileMkDir GFileUnlink u8_GFileGetUserConfigDir		\
	u8_GFileGetUserCacheDir u8_GFileGetUserDataDir u8_GFileGetHomeDir	\
	u8_GFileBuildName u8_GFileBaseName u32_GFileBaseName				\
	u32_GFileNormalize u32_GFileAppendFile

EXPORTED_GUTILS_SYMBOLS = $(EXPORTED_FSYS_SYMBOLS)					\
	$(EXPORTED_GIOFTP_SYMBOLS) GIOCreate GIO_PasswordCache			\
	GIO_SetThreadCallback GIOcancel GIOclose GIOdir GIOfileExists	\
	GIOgetDirData GIOguessMimeType GIOmkDir GImageCreate			\
	GImageDestroy GImageDrawImage GImageDrawRect GImageGetHeight	\
	GImageGetPixelRGBA GImageGetWidth GImageRead GImageRead_Bmp		\
	GImageRead_Jpeg GImageRead_Png GImageWriteBmp GImageWritePng	\
	GImageWriteXbm GImageWrite_Bmp GImageWrite_Jpeg GImageWrite_Png	\
	_GIO_decomposeURL _GImageGetPixelColor div_tables gColor2Hslrgb	\
	gColor2Hslrgba gHSL2RGB gHSV2RGB gHslrgb2Color gHslrgba2Color	\
	gRGB2HSV sgettext

libsortsmillff_gutils_la_LIBADD = $(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/lib/libgnu.la
libsortsmillff_gutils_la_LDFLAGS = $(call ldflags,EXPORTED_GUTILS_SYMBOLS)
libsortsmillff_gutils_la_DEPENDENCIES = Makefile $(libsortsmillff_gutils_la_LIBADD)

#--------------------------------------------------------------------------
