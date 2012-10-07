/* Copyright (C) 2002-2012 by George Williams */
/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _CONFIG_FONTFORGE_H_
#define _CONFIG_FONTFORGE_H_


/* ************************************************************************** */
/* *********************** Set by configure script ************************** */
/* ************************************************************************** */

/* The following are expected to be set by the configure script, but I suppose*/
/*  you could set them here too 					      */

/* If you are on a Mac then set __Mac					      */
/* If you are on a windows box with cygwin set __CygWin			      */

/* If you are on a Mac where cursors are restricted to 16x16 pixel boxes then */
/*  set _CursorsMustBe16x16						      */

/* If you are on cygwin where even the modifier keys autorepeat then set      */
/*  _ModKeysAutoRepeat							      */

/* If you are on cygwin where some of the drawmode funtions (like AND) don't  */
/*  work then set _BrokenBitmapImages					      */

/* FontForge knows about 4 different keyboard settings, a windows keyboard, a */
/*  mac keyboard, a mac keyboard under SUSE linux, and a sun keyboard	      */
/*  When it starts up FontForge assumes that the keyboard is some default type*/
/*  You can override the type by setting _Keyboard to			      */
/* 0 -- windows								      */
/* 1 -- mac running mac osx						      */
/* 3 -- mac running SUSE linux (7.1)					      */
/* 2 -- sparc								      */
/* Basically this affects the text that appears in menus. The sun keyboard    */
/*  uses meta where the windows one uses alt, and the mac use command and     */
/*  option.								      */

/* If there are no freetype header files then define _NO_FREETYPE	      */
/* If the freetype library has the bytecode debugger then define FREETYPE_HAS_DEBUGGER */
/* If there is no mmap system call then define _NO_MMAP			      */

/* If there is no ungif library (or if it is out of date) define _NO_LIBUNGIF */
/* If there is no png (or z) library define _NO_LIBPNG			      */
/* If there libpng is version 1.2 define _LIBPNG12			      */
/* If there is no jpeg library define _NO_LIBJPEG			      */
/* If there is no tiff library define _NO_LIBTIFF			      */
/* If there is no xml2 library define _NO_LIBXML			      */

/* If the XInput extension is not available define _NO_XINPUT		      */
/* If the Xkb extension is not available define _NO_XKB			      */

/* If the compiler supports long long define _HAS_LONGLONG		      */

#endif
