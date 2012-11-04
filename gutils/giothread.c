/* Copyright (C) 2000-2012 by George Williams */
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

#include <config.h>

#include "giofuncP.h"
#include "gfile.h"
#include "ustring.h"
#include "gresource.h"
#include "errno.h"

#include <stdarg.h>
#include <stdio.h>

#ifdef HAVE_PTHREAD_H

void _GIO_PostError(GIOControl *gc) {
    if ( gdraw_sync_thread!=NULL )
	(gdraw_sync_thread)(NULL,(void (*)(void *)) gc->receiveerror,gc);
}

void _GIO_PostInter(GIOControl *gc) {
    if ( gdraw_sync_thread!=NULL )
	(gdraw_sync_thread)(NULL,(void (*)(void *)) gc->receiveintermediate,gc);
}

void _GIO_PostSuccess(GIOControl *gc) {
    if ( gdraw_sync_thread!=NULL )
	(gdraw_sync_thread)(NULL,(void (*)(void *)) gc->receivedata,gc);
}

#else // ! HAVE_PTHREAD_H

void _GIO_PostError(GIOControl *gc) {
    gc->receiveerror(gc);
}

void _GIO_PostInter(GIOControl *gc) {
    gc->receiveintermediate(gc);
}

void _GIO_PostSuccess(GIOControl *gc) {
    gc->receivedata(gc);
}
#endif // ! HAVE_PTHREAD_H
