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

#include "gio.h"
#include "gfile.h"
#include "ustring.h"

uint32_t unknown[] = { '*','/','*', '\0' };
uint32_t textplain[] = { 't','e','x','t','/','p','l','a','i','n', '\0' };
uint32_t texthtml[] = { 't','e','x','t','/','h','t','m','l', '\0' };
uint32_t textxml[] = { 't','e','x','t','/','x','m','l', '\0' };
uint32_t textc[] = { 't','e','x','t','/','c', '\0' };
uint32_t textcss[] = { 't','e','x','t','/','c','s','s', '\0' };
uint32_t textmake[] = { 't','e','x','t','/','m','a','k','e', '\0' };
uint32_t textjava[] = { 't','e','x','t','/','j','a','v','a', '\0' };
uint32_t textps[] = { 't','e','x','t','/','p','s', '\0' };
	/* Officially registered with IANA on 14 May 2008 */
uint32_t sfdfont[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','v','n','d','.','f','o','n','t','-','f','o','n','t','f','o','r','g','e','-','s','f','d', '\0' };
uint32_t textpsfont[] = { 't','e','x','t','/','f','o','n','t','p','s', '\0' };
uint32_t textbdffont[] = { 't','e','x','t','/','f','o','n','t','b','d','f', '\0' };
uint32_t imagegif[] = { 'i','m','a','g','e','/','g','i','f', '\0' };
uint32_t imagejpeg[] = { 'i','m','a','g','e','/','j','p','e','g', '\0' };
uint32_t imagepng[] = { 'i','m','a','g','e','/','p','n','g', '\0' };
uint32_t imagesvg[] = { 'i','m','a','g','e','/','s','v','g','+','x','m','l', '\0' };
uint32_t videoquick[] = { 'v','i','d','e','o','/','q','u','i','c','k','t','i','m','e', '\0' };
uint32_t audiowav[] = { 'a','u','d','i','o','/','w','a','v', '\0' };
uint32_t pdf[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','p','d','f', '\0' };
uint32_t object[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','o','b','j','e','c','t', '\0' };
uint32_t dir[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','n','a','v','i','d','i','r', '\0' };
uint32_t core[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','c','o','r','e', '\0' };
uint32_t fontttf[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','t','t','f', '\0' };
uint32_t fontotf[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','o','t','f', '\0' };
uint32_t fontcid[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','c','i','d', '\0' };
uint32_t fonttype1[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','t','y','p','e','1', '\0' };
uint32_t fontmacsuit[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','m','a','c','-','s','u','i','t', '\0' };
uint32_t macbin[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','m','a','c','b','i','n','a','r','y', '\0' };
uint32_t machqx[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','m','a','c','-','b','i','n','h','e','x','4','0', '\0' };
uint32_t macdfont[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','m','a','c','-','d','f','o','n','t', '\0' };
uint32_t compressed[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','c','o','m','p','r','e','s','s','e','d', '\0' };
uint32_t tar[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','t','a','r', '\0' };
uint32_t fontpcf[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','p','c','f', '\0' };
uint32_t fontsnf[] = { 'a','p','p','l','i','c','a','t','i','o','n','/','x','-','f','o','n','t','-','s','n','f', '\0' };

#ifdef __Mac
#include <Developer/Headers/FlatCarbon/Files.h>
#define CHR(ch1,ch2,ch3,ch4) (((ch1)<<24)|((ch2)<<16)|((ch3)<<8)|(ch4))

uint32_t *_GioMacMime(const char *path) {
    /* If we're on a mac, we can try to see if we've got a real resource fork */
    FSRef ref;
    FSCatalogInfo info;

    if ( FSPathMakeRef( (uint8_t *) path,&ref,NULL)!=noErr )
return( NULL );
    if ( FSGetCatalogInfo(&ref,kFSCatInfoFinderInfo,&info,NULL,NULL,NULL)!=noErr )
return( NULL );
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('F','F','I','L') )
return( fontmacsuit );
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('G','I','F','f') )
return( imagegif );
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('P','N','G',' ') )
return( imagepng );
/*
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('B','M','P',' ') )
return( imagebmp );
*/
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('J','P','E','G') )
return( imagejpeg );
/*
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('T','I','F','F') )
return( imagetiff );
*/
    if ( ((FInfo *) (info.finderInfo))->fdType==CHR('T','E','X','T') )
return( textplain );

return( NULL );
}
#endif

uint32_t *GIOguessMimeType(const uint32_t *path,int isdir) {
    uint32_t *pt;

    if ( isdir )
return( dir );
    path = u32_GFileBaseName(path);
    pt = u32_strrchr(path,'.');

    if ( pt==NULL ) {
	if ( u32_casecompare (path, x_gc_u8_to_u32 ("makefile"))==0 || u32_casecompare (path, x_gc_u8_to_u32 ("makefile~"))==0 )
return( textmake );
	else if ( u32_casecompare (path, x_gc_u8_to_u32 ("core"))==0 )
return( core );
    } else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".text"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".txt"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".text~"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".txt~"))==0 )
return( textplain );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".c"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".h"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".c~"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".h~"))==0 )
return( textc );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".java"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".java~"))==0 )
return( textjava );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".css"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".css~"))==0 )
return( textcss );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".html"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".htm"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".html~"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".htm~"))==0 )
return( texthtml );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".xml"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".xml~"))==0 )
return( textxml );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".pfa"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".pfb"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".pt3"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".cff"))==0 )
return( textpsfont );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".sfd"))==0 )
return( sfdfont );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".ttf"))==0 )
return( fontttf );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".otf"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".otb"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".gai"))==0 )
return( fontotf );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".cid"))==0 )
return( fontcid );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".ps"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".eps"))==0 )
return( textps );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".bdf"))==0 )
return( textbdffont );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".pdf"))==0 )
return( pdf );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".gif"))==0 )
return( imagegif );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".png"))==0 )
return( imagepng );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".svg"))==0 )
return( imagesvg );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".jpeg"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".jpg"))==0 )
return( imagejpeg );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".mov"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".movie"))==0 )
return( videoquick );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".wav"))==0 )
return( audiowav );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".o"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".obj"))==0 )
return( object );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".bin"))==0 )
return( macbin );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".hqx"))==0 )
return( machqx );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".dfont"))==0 )
return( macdfont );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".gz"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".tgz"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".Z"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".zip"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".bz2"))==0 || u32_casecompare (pt, x_gc_u8_to_u32 (".tbz"))==0 ||
	    u32_casecompare (pt, x_gc_u8_to_u32 (".rpm"))==0 )
return( compressed );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".tar"))==0 )
return( tar );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".pcf"))==0 )
return( fontpcf );
    else if ( u32_casecompare (pt, x_gc_u8_to_u32 (".snf"))==0 )
return( fontsnf );

return( unknown );
}
