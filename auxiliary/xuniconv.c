#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz */
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

#include <xunistring.h>

// Generate non-inline versions of these functions.
uint8_t *x_u8_strconv_from_locale (const char *string);
uint16_t *x_u16_strconv_from_locale (const char *string);
uint32_t *x_u32_strconv_from_locale (const char *string);
char *x_u8_strconv_to_locale (const uint8_t * string);
char *x_u16_strconv_to_locale (const uint16_t * string);
char *x_u32_strconv_to_locale (const uint32_t * string);
uint8_t *x_gc_u8_strconv_from_locale (const char *string);
uint16_t *x_gc_u16_strconv_from_locale (const char *string);
uint32_t *x_gc_u32_strconv_from_locale (const char *string);
char *x_gc_u8_strconv_to_locale (const uint8_t * string);
char *x_gc_u16_strconv_to_locale (const uint16_t * string);
char *x_gc_u32_strconv_to_locale (const uint32_t * string);
