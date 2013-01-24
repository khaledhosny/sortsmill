#include <config.h>		/* -*- coding: utf-8 -*- */

/* Copyright (C) 2003-2012 by George Williams */
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

#include "fontforgeui.h"
#include <gkeysym.h>
#include <ustring.h>
#include "ttf.h"

/* ************************************************************************** */
/* Sigh. This list is duplicated in macenc.c */
static GTextInfo maclanguages[] = {
    { (uint32_t *) NC_("Language", "English"), NULL, 0, 0, (void *) 0, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "French"), NULL, 0, 0, (void *) 1, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "German"), NULL, 0, 0, (void *) 2, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Italian"), NULL, 0, 0, (void *) 3, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Dutch"), NULL, 0, 0, (void *) 4, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Swedish"), NULL, 0, 0, (void *) 5, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Spanish"), NULL, 0, 0, (void *) 6, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Danish"), NULL, 0, 0, (void *) 7, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Portuguese"), NULL, 0, 0, (void *) 8, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Norwegian"), NULL, 0, 0, (void *) 9, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Hebrew"), NULL, 0, 0, (void *) 10, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Japanese"), NULL, 0, 0, (void *) 11, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Arabic"), NULL, 0, 0, (void *) 12, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Finnish"), NULL, 0, 0, (void *) 13, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Greek"), NULL, 0, 0, (void *) 14, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Icelandic"), NULL, 0, 0, (void *) 15, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Maltese"), NULL, 0, 0, (void *) 16, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Turkish"), NULL, 0, 0, (void *) 17, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Croatian"), NULL, 0, 0, (void *) 18, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Traditional Chinese"), NULL, 0, 0, (void *) 19, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Urdu"), NULL, 0, 0, (void *) 20, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Hindi"), NULL, 0, 0, (void *) 21, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Thai"), NULL, 0, 0, (void *) 22, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Korean"), NULL, 0, 0, (void *) 23, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Lithuanian"), NULL, 0, 0, (void *) 24, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Polish"), NULL, 0, 0, (void *) 25, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Hungarian"), NULL, 0, 0, (void *) 26, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Estonian"), NULL, 0, 0, (void *) 27, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Latvian"), NULL, 0, 0, (void *) 28, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Sami (Lappish)"), NULL, 0, 0, (void *) 29, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Faroese (Icelandic)"), NULL, 0, 0, (void *) 30, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Farsi/Persian"), NULL, 0, 0, (void *) 31, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Russian"), NULL, 0, 0, (void *) 32, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Simplified Chinese"), NULL, 0, 0, (void *) 33, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Flemish"), NULL, 0, 0, (void *) 34, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Irish Gaelic"), NULL, 0, 0, (void *) 35, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Albanian"), NULL, 0, 0, (void *) 36, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Romanian"), NULL, 0, 0, (void *) 37, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Czech"), NULL, 0, 0, (void *) 38, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Slovak"), NULL, 0, 0, (void *) 39, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Slovenian"), NULL, 0, 0, (void *) 40, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Yiddish"), NULL, 0, 0, (void *) 41, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Serbian"), NULL, 0, 0, (void *) 42, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Macedonian"), NULL, 0, 0, (void *) 43, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Bulgarian"), NULL, 0, 0, (void *) 44, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Ukrainian"), NULL, 0, 0, (void *) 45, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Byelorussian"), NULL, 0, 0, (void *) 46, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Uzbek"), NULL, 0, 0, (void *) 47, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kazakh"), NULL, 0, 0, (void *) 48, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Axerbaijani (Cyrillic)"), NULL, 0, 0, (void *) 49, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Axerbaijani (Arabic)"), NULL, 0, 0, (void *) 50, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Armenian"), NULL, 0, 0, (void *) 51, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Georgian"), NULL, 0, 0, (void *) 52, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Moldavian"), NULL, 0, 0, (void *) 53, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kirghiz"), NULL, 0, 0, (void *) 54, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tajiki"), NULL, 0, 0, (void *) 55, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Turkmen"), NULL, 0, 0, (void *) 56, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Mongolian (Mongolian)"), NULL, 0, 0, (void *) 57, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Mongolian (cyrillic)"), NULL, 0, 0, (void *) 58, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Pashto"), NULL, 0, 0, (void *) 59, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kurdish"), NULL, 0, 0, (void *) 60, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kashmiri"), NULL, 0, 0, (void *) 61, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Sindhi"), NULL, 0, 0, (void *) 62, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tibetan"), NULL, 0, 0, (void *) 63, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Nepali"), NULL, 0, 0, (void *) 64, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Sanskrit"), NULL, 0, 0, (void *) 65, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Marathi"), NULL, 0, 0, (void *) 66, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Bengali"), NULL, 0, 0, (void *) 67, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Assamese"), NULL, 0, 0, (void *) 68, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Gujarati"), NULL, 0, 0, (void *) 69, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Punjabi"), NULL, 0, 0, (void *) 70, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Oriya"), NULL, 0, 0, (void *) 71, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Malayalam"), NULL, 0, 0, (void *) 72, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kannada"), NULL, 0, 0, (void *) 73, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tamil"), NULL, 0, 0, (void *) 74, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Telugu"), NULL, 0, 0, (void *) 75, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Sinhalese"), NULL, 0, 0, (void *) 76, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Burmese"), NULL, 0, 0, (void *) 77, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Khmer"), NULL, 0, 0, (void *) 78, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Lao"), NULL, 0, 0, (void *) 79, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Vietnamese"), NULL, 0, 0, (void *) 80, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Indonesian"), NULL, 0, 0, (void *) 81, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tagalog"), NULL, 0, 0, (void *) 82, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Malay (roman)"), NULL, 0, 0, (void *) 83, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Malay (arabic)"), NULL, 0, 0, (void *) 84, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Amharic"), NULL, 0, 0, (void *) 85, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tigrinya"), NULL, 0, 0, (void *) 86, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Galla"), NULL, 0, 0, (void *) 87, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Somali"), NULL, 0, 0, (void *) 88, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Swahili"), NULL, 0, 0, (void *) 89, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Kinyarwanda/Ruanda"), NULL, 0, 0, (void *) 90, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Rundi"), NULL, 0, 0, (void *) 91, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Nyanja/Chewa"), NULL, 0, 0, (void *) 92, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Malagasy"), NULL, 0, 0, (void *) 93, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Esperanto"), NULL, 0, 0, (void *) 94, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Welsh"), NULL, 0, 0, (void *) 128, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Basque"), NULL, 0, 0, (void *) 129, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Catalan"), NULL, 0, 0, (void *) 130, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Latin"), NULL, 0, 0, (void *) 131, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Quechua"), NULL, 0, 0, (void *) 132, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Guarani"), NULL, 0, 0, (void *) 133, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Aymara"), NULL, 0, 0, (void *) 134, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tatar"), NULL, 0, 0, (void *) 135, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Uighur"), NULL, 0, 0, (void *) 136, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Dzongkha"), NULL, 0, 0, (void *) 137, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Javanese (roman)"), NULL, 0, 0, (void *) 138, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Sundanese (roman)"), NULL, 0, 0, (void *) 139, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Galician"), NULL, 0, 0, (void *) 140, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Afrikaans"), NULL, 0, 0, (void *) 141, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Breton"), NULL, 0, 0, (void *) 142, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Inuktitut"), NULL, 0, 0, (void *) 143, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Scottish Gaelic"), NULL, 0, 0, (void *) 144, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Manx Gaelic"), NULL, 0, 0, (void *) 145, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Irish Gaelic (with dot)"), NULL, 0, 0, (void *) 146, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Tongan"), NULL, 0, 0, (void *) 147, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Greek (polytonic)"), NULL, 0, 0, (void *) 148, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Greenlandic"), NULL, 0, 0, (void *) 149, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    { (uint32_t *) NC_("Language", "Azebaijani (roman)"), NULL, 0, 0, (void *) 150, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0' },
    GTEXTINFO_EMPTY
};

static void initmaclangs(void) {
    static int inited = false;
    int i;

    if ( !inited ) {
	inited = true;
	for ( i=0; maclanguages[i].text!=NULL; ++i )
	    maclanguages[i].text = (uint32_t *) g_dpgettext2(NULL, "Language", (char *) maclanguages[i].text);
    }
}

    /* These first three must match the values in prefs.c */
#define CID_Features	101
#define CID_FeatureDel	103
#define CID_FeatureEdit	105

#define CID_Settings	101
#define CID_SettingDel	103
#define CID_SettingEdit	105

#define CID_NameList	201
#define CID_NameNew	202
#define CID_NameDel	203
#define CID_NameEdit	205

#define CID_Cancel	300
#define CID_OK		301
#define CID_Id		302
#define CID_Name	303
#define CID_Language	304
#define CID_On		305
#define CID_Mutex	306

static char *spacer = " ⇒ ";	/* right double arrow */

static GTextInfo *Pref_MacNamesList(struct macname *all) {
    GTextInfo *ti;
    int i, j;
    struct macname *mn;
    char *temp, *full;

    initmaclangs();

    for ( i=0, mn=all; mn!=NULL; mn=mn->next, ++i );
    ti = xcalloc(i+1,sizeof( GTextInfo ));

    for ( i=0, mn=all; mn!=NULL; mn=mn->next, ++i ) {
	temp = MacStrToUtf8(mn->name,mn->enc,mn->lang);
	if ( temp==NULL )
    continue;
	for ( j=0 ; maclanguages[j].text!=0; ++j )
	    if ( maclanguages[j].userdata == (void *) (intptr_t) (mn->lang ))
	break;
	if ( maclanguages[j].text!=0 ) {
	    char *lang = (char *) maclanguages[j].text;
	    full = xmalloc((strlen(lang)+strlen(temp)+strlen(spacer)+1));
	    strcpy(full,lang);
	} else {
	    char *hunh = "???";
	    full = xmalloc((strlen(hunh)+strlen(temp)+strlen(spacer)+1));
	    strcpy(full,hunh);
	}
	strcat(full,spacer);
	strcat(full,temp);
	free(temp);
	ti[i].text = (uint32_t *) full;
	ti[i].text_is_1byte = true;
	ti[i].userdata = (void *) mn;
    }
return( ti );
}

struct namedata {
    GWindow gw;
    int index;
    int done;
    struct macname *all, *changing;
    GGadget *namelist;		/* Not in this dlg, in the dlg which created us */
};

static int name_e_h(GWindow gw, GEvent *event) {
    struct namedata *nd = GDrawGetUserData(gw);
    int i;
    int32_t len;
    GTextInfo **ti, *sel;
    char *ret1, *temp; uint32_t *full;
    int val1, val2;
    struct macname *mn;
    int language;

    if ( event->type==et_close ) {
	nd->done = true;
	if ( nd->index==-1 )
	    MacNameListFree(nd->changing);
    } else if ( event->type==et_char ) {
	if ( event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help ) {
	    help("prefs.html#Features");
return( true );
	}
return( false );
    } else if ( event->type==et_controlevent && event->u.control.subtype == et_buttonactivate ) {
	if ( GGadgetGetCid(event->u.control.g) == CID_Cancel ) {
	    nd->done = true;
	    if ( nd->index==-1 )
		MacNameListFree(nd->changing);
	} else if ( GGadgetGetCid(event->u.control.g) == CID_OK ) {
	    sel = GGadgetGetListItemSelected(GWidgetGetControl(nd->gw,CID_Language));
	    language = nd->changing->lang;
	    if ( sel!=NULL )
		language = (intptr_t) sel->userdata;
	    else if ( nd->index==-1 ) {
		ff_post_error(_("Bad Language"),_("Bad Language"));
return( true );
	    }	/* Otherwise use the original language, it might not be one we recognize */
	    if ( language != nd->changing->lang )
		nd->changing->enc = MacEncFromMacLang(language);
	    nd->changing->lang = language;
	    val1 = (nd->changing->enc<<16) | nd->changing->lang;
	    ret1 = GGadgetGetTitle8(GWidgetGetControl(nd->gw,CID_Name));
	    free(nd->changing->name);
	    nd->changing->name = Utf8ToMacStr(ret1,nd->changing->enc,nd->changing->lang);
	    free(ret1);

	    ti = GGadgetGetList(nd->namelist,&len);
	    for ( i=0; i<len; ++i ) if ( i!=nd->index ) {
		val2 = (((struct macname *) (ti[i]->userdata))->enc<<16) |
			(((struct macname *) (ti[i]->userdata))->lang);
		if ( val2==val1 ) {
		    ff_post_error(_("This feature code is already used"),_("This feature code is already used"));
return( true );
		}
	    }

	    temp = MacStrToUtf8(nd->changing->name,nd->changing->enc,nd->changing->lang);
	    if ( sel!=NULL ) {
		const uint32_t *lang = sel->text;
		full = xmalloc((u32_strlen(lang)+strlen(temp)+6)*sizeof(uint32_t));
		u32_strcpy(full,lang);
	    } else {
		char *hunh = "???";
		full = xmalloc((strlen(hunh)+strlen(temp)+6)*sizeof(uint32_t));
		u32_strcpy(full, x_gc_u8_to_u32 (hunh));
	    }
	    u32_strcat (full, x_gc_u8_to_u32 (spacer));
	    utf82u_strcpy(full+u32_strlen(full),temp);

	    if ( nd->index==-1 )
		GListAddStr(nd->namelist,full,nd->changing);
	    else {
		GListReplaceStr(nd->namelist,nd->index,full,nd->changing);
		if ( nd->all==nd->changing )
		    nd->all = nd->changing->next;
		else {
		    for ( mn=nd->all ; mn!=NULL && mn->next!=nd->changing; mn=mn->next );
		    if ( mn!=NULL ) mn->next = nd->changing->next;
		}
	    }
	    nd->changing->next = NULL;
	    if ( nd->all==NULL || val1< ((nd->all->enc<<16)|nd->all->lang) ) {
		nd->changing->next = nd->all;
		nd->all = nd->changing;
	    } else {
		for ( mn=nd->all; mn->next!=NULL && ((mn->next->enc<<16)|mn->next->lang)<val1; mn=mn->next );
		nd->changing->next = mn->next;
		mn->next = nd->changing;
	    }
	    GGadgetSetUserData(nd->namelist,nd->all);
	    nd->done = true;
	}
    }
return( true );
}

static char *AskName(struct macname *changing,struct macname *all,GGadget *list, int index) {
    GRect pos;
    GWindow gw;
    GWindowAttrs wattrs;
    GGadgetCreateData gcd[8];
    GTextInfo label[8];
    struct namedata nd;
    int i;

    initmaclangs();

    memset(&nd,0,sizeof(nd));
    nd.namelist = list;
    nd.index = index;
    nd.changing = changing;
    nd.all = all;

    memset(&wattrs,0,sizeof(wattrs));
    wattrs.mask = wam_events|wam_cursor|wam_utf8_wtitle|wam_undercursor|wam_restrict|wam_isdlg;
    wattrs.event_masks = ~(1<<et_charup);
    wattrs.restrict_input_to_me = 1;
    wattrs.is_dlg = 1;
    wattrs.undercursor = 1;
    wattrs.cursor = ct_pointer;
    wattrs.utf8_window_title = _("Setting");
    pos.x = pos.y = 0;
    pos.width = GGadgetScale(GDrawPointsToPixels(NULL,270));
    pos.height = GDrawPointsToPixels(NULL,98);
    gw = GDrawCreateTopWindow(NULL,&pos,name_e_h,&nd,&wattrs);
    nd.gw = gw;

    memset(gcd,0,sizeof(gcd));
    memset(label,0,sizeof(label));

    label[0].text = (uint32_t *) _("_Language:");
    label[0].text_is_1byte = true;
    label[0].text_has_mnemonic = true;
    gcd[0].gd.label = &label[0];
    gcd[0].gd.pos.x = 5; gcd[0].gd.pos.y = 5+4;
    gcd[0].gd.flags = gg_enabled|gg_visible;
    gcd[0].creator = GLabelCreate;

    gcd[1].gd.pos.x = 60; gcd[1].gd.pos.y = 5;
    gcd[1].gd.pos.width = 200;
    gcd[1].gd.flags = gg_enabled|gg_visible | gg_list_alphabetic;
    gcd[1].gd.u.list = maclanguages;
    gcd[1].gd.cid = CID_Language;
    gcd[1].creator = GListButtonCreate;

    for ( i=0; maclanguages[i].text!=NULL; ++i ) {
	if ( maclanguages[i].userdata == (void *) (intptr_t) (changing->lang) )
	    maclanguages[i].selected = true;
	else
	    maclanguages[i].selected = false;
	if ( changing->lang==65535 )
	    maclanguages[0].selected = true;
    }

    label[2].text = (uint32_t *) _("_Name:");
    label[2].text_is_1byte = true;
    label[2].text_has_mnemonic = true;
    gcd[2].gd.label = &label[2];
    gcd[2].gd.pos.x = 5; gcd[2].gd.pos.y = gcd[0].gd.pos.y+28;
    gcd[2].gd.flags = gg_enabled|gg_visible;
    gcd[2].creator = GLabelCreate;

    label[3].text = (uint32_t *) MacStrToUtf8(changing->name,changing->enc,changing->lang);
    label[3].text_is_1byte = true;
    gcd[3].gd.label = changing->name==NULL ? NULL : &label[3];
    gcd[3].gd.pos.x = gcd[1].gd.pos.x; gcd[3].gd.pos.y = gcd[2].gd.pos.y-4;
    gcd[3].gd.pos.width = 200;
    gcd[3].gd.flags = gg_enabled|gg_visible;
    gcd[3].gd.cid = CID_Name;
    gcd[3].creator = GTextFieldCreate;

    i = 4;

    gcd[i].gd.pos.x = 13-3; gcd[i].gd.pos.y = gcd[i-1].gd.pos.y+30;
    gcd[i].gd.pos.width = -1; gcd[i].gd.pos.height = 0;
    gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
    label[i].text = (uint32_t *) _("_OK");
    label[i].text_is_1byte = true;
    label[i].text_has_mnemonic = true;
    gcd[i].gd.label = &label[i];
    gcd[i].gd.cid = CID_OK;
    /*gcd[i].gd.handle_controlevent = Prefs_Ok;*/
    gcd[i++].creator = GButtonCreate;

    gcd[i].gd.pos.x = -13; gcd[i].gd.pos.y = gcd[i-1].gd.pos.y+3;
    gcd[i].gd.pos.width = -1; gcd[i].gd.pos.height = 0;
    gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
    label[i].text = (uint32_t *) _("_Cancel");
    label[i].text_is_1byte = true;
    label[i].text_has_mnemonic = true;
    gcd[i].gd.label = &label[i];
    gcd[i].gd.cid = CID_Cancel;
    gcd[i].creator = GButtonCreate;

    GGadgetsCreate(gw,gcd);
    GDrawSetVisible(gw,true);
    GWidgetIndicateFocusGadget(gcd[1].ret);
    while ( !nd.done )
	GDrawProcessOneEvent(NULL);
    GDrawDestroyWindow(gw);
return( false );
}

static void ChangeName(GGadget *list,int index) {
    struct macname *mn = GGadgetGetListItemSelected(list)->userdata,
		    *all = GGadgetGetUserData(list);

    AskName(mn,all,list,index);
}

static int Pref_NewName(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	GWindow gw = GGadgetGetWindow(g);
	GGadget *list = GWidgetGetControl(gw,CID_NameList);
	struct macname *new, *all;

	all = GGadgetGetUserData(list);
	new = (struct macname *) xzalloc(sizeof (struct macname));
	new->lang = -1;
	AskName(new,all,list,-1);
    }
return( true );
}

static int Pref_DelName(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	struct macname *mn, *p, *all, *next;
	GWindow gw = GGadgetGetWindow(g);
	GGadget *list = GWidgetGetControl(gw,CID_NameList);
	int32_t len;
	GTextInfo **ti = GGadgetGetList(list,&len);
	int i;

	all = GGadgetGetUserData(list);
	for ( mn = all, p=NULL; mn!=NULL; mn = next ) {
	    next = mn->next;
	    for ( i=len-1; i>=0; --i ) {
		if ( ti[i]->selected && ti[i]->userdata==mn )
	    break;
	    }
	    if ( i>=0 ) {
		if ( p==NULL )
		    all = next;
		else
		    p->next = next;
		mn->next = NULL;
		MacNameListFree(mn);
	    } else
		p = mn;
	}
	GGadgetSetUserData(list,all);
	GListDelSelected(list);
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameDel),false);
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameEdit),false);
    }
return( true );
}

static int Pref_EditName(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	GGadget *list = GWidgetGetControl(GGadgetGetWindow(g),CID_NameList);
	ChangeName(list,GGadgetGetFirstListSelectedItem(list));
    }
return( true );
}

static int Pref_NameSel(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_listselected ) {
	int32_t len;
	GTextInfo **ti = GGadgetGetList(g,&len);
	GWindow gw = GGadgetGetWindow(g);
	int i, sel_cnt=0;
	for ( i=0; i<len; ++i )
	    if ( ti[i]->selected ) ++sel_cnt;
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameDel),sel_cnt!=0);
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameEdit),sel_cnt==1);
    } else if ( e->type==et_controlevent && e->u.control.subtype == et_listdoubleclick ) {
	ChangeName(g,e->u.control.u.list.changed_index!=-1?e->u.control.u.list.changed_index:
		GGadgetGetFirstListSelectedItem(g));
    }
return( true );
}

struct macname *NameGadgetsGetNames( GWindow gw ) {
return( GGadgetGetUserData(GWidgetGetControl(gw,CID_NameList)) );
}

void NameGadgetsSetEnabled( GWindow gw, int enable ) {

    GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameList),enable);
    GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameNew),enable);
    if ( !enable ) {
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameDel),false);
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameEdit),false);
    } else {
	int32_t len;
	GGadget *list = GWidgetGetControl(gw,CID_NameList);
	GTextInfo **ti = GGadgetGetList(list,&len);
	int i, sel_cnt=0;
	for ( i=0; i<len; ++i )
	    if ( ti[i]->selected ) ++sel_cnt;
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameDel),sel_cnt>0);
	GGadgetSetEnabled(GWidgetGetControl(gw,CID_NameEdit),sel_cnt=1);
    }
}

int GCDBuildNames(GGadgetCreateData *gcd,GTextInfo *label,int pos,struct macname *names) {

    gcd[pos].gd.pos.x = 6; gcd[pos].gd.pos.y = pos==0 ? 6 :
	    gcd[pos-1].creator==GTextFieldCreate ? gcd[pos-1].gd.pos.y+30 :
						    gcd[pos-1].gd.pos.y+14;
    gcd[pos].gd.pos.width = 250; gcd[pos].gd.pos.height = 5*12+10;
    gcd[pos].gd.flags = gg_visible | gg_enabled | gg_list_alphabetic | gg_list_multiplesel;
    gcd[pos].gd.cid = CID_NameList;
    gcd[pos].data = names = MacNameCopy(names);
    gcd[pos].gd.u.list = Pref_MacNamesList(names);
    gcd[pos].gd.handle_controlevent = Pref_NameSel;
    gcd[pos++].creator = GListCreate;

    gcd[pos].gd.pos.x = 6; gcd[pos].gd.pos.y = gcd[pos-1].gd.pos.y+gcd[pos-1].gd.pos.height+10;
    gcd[pos].gd.flags = gg_visible | gg_enabled;
    label[pos].text = (uint32_t *) C_("MacName", "_New...");
    label[pos].text_is_1byte = true;
    label[pos].text_has_mnemonic = true;
    gcd[pos].gd.label = &label[pos];
    gcd[pos].gd.handle_controlevent = Pref_NewName;
    gcd[pos].gd.cid = CID_NameNew;
    gcd[pos++].creator = GButtonCreate;

    gcd[pos].gd.pos.x = gcd[pos-1].gd.pos.x+20+GIntGetResource(_NUM_Buttonsize)*100/GIntGetResource(_NUM_ScaleFactor);
    gcd[pos].gd.pos.y = gcd[pos-1].gd.pos.y;
    gcd[pos].gd.flags = gg_visible ;
    label[pos].text = (uint32_t *) _("_Delete");
    label[pos].text_is_1byte = true;
    label[pos].text_has_mnemonic = true;
    gcd[pos].gd.label = &label[pos];
    gcd[pos].gd.cid = CID_NameDel;
    gcd[pos].gd.handle_controlevent = Pref_DelName;
    gcd[pos++].creator = GButtonCreate;

    gcd[pos].gd.pos.x = gcd[pos-1].gd.pos.x+20+GIntGetResource(_NUM_Buttonsize)*100/GIntGetResource(_NUM_ScaleFactor);
    gcd[pos].gd.pos.y = gcd[pos-1].gd.pos.y;
    gcd[pos].gd.flags = gg_visible ;
    label[pos].text = (uint32_t *) _("_Edit...");
    label[pos].text_is_1byte = true;
    label[pos].text_has_mnemonic = true;
    gcd[pos].gd.label = &label[pos];
    gcd[pos].gd.cid = CID_NameEdit;
    gcd[pos].gd.handle_controlevent = Pref_EditName;
    gcd[pos++].creator = GButtonCreate;

return( pos );
}
