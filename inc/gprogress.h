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

#ifndef _GPROGRESS_H
#define _GPROGRESS_H

#include <config.h>

#include <basics.h>
#include <intl.h>

/* Ends the topmost indicator */
VISIBLE extern void GProgressEndIndicator (void);

/* Changes the expected length in the topmost */
VISIBLE extern void GProgressChangeTotal (int tot);

/* Changes the expected number of stages in the topmost */
VISIBLE extern void GProgressChangeStages (int stages);

/* Move to the next stage in the topmost indicator */
VISIBLE extern int GProgressNextStage (void);

/* Increment progress by one sub-entity */
VISIBLE extern int GProgressNext (void);

/* Increment progress by cnt sub-entities */
VISIBLE extern int GProgressIncrementBy (int cnt);

/* Set progress to 0 */
VISIBLE extern int GProgressReset (void);

/* Don't bring up the progress dlg because of time spent between a
   pause and resume */
VISIBLE extern void GProgressPauseTimer (void);
VISIBLE extern void GProgressResumeTimer (void);

/* Display the damn thing whether we should or not */
VISIBLE extern void GProgressShow (void);

VISIBLE extern void GProgressStartIndicator8 (int delay,         /* in tenths of seconds */
                                              const char *title, /* for the window decoration */
                                              const char *line1, /* First line of description */
                                              const char *line2, /* Second line */
                                              int tot,           /* Number of sub-entities in the operation */
                                              int stages,        /* Number of stages, each processing tot sub-entities */
                                              bool has_stop);

/* Changes the text in the topmost */
VISIBLE extern void GProgressChangeLine1_8 (const char *line1);

/* Changes the text in the topmost */
VISIBLE extern void GProgressChangeLine2_8 (const char *line2);

#define gwwv_progress_start_indicator	GProgressStartIndicator8
#define gwwv_progress_next		GProgressNext
#define gwwv_progress_next_stage	GProgressNextStage
#define gwwv_progress_end_indicator	GProgressEndIndicator
#define gwwv_progress_show		GProgressShow
#define gwwv_progress_change_line1	GProgressChangeLine1_8
#define gwwv_progress_change_line2	GProgressChangeLine2_8
#define gwwv_progress_change_total	GProgressChangeTotal
#define gwwv_progress_change_stages	GProgressChangeStages
#define gwwv_progress_increment		GProgressIncrementBy
#define gwwv_progress_reset		GProgressReset
#define gwwv_progress_pause_timer	GProgressPauseTimer
#define gwwv_progress_resume_timer	GProgressResumeTimer
#endif
