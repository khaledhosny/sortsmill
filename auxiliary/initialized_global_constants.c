#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/initialized_global_constants.h>

static void
pthread_mutex_unlocker (void *mutex_ptr)
{
  pthread_mutex_unlock ((pthread_mutex_t *) mutex_ptr);
}

VISIBLE void
scm_dynwind_pthread_mutex_unlock (void *mutex_ptr)
{
  scm_dynwind_unwind_handler (pthread_mutex_unlocker, mutex_ptr,
                              SCM_F_WIND_EXPLICITLY);
}
