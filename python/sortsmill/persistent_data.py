# -*- coding: utf-8 -*-

# Copyright (C) 2010, 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

import sortsmill.ffcompat as fontforge

#--------------------------------------------------------------------------

def clear_persistent(font):
    font.persistent = None

def has_persistent(font):
    return font.persistent not in (None, {})

fontforge.registerMenuItem((lambda _, font: clear_persistent(font)),
                           (lambda _, font: has_persistent(font)),
                           None, 'Font', 'None',
                           'Clear persistent data')

#--------------------------------------------------------------------------
