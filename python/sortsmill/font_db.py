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

#--------------------------------------------------------------------------

import os
import shelve

#--------------------------------------------------------------------------

def create_temporary(font):
    if font.temporary is None:
        font.temporary = {}

def db_file_name(font):
    return font.fontname + '.db'

def db_open(font, flag = 'c'):
    if font.temporary is None or 'db' not in font.temporary or font.temporary['db'] is None:
        db_name = db_file_name(font)
        if flag not in ('c', 'n') and not os.path.exists(db_name):
            db = shelve.open(db_name, flag = 'c')
            db.close()
        db = shelve.open(db_name, flag = flag)
        create_temporary(font)
        font.temporary['db'] = db
    return font.temporary['db']

def db_create(font):
    return db_open(font, flag = 'c')

def db_close(font):
    if font.temporary is not None and 'db' in font.temporary and font.temporary['db'] is not None:
        font.temporary['db'].close()
        font.temporary['db'] = None

def db_exists(font):
    return os.path.exists(db_file_name(font))

def db_remove(font):
    if db_exists(font):
        db_close(font)
        os.remove(db_file_name(font))

def db_init_binding(font, key, default_value):
    db = db_open(font)
    if key not in db:
        db[key] = default_value

#--------------------------------------------------------------------------
