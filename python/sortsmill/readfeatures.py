# Copyright (C) 2009, 2013 Khaled Hosny and Barry Schwartz
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
from os.path import exists

#--------------------------------------------------------------------------

def merge_pair_positioning_subtables(font):
    for lookup in font.gpos_lookups:
        if font.getLookupInfo(lookup)[0] == 'gpos_pair':
            subtables = font.getLookupSubtables(lookup)
            i = 0
            while i < len(subtables) and font.isKerningClass(subtables[i]):
                i += 1
            for subtab in subtables[i + 1:]:
                if not font.isKerningClass(subtab):
                    font.mergeLookupSubtables(subtables[i], subtab)

#--------------------------------------------------------------------------

def feature_file_exists(bitbucket, font):
    feature_file_name = font.fontname + "_main.fea"
    return exists(feature_file_name)

def read_features(bitbucket, font):
    feature_file_name = font.fontname + "_main.fea"
    font.mergeFeature(feature_file_name)
#    merge_pair_positioning_subtables(font) # FIX/TODO: This seems to trigger a metrics view bug in FontForge.
#                                           # I need to investigate and report. Meanwhile, put the call in
#                                           # make-fonts.py, or just not bother.


def erase_and_read_features(bitbucket, font):
    all_lookups = font.gpos_lookups + font.gsub_lookups
    for lookup in all_lookups:
        if font.getLookupInfo(lookup)[0] != 'gpos_mark2base':
            font.removeLookup(lookup)
    read_features(bitbucket, font)
    font.buildOrReplaceAALTFeatures()

fontforge.registerMenuItem(erase_and_read_features,
                           feature_file_exists, None, "Font", "None",
                           "Delete lookups and read _main feature file")

#--------------------------------------------------------------------------
