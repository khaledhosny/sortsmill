# Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
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

# Convert a module name to uppercase. (Actually this is just a generic
# ASCII ‘convert to uppercase’ function.)
mod_to_upper=$(shell echo '$(1)' | LC_ALL=C tr '[a-z]' '[A-Z]')

# Convert a module name to the appropriate letter case.
modcase=$(if $(FC_MODCASE_LOWER),$(1),$(if $(FC_MODCASE_UPPER),$(call mod_to_upper,$(1)),))

# Convert a module name to the module file name. This file name is the
# module name in either lowercase or uppercase, depending on the
# compiler, followed by a compiler-specific extension.
modfile=$(if $(FC_MODEXT),$(call modcase,$(1)),).$(FC_MODEXT)
