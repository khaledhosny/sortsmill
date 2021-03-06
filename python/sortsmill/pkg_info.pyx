# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

include 'sortsmill/cython/config.pxi'

package = PACKAGE
package_bugreport = PACKAGE_BUGREPORT
package_name = PACKAGE_NAME
package_string = PACKAGE_STRING
package_tarname = PACKAGE_TARNAME
package_url = PACKAGE_URL
package_version = PACKAGE_VERSION

version_major = VERSION_MAJOR
version_minor = VERSION_MINOR
version_patch = VERSION_PATCH
version_extra = VERSION_EXTRA
version_extra_short = VERSION_EXTRA_SHORT
version = '{}.{}.{}{}'.format (version_major, version_minor,
                               version_patch, version_extra_short)

have_gui = HAVE_GUI
