# Makefile.in -- Makefile for edict
# Created: Mon Oct 20 17:23:29 2003 Aleksey Cheusov <vle@gmx.net>
# Copyright: 2003 Aleksey Cheusov <vle@gmx.met>
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 1, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 675 Mass Ave, Cambridge, MA 02139, USA.
# 
# $Id: Makefile.am,v 1.3 2004-04-22 18:25:37 cheusov Exp $
#

# Add a _letter_ if you change the version number and release your own version.
# Numbers are for the original author(s) only.

AUTOMAKE_OPTIONS = foreign

#bin_SCRIPTS = dictem
#man_MANS    = dictem.1
lisp_LISP   = dictem.el dictem-opt.el dictem-ll.el

.PHONY : cvsclean
cvsclean : distclean
	rm -f Makefile.in configure aclocal.m4
	rm -rf autom4te.cache stamp-h.in
