# Makefile.in -- Makefile for dictem
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
# $Id: Makefile.am,v 1.14 2006-04-03 12:34:06 cheusov Exp $
#

# Add a _letter_ if you change the version number and release your own version.
# Numbers are for the original author(s) only.

#bin_SCRIPTS = dictem
#man_MANS    = dictem.1
lisp_LISP=dictem.el

.PHONY : cvsclean
cvsclean : distclean
	rm -f Makefile.in configure aclocal.m4
	rm -rf autom4te.cache stamp-h.in

.PHONY: ChangeLog
ChangeLog:
	@(echo "***** Making ChangeLog..."; \
	rm -f ChangeLog; \
	rcs2log -i 2 -r . | \
	sed -e 's,/[^ ]*cvs[^ ]*dictem/,,g' \
	    -e 's,cheusov@[^>]*,vle@gmx.net,g' \
	    -e 's,\(.*\)<\([^@<>]\+\)@\([^@<>]\+\)>\(.*\),\1<\2 at \3[}]\4,g' \
	> ChangeLog;)

cvsdist:
	@( CVSROOT=`cat CVS/Root` && \
	export CVSROOT && \
	VERSION=$(PACKAGE_VERSION) && \
	VERSION_CVS=`echo $${VERSION} | tr . -` && \
	export VERSION VERSION_CVS && \
	make ChangeLog && \
	cp ChangeLog /tmp/dictem.ChangeLog.$${VERSION} && \
	echo "***** Exporting files for dictem-$${VERSION}..." && \
	cd /tmp && \
	rm -rf /tmp/dictem-$${VERSION} && \
	cvs export -d dictem-$${VERSION} -r dictem-$${VERSION_CVS} dictem && \
	cd dictem-$${VERSION} && \
	mv /tmp/dictem.ChangeLog.$${VERSION} ChangeLog && \
	autoreconf && \
	automake && \
	rm -rf autom4te.cache libmaa/autom4te.cache && \
	chmod -R a+rX,g-w . && \
	cd .. && \
	echo "***** Taring and zipping dictem-$${VERSION}.tar.gz..." && \
	tar cvvf dictem-$${VERSION}.tar dictem-$${VERSION} && \
	gzip -9f dictem-$${VERSION}.tar && \
	echo "***** Done making /tmp/dictem-$${VERSION}.tar.gz")
