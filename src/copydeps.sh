#!/bin/sh
# Warren Gay  Wed May 30 11:03:58 2007
#
# Produce list of COPY dependencies

cat $* | sed -n 's|^ *||g;s|\. *$||g;/COPY [A-Za-z0-9-]*$/p' \
	| sed 's|COPY *|copybk/|' | sort -u \
	| sed 's|^|\.\./|;s|$|\.cbl|'
	
# End
