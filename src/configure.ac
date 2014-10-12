# configure.ac for CobCurses
# Warren W. Gay

AC_REVISION($Id: configure.ac,v 1.55 2007/10/31 14:55:03 ve3wwg Exp $)
AC_PREREQ(2.61)

VERSION=0.95

AC_INIT([CobCurses],[0.95],[ve3wwg@cogeco.ca],[cobcurses])
AM_INIT_AUTOMAKE

enable_mouse=no
enable_atexit=no

USER_SHELL="$SHELL"

AC_ARG_ENABLE(mouse,    [  --enable-mouse          Build with mouse support [default no]])
AC_ARG_ENABLE(atexit,   [  --enable-atexit         enable atexit(3) processing [default=no]])
AC_ARG_ENABLE(ncurses,  [  --enable-ncurses        force to build with ncurses])
AC_ARG_ENABLE(curses,   [  --enable-curses         force to build with traditional curses])
AC_ARG_ENABLE(mingw,    [  --enable-mingw          force build for MinGW environment])

COBCURSES_MOUSE_SUPPORT=no
COBCURSES_ATEXIT_SUPPORT="$enable_atexit"

AC_PROG_CC
AC_PROG_CPP
AC_PROG_EGREP
AC_PROG_INSTALL

#
# Determine Host OS name, and release level
#
host_os=`uname -s`
host_os_release=`uname -r`
host_platform=`uname -p 2>/dev/null`

case "x$prefix" in
x | xNONE )	prefix=/usr/local
		echo "DEFAULT: --prefix=$prefix";;
* )		;;
esac

#
# This list is incomplete and unfinished :
#
SHARED_PIC_FLAGS=""
case $host_os in
	MINGW32* )    #	MINGW32_NT-5.1
			enable_mingw=yes	# Enable if not user specified already
			;;
	SunOS )
			case "$host_platform" in
			sparc )	SHARED_PIC_FLAGS="-fPIC -DPIC"
				;;
			* )	;;
			esac;;
        HP-UX )		echo "checking for HP's CPU model.."
			rm -f hp_cpuid.o hp_cpuid
			if test x$CC = xcc ; then
				$CC +DAportable hp_cpuid.c -o ./hp_cpuid
			else
				$CC $CFLAGS hp_cpuid.c -o ./hp_cpuid
			fi
			if test $? -ne 0 ; then
				echo "$CC $CPPFLAGS $CFLAGS hp_cpuid.c -o ./hp_cpuid"
				echo "Error compiling the CPU test program!"
				exit 1
			fi
			set -- `./hp_cpuid`
			if test $# -ne 3 ; then
				echo "Error running the CPU test program!"
				echo "Incorrect architecture or compile options?"
				exit 1
			fi
			rm -f ./hp_cpuid hp_cpuid.o

			HP_RELEASE="$1"
			HP_SYSTEM_ID="$2"
			HP_CPU_REVISION="$3"

			case "$HP_CPU_REVISION" in
			20B )	# HP PA-RISC1.0
				hp_arch="+DA1.0";;
			210 )	# HP PA-RISC1.1
				hp_arch="+DA1.1";;
			211 ) 	# HP PA-RISC1.2
				hp_arch="+DA1.2";;
			214 )	# HP PA-RISC2.0
				case x"$ABI" in
				x | x2.0W | x2.0w )
					hp_arch="+DA2.0W";;
				x2.0N | x2.0n | * )
					# ABI=2.0N
					hp_arch="+DA2.0N";;
				esac;;
			300 ) 	# HP-INTEL EM
				# This is a wild guess :
				hp_arch="+DD64";;
			* )	hp_arch="+DAportable";;
			esac

			echo "Using HP architecture: ${hp_arch}"
			CPPFLAGS="$CPPFLAGS ${hp_arch}"
			CFLAGS="$CFLAGS ${hp_arch}"

			case "$hp_arch" in
			+DA2.0W | +DD64 )
				if test x"$LPATH" = x ; then
					export LPATH=/usr/lib/pa20_64:/usr/lib
				fi;;
			+DA2.0N )
				if test x"$LPATH" = x ; then
					export LPATH=/usr/lib/pa20_32:/usr/lib
				fi;;
			* )	;;
			esac
			;; # End HP-UX
	* )		;;
esac

if test "x$enable_mingw" = xyes ; then
	echo "This is a MinGW build (under MSYS hopefully).."
else
	enable_mingw=no
fi

#
#	Optimize a better shell if possible
#
case $host_os in
	MINGW32* )	;;	# Just stay with current USER_SHELL="$SHELL"
	* )
		if test -x /bin/ksh ; then
			USER_SHELL=/bin/ksh
		fi
		if test -x /bin/bash ; then
			USER_SHELL=/bin/bash
		fi
	;;
esac

AC_CHECK_HEADERS([direct.h])	# For MinGW builds (needs mkdir(2) defined)

#
# Make sure we have the Open-Cobol compiler :
#
AC_CHECK_PROG([COBCURSES_HAS_COBC],[cobc],[yes],[no])
if test "$COBCURSES_HAS_COBC" != yes ; then
       echo "ERROR: There appears to be no access to the "
       echo "Open-Cobol compiler command 'cobc'. Please "
       echo "check that the compiler is correctly installed."
       exit 1
fi

AC_PREFIX_DEFAULT([/usr/local])

#
# Check compiler's version :
#
# cobc (OpenCOBOL) 0.33.0
#
COBC_VERS=`cobc --version | sed '2,$d;s|^.*) *||g'`
[COBC_MAJ_VERS=`expr "$COBC_VERS" : '\([0-9]*\)'`]
[COBC_MIN_VERS=`expr "$COBC_VERS" : '[0-9]*\.\([0-9]*\)'`]
[COBC_DOT_VERS=`expr "$COBC_VERS" : '[0-9]*\.[0-9]*\.\([0-9]*\)'`]

if test $COBC_MAJ_VERS -le 0 ; then
	if test $COBC_MIN_VERS -lt 33 ; then
		echo "Your Open-Cobol compilter must be version 0.33.0"
		echo "or later. Even at 0.33.0, make sure that for HP"
		echo "ports that it is newer than May 2007."
		exit 1
	fi
fi

libexecdir='${prefix}/libexec/cobcurses'

#
# This is where the user's personal Indexed Data Files go
#
COBCURSES_INST_DATAROOTDIR="$datarootdir"
COBCURSES_INST_DATADIR="$datadir/cobcurses"
COBCURSES_INST_BINDIR="${COBCURSES_INST_DATADIR}/bin"
COBCURSES_INST_LIBDIR="$libdir"
COBCURSES_INST_LIBEXECDIR="$libexecdir"

#
# Where do the CobCurses files go?
#
case $host_os in
	MINGW* )	COBCURSES_TOPDIR="${prefix}/COBCURSES"
			libexecdir="$COBCURSES_TOPDIR/bin";;
	* )		COBCURSES_TOPDIR='${HOME}/.cobcurses';;
esac
COBCURSES_DATADIR='${COBCURSES_TOPDIR}/data'
eval COBCURSES_SHAREDIR="$COBCURSES_INST_DATADIR"	# To resolve indirection
eval COBCURSES_SHAREDIR="$COBCURSES_SHAREDIR"		# One more time to resolve ${prefix}

AC_CONFIG_HEADERS([config.h])
      
# Checks for library functions.

AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([malloc realloc memcpy memset free abort atexit sleep setenv strtof])

#
# Determine the shared library extension that Open COBOL uses :
#
(cd ./lib ; cobc -b shlibtest.cob )
COBCURSES_MODULE_EXT=`cd ./lib; echo shlibtest.* | sed 's| *shlibtest.cob *||;s|shlibtest.||'`
rm -f lib/shlibtest.${COBCURSES_MODULE_EXT}

#
# Check for ncurses/pdcurses/curses.
#
USE_NCURSES=no
USE_CURSES=no
USE_PDCURSES=no
WHICH_CURSES=none

if test "$enable_ncurses" = "yes" ; then
	enable_curses=no
	enable_pdcurses=no
	USE_NCURSES=yes
	USE_CURSES=no
	USE_PDCURSES=no
	WHICH_CURSES=ncurses
fi

if test "$enable_curses" = "yes" ; then
	enable_ncurses=no
	enable_pdcurses=no
	USE_NCURSES=no
	USE_CURSES=yes
	USE_PDCURSES=no
	WHICH_CURSES=curses
fi

if test "$enable_pdcurses" = "yes" ; then
	enable_curses=no
	enable_ncurses=no
	USE_NCURSES=no
	USE_CURSES=no
	USE_PDCURSES=yes
	WHICH_CURSES=pdcurses
fi

# curses :   Supplies curses.h  & libcurses
# ncurses :  Supplies ncurses.h or ncurses/ncurses.h & libncurses
# PDCurses : Supplies pdcurses.h & libpdurses

if test $WHICH_CURSES = none ; then 
#	AC_CHECK_HEADERS([ncurses.h ncurses/ncurses.h curses.h pdcurses.h])
	AC_CHECK_LIB([ncurses], [initscr])
	if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
		USE_NCURSES="yes"
		WHICH_CURSES=ncurses
	else
		AC_CHECK_LIB([curses], [initscr])
		if test "x$ac_cv_lib_curses_initscr" = "xyes"; then
			USE_CURSES="yes"
			WHICH_CURSES=curses
		else
			AC_CHECK_LIB([pdcurses], [initscr])
			if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"; then
				USE_PDCURSES=yes
				WHICH_CURSES=pdcurses
			fi
		fi  
	fi    
fi

if test $WHICH_CURSES = ncurses ; then
	AC_CHECK_LIB([ncurses], [getmouse])
	if test "x$ac_cv_lib_ncurses_getmouse" = xyes ; then
		COBCURSES_MOUSE_SUPPORT="$enable_mouse"
	fi
fi

if test $USE_CURSES = no ; then
  if test $USE_NCURSES = no ; then
    if test $USE_PDCURSES = no ; then
	echo "You don't seem to have any curses support."
	echo
	echo "These options also influence the choice of support:"
	echo
	echo "  --enable-curses     (standard curses)"
	echo "  --enable-ncurses    (use ncurses)"
	echo "  --enable-pdcurses   (use PDCurses)"
	exit 1
    fi
  fi
fi

#
# If one of the enable-*curses options used, then we still
# need to scan for support :
#
if test $WHICH_CURSES != none ; then
  if test $USE_NCURSES = yes ; then
	AC_CHECK_HEADERS([ncurses.h ncurses/ncurses.h curses.h pdcurses.h])
	AC_CHECK_LIB([ncurses], [initscr])
	if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
		LIBCOBCURSES_LIBS="$LIBCOBCURSES_LIBS -lncurses"
	fi
  fi
  if test $USE_CURSES = yes ; then
	AC_CHECK_HEADERS([curses.h])
	AC_CHECK_LIB([curses], [initscr])
	if test "x$ac_cv_lib_curses_initscr" = "xyes"; then
		LIBCOBCURSES_LIBS="$LIBCOBCURSES_LIBS -lcurses"
	fi
  fi
  if test $USE_PDCURSES = yes ; then
	AC_CHECK_HEADERS([pdcurses.h])
	AC_CHECK_LIB([pdcurses], [initscr])
	if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"; then
		LIBCOBCURSES_LIBS="$LIBCOBCURSES_LIBS -lpdcurses"
	fi
  fi
else
  echo "WARNING: No specific implementation of curses was specified."
  none=yes
fi

if test $USE_NCURSES = yes ; then
	echo "DEFAULT: Using ncurses.."
	none=no
fi
if test $USE_CURSES = yes ; then
	echo "DEFAULT: Using the system standard curses.."
	none=no
fi
if test $USE_PDCURSES = yes ; then
	echo "DEFAULT: Using pdcurses.."
	none=no
fi

#
#	INSIST ON SOME FORM OF CURSES :
#
if test $none = yes ; then
	echo "ERROR: No curses support found."
	exit 1
fi

# Hack to stop CXX/F77 checks (stole from OC 0.33)
tagnames=

if echo $CFLAGS | grep 'aix64' 1>/dev/null 2>&1
then
        if test -f /usr/ccs/bin/ar
        then
                AR="/usr/ccs/bin/ar -X64"
        else
                AR="ar -X64"
        fi
        NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AH_TEMPLATE([COBCURSES_STRIP_CMD], [Strip command])
AH_TEMPLATE([COBCURSES_MOUSE_SUPPORT], [Mouse is supported in ncurses])
AH_TEMPLATE([COBCURSES_ATEXIT_SUPPORT], [Compile in cobcurses.c atexit(3C) support])
AH_TEMPLATE([USE_NCURSES], [Using ncurses(3X) support])
AH_TEMPLATE([USE_CURSES], [Using older curses(3X) support])
AH_TEMPLATE([USE_PDCURSES], [Using public domain curses support])
AH_TEMPLATE([COBCURSES_TOPDIR], [User's own top-level cobcurses directory])
AH_TEMPLATE([COBCURSES_DATADIR], [User's own cobcurses/data directory])
AH_TEMPLATE([COBCURSES_SHAREDIR], [Common files directory for CobCurses])
AH_TEMPLATE([BUILD_MINGW], [Building for a MinGW environment])

AH_TEMPLATE([USER_SHELL], [User shell to use])

# Special stuff

case "$host_os" in
HPUX )		AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl
		;;
* )		;;
esac

AC_CHECK_HEADERS([libcob.h stdlib.h memory.h string.h ctype.h malloc.h getopt.h unistd.h])

# CobCurses Configuration

CPPFLAGS="${CPPFLAGS} -I.."
COBCURSES_INCLUDE_DIR="$includedir/cobcurses"
COBCURSES_LIBS="-L$libdir ${LIBCOBCURSES_LIBS}"
COBCURSES_LDFLAGS=""
COBCURSES_LIBRARY_PATH="$libdir"

#
# This is for : cobcurses-config --cflags
#
COBCURSES_CFLAGS="-I${COBCURSES_INCLUDE_DIR}"

case $host_os in
	CYGWIN* )
	        ;;
	HP-UX )  
        	if test "`basename $CC`" != "gcc" ; then
	                AC_DEFINE([_XOPEN_SOURCE_EXTENDED],1)
	                CFLAGS="$CFLAGS +ESlit +Z -O "
	        fi

		#
		# For cc compiler, cannot use -g with -O when using +DA2.0*
		#
		if test x"$CC" = "xcc" ; then
			case "$hp_arch" in
			+DA2.0W | +DD64 | +DA2.0N )
				if test `expr "$CFLAGS" : '.* -O.* .*'` -gt 0 ; then
					# Remove the -g option to avoid warnings
					CPPFLAGS=`echo "$CFLAGS" | sed 's| -g | |g;s|^-g ||g'`
					CFLAGS=`echo "$CFLAGS" | sed 's| -g | |g;s|^-g ||g'`
					echo "Note: Dropping -g compile option, which is incompatible with -O in $hp_arch"
				fi;;
			* )	# Must add -DPIC for non 64-bit builds :
				CFLAGS="$CFLAGS -DPIC";;
			esac
		fi

		#
		# HP's install doesn't add +x for scripts!!
		# and leaves mode 500 for *.sl's!!
		#
		INSTALL="${SHELL} ${PWD}/install-sh"
	        ;;
	* )	;;
esac

if test "`basename $CC`" = "gcc"
then
        CFLAGS="$CFLAGS -funsigned-char -Wall -Wwrite-strings -Wmissing-prototypes"
fi

#
# Generate the output
#
if test "x$striplib" != "x"
then
  AC_DEFINE_UNQUOTED(COBCURSES_STRIP_CMD, "$striplib")
fi
if test "$COBCURSES_MOUSE_SUPPORT" = "yes" ; then
	AC_DEFINE_UNQUOTED(COBCURSES_MOUSE_SUPPORT)
fi
if test "$COBCURSES_ATEXIT_SUPPORT" = "yes"; then
	AC_DEFINE_UNQUOTED(COBCURSES_ATEXIT_SUPPORT)
fi
if test "$USE_NCURSES" = "yes" ; then
	AC_DEFINE_UNQUOTED(USE_NCURSES)
fi
if test "$USE_CURSES" = "yes" ; then
	AC_DEFINE_UNQUOTED(USE_CURSES)
fi
if test "$USE_PDCURSES" = "yes" ; then
	AC_DEFINE_UNQUOTED(USE_PDCURSES)
fi

AC_SUBST(VERSION)
AC_SUBST(COBCURSES_INCLUDE_DIR)
AC_SUBST(COBCURSES_LIBS)
AC_SUBST(COBCURSES_LIBRARY_PATH)
AC_SUBST(COBCURSES_MODULE_EXT)
AC_SUBST(COBCURSES_CFLAGS)
AC_SUBST(COBCURSES_LDFLAGS)
AC_SUBST(COBCURSES_INST_DATAROOTDIR)
AC_SUBST(COBCURSES_INST_DATADIR)
AC_SUBST(COBCURSES_TOPDIR)
AC_SUBST(COBCURSES_DATADIR)
AC_SUBST(COBCURSES_SHAREDIR)
AC_SUBST(COBCURSES_INST_BINDIR)
AC_SUBST(COBCURSES_INST_LIBDIR)
AC_SUBST(COBCURSES_INST_LIBEXECDIR)
AC_SUBST(SHARED_PIC_FLAGS)

AC_DEFINE_UNQUOTED(COBCURSES_TOPDIR,"${COBCURSES_TOPDIR}")
AC_DEFINE_UNQUOTED(COBCURSES_DATADIR,"${COBCURSES_DATADIR}")
AC_DEFINE_UNQUOTED(COBCURSES_SHAREDIR,"${COBCURSES_SHAREDIR}")

if test $enable_mingw = yes ; then
	AC_DEFINE_UNQUOTED(BUILD_MINGW)
fi

AC_DEFINE_UNQUOTED(USER_SHELL,"$USER_SHELL")

#COBCURSES_BIGENDIAN="$ac_cv_c_bigendian"
#AC_SUBST(COBCURSES_BIGENDIAN) 

AC_CONFIG_FILES([
	Makefile
	cobcurses-config
	bak/Makefile
	copybk/Makefile
	lib/Makefile
	utils/Makefile
	sdesign/Makefile
	edesign/Makefile
	test/Makefile
])

AC_OUTPUT

cat <<EOF

CobCurses Configuration:

  host_os .....................	${host_os}
  host_os_release .............	${host_os_release}
  Open-Cobol Version .......... ${COBC_VERS}
  INSTALL .....................	${INSTALL}
  CC ..........................	${CC}
  CPPFLAGS ....................	${CPPFLAGS}
  CFLAGS ......................	${CFLAGS}
  LDFLAGS .....................	${LDFLAGS}
  PREFIX ...................... ${prefix}
  libexecdir .................. ${libexecdir}
  LIBCOBCURSES_LIBS ........... ${LIBCOBCURSES_LIBS}
  COBCURSES_INCLUDE_DIR .......	${COBCURSES_INCLUDE_DIR}
  COBCURSES_INST_DATAROOTDIR ..	${COBCURSES_INST_DATAROOTDIR}
  COBCURSES_INST_DATADIR ......	${COBCURSES_INST_DATADIR}
  COBCURSES_TOPDIR ............	${COBCURSES_TOPDIR}
  COBCURSES_DATADIR ...........	${COBCURSES_DATADIR}
  COBCURSES_SHAREDIR .......... ${COBCURSES_SHAREDIR}
  COBCURSES_LDFLAGS ...........	${COBCURSES_LDFLAGS}
  COBCURSES_LIBRARY_PATH ......	${COBCURSES_LIBRARY_PATH}
  COBCURSES_MODULE_EXT ........	${COBCURSES_MODULE_EXT}
  WHICH_CURSES ................ ${WHICH_CURSES}
  COBCURSES_ATEXIT_SUPPORT .... ${COBCURSES_ATEXIT_SUPPORT}
  COBCURSES_MOUSE_SUPPORT ..... ${COBCURSES_MOUSE_SUPPORT}

Note:
-----
  Make sure that the curses library used, matches the same
  one that the Open-Cobol compiler was built with.

EOF

#
#	Issue HPUX LPATH warning:
#
case "$hp_arch" in
+DA2.0W | +DD64 )
	cat <<EOF

For $hp_arch architecture, be sure to:

	LPATH=/usr/lib/pa20_64:/usr/lib
	export LPATH

before you start make.

EOF
	;;
+DA2.0N )
	cat <<EOF

For $hp_arch architecture, be sure to:

	LPATH=/usr/lib/pa20_32:/usr/lib
	export LPATH

before you start make.

EOF
	;;
* ) 	;;
esac

# End configure.ac
