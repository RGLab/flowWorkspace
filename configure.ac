AC_PREREQ(2.61)

AC_INIT([flowWorkspace],0.99.10)

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

## pick up compiler as will be used by R CMD INSTALL/SHLIB
if test -n "${R_HOME}"; then
  CC=`${R_HOME}/bin/R CMD config CC`
  CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`
fi

#test for different system paths to search for libraries and include files
AC_MSG_CHECKING(searching for non-standard locations of system libraries)
if test   -d "/opt/local/include"  -a  -d "/opt/local/lib" ; then
	AC_MSG_RESULT(/opt/local)
	CFLAGS="$CFLAGS -I/opt/local/include"
	CXXFLAGS="$CXXFLAGS -I/opt/local/include"
	LDFLAGS="$LDFLAGS -L/opt/local/lib"
fi
if test  -e /opt/include  -a  -e /opt/lib  -a  -d /opt/include  -a  -d /opt/lib  ; then
        AC_MSG_RESULT(/opt)
		CFLAGS="$CFLAGS -I/opt/include"
        CXXFLAGS="$CXXFLAGS -I/opt/include"
        LDFLAGS="$LDFLAGS -L/opt/lib"
fi

if test -e /sw/local/include  -a  -e /sw/local/lib  -a  -d /sw/local/include  -a  -d /sw/local/lib ; then
        AC_MSG_RESULT(/sw/local)
		CFLAGS="$CFLAGS -I/sw/local/include"
        CXXFLAGS="$CXXFLAGS -I/sw/local/include"
        LDFLAGS="$LDFLAGS -L/sw/local/lib"
fi

if test -e /sw/include  -a  -e /sw/lib  -a  -d /sw/include  -a -d /sw/lib  ; then
        AC_MSG_RESULT(/sw)
		CFLAGS="$CFLAGS -I/sw/include"
        CXXFLAGS="$CXXFLAGS -I/sw/include"
        LDFLAGS="$LDFLAGS -L/sw/lib"
fi
AC_MSG_RESULT(done)

if  test -e /usr/local/include  -a  -e /usr/local/lib  -a  -d /usr/local/include   -a  -d /usr/local/lib ; then
		CFLAGS="$CFLAGS -I/usr/local/include"
        CXXFLAGS="$CXXFLAGS -I/usr/local/include"
        LDFLAGS="$LDFLAGS -L/usr/local/lib"
fi
if test -d /usr/include  -a  -d /usr/lib ; then
		CFLAGS="$CFLAGS -I/usr/include"
        CXXFLAGS="$CXXFLAGS -I/usr/include"
        LDFLAGS="$LDFLAGS -L/usr/lib"
fi

if test -e $HOME/include -a -e $HOME/lib -a -d $HOME/include -a -d $HOME/lib ; then
	AC_MSG_RESULT($HOME)
		CFLAGS="-I$HOME/include $CFLAGS"
	CXXFLAGS="-I$HOME/include $CXXFLAGS"
	LDFLAGS="-L$HOME/lib $LDFLAGS"
fi

if test -e $HOME/local/include -a -e $HOME/local/lib -a -d $HOME/local/include -a -d $HOME/local/lib ; then
	AC_MSG_RESULT($HOME)
		CFLAGS="-I$HOME/local/include $CFLAGS"
	CXXFLAGS="-I$HOME/local/include $CXXFLAGS"
	LDFLAGS="-L$HOME/local/lib $LDFLAGS"
fi

CFLAGS="$CFLAGS"
CXXFLAGS="$CXXFLAGS"
LDFLAGS="$LDFLAGS"

AC_ARG_WITH([netcdf],[AS_HELP_STRING([--with-netcdf], [Support for netcdf])],[],[with_readline=check])

LIBNETCDF=
AS_IF([test "x$with_netcdf" != xno],
[AC_CHECK_LIB([netcdf], [nc_def_var_deflate],
[AC_SUBST([LIBNETCDF], ["-lnetcdf"])
AC_DEFINE([HAVE_LIBNETCDF], [1],
[Define if you have netcdf])
AC_SEARCH_LIBS(nc_def_var_deflate,netcdf,,)
AC_CHECK_HEADERS([netcdf.h],[],[])
if test "x$ac_cv_lib_netcdf_nc_def_var_deflate" = "xyes" -a "x$ac_cv_header_netcdf_h" = "xyes"; then
  HAVE_NETCDF=TRUE
else
  HAVE_NETCDF=FALSE
  AC_MSG_WARN(Package should still build but you won't have netcdf functionality)
fi
],
[if test "x$with_netcdf" != x; then
AC_MSG_WARN([--with-netcdf was given, but test for netcdf failed])
fi
HAVE_NETCDF=FALSE
AC_MSG_WARN(Package should still build but you won't have netcdf functionality)

], )
])

AC_C_CONST
AC_TYPE_SIZE_T

AC_SUBST(HAVE_NETCDF)
AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(DEFS)
AC_CONFIG_FILES([R/AllMethods.R])
AC_OUTPUT

