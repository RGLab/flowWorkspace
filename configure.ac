#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# This autoconf based configure is replaced by more portable R package 'configure' 
AC_PREREQ([2.65])
AC_INIT([cytolib], [2.0], [wjiang2@fhcrc.org])
AC_PROG_SED

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi

CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CXX

## Can we use pkg-config?
AC_PATH_PROG(have_pkg_config, pkg-config, no)
## If yes, also check for whether pkg-config knows tiledb
if test x"${have_pkg_config}" != x"no"; then
    AC_MSG_CHECKING([pkg-config knows TileDB])
    if pkg-config --exists tiledb; then
        AC_MSG_RESULT([yes])
        tiledb_include=$(pkg-config --cflags tiledb)
        tiledb_libs=$(pkg-config --libs tiledb)
        AC_SUBST([TILEDB_INCLUDE], "${tiledb_include}")
        AC_SUBST([TILEDB_LIBS],    "${tiledb_libs}")
        AC_SUBST([TILEDB_RPATH],   "")
    else
        AC_MSG_RESULT([no])
        have_pkg_config="no"
    fi
fi
## Otherwise use fallback values
if test x"${have_pkg_config}" = x"no"; then
    ## Default values suitable for system install, overridde as needed below
    AC_SUBST([TILEDB_INCLUDE], "")
    AC_SUBST([TILEDB_LIBS], "-ltiledb")
    AC_SUBST([TILEDB_RPATH], "")
fi

## Top-level system, recognising SunOS (Solaris), Darwin (macOS), Linux
uname=`uname`

AC_ARG_WITH([tiledb],
            AC_HELP_STRING([--with-tiledb=PREFIX],
                       [path to installed built of TileDB]),
            [TILEDB_INCLUDE="-I${with_tiledb}/include ${TILEDB_INCLUDE}"
             TILEDB_LIBS="-L${with_tiledb}/lib ${TILEDB_LIBS}"
             TILEDB_RPATH="-Wl,-rpath,${with_tiledb}/lib"],
            [])


## Setup
origCPPFLAGS="${CPPFLAGS}"
origLDFLAGS="${LDFLAGS}"
CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

#
AC_CHECK_HEADER([tiledb/tiledb.h], [have_tiledb="yes"], [have_tiledb="no"])
if test x"${have_tiledb}" = x"no"; then
	AC_MSG_NOTICE([No tiledb is found, build without tiledb...])
else
	AC_MSG_NOTICE([tiledb is found, build with tiledb enabled.])
	AC_MSG_NOTICE([Using the following compilation and linking flags])
    AC_SUBST([TILEDB_INCLUDE], ${TILEDB_INCLUDE})
    AC_SUBST([TILEDB_LIBS], ${TILEDB_LIBS})
    #AC_SUBST([TILEDB_RPATH], ${TILEDB_RPATH})

	AC_MSG_NOTICE([   TILEDB_INCLUDE=${TILEDB_INCLUDE}])
	AC_MSG_NOTICE([   TILEDB_LIBS=${TILEDB_LIBS}])
fi

    AC_SUBST([HAVE_TILEDB], ${have_tiledb})

	   
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_FILES([R/have_tiledb.R])
AC_OUTPUT
