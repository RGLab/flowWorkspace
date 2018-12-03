#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([flowWorkspace], [1.5.58], [wjiang2@fhcrc.org])
AC_CONFIG_SRCDIR([src/R_GatingSet.cpp])
#AC_CONFIG_HEADERS([config.h])
AC_ARG_WITH(xml2,
           [AS_HELP_STRING([--with-xml2=DIR],
                           [root directory of xml2 installation (defaults to /usr/local)])],
           [XML2_DIR="${with_xml2}"],
	   [XML2_DIR="yes"])


# Checks for libraries.

# Checks for header files.


# Checks for typedefs, structures, and compiler characteristics.(these two macros caches the CC setting thus void the settings of below)
#AC_HEADER_STDBOOL
#AC_TYPE_SIZE_T

# Check the compiler configured with R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
COMPILER=`"${R_HOME}/bin/R" CMD config CC`
CXX11=`"${R_HOME}/bin/R" CMD config CXX11`
CXX11STD=`"${R_HOME}/bin/R" CMD config CXX11STD`
CXX="${CXX11} ${CXX11STD}"
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
AC_LANG(C++)

CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
MAKE=`${R_HOME}/bin/R CMD config MAKE`

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC

#xml2 lib
AC_MSG_NOTICE([setting xml2 flags...])

if test  "${XML2_DIR}" = "yes" ; then
        AC_MSG_NOTICE([No directory was specified for --with-xml2. Trying to find xml2 using other methods.])
        AC_PATH_PROG(XML2_CONFIG, xml2-config)

        if test -z "${XML2_CONFIG}" ; then
                AC_MSG_NOTICE([xml2-config was not detected. Verify xml2 is installed correctly.])
                AC_MSG_NOTICE([Trying with /usr/local anyway.])
                XML2_CFLAGS="-I/usr/local/include/libxml2"
		XML2_LIBS="-L/usr/local/lib -lxml2"        
	else
                XML2_CFLAGS="`${XML2_CONFIG} --cflags`"
                XML2_LIBS="`${XML2_CONFIG} --libs`"
        fi
else
        AC_MSG_NOTICE([Using xml2 dir '${XML2_DIR}'])

        XML2_CFLAGS="-I${XML2_DIR}/include/libxml2"
        XML2_LIBS="-L${XML2_DIR}/lib -lxml2"
fi


PKG_CPPFLAGS="$XML2_CFLAGS"
if grep -q "^clang" <<< "${CC}" ; then
  PKG_CPPFLAGS+=" -ftemplate-depth=900"
fi

PKG_LIBS="$XML2_LIBS"

# Checks for library functions.
AC_MSG_NOTICE([Using the following compilation and linking flags for flowWorkspace])
AC_MSG_NOTICE([   PKG_CPPFLAGS=${PKG_CPPFLAGS}])
AC_SUBST(PKG_CPPFLAGS)
AC_MSG_NOTICE([   PKG_LIBS=${PKG_LIBS}])
AC_SUBST(PKG_LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
