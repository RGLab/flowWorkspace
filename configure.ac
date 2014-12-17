#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([flowWorkspace], [1.5.58], [wjiang2@fhcrc.org])
AC_CONFIG_SRCDIR([src/GatingHierarchy.cpp])
#AC_CONFIG_HEADERS([config.h])
AC_ARG_WITH(xml2,
           [AS_HELP_STRING([--with-xml2=DIR],
                           [root directory of xml2 installation (defaults to /usr/local)])],
           [XML2_DIR="${with_xml2}"],
	   [XML2_DIR="yes"])
AC_ARG_WITH(boost,
           [AS_HELP_STRING([--with-boost=DIR],
                           [root directory of boost installation (defaults to /usr/local)])],
           [BOOST_DIR="${with_boost}"],
	   [BOOST_DIR=""])
AC_ARG_WITH(protobuf,
           [AS_HELP_STRING([--with-protobuf=DIR],
                           [root directory of protocol buffer installation (defaults to /usr/local)])],
           [PB_DIR="${with_protobuf}"],
	   [PB_DIR="yes"])



# Checks for programs.
AC_PROG_CXX
AC_PROG_CC

# Checks for libraries.

# Checks for header files.


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_SIZE_T

# Check the compiler configured with R
COMPILER=`R CMD config CC`

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


#boost serialzation library
if test  "${BOOST_DIR}" = "yes" || test -z "${BOOST_DIR}"; then
        AC_MSG_NOTICE([No directory was specified for --with-boost. Using bundled boost.])

	BASEBOOSTNAME="bst"
	BOOSTTGZNAME=${BASEBOOSTNAME}.tar.gz
	cd src
	if test -d ./${BASEBOOSTNAME};
		then 
			echo 'found ' $BASEBOOSTNAME ' header sources and tar archive;using what is there.'
		else
			echo "untarring boost include tree...";
			gunzip -dc ${BOOSTTGZNAME} | tar xf -;
	fi;
	cd ../
	boost_bundled=yes
else
        AC_MSG_NOTICE([Using external boost '${BOOST_DIR}'])
        BOOST_CFLAGS="-I${BOOST_DIR}/include/boost"
        BOOST_LIBS="-L${BOOST_DIR}/lib -L${BOOST_DIR}/lib64 -lboost_serialization -lboost_regex"
        boost_bundled=no
fi

AC_MSG_NOTICE([setting protobuf flags...])

if test  "${PB_DIR}" = "yes" ; then
        AC_MSG_NOTICE([No directory was specified for --with-protobuf. Trying to find protobuf using other methods.])
        AC_PATH_PROG(PKG_CONFIG, pkg-config)

        if test -z "${PKG_CONFIG}" ; then
                AC_MSG_NOTICE([pkg-config was not detected. Verify pkg-config is installed correctly.])
                AC_MSG_NOTICE([Trying with /usr/local anyway.])
                PB_CFLAGS="-I/usr/local/include"
		PB_LIBS="-L/usr/local/lib -lprotobuf"        
	else
                PB_CFLAGS="`${PKG_CONFIG} --cflags protobuf`"
                PB_LIBS="`${PKG_CONFIG} --libs protobuf`"
        fi
else
        AC_MSG_NOTICE([Using protobuf dir '${PB_DIR}'])

        PB_CFLAGS="-I${PB_DIR}/include"
        PB_LIBS="-L${PB_DIR}/lib -lprotobuf"
fi
#protocol buffer library
#if test  "${PB_DIR}" = "yes" || test -z "${PB_DIR}"; then
#        AC_MSG_NOTICE([No directory was specified for --with-protobuf. Using bundled protocol buffer.])
#
#	BASEPBNAME="gpb"
#	PBTGZNAME=${BASEPBNAME}.tar.gz
#	cd src
#	if test -d ./${BASEPBNAME};
#		then 
#			echo 'found ' $BASEPBNAME ' header sources and tar archive;using what is there.'
#		else
#			echo "untarring boost include tree...";
#			gunzip -dc ${PBTGZNAME} | tar xf -;
#	fi;
#	cd ../
#	boost_bundled=yes
#else
#        AC_MSG_NOTICE([Using external boost '${PB_DIR}'])
#        PB_CFLAGS="-I${PB_DIR}/include"
#        PB_LIBS="-L${PB_DIR}/lib -lprotobuf"
#        boost_bundled=no
#fi



PKG_CPPFLAGS="$XML2_CFLAGS $BOOST_CFLAGS $PB_CFLAGS"
if test "${COMPILER}" = "clang"; then
  PKG_CPPFLAGS+=" -ftemplate-depth=900"
fi

PKG_LIBS="$XML2_LIBS $BOOST_LIBS $PB_LIBS"

# Checks for library functions.
AC_MSG_NOTICE([Using the following compilation and linking flags for flowWorkspace])
AC_MSG_NOTICE([   PKG_CPPFLAGS=${PKG_CPPFLAGS}])
AC_SUBST(PKG_CPPFLAGS)
AC_MSG_NOTICE([   PKG_LIBS=${PKG_LIBS}])
AC_SUBST(PKG_LIBS)
AC_SUBST(boost_bundled)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
