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
AC_ARG_WITH(protobuf,
           [AS_HELP_STRING([--with-protobuf=DIR],
                           [root directory of protocol buffer installation (defaults to /usr/local)])],
           [PB_DIR="${with_protobuf}"],
	   [PB_DIR="yes"])




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
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
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


AC_MSG_NOTICE([setting protobuf flags...])

USE_BUNDLED_PB=false

if test  "${PB_DIR}" = "yes" || test -z "${PB_DIR}" ; then
        AC_MSG_NOTICE([No directory was specified for --with-protobuf. Using bundled protocol buffer.])

		BASEPBNAME="protobuf-2.6.0"
		PBTGZNAME=${BASEPBNAME}.tgz
		cd src
		if test -d ./${BASEPBNAME}; then 
			echo 'found ' $BASEPBNAME ' header sources and tar archive;using what is there.'
		else
			echo "untarring protobuf ...";
			gunzip -dc ${PBTGZNAME} | tar xf -;
		fi;

        #run this bootstrap script to update all the gnu auto build files (e.g.  automake symlinked fils (e.g. ./compile), alocal.m4,etc... )		
        cd ${BASEPBNAME}
        ./autogen.sh
        automake --add-missing #do this because the old Autoconf (e.g. 2.13) may not handle this in autoreconf
        mv m4/lt~obsolete.m4 m4/lt-obsolete.m4 # R CMD check gives warning about ~ character in filename 
        cd ..

		echo "building protobuf...";
		PBBUILD="pb_build"
		if test -d ./${PBBUILD}; then
			echo 'found ' $PBBUILD ' ;using what is there.'
		else
			mkdir ${PBBUILD}
		fi;
		cd ${PBBUILD};
		../${BASEPBNAME}/configure --enable-static=no CXX="${CXX}" CXXFLAGS="${CXXFLAGS}" CC="${CC}" CFLAGS="${CFLAGS}"
		$MAKE 
	
		PB_CFLAGS="-I${BASEPBNAME}/src"
		#PB_LIBS will be set in Makevars file
		cd ../../
		USE_BUNDLED_PB=true
else
        AC_MSG_NOTICE([Using protobuf dir '${PB_DIR}'])

        PB_CFLAGS="-I${PB_DIR}/include"
        PB_LIBS="-L${PB_DIR}/lib -lprotobuf"
fi



PKG_CPPFLAGS="$XML2_CFLAGS $PB_CFLAGS"
if grep -q "^clang" <<< "${CC}" ; then
  PKG_CPPFLAGS+=" -ftemplate-depth=900"
fi

PKG_LIBS="$XML2_LIBS $PB_LIBS"

# Checks for library functions.
AC_MSG_NOTICE([Using the following compilation and linking flags for flowWorkspace])
AC_MSG_NOTICE([   PKG_CPPFLAGS=${PKG_CPPFLAGS}])
AC_SUBST(PKG_CPPFLAGS)
AC_MSG_NOTICE([   PKG_LIBS=${PKG_LIBS}])
AC_SUBST(PKG_LIBS)
if test USE_BUNDLED_PB = true; then
	AC_MSG_NOTICE([   bundled protobuf is in ${PBBUILD}])
fi
AC_SUBST(PBBUILD)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
