dnl Run autoconf to generate configure from this file
dnl Based on configure.ac from HSQL

AC_INIT([HaskellDB],[0.9],[haskelldb-users@lists.sourceforge.net],[haskelldb])

AC_PREREQ(2.53)

AC_REVISION($Revision: 1.40 $)

AC_CONFIG_FILES([config.mk haskelldb.pkg])

AC_CANONICAL_SYSTEM

dnl ***********************************************
dnl Some defaults
dnl ***********************************************

ghc_req_major_verison=6
ghc_req_minor_verison=2

GHCFLAGS="$GHCFLAGS -fglasgow-exts -fallow-overlapping-instances"

HUGSFLAGS="$HUGSFLAGS -98 +o"


dnl ***********************************************
dnl Enable/Disable verbose warnings
dnl ***********************************************

AC_ARG_ENABLE(verbose-warnings,
AC_HELP_STRING([--enable-verbose-warnings],
	       [Turn on lots of warnings in ghc. Mainly for 
		maintainer use. (default = no)]),
[if test "$enableval" = "yes"; then
	GHCFLAGS="$GHCFLAGS -W -fwarn-missing-signatures"
fi],
)

dnl ***********************************************
dnl Enable/Disable building Hugs libraries
dnl ***********************************************

AC_ARG_ENABLE(hugs,
AC_HELP_STRING([--enable-hugs],
	       [Enable builing Hugs libraries. 
	        (default = yes)]),
[ENABLE_HUGS="$enableval"],
[ENABLE_HUGS=yes]
)
AC_SUBST(ENABLE_HUGS)

dnl ***********************************************
dnl Enable/Disable building GHC libraries
dnl ***********************************************

AC_ARG_ENABLE(ghc,
AC_HELP_STRING([--enable-ghc],
	       [Enable builing GHC libraries. 
	        (default = yes)]),
[ENABLE_GHC="$enableval"],
[ENABLE_GHC=yes]
)
AC_SUBST(ENABLE_GHC)

dnl ***********************************************
dnl Enable/Disable HSQL
dnl ***********************************************

AC_ARG_ENABLE(hsql,
AC_HELP_STRING([--enable-hsql],
	       [Enable use of HSQL. (default = yes)]),
[WITH_HSQL="$enableval"],
[WITH_HSQL=yes]
)

dnl ***********************************************
dnl Enable/Disable HSQL for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-hsql,
AC_HELP_STRING([--enable-ghc-hsql],
	       [Enable use of HSQL with ghc. (default = yes)]),
[WITH_GHC_HSQL="$enableval"],
[WITH_GHC_HSQL="$WITH_HSQL"]
)
AC_SUBST(WITH_GHC_HSQL)

dnl ***********************************************
dnl Enable/Disable HSQL for hugs
dnl ***********************************************

AC_ARG_ENABLE(hugs-hsql,
AC_HELP_STRING([--enable-hugs-hsql],
	       [Enable use of HSQL for Hugs. (default = yes)]),
[WITH_HUGS_HSQL="$enableval"],
[WITH_HUGS_HSQL="$WITH_HSQL"]
)
AC_SUBST(WITH_HUGS_HSQL)

dnl ***********************************************
dnl Enable/Disable HSQL ODBC binding for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-hsql-odbc,
AC_HELP_STRING([--enable-ghc-hsql-odbc],
	       [Enable use of the HSQL ODBC driver
	       for ghc. (default = yes)]),
[WITH_GHC_HSQL_ODBC="$enableval"],
[WITH_GHC_HSQL_ODBC=yes]
)
AC_SUBST(WITH_GHC_HSQL_ODBC)

dnl ***********************************************
dnl Enable/Disable HSQL ODBC binding for Hugs
dnl ***********************************************

AC_ARG_ENABLE(hugs-hsql-odbc,
AC_HELP_STRING([--enable-hugs-hsql-odbc],
	       [Enable use of the HSQL ODBC driver
	        for Hugs. (default = yes)]),
[WITH_HUGS_HSQL_ODBC="$enableval"],
[WITH_HUGS_HSQL_ODBC=yes]
)
AC_SUBST(WITH_HUGS_HSQL_ODBC)

dnl ***********************************************
dnl Enable/Disable HSQL MySQL binding for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-hsql-mysql,
AC_HELP_STRING([--enable-ghc-hsql-mysql],
	       [Enable use of the HSQL MySQL driver  
	        for ghc. (default = yes)]),
[WITH_GHC_HSQL_MYSQL="$enableval"],
[WITH_GHC_HSQL_MYSQL=yes]
)
AC_SUBST(WITH_GHC_HSQL_MYSQL)

dnl ***********************************************
dnl Enable/Disable HSQL MySQL binding for Hugs
dnl ***********************************************

AC_ARG_ENABLE(hugs-hsql-mysql,
AC_HELP_STRING([--enable-hugs-hsql-mysql],
	       [Enable use of the HSQL MySQL driver  
	        for Hugs. (default = yes)]),
[WITH_HUGS_HSQL_MYSQL="$enableval"],
[WITH_HUGS_HSQL_MYSQL=yes]
)
AC_SUBST(WITH_HUGS_HSQL_MYSQL)

dnl ***********************************************
dnl Enable/Disable HSQL SQLite binding for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-hsql-sqlite,
AC_HELP_STRING([--enable-ghc-hsql-sqlite],
	       [Enable use of the HSQL SQLite driver  
	        for ghc. (default = yes)]),
[WITH_GHC_HSQL_SQLITE="$enableval"],
[WITH_GHC_HSQL_SQLITE=yes]
)
AC_SUBST(WITH_GHC_HSQL_SQLITE)

dnl ***********************************************
dnl Enable/Disable HSQL SQLite binding for Hugs
dnl ***********************************************

AC_ARG_ENABLE(hugs-hsql-sqlite,
AC_HELP_STRING([--enable-hugs-hsql-sqlite],
	       [Enable use of the HSQL SQLite driver
	        for Hugs. (default = yes)]),
[WITH_HUGS_HSQL_SQLITE="$enableval"],
[WITH_HUGS_HSQL_SQLITE=yes]
)
AC_SUBST(WITH_HUGS_HSQL_SQLITE)

dnl ***********************************************
dnl Enable/Disable HSQL PostgreSQL binding for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-hsql-postgresql,
AC_HELP_STRING([--enable-ghc-hsql-postgresql],
	       [Enable use of the HSQL PostgreSQL driver  
	        for ghc. (default = yes)]),
[WITH_GHC_HSQL_POSTGRESQL="$enableval"],
[WITH_GHC_HSQL_POSTGRESQL=yes]
)
AC_SUBST(WITH_GHC_HSQL_POSTGRESQL)

dnl ***********************************************
dnl Enable/Disable HSQL PostgreSQL binding for Hugs
dnl ***********************************************

AC_ARG_ENABLE(hugs-hsql-postgresql,
AC_HELP_STRING([--enable-hugs-hsql-postgresql],
	       [Enable use of the HSQL PostgreSQL driver
	        for Hugs. (default = yes)]),
[WITH_HUGS_HSQL_POSTGRESQL="$enableval"],
[WITH_HUGS_HSQL_POSTGRESQL=yes]
)
AC_SUBST(WITH_HUGS_HSQL_POSTGRESQL)

dnl ***********************************************
dnl Enable/Disable WxHaskell binding for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-wx,
AC_HELP_STRING([--enable-ghc-wx],
	       [Enable use of the WxHaskell driver
	       for ghc. (default = yes)]),
[WITH_GHC_WX="$enableval"],
[WITH_GHC_WX=yes]
)
AC_SUBST(WITH_GHC_WX)

dnl ***********************************************
dnl Enable/Disable WxHaskell binding for Hugs
dnl ***********************************************

WITH_HUGS_WX=no
AC_SUBST(WITH_HUGS_WX)

dnl ***********************************************
dnl Enable/Disable using hs-plugins for ghc
dnl ***********************************************

AC_ARG_ENABLE(ghc-plugins,
AC_HELP_STRING([--enable-ghc-plugins],
               [Enable use of hs-plugins
               for ghc. (default = yes)]),
[WITH_GHC_PLUGINS="$enableval"],
[WITH_GHC_PLUGINS=yes]
)
AC_SUBST(WITH_GHC_PLUGINS)


dnl ***********************************************
dnl C compiler
dnl ***********************************************

AC_PROG_CC
AC_SUBST(CC)

dnl ***********************************************
dnl GHC
dnl ***********************************************

if test "$ENABLE_GHC" = "yes"; then
  AC_ARG_WITH(ghc,
  AC_HELP_STRING([--with-ghc=<ghc command>],
               [Use a different command instead of 
                'ghc' for the Haskell compiler.]),
  [AC_CHECK_FILE("$withval",GHC="$withval",[AC_PATH_PROG(GHC,"$withval")])],
  [AC_PATH_PROG(GHC,ghc)])

  if test "$GHC" = "" || test ! -x "$GHC"; then
    AC_MSG_WARN([GHC not found. Not building GHC libraries.])
    GHC=""
    ENABLE_GHC=no
  else
    GHCI=$(dirname $GHC)/ghci
    
    AC_HS_CHECK_GHC_VERSION_IFELSE([$ghc_req_major_verison],
    [$ghc_req_minor_verison],,[
      AC_MSG_ERROR([ghc >= $ghc_req_major_verison.$ghc_req_minor_verison
	            is required to build HaskellDB.]) ])
  fi
fi

AC_SUBST(GHC)
AC_SUBST(GHCI)
AC_SUBST(GHC_VERSION)

dnl ***********************************************
dnl GHC flags
dnl ***********************************************

AC_ARG_WITH(ghc-flags,
AC_HELP_STRING([--with-ghc-flags=FLAGS],
               [Add the given flags when running ghc.]),
[GHCFLAGS="$GHCFLAGS $withval"],)

AC_SUBST(GHCFLAGS)


dnl ***********************************************
dnl GHC directory 
dnl ***********************************************

if test "$ENABLE_GHC" = "yes"; then
  AC_ARG_WITH(ghc-dir,
  AC_HELP_STRING([--with-ghc-dir=DIR],
               [Install GHC libraries in the given GHC directory 
                (default is "ghc --print-libdir")]),
  [GHC_DIR="$withval"],
  [GHC_DIR=`$GHC --print-libdir`])

	if test "$GHC_DIR" = "" || test ! -d $GHC_DIR; then
	   AC_MSG_ERROR([GHC_DIR $GHC_DIR does not exist])
	fi
fi

AC_SUBST(GHC_DIR)

dnl ***********************************************
dnl ghc-pkg
dnl ***********************************************

if test "$ENABLE_GHC" = "yes"; then
  AC_ARG_WITH(ghc-pkg,
  AC_HELP_STRING([--with-ghc-pkg=<ghc-pkg command>],
               [Use a different command instead of 'ghc-pkg']),
  [GHC_PKG="$withval"
   if test ! -x "$GHC_PKG"; then AC_PATH_PROG(GHC_PKG,"$withval") fi],
  [AC_CHECK_FILE("$(dirname $GHC)/ghc-pkg",
	       GHC_PKG="$(dirname $GHC)/ghc-pkg",
	       [AC_PATH_PROG(GHC_PKG,ghc-pkg)])])

	if test "$GHC_PKG" = "" || test ! -x "$GHC_PKG"; then
	   AC_MSG_ERROR([ghc-pkg is required for GHC support])
	fi
fi

AC_SUBST(GHC_PKG)

dnl ***********************************************
dnl HUGS
dnl ***********************************************

if test "$ENABLE_HUGS" = "yes"; then
  AC_ARG_WITH(hugs,
 AC_HELP_STRING([--with-hugs=<hugs command>],
               [Use a different command instead of 'hugs' 
	        for the Hugs interpreter.]),
  [AC_CHECK_FILE("$withval",HUGS="$withval",[AC_PATH_PROG(HUGS,"$withval")])],
  [AC_PATH_PROG(HUGS,hugs)])

  if test "$HUGS" = "" || test ! -x "$HUGS"; then
    AC_MSG_WARN([The build for HUGS will be skipped.])
    HUGS=""
    ENABLE_HUGS="no"
  else
    RUNHUGS=$(dirname $HUGS)/runhugs
  fi

fi

case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
        SO_EXT=dll;;
    *)  SO_EXT=so;;
esac

AC_SUBST(HUGS)
AC_SUBST(RUNHUGS)
AC_SUBST(SO_EXT)

dnl ***********************************************
dnl HUGS flags
dnl ***********************************************

AC_ARG_WITH(hugs-flags,
AC_HELP_STRING([--with-hugs-flags=FLAGS],
               [Add the given flags when running Hugs.]),
[HUGSFLAGS="$HUGSFLAGS $withval"],)

AC_SUBST(HUGSFLAGS)

dnl ***********************************************
dnl HUGS directory
dnl ***********************************************

if test "$ENABLE_HUGS" = "yes"; then
  AC_ARG_WITH(hugs-dir,
  AC_HELP_STRING([--with-hugs-dir=DIR],
               [Install Hugs libraries in the given Hugs directory 
                (default is the Hugs libraries directory)]),
  [HUGS_DIR="$withval"],
  [case $ac_cv_target_alias in
      i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
          HUGS_DIR=$(dirname $HUGS);;
      *)  HUGS_DIR=$(dirname $HUGS)/../lib/hugs;;
   esac])

	if test "$HUGS_DIR" = "" || test ! -d "$HUGS_DIR"; then
	   AC_MSG_ERROR([HUGS_DIR $HUGS_DIR does not exist])
	fi
fi

AC_SUBST(HUGS_DIR)


dnl ***********************************************
dnl Check for HSQL for ghc
dnl ***********************************************

if test "$ENABLE_GHC" = "yes" && test "$WITH_GHC_HSQL" = "yes"; then

  GHCFLAGS="$GHCFLAGS -package hsql"

  AC_HS_GHC_MODULE_IFELSE([Database.HSQL],[query],,
    [AC_MSG_WARN([HSQL for ghc not found, not 
                 building ghc HSQL support])
    WITH_GHC_HSQL="no"])

  if test "$WITH_GHC_HSQL" != "yes"; then
    GHCFLAGS="`echo $GHCFLAGS | sed 's/-package hsql//'`"
  fi

else
  WITH_GHC_HSQL="no"
fi

dnl ***********************************************
dnl Check for HSQL for Hugs
dnl ***********************************************

if test "$ENABLE_HUGS" = "yes" && test "$WITH_HUGS_HSQL" = "yes"; then

  if test "$prefix" != "NONE"; then
    HUGSFLAGS="$HUGSFLAGS -P:$prefix/lib/HSQL/Hugs/libraries"
  else
    HUGSFLAGS="$HUGSFLAGS -P:$ac_default_prefix/lib/HSQL/Hugs/libraries"
  fi

    AC_HS_HUGS_MODULE_IFELSE([Database.HSQL], [query],,
	 [AC_MSG_WARN([HSQL for Hugs not found.])
	 WITH_HUGS_HSQL="no"])
else
  WITH_HUGS_HSQL="no"
fi

dnl ***********************************************
dnl Check for wxHaskell for GHC
dnl ***********************************************

if test "$ENABLE_GHC" = "yes" && test "$WITH_GHC_WX" = "yes"; then

  GHCFLAGS="$GHCFLAGS -package wxcore"

  AC_HS_GHC_MODULE_IFELSE([Graphics.UI.WXCore.Db],[dbQuery],,
                          [WITH_GHC_WX="no"])

  if test "$WITH_GHC_WX" != "yes"; then
    GHCFLAGS="`echo $GHCFLAGS | sed 's/-package wxcore//'`"
  fi
else
  WITH_GHC_WX="no"
fi

dnl ***********************************************
dnl Check for hs-plugins for GHC
dnl ***********************************************

if test "$ENABLE_GHC" = "yes" && test "$WITH_GHC_PLUGINS" = "yes"; then

  GHCFLAGS="$GHCFLAGS -package plugins"

  AC_HS_GHC_MODULE_IFELSE([Plugins],[load],,
                          [WITH_GHC_PLUGINS="no"])

  if test "$WITH_GHC_PLUGINS" != "yes"; then
    GHCFLAGS="`echo $GHCFLAGS | sed 's/-package plugins//'`"
  fi
else
  WITH_GHC_PLUGINS="no"
fi

dnl ***********************************************
dnl HADDOCK
dnl ***********************************************
AC_ARG_WITH(haddock,
AC_HELP_STRING([--with-haddock=<haddock command>],
               [Use a different command instead of 'haddock' 
                for the documentation builder.]),
[AC_CHECK_FILE("$withval",HADDOCK="$withval",
[AC_PATH_PROG(HADDOCK,"$withval")])],
[AC_PATH_PROG(HADDOCK,haddock)]
)

if test "$HADDOCK" = "" || test "$HADDOCK" = "no" || test ! -x "$HADDOCK"; then
  BUILD_DOC="no"
  AC_MSG_RESULT([HADDOCK is required to build the documentations])
else
  BUILD_DOC="yes"
fi

AC_SUBST(BUILD_DOC)
AC_SUBST(HADDOCK)

dnl ***********************************************
dnl other progs
dnl ***********************************************

AC_PROG_INSTALL

AC_PATH_PROG(AR,ar)
AC_SUBST(AR)

AC_PATH_PROG(LD,ld)
AC_SUBST(LD)

case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
        WIN32=YES
        ;;
    *)  WIN32=NO
        ;;
esac

AC_SUBST(WIN32)

dnl ***********************************************
dnl check for ODBC module for ghc
dnl ***********************************************

if test "$WITH_GHC_HSQL" = "yes" && test "$WITH_GHC_HSQL_ODBC" = "yes"; then
  AC_HS_GHC_MODULE_IFELSE([Database.HSQL.ODBC],[connect],,
    [AC_MSG_WARN([ODBC module for ghc not found, skipping ODBC])
    WITH_GHC_HSQL_ODBC=no])
else
    WITH_GHC_HSQL_ODBC="no"
fi

dnl ***********************************************
dnl check for ODBC module for hugs
dnl ***********************************************

if test "$WITH_HUGS_HSQL" = "yes" && test "$WITH_HUGS_HSQL_ODBC" = "yes"; then
  AC_HS_HUGS_MODULE_IFELSE([Database.HSQL.ODBC],[connect],,
    [AC_MSG_WARN([ODBC module for hugs not found, skipping ODBC])
    WITH_HUGS_HSQL_ODBC=no])
else
    WITH_HUGS_HSQL_ODBC="no"
fi

dnl ***********************************************
dnl check for PostgreSQL module for ghc
dnl ***********************************************

if test "$WITH_GHC_HSQL" = "yes" && test "$WITH_GHC_HSQL_POSTGRESQL" = "yes"; then
  AC_HS_GHC_MODULE_IFELSE([Database.HSQL.PostgreSQL],[connect],,
    [AC_MSG_WARN([PostgreSQL module for ghc not found, 
	          not building PostgreSQL support])
    WITH_GHC_HSQL_POSTGRESQL=no])
else
    WITH_GHC_HSQL_POSTGRESQL="no"
fi

dnl ***********************************************
dnl check for PostgreSQL module for hugs
dnl ***********************************************

if test "$WITH_HUGS_HSQL" = "yes" && test "$WITH_HUGS_HSQL_POSTGRESQL" = "yes"; then
  AC_HS_HUGS_MODULE_IFELSE([Database.HSQL.PostgreSQL],[connect],,
    [AC_MSG_WARN([PostgreSQL module for hugs not found, 
	          not building PostgreSQL support])
    WITH_HUGS_HSQL_POSTGRESQL=no])
else
    WITH_HUGS_HSQL_POSTGRESQL="no"
fi

dnl ***********************************************
dnl check for MySQL module for ghc
dnl ***********************************************

if test "$WITH_GHC_HSQL" = "yes" && test "$WITH_GHC_HSQL_MYSQL" = "yes"; then
  AC_HS_GHC_MODULE_IFELSE([Database.HSQL.MySQL],[connect],,
    [AC_MSG_WARN([MySQL module for ghc not found, 
	          not building MySQL support])
    WITH_GHC_HSQL_MYSQL=no])
else
    WITH_GHC_HSQL_MYSQL="no"
fi

dnl ***********************************************
dnl check for MySQL module for hugs
dnl ***********************************************

if test "$WITH_HUGS_HSQL" = "yes" && test "$WITH_HUGS_HSQL_MYSQL" = "yes"; then
  AC_HS_HUGS_MODULE_IFELSE([Database.HSQL.MySQL],[connect],,
    [AC_MSG_WARN([MySQL module for hugs not found, 
	          not building MySQL support])
    WITH_HUGS_HSQL_MYSQL=no])
else
    WITH_HUGS_HSQL_MYSQL="no"
fi

dnl ***********************************************
dnl check for SQLite module for ghc
dnl ***********************************************

if test "$WITH_GHC_HSQL" = "yes" && test $WITH_GHC_HSQL_SQLITE = yes; then
  AC_HS_GHC_MODULE_IFELSE([Database.HSQL.SQLite],[connect],,
    [AC_MSG_WARN([SQLite module for ghc not found, 
	          not building SQLite support])
    WITH_GHC_HSQL_SQLITE=no])
else
    WITH_GHC_HSQL_SQLITE="no"
fi

dnl ***********************************************
dnl check for SQLite module for hugs
dnl ***********************************************

if test "$WITH_HUGS_HSQL" = "yes" && test $WITH_HUGS_HSQL_SQLITE = yes; then
  AC_HS_HUGS_MODULE_IFELSE([Database.HSQL.SQLite],[connect],,
    [AC_MSG_WARN([SQLite module for hugs not found, 
	          not building SQLite support])
    WITH_HUGS_HSQL_SQLITE=no])
else
    WITH_HUGS_HSQL_SQLITE="no"
fi

dnl ***********************************************
dnl subst
dnl ***********************************************

LDFLAGS="${LIBS} ${LDFLAGS}"
LIB_DIRS='"'${GHC_DIR}'"'

for lib_opt in ${LDFLAGS}
do
   case $lib_opt in
      -l*)
	   if test "$DEP_LIBS" = ""; then
		DEP_LIBS='"'`echo ${lib_opt} | sed s,-l,,`'"'
	   else
		DEP_LIBS=$DEP_LIBS,'"'`echo ${lib_opt} | sed s,-l,,`'"'
	   fi;;
      -L*)
	   LIB_DIRS=$LIB_DIRS,'"'`echo ${lib_opt} | sed s,-L,, | sed s,"'",, | sed s,"'",,`'"'
   esac
done

AC_SUBST(LDFLAGS)
AC_SUBST(DEP_LIBS)
AC_SUBST(LIB_DIRS)

dnl ***********************************************
dnl Package dependencies
dnl ***********************************************

if test "$WITH_GHC_HSQL" = "yes"; then
	PACKAGE_DEPS="$PACKAGE_DEPS,\"hsql\""
fi

if test "$WITH_GHC_WX" = "yes"; then
	PACKAGE_DEPS="$PACKAGE_DEPS,\"wxcore\""
fi

AC_SUBST(PACKAGE_DEPS)

dnl ***********************************************
dnl Create directories and output files
dnl ***********************************************

AC_OUTPUT

AC_MSG_NOTICE([Creating output directories])
mkdir -p build

echo
echo "HaskellDB will support:"
echo "-----------------------"
echo
echo "GHC:               $ENABLE_GHC"
echo "  HSQL:            $WITH_GHC_HSQL"
echo "    ODBC:          $WITH_GHC_HSQL_ODBC"
echo "    MySql:         $WITH_GHC_HSQL_MYSQL"
echo "    SQLite:        $WITH_GHC_HSQL_SQLITE"
echo "    PostgreSQL:    $WITH_GHC_HSQL_POSTGRESQL"
echo "  WxHaskell:       $WITH_GHC_WX"
echo "  hs-plugins:      $WITH_GHC_PLUGINS"
echo "Hugs:              $ENABLE_HUGS"
echo "  HSQL:            $WITH_HUGS_HSQL"
echo "    ODBC:          $WITH_HUGS_HSQL_ODBC"
echo "    MySql:         $WITH_HUGS_HSQL_MYSQL"
echo "    SQLite:        $WITH_HUGS_HSQL_SQLITE"
echo "    PostgreSQL:    $WITH_HUGS_HSQL_POSTGRESQL"
echo "  WxHaskell:       $WITH_HUGS_WX"
