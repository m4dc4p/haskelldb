dnl Run autoconf to generate configure from this file
dnl Based on configure.ac from HSQL

AC_INIT([HaskellDB],[0.6],[dp03-7@mdstud.chalmers.se],[haskelldb])

AC_PREREQ(2.53)

AC_REVISION($Revision: 1.23 $)

AC_CONFIG_FILES([config.mk haskelldb.pkg])

AC_CANONICAL_SYSTEM

dnl ***********************************************
dnl Some defaults
dnl ***********************************************

ghc_req_major_verison=6
ghc_req_minor_verison=2

GHCFLAGS="$GHCFLAGS -fglasgow-exts -fallow-overlapping-instances"

HUGSFLAGS="$HUGSFLAGS -98 +o"

AC_SUBST(GHCFLAGS)
AC_SUBST(HUGSFLAGS)

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
dnl Enable/Disable HSQL
dnl ***********************************************

AC_ARG_ENABLE(hsql,
AC_HELP_STRING([--enable-hsql],
	       [Enable use of HSQL. (default = yes)]),
[WITH_HSQL="$enableval"],
[WITH_HSQL=yes]
)
AC_SUBST(WITH_HSQL)

dnl ***********************************************
dnl Enable/Disable HSQL ODBC binding
dnl ***********************************************

AC_ARG_ENABLE(hsql-odbc,
AC_HELP_STRING([--enable-hsql-odbc],
	       [Enable use of the HSQL ODBC driver. 
	        (default = yes)]),
[WITH_HSQL_ODBC="$enableval"],
[WITH_HSQL_ODBC=yes]
)
AC_SUBST(WITH_HSQL_ODBC)

dnl ***********************************************
dnl Enable/Disable HSQL MySQL binding
dnl ***********************************************

AC_ARG_ENABLE(hsql-mysql,
AC_HELP_STRING([--enable-hsql-mysql],
	       [Enable use of the HSQL MySQL driver.  
	        (default = yes)]),
[WITH_HSQL_MYSQL="$enableval"],
[WITH_HSQL_MYSQL=yes]
)
AC_SUBST(WITH_HSQL_MYSQL)

dnl ***********************************************
dnl Enable/Disable HSQL SQLite binding
dnl ***********************************************

AC_ARG_ENABLE(hsql-sqlite,
AC_HELP_STRING([--enable-hsql-sqlite],
	       [Enable use of the HSQL SQLite driver.  
	        (default = yes)]),
[WITH_HSQL_SQLITE="$enableval"],
[WITH_HSQL_SQLITE=yes]
)
AC_SUBST(WITH_HSQL_SQLITE)

dnl ***********************************************
dnl Enable/Disable HSQL PostgreSQL binding
dnl ***********************************************

AC_ARG_ENABLE(hsql-postgresql,
AC_HELP_STRING([--enable-hsql-postgresql],
	       [Enable use of the HSQL PostgreSQL driver. 
	        (default = yes)]),
[WITH_HSQL_POSTGRESQL="$enableval"],
[WITH_HSQL_POSTGRESQL=yes]
)
AC_SUBST(WITH_HSQL_POSTGRESQL)

dnl ***********************************************
dnl Enable/Disable WxHaskell binding
dnl ***********************************************

AC_ARG_ENABLE(wx,
AC_HELP_STRING([--enable-wx],
	       [Enable use of the WxHaskell driver. (default = yes)]),
[WITH_WX="$enableval"],
[WITH_WX=yes]
)
AC_SUBST(WITH_WX)

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
dnl C compiler
dnl ***********************************************

AC_PROG_CC
AC_SUBST(CC)

dnl ***********************************************
dnl GHC
dnl ***********************************************
AC_ARG_WITH(ghc,
AC_HELP_STRING([--with-ghc=<ghc command>],
               [Use a different command instead of 
                'ghc' for the Haskell compiler.]),
[GHC="$withval"],
[AC_PATH_PROG(GHC,ghc)]
)

if test "$GHC" = "" || test ! -f $GHC; then
   AC_MSG_ERROR([GHC not found. GHC is needed to build 
	         HaskellDB even you are only building libraries for Hugs.])
  GHC=""
else
  GHCI=$(dirname $GHC)/ghci
fi

if test "$GHC" != ""; then
  AC_HS_CHECK_GHC_VERSION_IFELSE([$ghc_req_major_verison],
    [$ghc_req_minor_verison],,[
      AC_MSG_ERROR([ghc >= $ghc_req_major_verison.$ghc_req_minor_verison
	            is required to build HaskellDB.]) ])
fi

AC_SUBST(GHC)
AC_SUBST(GHCI)
AC_SUBST(GHC_VERSION)

dnl ***********************************************
dnl GHC directory
dnl ***********************************************
AC_ARG_WITH(ghc-dir,
AC_HELP_STRING([--with-ghc-dir=DIR],
               [Install GHC libraries in the given GHC directory 
                (default is "ghc --print-libdir")]),
[GHC_DIR="$withval"],
[if test "$GHC" != ""; then
   GHC_DIR=`$GHC --print-libdir`
 fi]
)

if test "$ENABLE_GHC" = "yes" && test "$GHC" != ""; then
	if test "$GHC_DIR" = "" || test ! -d $GHC_DIR; then
	   AC_MSG_ERROR([GHC_DIR $GHC_DIR does not exist])
	fi
fi

AC_SUBST(GHC_DIR)

dnl ***********************************************
dnl ghc-pkg
dnl ***********************************************
AC_ARG_WITH(ghc-pkg,
AC_HELP_STRING([--with-ghc-pkg=<ghc-pkg command>],
               [Use a different command instead of 'ghc-pkg']),
[GHC_PKG="$withval"],
[AC_PATH_PROG(GHC_PKG,ghc-pkg)]
)

if test "$ENABLE_GHC" = "yes" && test "$GHC" != ""; then
	if test "$GHC_PKG" = "" || test ! -f $GHC_PKG; then
	   AC_MSG_ERROR([ghc-pkg is required for GHC support])
	fi
fi

AC_SUBST(GHC_PKG)

dnl ***********************************************
dnl HUGS
dnl ***********************************************
AC_ARG_WITH(hugs,
AC_HELP_STRING([--with-hugs=<hugs command>],
               [Use a different command instead of 'hugs' 
	        for the Hugs interpreter.]),
[HUGS="$withval"],
[AC_PATH_PROG(HUGS,hugs)]
)

if test "$ENABLE_HUGS" != "yes" || test "$HUGS" = "" || test ! -f $HUGS; then
  AC_MSG_RESULT([The build for HUGS will be skipped.])
  HUGS=""
  ENABLE_HUGS="no"
else
  RUNHUGS=$(dirname $HUGS)/runhugs
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
dnl Hugs directory
dnl ***********************************************
AC_ARG_WITH(hugs-dir,
AC_HELP_STRING([--with-hugs-dir=DIR],
               [Install Hugs libraries in the given Hugs directory 
                (default is the Hugs libraries directory)]),
[HUGS_DIR="$withval"],
[if test "$HUGS" != ""; then
   case $ac_cv_target_alias in
      i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
          HUGS_DIR=$(dirname $HUGS);;
      *)  HUGS_DIR=$(dirname $HUGS)/../lib/hugs;;
   esac
 fi]
)

if test "$HUGS" != ""; then
	if test "$HUGS_DIR" = "" || test ! -d $HUGS_DIR; then
	   AC_MSG_ERROR([HUGS_DIR $HUGS_DIR does not exist])
	fi
fi

AC_SUBST(HUGS_DIR)

dnl ***********************************************
dnl Check for HSQL
dnl ***********************************************

if test "$WITH_HSQL" = "yes"; then

  GHCFLAGS="$GHCFLAGS -package hsql"

  AC_HS_MODULE_IFELSE([Database.HSQL],[query],,
    [AC_MSG_WARN([HSQL not found, not building HSQL support])
    WITH_HSQL="no"])

  if test "$WITH_HSQL" != "yes"; then
    GHCFLAGS="echo $GHCFLAGS | sed 's/-package hsql//'"
  fi

fi

dnl ***********************************************
dnl WxHaskell for GHC
dnl ***********************************************

if test "$GHC" != "" && test "$WITH_WX" = "yes"; then

  GHCFLAGS="$GHCFLAGS -package wxcore"

  AC_HS_GHC_MODULE_IFELSE([Graphics.UI.WXCore.Db],[dbQuery],,[WITH_WX="no"])

  if test "$WITH_WX" != "yes"; then
    GHCFLAGS="echo $GHCFLAGS | sed 's/-package wxcore//'"
  fi

fi

dnl ***********************************************
dnl HADDOCK
dnl ***********************************************
AC_ARG_WITH(haddock,
AC_HELP_STRING([--with-haddock=<haddock command>],
               [Use a different command instead of 'haddock' 
                for the documentation builder.]),
[HADDOCK="$withval"],
[AC_PATH_PROG(HADDOCK,haddock)]
)

if test "$HADDOCK" = "" || test ! -f $HADDOCK; then
  AC_MSG_RESULT([HADDOCK is required to build the documentations])
fi

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
dnl check for ODBC module and libraries 
dnl ***********************************************

if test "$WITH_HSQL" != "yes"; then
	WITH_HSQL_ODBC="no"
fi

AC_ARG_WITH(odbc-libs,
  AC_HELP_STRING([--with-odbc-libs=DIR],
             [Look for ODBC libraries in DIR.]),
  [ODBC_LIB_DIR="$withval"],
  [
  case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
	ODBC_LIB_DIR="/cygdrive/c/program/Microsoft\ Data\ Access\ SDK/lib/x86"
        ;;
  esac
  ]
)

if test "$WITH_HSQL_ODBC" = "yes"; then
  AC_HS_MODULE_IFELSE([Database.HSQL.ODBC],[connect],,
    [AC_MSG_WARN([ODBC module not found, skipping ODBC])
    WITH_HSQL_ODBC=no])
fi

if test "$WITH_HSQL_ODBC" = "yes" && test "$HSQL_AUTO_PACKAGE" != "yes"; then

  if test "$ODBC_LIB_DIR" != ""; then
    LDFLAGS="$LDFLAGS -L$ODBC_LIB_DIR"
  fi

  case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
	# FIXME: not checking for libodbc on win32
        LIBS="$LIBS -lodbc32"
        ;;
    *)  AC_CHECK_LIB(odbc,SQLAllocEnv,,AC_MSG_ERROR(
	[libodbc required to build ODBC programs. Skipping ODBC support.]))
        ;;
  esac

fi

dnl ***********************************************
dnl check for PostgreSQL module and libraries
dnl ***********************************************

if test "$WITH_HSQL" != "yes"; then
	WITH_HSQL_POSTGRESQL="no"
fi

if test $WITH_HSQL_POSTGRESQL = yes; then
  AC_HS_MODULE_IFELSE([Database.HSQL.PostgreSQL],[connect],,
    [AC_MSG_WARN([PostgreSQL module not found, 
	          not building PostgreSQL support])
    WITH_HSQL_POSTGRESQL=no])
fi

if test "$WITH_HSQL_POSTGRESQL" = "yes" && test "$HSQL_AUTO_PACKAGE" != "yes"; then
  AC_PATH_PROG(PG_CONFIG, pg_config)

  if test "$PG_CONFIG" = "" || test ! -f $PG_CONFIG; then
    AC_MSG_WARN([pg_config is required for PostgreSQL, 
	          skipping PostgreSQL])
    WITH_HSQL_POSTGRESQL=no
  fi

  if test $WITH_HSQL_POSTGRESQL = yes; then
    libdir=`$PG_CONFIG --libdir`
    case $ac_cv_target_alias in
      i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
	libdir=`cygpath -m "$libdir"`
        ;;
    esac

    LDFLAGS="$LDFLAGS -L$libdir"

    AC_CHECK_LIB(pq,PQsetdbLogin,,[
	AC_MSG_WARN([libpq library not found])
        WITH_HSQL_POSTGRESQL=no
    ])
  fi
fi

dnl ***********************************************
dnl check for MySQL module and libraries
dnl ***********************************************

if test "$WITH_HSQL" != "yes"; then
	WITH_HSQL_MYSQL="no"
fi

if test $WITH_HSQL_MYSQL = yes; then
  AC_HS_MODULE_IFELSE([Database.HSQL.MySQL],[connect],,
    [AC_MSG_WARN([MySQL module not found, 
	          not building MySQL support])
    WITH_HSQL_MYSQL=no])
fi

if test $WITH_HSQL_MYSQL = yes && test "$HSQL_AUTO_PACKAGE" != "yes"; then
  AC_PATH_PROG(MYSQL_CONFIG, mysql_config)

  if test "$MYSQL_CONFIG" = "" || test ! -f $MYSQL_CONFIG; then
    AC_MSG_WARN([mysql_config is required to build MySQL binding])
   WITH_HSQL_MYSQL=no
  fi

  LDFLAGS="$LDFLAGS `$MYSQL_CONFIG --libs`"
fi

dnl ***********************************************
dnl check for SQLite module and libraries
dnl ***********************************************

if test "$WITH_HSQL" != "yes"; then
	WITH_HSQL_SQLITE="no"
fi

if test $WITH_HSQL_SQLITE = yes; then
  AC_HS_MODULE_IFELSE([Database.HSQL.SQLite],[connect],,
    [AC_MSG_WARN([SQLite module not found, 
	          not building SQLite support])
    WITH_HSQL_SQLITE=no])
fi

if test "$WITH_HSQL_SQLITE" = "yes" && test "$HSQL_AUTO_PACKAGE" != "yes"; then
   AC_CHECK_LIB(sqlite,sqlite_open,,
	AC_MSG_ERROR([libsqlite required to build SQLite building.]))
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

if test "$WITH_HSQL" = "yes"; then
	PACKAGE_DEPS="$PACKAGE_DEPS,\"hsql\""
fi

if test "$WITH_WX" = "yes"; then
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
echo "HSQL:              $WITH_HSQL"
echo "    ODBC:          $WITH_HSQL_ODBC"
echo "    MySql:         $WITH_HSQL_MYSQL"
echo "    SQLite:        $WITH_HSQL_SQLITE"
echo "    PostgreSQL     $WITH_HSQL_POSTGRESQL"
echo "WxHaskell:         $WITH_WX"
echo "Hugs:              $ENABLE_HUGS"
echo "GHC:               $ENABLE_GHC"
