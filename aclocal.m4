# Some macros for haskell projects
# By Bjorn Bringert, 2004


# AC_HS_GHC_COMPILE_IFELSE(program, action-if-true, action-if-false)
# try to compile a program with GHC
AC_DEFUN([AC_HS_GHC_COMPILE_IFELSE],
[rm -f conftest.hs conftest.hi conftest.o
cat << \EOF > conftest.hs 
[$1]
EOF
if test "$GHC" != "" && AC_TRY_COMMAND($GHC $GHCFLAGS -c conftest.hs) && test -f conftest.o; then
	m4_default([$2],:)
else
	m4_default([$3],:)
fi
rm -f conftest.hs conftest.hi conftest.o
])

# AC_HS_HUGS_RUN_IFELSE(program, action-if-true, action-if-false)
# Try to run a program with runhugs
AC_DEFUN([AC_HS_HUGS_RUN_IFELSE],
[rm -f conftest.hs 
cat << \EOF > conftest.hs
[$1]
EOF
if test "$RUNHUGS" != "" && AC_TRY_COMMAND($RUNHUGS $HUGSFLAGS conftest.hs); then
	m4_default([$2],:)
else
	m4_default([$3],:)
fi
rm -f conftest.hs
])

# AC_HS_MODULE_TEST(module, function)
# Make a Haskell module that tests that function exists in a module
AC_DEFUN([AC_HS_MODULE_TEST],[
import [$1] ([$2])

test = [$2]

main :: IO ()
main = return ()
])

# AC_HS_GHC_MODULE_IFELSE(module, function, action-if-true, action-if-false)
AC_DEFUN([AC_HS_GHC_MODULE_IFELSE],[
AC_MSG_CHECKING([for module $1 for GHC])
AC_HS_GHC_COMPILE_IFELSE(AC_HS_MODULE_TEST([$1],[$2]),
  AC_MSG_RESULT([ok])
  [$3],
  AC_MSG_RESULT([failed])
  [$4]
  )
])

# AC_HS_HUGS_MODULE_IFELSE(module, function, action-if-true, action-if-false)
AC_DEFUN([AC_HS_HUGS_MODULE_IFELSE],[
AC_MSG_CHECKING([for module $1 for Hugs])
AC_HS_HUGS_RUN_IFELSE(AC_HS_MODULE_TEST([$1],[$2]),
  AC_MSG_RESULT([ok])
  [$3],
  AC_MSG_RESULT([failed])
  [$4]
  )
])

# AC_HS_MODULE_IFELSE(module, function, action-if-true, action-if-false)
AC_DEFUN([AC_HS_MODULE_IFELSE],[
if test "$GHC" != ""; then
  AC_HS_GHC_MODULE_IFELSE([$1],[$2],[$3],[$4])
fi
if test "$HUGS" != ""; then
  AC_HS_HUGS_MODULE_IFELSE([$1],[$2],[$3],[$4])
fi
])

# AC_HS_CHECK_GHC_VERSION_IFELSE(minimum major, mininum minor, 
#                                action-if-true, action-if-false)
AC_DEFUN([AC_HS_CHECK_GHC_VERSION_IFELSE],[
  if test "$GHC" == ""; then
    AC_MSG_ERROR([GHC not found])
  fi

  AC_MSG_CHECKING([for ghc >= $1.$2])

  GHC_VERSION=`${GHC} --version | sed 's/.*version //'`
  GHC_VERSION_MAJOR=[`echo $GHC_VERSION | sed 's/^\([^.]*\)\..*/\1/'`]
  GHC_VERSION_MINOR=[`echo $GHC_VERSION | sed 's/^[^.]*\.\([^.]*\).*/\1/'`]

  if test "$GHC_VERSION_MAJOR" -gt [$1] || (test "$GHC_VERSION_MAJOR" = [$1] && test "$GHC_VERSION_MINOR" -ge [$2]); then
	AC_MSG_RESULT([ok, $GHC_VERSION])
	m4_default([$3],:)
  else
	AC_MSG_RESULT([failed, $GHC_VERSION])
	m4_default([$4],:)
  fi
])