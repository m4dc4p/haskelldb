# Some macros for haskell projects
# By Bjorn Bringert, 2004

AC_DEFUN([AC_HS_GHC_COMPILE_IFELSE],
[rm -f conftest.hs conftest.hi conftest.o
cat << \EOF > conftest.hs 
[$1]
EOF
if AC_TRY_COMMAND($GHC $GHCFLAGS -c conftest.hs) && test -f conftest.o; then
	m4_default([$2],:)
else
	m4_default([$3],:)
fi
rm -f conftest.hs conftest.hi conftest.o
])

AC_DEFUN([AC_HS_HUGS_RUN_IFELSE],
[rm -f conftest.hs 
cat << \EOF > conftest.hs
[$1]
EOF
if AC_TRY_COMMAND($RUNHUGS $HUGSFLAGS conftest.hs); then
	m4_default([$2],:)
else
	m4_default([$3],:)
fi
rm -f conftest.hs
])

AC_DEFUN([AC_HS_MODULE_TEST],[
import [$1] ([$2])

test = [$2]

main :: IO ()
main = return ()
])

AC_DEFUN([AC_HS_GHC_MODULE_IFELSE],[
AC_MSG_CHECKING([for module $1 for GHC])
AC_HS_GHC_COMPILE_IFELSE(AC_HS_MODULE_TEST([$1],[$2]),
  AC_MSG_RESULT([ok])
  [$3],
  AC_MSG_RESULT([failed])
  [$4]
  )
])

AC_DEFUN([AC_HS_HUGS_MODULE_IFELSE],[
AC_MSG_CHECKING([for module $1 for Hugs])
AC_HS_HUGS_RUN_IFELSE(AC_HS_MODULE_TEST([$1],[$2]),
  AC_MSG_RESULT([ok])
  [$3],
  AC_MSG_RESULT([failed])
  [$4]
  )
])

AC_DEFUN([AC_HS_MODULE_IFELSE],[
if test "$GHC" != ""; then
  AC_HS_GHC_MODULE_IFELSE([$1],[$2],[$3],[$4])
fi
if test "$HUGS" != ""; then
  AC_HS_HUGS_MODULE_IFELSE([$1],[$2],[$3],[$4])
fi
])
