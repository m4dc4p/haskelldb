GHC = ghc
GHCI = ghci
GHCFLAGS = -fglasgow-exts -fallow-overlapping-instances

HUGS=hugs
RUNHUGS=runhugs
HUGSFLAGS=-98 +o

HSQL_DIR = ../../../../HSQL-cvs
UNIXODBC_PREFIX = /usr

HOST = $(shell hostname)
ifeq ("$(HOST)", "jackass.tekno.chalmers.se")
HSQL_DIR = /usr/local/dp03-7/HSQL-cvs
UNIXODBC_PREFIX=/usr/local/dp03-7/unixODBC-2.2.6
endif

UNIXODBC_LIBDIR = $(UNIXODBC_PREFIX)/lib
