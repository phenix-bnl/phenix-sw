#
#   flags.mk for PISA2000 (formerly flagsg77.mk for PISA99)
#
#   Determines Compile/Archiving switches from the system type
#   H. Kehayias 10/20/94
#   Anita Trivedi 08/18/95 ( Modified for OSF1 and SunOS )
#   Anita Trivedi 05/12/97 Fix for IRIX64 platform
#   C.F. Maguire  05/13/97 Fix for CFLAGS for PPro
#   C.F. Maguire  07/09/98 Add branch for Linux platform
#
#   C.F. Maguire  08/18/99 Temporary version for g77 compilation
#
#   C.F. Maguire  02/02/00 Changes for IRIX 6.5 -n32 compilations
#
#   C.F. Maguire  02/05/00 Changes for DEC Alpha compilation (CXX flags)
#
#   C.F. Maguire  02/10/00 Changes for SunOS compilation (CXX flags)
#
PHNX_SYS:=$(shell uname -s)
#
#C Flags NOT TESTED
#

#hp-ux FLAGS
ifeq ($(PHNX_SYS),HP-UX)
ARFLAGS=rv
CFLAGS=-c -g -Aa
FFLAGS=+e -c +ppu -g
endif

#aix FLAGS
ifeq ($(PHNX_SYS),AIX)
ARFLAGS=rv 
FFLAGS=-c -qextname -qrndsngl -qcharlen=32767 -g
CFLAGS=-c -g 
endif

# irix FLAGS
ifeq ($(PHNX_SYS),IRIX)
export FC = f77
ARFLAGS=rv
FFLAGS=-c -static -g -nocpp -n32 
CFLAGS=-c -g -n32
CXXFLAGS = -g -c -n32
CC = cc
CXX = CC
endif

# Linux FLAGS
ifeq ($(PHNX_SYS),Linux)
export FC = g77 
ARFLAGS=rv
FFLAGS=-c -fdebug-kludge -g -fno-automatic -finit-local-zero
CFLAGS=-c -g
CXXFLAGS = -g -c -Wall -fPIC
CC = gcc
CXX = g++
endif

# OSF1 FLAGS
ifeq ($(PHNX_SYS),OSF1)
ARFLAGS=rv
FFLAGS=-c -g -fpe4
CFLAGS=-c -g 
CXX   = cxx
CXXFLAGS  = -g -c
endif

#SunOS FLAGS
ifeq ($(PHNX_SYS),SunOS)
export FC = f77
ARFLAGS=rv
FFLAGS=-c -lx  -lsocket -lnsl -lgen -lm -g
CC = cc
CFLAGS=-g -c 
CXX           = CC
CXXFLAGS      = -g -KPIC -I$(ROOTSYS)/include -c
endif
