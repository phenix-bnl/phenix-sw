#
#   pisa.mk for PISA2000  (formerly pisag77.mk in PISA99)
#
#   Builds PISA executable
#   H. Kehayias  03/28/95
#   Anita Trivedi 08/18/95    Modified for OSF1 and SunOS 
#   Charles Maguire 04/05/96  Added the PAD directory for PC2/PC3
#   Charles Maguire 04/08/98  Modified for IRIX6.4 platform
#   Charles Maguire 07/09/98  Modified to accept Linux platform at RCF
#   Charles Maguire 07/18/98  Modified to accept rsun00 (Sun Ultra) at RCF
#
#   Charles Maguire 08/18/99  Temporary version for g77 build with ROOT 
#
#   Charles Maguire 02/02/00  Changes for IRIX6.4 with -new compilation
#
#   Charles Maguire 06/27/01  Add (back) inr subdirectory for upgrades work
#
#   Charles Maguire 01/35/03  Add fcl subdirectory
#
# Define present working directory as PISA_HOME
PISA_HOME:=$(dir $(shell pwd))

# Define include directory 
INCDIRS=$(PISA_HOME)inc $(ROOTSYS)/include $(PISA_HOME)src/root

# List of source directories
SUB_DIR=bbc aer cdf phnxcore crk emc fcl hbd itr inr mum mun muw ntc pad tof trd tzr ver zdc muonTrg_pad

# Including source directories
SRCDIRS= $(patsubst %,$(PISA_HOME)src/%,$(SUB_DIR) )

# Assigning library file name
LIB_NAME=pisa
LIB=lib$(LIB_NAME).a

# This takes CPP_SYMBOLS defined at the command line which will be used by
# C preprocessor
CPP_SYMBOLS =
D_SYMBOLS = $(patsubst %,-D%,$(CPP_SYMBOLS))

# Including flags.mk file which describes flags for various systems

include flags.mk
export ARFLAGS CFLAGS FFLAGS PHNX_SYS PISA_HOME D_SYMBOLS INCDIRS SRCDIRS LIB CXXFLAGS CXX FC CC 

LDFLAGS= -g

# The following assigns LINK_FILE and SUBSYS names depending upon the platforms
ifeq ($(PHNX_SYS),AIX)
  LINK_FILE=pisalink.ibm
  SUBSYS=aix
endif

ifeq ($(PHNX_SYS),Linux)
  LINK_FILE=pisalink.g77linux
  SUBSYS=linux
endif

ifeq ($(PHNX_SYS),IRIX)
  LINK_FILE=pisalink.sgi64
  SUBSYS=irix64
endif

ifeq ($(PHNX_SYS),HP-UX)
  LINK_FILE=pisalink.hp
  SUBSYS=hpux
endif

ifeq ($(PHNX_SYS),OSF1)
  LINK_FILE=pisalink.alpha
  SUBSYS=alpha
endif

ifeq ($(PHNX_SYS),SunOS)
  LINK_FILE=pisalink.sun
  SUBSYS=sunUltra
  PHNX_CPU:=$(shell uname -i)
  ifeq ($(PHNX_CPU),i86pc)
    SUBSYS=sun
  endif
endif


.PHONY: genlib clean_all clean_obj

# Ready to generate the library file. First checks if the CERNLIB is set.
# Next, links pisa using link file in the platform directory. The final
# pisa executable is placed in the bin/platform directory.
#
pisa: genlib
ifeq ($(CERNLIB), )
	@echo "** CERN Library location is not set ***"
	@echo "   Set environment variable CERNLIB first and try again."
	@echo '   Or invoke this make file with "CERNLIB=<dir name>" flag.'
else
	@echo "Linking pisa using $(LINK_FILE) file"
	cd $(SUBSYS); $(LINK_FILE)
endif

# Generating library file
genlib:
	@echo "Making $(LIB)"
	cd $(SUBSYS); $(MAKE) -f $(PISA_HOME)lib/libmake.mk

# The following if invoked through command line using ``make clean_all''
# command, will clean .d  files from the subsystem directory in lib and pisa 
# executable from  bin/subsystem directory. 

clean_all: clean_obj
	rm -f $(SUBSYS)/*.d ../bin/$(SUBSYS)/pisa

clean_obj: 
	cd $(SUBSYS); rm -f *.o *.a

# This dumps the following information on the screen
 
echoopt:
	@echo " PISA top directory = $(PISA_HOME)"
	@echo " PHENIX system type = $(PHNX_SYS)"
	@echo " ARFLAGS            = $(ARFLAGS)"
	@echo " FFLAGS             = $(FFLAGS)"
	@echo " CFLAGS             = $(CFLAGS)"
	@echo " LDFLAGS            = $(LDFLAGS)"
	@echo " CERNLIB            = $(CERNLIB)"
	@echo " Object libraries   = $(LIB)"
	@echo " CPP Defined Symbols = \n $(D_SYMBOLS)"
	@echo " Source Directories = \n $(SRCDIRS)"
	@echo " Include Directories = \n $(INCDIRS)"

# This dumps all the echo information set in libmake.mk file on the screen
echosub:
	$(MAKE) -f $(PISA_HOME)lib/libmake.mk echoopt


