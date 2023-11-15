#
#   libmake.mk
#   Builds PISA object library 
#   H. Kehayias  01/19/95
#  
#  Defines path for include files
INCPATH=$(subst $(EMPTY) $(EMPTY),:,$(strip $(INCDIRS)))

# Defines path for source files 
SRCPATH=$(subst $(EMPTY) $(EMPTY),:,$(strip $(SRCDIRS))) 

# Defines f_FILES as *f files
f_FILES=$(sort $(notdir $(foreach dir, $(SRCDIRS) ., $(wildcard $(dir)/*.f))))


# Defines F_FILES as *F files. The *F files are fortran files that pass through
# C preprocessor
F_FILES=$(sort $(notdir $(foreach dir, $(SRCDIRS) ., $(wildcard $(dir)/*.F))))


# Defines C_FILES as *.c files
C_FILES=$(sort $(notdir $(foreach dir, $(SRCDIRS) ., $(wildcard $(dir)/*.c))))

# Defines C_FILES as *.cc files
CC_FILES=$(sort $(notdir $(foreach dir, $(SRCDIRS) ., $(wildcard $(dir)/*.cc))))

# Defines cdf_FILES as *cdf files
cdf_FILES=$(sort $(notdir $(foreach dir, $(SRCDIRS) ., \
          $(wildcard $(dir)/*.cdf))))

# sets object module
LIBOBJ=$(sort $(f_FILES:.f=.o) $(F_FILES:.F=.o) $(C_FILES:.c=.o) \
       $(CC_FILES:.cc=.o) $(cdf_FILES:.cdf=.o) )

# Defines dependency files
D_FILES=$(LIBOBJ:.o=.d)

# Sets CPPFLAGS for C preprocessor  and GLAGS 
CPPFLAGS= $(patsubst %,-I%,$(INCDIRS))
GFLAGS=-traditional-cpp -M -x c

CPXFLAGS = $(CPPFLAGS) -I$(ROOTSYS)/include

# irix Fortran Include directory needs a special switches
SGIFOPT=-Wf,-I


# Flags to include inc directory  

ifeq ($(PHNX_SYS),IRIX)
INCFLAGS=$(patsubst %,$(SGIFOPT)%,$(INCDIRS))
else
INCFLAGS= $(patsubst %,-I%,$(INCDIRS))
endif

# VPATH  specifies a directory to search for prerequisites 

# look for *c files in the source directory
vpath %.c $(SRCPATH)

# look for *cc files in the source directory
vpath %.cc $(SRCPATH)

# look for *h in the include directory      
vpath %.h $(INCPATH)       

# look for *f in the source directory
vpath %.f $(SRCPATH)       

# look for *F in the source directory
vpath %.F $(SRCPATH)       

# look for *inc in the include directory
vpath %.inc $(INCPATH) 

# look for *cdf in the source directory    
vpath %.cdf $(SRCPATH)     

lib:    $(LIB)
include $(D_FILES)

# The following describe how to make .d from .c, .o from .c etc. The $<
# evaluates to whatever prerequisite triggered the rule. The .o file is the 
# target file and .c file is prerequisite. 

%.d:%.c
	gcc -MM $(CPPFLAGS) $< | sed -e 's/$*.o/& $@/g'\
	-e 's/[^ :\/]*\/\/*//g' >$@
%.o:%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@

%.d:%.cc
	gcc -MM $(CPXFLAGS) $< | sed -e 's/$*.o/& $@/g'\
	-e 's/[^ :\/]*\/\/*//g' >$@
%.o:%.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@

%.d:%.f
	sed -e "/^       *[iI][nN][cC][lL][uU][dD][eE]/s/""'/"'"/g' \
	-e 's/^       *[iI][nN][cC][lL][uU][dD][eE]/#include/g' $< >$*.P
	gcc $(GFLAGS) $(CPPFLAGS) $*.P | sed -e 's/$*.P.o/$*.o $@/g'\
	-e 's/$*.P/$*.f/g'\
	-e 's/[^ :\/]*\/\/*//g' >$@
	rm $*.P

%.d:%.F
	sed -e "/^       *[iI][nN][cC][lL][uU][dD][eE]/s/""'/"'"/g' \
	-e 's/^       *[iI][nN][cC][lL][uU][dD][eE]/#include/g' $< >$*.P
	gcc $(GFLAGS) $(CPPFLAGS) $*.P | sed -e 's/$*.P.o/$*.o $@/g'\
	-e 's/$*.P/$*.F/g'\
	-e 's/[^ :\/]*\/\/*//g' >$@
	rm $*.P

%.o : %.f
	$(FC) $(FFLAGS) $(INCFLAGS) $< -o $@

%.o : %.F
	gcc -E -P -x c $(CPPFLAGS) $(D_SYMBOLS) -o $*.f $<
	$(FC) $(FFLAGS) $(INCFLAGS) $*.f -o $*.o
	rm $*.f

%.d:%.cdf
	gcc $(GFLAGS) $(CPPFLAGS) $< | sed -e 's/$*.cdf.o/$*.o $@/g'\
	-e 's/[^ :\/]*\/\/*//g' >$@

%.o : %.cdf
	kuipc $< $*.f
	$(FC) $(FFLAGS) $(INCFLAGS) $*.f -o $@
	rm $*.f


# Linking all the object files. This makes libpisa.a file.
#
$(LIB): $(LIBOBJ)
	$(AR) $(ARFLAGS) $@ $?

echoopt:
	@echo " PISA top directory = $(PISA_HOME)"
	@echo " PHENIX system type = $(PHNX_SYS)"
	@echo " ARFLAGS            = $(ARFLAGS)"
	@echo " FFLAGS             = $(FFLAGS)"
	@echo " CFLAGS             = $(CFLAGS)"
	@echo " CPPFLAGS           = $(CPPFLAGS)"
	@echo " GFLAGS             = $(GFLAGS)"
	@echo " INCFLAGS =  $(INCFLAGS)"
	@echo " SRCPATH =  $(SRCPATH)"
	@echo " INCPATH =  $(INCPATH)"
	@echo " D_SYMBOLS =  $(D_SYMBOLS)"

