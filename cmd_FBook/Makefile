# For Linux
# F_COMP = gfortran
F_COMP = gfortran
F_FLAGS= -c -w -O2 -fpic -fno-automatic


# name 
MODULE = fbook
LIBDIR = ../lib
#
#
# do EMC 
SOURCE_FILES := $(shell ls -1 ../src/FBook/*.f)
# SOURCE_FILES += $(shell ls -1 ../src/Minuit/*.cpp)
# SOURCE_FILES += $(shell ls -1 ../src/Midnight/*.cpp)



LIBOBJS = $(patsubst %.f,%.o,$(SOURCE_FILES))
SUBDIRLOOK = $(sort $(dir $(SOURCE_FILES)))

# where to put the library, will need install-target later
EXECDIR = bin


# Fortran compile options
F_INCLUDEDIRS = -I../../inc
F_INCLUDEDIRS := $(F_EXTRA_INCLUDEDIRS) $(F_INCLUDEDIRS)

# We need to compile directly against the following files
CPP_TIME := $(shell date +%H%M)
CPP_DATE := $(shell date +%d%m%y)
CPP_VERS := $(shell if [ -f CVS/Tag ] ; \
                    then cat CVS/Tag | sed -e "s/[a-zA-Z_]//g"; fi)
CPP_TITLE := "FBOOK"

ext = $(CPP_VERS)

#DEBUG = -g

export F_COMP CPP_INCLUDEDIRS CPP_TIME CPP_DATE CPP_VERS CPP_TITLE \
       DEFINES DEBUG F_FLAGS 
all: lib 

objects: version makefiles 
	@for subdir in $(SUBDIRLOOK);\
	  do $(MAKE) -C $$subdir all;\
	done
clean: makefiles
	@rm -f ../src/*/*.o
	@rm -f ../src/*.o
	@rm -f ../lib/*.*
	@rm -f ../doc/*/*.exe
	@rm -f ../doc/*/*.o

clobber: makefiles
	for subdir in $(SUBDIRLOOK);\
	  do $(MAKE) -C $$subdir clobber;\
	done
	-@rm version.h qftitle.inc *~ *.a 2>/dev/null
version: version.h qftitle.inc
version.h::
	@if [ -f version.h ]; then rm version.h ; fi
	@echo >version.h "#define CPP_TIME $(CPP_TIME)"
	@echo >>version.h "#define CPP_DATE $(CPP_DATE)"
	@echo >>version.h "#define CPP_VERS $(CPP_VERS)"
	@echo >>version.h "#define CPP_TITLE $(CPP_TITLE)"
	@echo >>version.h "#define CPP_VERSS '$(CPP_VERS)'"
qftitle.inc::
	@if [ -f qftitle.inc ]; then rm qftitle.inc ; fi
	@echo >qftitle.inc "     *' $(CPP_TITLE)           CVS version: $(CPP_VERS) '"
makefiles: Makefile Makefile_sub
	for subdir in $(SUBDIRLOOK);\
	  do [ -f $$subdir/Makefile ] && rm $$subdir/Makefile;\
	  echo >$$subdir/Makefile "F_INCLUDEDIRS=$(CPP_INCLUDEDIRS)";\
	  echo >>$$subdir/Makefile "DEBUG=$(DEBUG)";\
	  cat Makefile_sub >>$$subdir/Makefile;\
	done


lib: lib$(MODULE)$(ext).a

lib$(MODULE)$(ext).a: objects
	ar -rv $(LIBDIR)/lib$(MODULE)$(ext).a $(LIBOBJS)
	ranlib $(LIBDIR)/lib$(MODULE)$(ext).a
doc:
	$(MAKE) -C manual all







