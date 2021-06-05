# For Linux
CPP_COMP = g++
CPP_FLAGS= -ansi -Wall -c


# name 
MODULE = cbook
LIBDIR = ../lib
#
#
# do EMC 
SOURCE_FILES := $(shell ls -1 ../src/HCL/*.cpp)
SOURCE_FILES += $(shell ls -1 ../src/CBook/*.cpp)
# SOURCE_FILES += $(shell ls -1 ../src/Midnight/*.cpp)



LIBOBJS = $(patsubst %.cpp,%.o,$(SOURCE_FILES))
SUBDIRLOOK = $(sort $(dir $(SOURCE_FILES)))

# where to put the library, will need install-target later
EXECDIR = bin


# Fortran compile options
CPP_INCLUDEDIRS = -I../../inc
CPP_INCLUDEDIRS := $(CPP_EXTRA_INCLUDEDIRS) $(CPP_INCLUDEDIRS)

# We need to compile directly against the following files
CPP_TIME := $(shell date +%H%M)
CPP_DATE := $(shell date +%d%m%y)
CPP_VERS := $(shell if [ -f CVS/Tag ] ; \
                    then cat CVS/Tag | sed -e "s/[a-zA-Z_]//g"; fi)
CPP_TITLE := "cbook_hcl"

ext = $(CPP_VERS)

#DEBUG = -g

export CPP_COMP CPP_INCLUDEDIRS CPP_TIME CPP_DATE CPP_VERS CPP_TITLE \
       DEFINES DEBUG CPP_FLAGS 
all: lib 

install: all
	( [ -d lib ] || mkdir lib ) && mv libbases$(ext).a lib 

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







