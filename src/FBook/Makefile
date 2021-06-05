F_INCLUDEDIRS=
DEBUG=
SOURCE_FILES = $(shell ls -1 *.f )

OBJS = $(patsubst %.f,%.o,$(SOURCE_FILES))
%.o: %.f
	$(F_COMP) $(F_FLAGS) $(DEBUG) $(DEFINES) $(F_INCLUDEDIRS) -o $(@F) $<

all: $(OBJS)

clean:
	-rm  *.o 2>/dev/null
clobber:
	-rm *.o *~ 2>/dev/null
