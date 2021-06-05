F_INCLUDEDIRS= -I../../inc
DEBUG=
SOURCE_FILES = $(shell ls -1 *.cpp )

OBJS = $(patsubst %.cpp,%.o,$(SOURCE_FILES))
%.o: %.cpp
	$(CPP_COMP) $(CPP_FLAGS) $(DEBUG) $(DEFINES) $(CPP_INCLUDEDIRS) -o $(@F) $<

all: $(OBJS)

clean:
	-rm  *.o 2>/dev/null
clobber:
	-rm *.o *~ 2>/dev/null
