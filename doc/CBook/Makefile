#          this is without optimization 
CXX       = g++
OPT       = -O -Wall -fPIC
LFLAGS    = -lcbook
LIBS      = -L../../lib
INCL      = -I../../inc
#
CXXFLAGS = $(INCL) $(LIBS) $(LFLAGS)
#
objects  :=	example.o

global	=	Makefile

tasks:          example.exe 


example.exe:    $(objects)
	$(CXX) $(CFLAGS) $(objects) $(INCL) $(LIBS) $(LFLAGS) -o $@


%.o : %.cpp $(global)
	$(CXX) $(OPT) $(CXXFLAGS) -o $@ -c $<
clean:
	rm *.o example*.o *~
