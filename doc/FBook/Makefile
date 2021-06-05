#          this is without optimization 
FCOMPILER = gfortran
FFLAGS	  = -O2
LFLAGS    = -lfbook
LIBS      = -L../../lib
#
#
objects  :=	example.o stat.o

global	=	Makefile

tasks:          example.exe 

example.exe: 	$(objects)
	$(FCOMPILER) $(FFLAGS) $(objects) $(LIBS) -o $@ $(LFLAGS) 

%.o : %.f $(global)
	$(FCOMPILER) $(FFLAGS) -c $< -o $@

clean:
	rm *.o example*.o *~
