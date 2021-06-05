# CFBook. Simple library for histogram booking for C++ and Fortran programs

CFBook is a simple library providing histogram booking and related facilities for C++ or Fortran programs. 1D and 2D Histograms can be booked and filled in either C++ or FORTRAN code. The uniform XML output with histograms can be read by Jas4pp or DataMelt. This distribution contains the code required to build the 2 libraries: cbook (to be linked with C++ code) and fbook (to be linked with FORTRAN), as well as a few example programs.

## Installation

To compile this library, you need g++, ar, gfortran and make tool. 

### Compiling C++ library

    cd cmd_CBook
    make
   
   
### Compile the FORTRAN library 

    cd cmd_FBook
    make
    
These commands create object libraries: lib/libcbook.a and lib/libfbook.a in the directory "lib".
These libraries should be linked to your C++ or Fortan program.   


## Running examples:

Here is a description of how to use the compiled libraries. Look at the directory "doc": 

### C++ example: 

This example shows how to use the library with a C++ code:
 
     cd doc/CBook; make; example.exe; 

The main code used for compilation is "example.cpp". The execution of example.exe produces the XML file cpp.xml with histograms. 

### Fortran example: 

This example shows how to use the library with a Fortran code:
    
    cd doc/FBook; make; example.exe; 

The main code used for compilation is "example.f".  The execution of example.exe produces the XML file fortran.xml with histograms. 
    

Now you can show the histograms located in cpp.xml or fortran.xml files. Run the script "dmelt.py" located in these directories using Jas4pp or DataMelt. This script uses Jython to call the CFBook Java class that retrieves H1D, H2D and other histogram objects and display them.

Acknowledgement: CFBook includes code from the HCL package (G.Barrand, OpenScientist package) and GBook package (T.Sjostrand and M.Seymour). 

This library was  created in 2006 and updated in 2021.

S.Chekanov (ANL)
 

