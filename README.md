# CFBook. Histogramming package for C++ and Fortran

CFBook is a library providing histogram booking and related facilities for C++ or Fortran programs. 1D and 2D Histograms can be booked and filled in either C++ or FORTRAN code. The uniform XML output with histograms can be read by DataMelt. This distribution contains the code required to build the 2 libraries: cbook (to be linked with C++ code) and fbook (to be linked with FORTRAN), as well as a few example programs.

## Installation

To compile this library, you need gcc, gfortran and make

    Compile the C++ library from the cmd_CBook directory (type "make")
    Compile the FORTRAN library from the cmd_FBook directory (type "make")

## Running examples:

Compile and run examples in the directory "doc"

    C++ example: cd doc/CBook; make; example.exe; This produces the XML file cpp.xml with histograms

    Fortran example: cd doc/FBook; make; example.exe; This produces the XML file fortran.xml with histograms.

Now you can show the histograms located in cpp.xml or fortran.xml files. Run the script "dmelt.py" located in these directories using DataMelt. This script uses Jython to call the CFBook Java class that retrieves H1D, H2D and other histogram objects and display them.

CFBook is based on HCL package (G.Barrand, OpenScientist package) and GBook package (T.Sjostrand and M.Seymour)

DataMelt project (S.Chekanov)
