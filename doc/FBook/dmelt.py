# Run this example in DataMelt
# This example reads histograms  created by C++/Fortan CFBook package
# See: https://datamelt.org/?id=cfbook-library

from jhplot  import *
from jhplot.io import *

hb = CFBook()
hb.read("fortran.xml")
print hb.listAll()
print hb.getKeysH1D() # list keys

h1=hb.getH1D(1)
c1=HPlot("Test")
c1.setGTitle("Histograms from a file");
c1.visible(1)
c1.setAutoRange()
c1.draw(h1)
