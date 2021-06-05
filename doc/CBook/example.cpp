#include <CBook/CBook.h>
#include <CBook/H1D.h>
#include <CBook/H2D.h>
#include <iostream>

static double ShootRandomFlat();
static double ShootRandomGauss(double,double);
static double ShootRandomBreitWigner(double,double);

//////////////////////////////////////////////////////////////////////////////
int main(int aArgc, char** aArgs )
//////////////////////////////////////////////////////////////////////////////
//  Book and fill some histos.
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//
{

// book histogram manager
  CBook*  cbook = CBook::Instance();



   int entries = 10000;

// book histograms
   H1D* h1 = new H1D("Gauss1",10,-3,3);
   H1D* h2 = new H1D("Gauss2",5,-5,5);
// book 2D histograms
    H2D* histogram = new H2D("Gauss_BW",10,-5,5,10,-2,2);


// fill 1D histograms
   for(int count=0;count<entries;count++) {
     h1->fill(ShootRandomGauss(1,2),1.4);
     h2->fill(ShootRandomGauss(1,2),1.5);
   }

// fill 2D histogram
   for(int count=0;count<entries;count++) {
     histogram->fill(ShootRandomGauss(1,2),ShootRandomBreitWigner(0,1),0.8);
   }


// check the size of CBook manager
   std::cout << " size so far = " << cbook->size() << std::endl;


// book another histogram
    H1D* h3 = new H1D("Gauss3",20,-3,3);

// fiill it
   for(int count=0;count<entries;count++) {
     h1->fill(ShootRandomGauss(1,2),1.4);
     h2->fill(ShootRandomGauss(1,2),1.0);
   }

   std::cout << " size so far = " << cbook->size() << std::endl;

   // print on screen
   cbook->print();

   // write to XML file
   cbook->write("cpp.xml");

   // clear all histograms
   cbook->clear();



  return 0;
}


/// just to help to fill histograms
#include <stdlib.h> //rand
#include <math.h>
#ifndef M_PI
#define M_PI       3.1415926535897931160E0
#define M_PI_2     1.5707963267948965580E0
#endif
//////////////////////////////////////////////////////////////////////////////
double ShootRandomFlat(
)
//////////////////////////////////////////////////////////////////////////////
// Shoot random numbers according a flat distribution.
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//
{
  double value  = (double)::rand();
  value /= (double)RAND_MAX;
  return value;
}
//////////////////////////////////////////////////////////////////////////////
double ShootRandomGauss(
 double aMean
,double aStdDev
)
//////////////////////////////////////////////////////////////////////////////
//  Shoot random numbers according a
// gaussian distribution of mean 0 and sigma 1.
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//
{
  double v1,v2,r,fac;
  do {
    v1 = 2.0 * ShootRandomFlat() - 1.0;
    v2 = 2.0 * ShootRandomFlat() - 1.0;
    r = v1*v1 + v2*v2;
  } while ( r > 1.0 );
  fac = ::sqrt(-2.0*::log(r)/r);
  return (v2 * fac) * aStdDev + aMean;
}
//////////////////////////////////////////////////////////////////////////////
double ShootRandomBreitWigner(
 double aMean
,double aGamma
)
//////////////////////////////////////////////////////////////////////////////
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//
{
  double rval = 2.0 * ShootRandomFlat() - 1.0;
  double displ = 0.5 * aGamma * ::tan(rval * M_PI_2);
  return aMean + displ;
}
