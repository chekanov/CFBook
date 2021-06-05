#ifndef CBook_H 
#define CBook_H
#include <HCL/Histogram.h>

class CBook{
public:
    static CBook* Instance();  // gives back a real object!

     unsigned int  sizeH1D();
     unsigned int  sizeH2D();
     unsigned int  size(); // all size
     void  write(const char* filename);
     void  print();
     void  clear();
     void printH1D(unsigned int i);
     void printH2D(unsigned int i);
     void print(
                   HCL::Base1D& aHistogram);
     void print(
                   HCL::Base2D& aHistogram);

protected:
    CBook();                   // constructor
private:
    static CBook* _singleton;
};

#endif
