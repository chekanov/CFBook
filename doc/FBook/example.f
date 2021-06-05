
C how to book histograms using CFBook package
 
      PROGRAM EXAMPLE

      real f,rand        
      integer i       
      
C book histograms 
        CALL GBOOK1(1,"1D example",10,0D0,40D0)
        CALL GBOOK1(2,"1D again example",5,0D0,10D0)
        CALL GBOOK2(3,"2D example",10,0D0,1D0,10,0D0,5D0)


        do i=1,1000
          f=MCPOI(10.0) 
C fill histogram with f
          call GF1(1,2D0*f)
          f=MCPOI(5.0)
C fill another hitogram
           call GF1(2,0.2D0*f)
           f1=MCPOI(0.5)
           f2=MCPOI(2.0)
           call GF2(3,0.5D0*f1,0.5D0*f2)
        enddo


C...Write  histograms to a XML file.
        Call GWRITE("fortran.xml")

        END



