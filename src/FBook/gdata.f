C*********************************************************************
      BLOCK DATA GDATA
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      DATA (A(J), J=1,NMAX+2)/NMAX,2,NMAX*0/
      END
