

C*********************************************************************
      SUBROUTINE GSCALE(ID,F)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      IS=A(ID+2)+0.5
      DO 100 IC=IS+10,IS+18+INT(A(IS+1)+0.5)*INT(A(IS+5)+0.5)
  100 A(IC)=F*A(IC)
      RETURN
      END
