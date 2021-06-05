C*********************************************************************
      SUBROUTINE GRESET(ID)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      IS=A(ID+2)+0.5
      DO 110 IC=IS+9,IS+18+INT(A(IS+1)+0.5)*INT(A(IS+5)+0.5)
  110 A(IC)=0.
      RETURN
      END
