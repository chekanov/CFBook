C*********************************************************************
      SUBROUTINE GFILL1(ID,X,W)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      IS=A(ID+2)+0.5
      A(IS+9)=A(IS+9)+1.
      IOX=2
      IF(X.LT.A(IS+2)) IOX=1
      IF(X.GE.A(IS+3)) IOX=3
      A(IS+12+IOX)=A(IS+12+IOX)+W
      IF(IOX.NE.2) RETURN
      IX=(X-A(IS+2))/A(IS+4)
      A(IS+19+IX)=A(IS+19+IX)+W
      RETURN
      END
