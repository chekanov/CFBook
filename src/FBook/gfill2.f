


C*********************************************************************
      SUBROUTINE GFILL2(ID,X,Y,W)
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
      IOY=2
      IF(Y.LT.A(IS+6)) IOY=1
      IF(Y.GE.A(IS+7)) IOY=3
      A(IS+6+3*IOY+IOX)=A(IS+6+3*IOY+IOX)+W
      IF(IOX.NE.2.OR.IOY.NE.2) RETURN
      IX=(X-A(IS+2))/A(IS+4)
      IY=(Y-A(IS+6))/A(IS+8)
      IC=INT(A(IS+1)+0.5)*IY+IX
      A(IS+19+IC)=A(IS+19+IC)+W
      RETURN
      END
