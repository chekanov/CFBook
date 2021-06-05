C-----------------------------------------------------------------------
      SUBROUTINE GACCUM(ID)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=400)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      TOTAL=0.0
      IS=NINT(A(ID+2))
      IF (IS.EQ.0) RETURN
      DO 50 IX=1,NINT(A(IS+1))
        TOTAL=TOTAL+A(IS+18+IX)
        A(IS+18+IX)=TOTAL
 50   CONTINUE
      END
