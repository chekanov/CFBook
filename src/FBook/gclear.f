C*********************************************************************
      SUBROUTINE GCLEAR
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      DO 100 ID=1,INT(A(1)+0.5)
      IS=A(ID+2)+0.5
      IF(IS.EQ.0.OR.A(IS+9).LT.0.5) GOTO 100
      CALL GPRINT(ID)
      CALL GRESET(ID)
  100 CONTINUE
      RETURN
      END
