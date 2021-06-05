



C*********************************************************************
C fill assuming weight 1
C ********************************************************************
      SUBROUTINE GF2(ID,X,Y)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CALL GFILL2(ID,X,Y,1D0)
      RETURN
      END
