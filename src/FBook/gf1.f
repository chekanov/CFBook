



C*********************************************************************
C fill assuming weight 1 
C ********************************************************************
      SUBROUTINE GF1(ID,X)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CALL GFILL1(ID,X,1D0)
      RETURN
      END
