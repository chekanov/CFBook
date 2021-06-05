C-----------------------------------------------------------------------
      SUBROUTINE GAREA(ID,AREA)
C
C  SCALES HISTOGRAM ID SO THAT THE AREA UNDER IT BECOMES AREA
C
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      IF (ID.GT.NMAX) RETURN
      IS=NINT(A(ID+2))
      BNWDTH=A(IS+4)
      AREAOL=A(IS+14)
      IF (AREAOL*BNWDTH.EQ.0.0) RETURN
      AREASC=AREA/(AREAOL*BNWDTH)
      CALL GSCALE (ID,AREASC)
      END
