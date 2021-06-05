

C-----------------------------------------------------------------------
      SUBROUTINE GTITLE(ID,TITLE,LENTIT)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      CHARACTER TITLE*60
      EQUIVALENCE (REQ,IEQ)
      IF (ID.GT.NMAX) RETURN
      IS=NINT(A(ID+2))
      NX=NINT(A(IS+1))
      NY=NINT(A(IS+5))
      DO 100 IT=1, 20
         REQ=A(IS+18+NX*NY+IT)
         TITLE(3*IT-2:3*IT)=CHAR(IEQ/256**2)//CHAR(MOD(IEQ,256**2)
     &      /256)//CHAR(MOD(IEQ,256))
 100  CONTINUE
      LENTIT=60
 200  IF (LENTIT.GT.1 .AND. TITLE(LENTIT:LENTIT).EQ.' ') THEN
         LENTIT=LENTIT-1
         GOTO 200
      ENDIF
      END
