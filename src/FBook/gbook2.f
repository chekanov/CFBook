C*********************************************************************
      SUBROUTINE GBOOK2(ID,TITLE,NX,XL,XU,NY,YL,YU)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      CHARACTER TITLE*(*),TITFX*60
      EQUIVALENCE (REQ,IEQ)
      IF (ID.GT.NMAX .OR. A(1)+A(2)+38+NX*NY.GT.NSIZE) THEN
         WRITE (6,200) ID,NMAX,INT(A(1)+A(2)+38+NX*NY+0.5),NSIZE
         RETURN
      ENDIF
      A(ID+2)=A(1)+A(2)
      A(2)=A(2)+38+NX*NY
      IS=A(ID+2)+0.5
      A(IS+1)=NX
      A(IS+2)=XL
      A(IS+3)=XU
      A(IS+4)=(XU-XL)/NX
      A(IS+5)=NY
      A(IS+6)=YL
      A(IS+7)=YU
      A(IS+8)=(YU-YL)/NY
      CALL GRESET(ID)
      TITFX=TITLE//' '
      DO 100 IT=1,20
      IEQ=256**2*ICHAR(TITFX(3*IT-2:3*IT-2))+256*ICHAR(TITFX(3*IT-1:
     &3*IT-1))+ICHAR(TITFX(3*IT:3*IT))
  100 A(IS+18+NX*NY+IT)=REQ
      RETURN
  200 FORMAT (' ERROR: Too much space requested in GBOOK2!'/
     &  ' Requested   ID=',I4,',    Maximum   ID=',I4/
     &  ' Requested space=',I6,', Maximum space=',I6/
     &  ' Recompile with larger NMAX and/or NSIZE')
      END
