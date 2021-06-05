

C ---------------------------------------------
      SUBROUTINE GWRITE(fileName) 
C---OUTPUT ALL HISTOGRAMS TO TOPDRAWER with error bars
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      CHARACTER fileName*(*)
      CHARACTER*30 FTITLE
      COMMON /GBOOK_OUT/ FTITLE

      FTITLE=fileName
      DO 100 ID=1,INT(A(1)+0.5)
      IS=A(ID+2)+0.5
      IF(IS.EQ.0.OR.A(IS+9).LT.0.5) GOTO 100
      CALL GTOPER(ID,.TRUE.,.TRUE.,.FALSE.,ID)
  100 CONTINUE

      write(21,*) '</jhepwork>' 
      close(21)
      END
