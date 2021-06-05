C-----------------------------------------------------------------------
      SUBROUTINE GOPENF
C---OPEN TopDrawer FILE
      CHARACTER*30 FTITLE
      COMMON /GBOOK_OUT/FTITLE 
      LOGICAL OPEN
      DATA OPEN/.FALSE./
      IF (OPEN) RETURN
      OPEN=.TRUE.
      N=0
      OPEN (21,FILE=FTITLE,STATUS='UNKNOWN')
      WRITE (6,*) 'Output file=',FTITLE 
      write(21,*)'<jhepwork>'
      WRITE (21,*)'  <created-by>',
     +      'FORTRAN FBook</created-by>'

      RETURN
      END
