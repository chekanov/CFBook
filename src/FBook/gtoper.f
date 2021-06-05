C-----------------------------------------------------------------------
      SUBROUTINE GTOPER(ID,NEW,HIST,LOG,IDERR)
C
C  IDENTICAL TO GTOPDR EXCEPT THAT IF IDERR IS NON-ZERO IT IS USED FOR
C  ERROR BARS FOR 1-DIM HISTOGRAMS
C
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      LOGICAL NEW,HIST,LOG
      IF (ID.GT.NMAX) RETURN
      IS=NINT(A(ID+2))
      IF (IS .EQ. 0) RETURN
      IF (A(IS+14) .EQ. 0.0) THEN
         WRITE (6,1000) ID
      ELSE
C         WRITE (6,1010) ID
         IF (A(IS+5) .LT. 1.5) THEN
            CALL GTOP1(ID,NEW,HIST,LOG,IDERR)
C            write(6,*) "WRITE TO FILE!"
         ELSE
            CALL GTOP2(ID,NEW)
         ENDIF
      ENDIF
 1000 FORMAT (/5X,'HISTOGRAM NO',I4,' : NO ENTRIES FOUND')
 1010 FORMAT (/5X,'HISTOGRAM NO',I4,' : OUTPUTTING TO TOPDRAWER FILE')
      END
