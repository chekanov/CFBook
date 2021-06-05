C-----------------------------------------------------------------------
      SUBROUTINE GTOP1(ID,NEW,HIST,LOG,IDERR)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      LOGICAL NEW,HIST,LOG, DIV
      CHARACTER*8 TYPE(4),PLOT
      COMMON /GTCOM/ TYPE,NTYPE
      PARAMETER (NXMAX=1000)
      DIMENSION X(NXMAX),Y(NXMAX),E(NXMAX)
      CHARACTER TITLE*60



*      INTEGER*4         INTVAR
*      CHARACTER         STRING*10
*      INTVAR=20
*      WRITE(UNIT=STRING, FMT='(I8)') INTVAR
* 20   FORMAT(A,E5.1,A)  
*      write(6,20) 'ss:',real(INTVAR),':ss'
 
      IF (ID.GT.NMAX) RETURN
      CALL GOPENF
      WEIGHT=0.0
      AVGE=0.0
      VARIAN=0.0
      IS=NINT(A(ID+2))
      NX=NINT(A(IS+1))
      IF (NX.GT.NXMAX) THEN
        WRITE (6,2000) NXMAX
        NX=NXMAX
      ENDIF
      ISERR=0

C      write(6,*) ' IDERR =',IDERR

      IF (IDERR.NE.0) THEN
         ISERR=NINT(A(IDERR+2))
C          write(6,*) ' ISERR =',ISERR
 
         IF (ISERR .NE. 0) THEN
            IF (A(ISERR+5) .GT. 1.5) THEN
               WRITE (6,1000) IDERR
               ISERR=0
            ELSEIF (NX.NE.NINT(A(ISERR+1))) THEN
               WRITE (6,1010) IDERR
               ISERR=0
            ELSE
C               IF (ID.eq.IDERR) THEN 
C                    WRITE (6,1021) IDERR 
C                ELSE 
C                    WRITE (6,1020) IDERR
C                ENDIF 
            ENDIF
         ENDIF
      ENDIF
      YMIN=1D30
      DO 50  IX=1, NX
 50      IF (A(IS+18+IX).GT.0.0) YMIN=MIN(YMIN,A(IS+18+IX))
      DIV=.FALSE.
      DO 100 IX=1, NX
         X(IX)=(IX-0.5)*A(IS+4)+A(IS+2)
         Y(IX)=A(IS+18+IX)
         E(IX)=0D0 
         IF (ISERR.NE.0.and.ID.ne.IDERR) E(IX)=A(ISERR+18+IX)
C as SQRT in case if the same histogram is used for the errors
         IF (ISERR.NE.0.and.ID.eq.IDERR) E(IX)=dsqrt( A(ISERR+18+IX) ) 
C        write(6,*) X(IX), Y(IX), E(IX)
         WEIGHT=WEIGHT+Y(IX)
         AVGE=AVGE+X(IX)*Y(IX)
         VARIAN=VARIAN+X(IX)**2*Y(IX)
         IF (LOG .AND. Y(IX).LE.0.0) THEN
            WRITE (6,2010) Y(IX),YMIN/1000
            Y(IX)=YMIN/1000
            DIV=.TRUE.
         ENDIF
 100  CONTINUE
      IF (WEIGHT.EQ.0) WEIGHT=1
      AVGE=AVGE/WEIGHT
      VARIAN=VARIAN/WEIGHT-AVGE**2
      CALL GTITLE(ID,TITLE,LENTIT)
C  SET UP PAGE...
      IF (NEW) THEN

         WRITE (21,*)'<histogram-h1d title="',TITLE(1:LENTIT),
     +   '" id="',ID,'">'

         WRITE (21,*)'<x-axis>'
         WRITE (21,*)'  <range bins="', INT(A(IS+1)),
     +   '" min="', SNGL(A(IS+2)),
     +   '" max="',SNGL(A(IS+3)),'">'
         WRITE (21,*)'  <out-of-range underflow="',A(IS+13),
     +               '" overflow="',A(IS+15),'"/>'
         WRITE (21,*) '</x-axis>'

*   statistics
        WRITE (21,*)'<statistics>'
        WRITE (21,*)'  <all-entries>',
     +     A(IS+14), '</all-entries>' 

        WRITE (21,*)' <in-range-entries>',
     +     A(IS+14)-A(IS+13)-A(IS+15),'</in-range-entries>'
        WRITE (21,*)'  <mean>',
     +     AVGE, '</mean>'

        IF (VARIAN.GE.0.0) THEN
           RMS=SQRT(VARIAN)
          ELSE
           RMS=VARIAN
         ENDIF
        WRITE (21,*)'  <rms>', 
     +     AVGE, '</rms>'
        WRITE (21,*)'</statistics>'

*         WRITE (21,1100) A(IS+13)
*         WRITE (21,1110) A(IS+14)
*         WRITE (21,1120) A(IS+15)
*         WRITE (21,1130) AVGE
*         IF (VARIAN.GE.0.0) THEN
*          WRITE (21,1140) SQRT(VARIAN)
*          ELSE
*          WRITE (21,1150) VARIAN
*         ENDIF
C         WRITE (21,*) 'ORDER X Y DY'
         NTYPE=0
      ENDIF
      NTYPE=MOD(NTYPE,4)+1
C  PLOT...
*      WRITE (21,*) 'ORDER: X  Y  Error'
*      WRITE (21,*) 'BEGIN'

*  bin content
         WRITE (21,*)'<bincontents order="xLow,xHigh,y">'
         WRITE (21,*)'  binLower,binHigh,hight,error,entries'
         WRITE (21,*)'</bincontents>'


      WRITE (21,*)'<data>'


* 109  FORMAT (D15.7,A1,D15.7,A1,D15.7,A1,D15.7,A1,D15.7)

      
      DO 200 I=1, NX
         IF ((ABS(X(I)).GE.1E-3.AND.ABS(X(I)).LT.1E5.OR.X(I).EQ.0).AND.
     &       (ABS(Y(I)).GE.1E-5.AND.ABS(Y(I)).LT.1E3.OR.Y(I).EQ.0)) THEN
*            WRITE (21,'(E14.5,2E14.7)') X(I),Y(I),E(I)
       WRITE (21,*) X(I)-0.5*A(IS+4),',',
     +     X(I)+0.5*A(IS+4),',',Y(I),',',E(I),',',Y(I) 
       ELSE
*         WRITE (21,'(3(1PE14.4))') X(I),Y(I),E(I)
       WRITE (21,*) X(I)-0.5*A(IS+4),',',X(I)+0.5*A(IS+4),',',
     +   Y(I),',',E(I),',',Y(I)
      ENDIF
 200  CONTINUE
      WRITE (21,*)'</data>' 
      WRITE (21,*)'</histogram-h1d>' 
      WRITE (21,*)''
      PLOT=' '
C      IF (ISERR.NE.0) PLOT=' ; PLOT'
C      IF (HIST) THEN
C         WRITE (21,*) 'HIST ',TYPE(NTYPE),PLOT
C      ELSE
C         WRITE (21,*) 'JOIN ',TYPE(NTYPE),PLOT
C      ENDIF

C      WRITE (6,1100) A(IS+13)
C      WRITE (6,1110) A(IS+14)
C      WRITE (6,1120) A(IS+15)
C      WRITE (6,1130) AVGE
C      IF (VARIAN.GE.0.0) THEN
C         WRITE (6,1140) SQRT(VARIAN)
C      ELSE
C         WRITE (6,1150) VARIAN
C      ENDIF
 1000 FORMAT (5X,'HISTOGRAM NO',I4,' : SHOULD BE 1-DIM BUT ISN''T')
 1010 FORMAT (5X,'HISTOGRAM NO',I4,' : SHOULD BE SAME SIZE BUT ISN''T')
 1020 FORMAT (5X,'HISTOGRAM NO',I4,' : BEING USED FOR ERROR BARS')
 1021 FORMAT (5X,'HISTOGRAM NO',I4,' : SQRT(events) is used for errors')
 1100 FORMAT (5X,'Total underflow=',G10.3)
 1110 FORMAT (5X,'Total entry    =',G10.3)
 1120 FORMAT (5X,'Total overflow =',G10.3)
 1130 FORMAT (5X,'Mean value        =',G10.3)
 1140 FORMAT (5X,'Standard deviation=',G10.3)
 1150 FORMAT (5X,'Variance          =',G10.3)
 2000 FORMAT (5X,'WARNING: Histogram too big, using first',I4,' bins')
 2010 FORMAT (5X,'WARNING:',G10.3,' cannot be logged. Now using',G10.3)
      END
