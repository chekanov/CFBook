C-----------------------------------------------------------------------
      SUBROUTINE GTOP2(ID,NEW)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      LOGICAL NEW
      PARAMETER (NZ=250)
      DIMENSION Z(NZ,NZ)
      CHARACTER TITLE*60
      IF (ID.GT.NMAX) RETURN
      CALL GOPENF
      WEIGHT=0.0
      XAVGE=0.0
      XVARIN=0.0
      YAVGE=0.0
      YVARIN=0.0
      ZMAX=0
      IS=NINT(A(ID+2))
      NX=NINT(A(IS+1))
      NY=NINT(A(IS+5))
      IF (NX.GT.NZ) THEN
        WRITE (6,2000) NZ
        NX=NZ
      ENDIF
      IF (NY.GT.NZ) THEN
        WRITE (6,2000) NZ
        NY=NZ
      ENDIF
      DO 100 IX=1, NX
      DO 100 IY=1, NY
         X=(IX-1)*A(IS+4)+A(IS+2)
         Y=(IY-1)*A(IS+8)+A(IS+6)
         Z(IX,IY)=A(IS+18+NX*(IY-1)+IX)

         SMALL=1E-20
         IF (ABS(Z(IX,IY)).LT.SMALL) Z(IX,IY)=0.0
         IF (Z(IX,IY).GT.ZMAX) ZMAX=Z(IX,IY)
         WEIGHT=WEIGHT+Z(IX,IY)
         XAVGE=XAVGE+X*Z(IX,IY)
         XVARIN=XVARIN+X**2*Z(IX,IY)
         YAVGE=YAVGE+Y*Z(IX,IY)
         YVARIN=YVARIN+Y**2*Z(IX,IY)
 100  CONTINUE
      IF (ZMAX.GE.1 .AND. ZMAX.LE.20) THEN
         ZMAX=20
      ELSE
         WRITE (6,2010) ZMAX/20
      ENDIF
      XAVGE=XAVGE/WEIGHT
      XVARIN=XVARIN/WEIGHT-XAVGE**2
      YAVGE=YAVGE/WEIGHT
      YVARIN=YVARIN/WEIGHT-YAVGE**2
      CALL GTITLE(ID,TITLE,LENTIT)

* 109  FORMAT (D15.7,A1,D15.7)

C  SET UP PAGE...
      IF (NEW) THEN

         WRITE (21,*)'<histogram-h2d title="',TITLE(1:LENTIT),
     +   '" id="',ID,'">'

         WRITE (21,*)'<x-axis>'
         WRITE (21,*)'  <range bins="', INT(A(IS+1)),
     +   '" min="', SNGL(A(IS+2)),
     +   '" max="',SNGL(A(IS+3)),'">'
          WRITE (21,*)'  <variable-width-bins>'
          DO IX=1, NX
          WRITE (21,*) (IX-1)*A(IS+4)+A(IS+2),',',
     +    (IX-1)*A(IS+4)+A(IS+2)+A(IS+4) 
          ENDDO
          WRITE (21,*)'  </variable-width-bins>'
          WRITE (21,*)'</x-axis>'



         WRITE (21,*)'<y-axis>'
         WRITE (21,*)'  <range bins="', INT(A(IS+5)),
     +   '" min="', SNGL(A(IS+6)),
     +   '" max="',SNGL(A(IS+7)),'">'
          WRITE (21,*)' <variable-width-bins>'
          DO IY=1, NY
          WRITE (21,*) (IY-1)*A(IS+8)+A(IS+6),',',
     + (IY-1)*A(IS+8)+A(IS+6)+A(IS+8) 
          ENDDO
          WRITE (21,*)' </variable-width-bins>'
          WRITE (21,*)'</y-axis>'


          WRITE (21,*)'<out-of-range-data>'
          DO J=10,18 
          WRITE (21,*) A(IS+J)
          ENDDO 
          WRITE (21,*)'</out-of-range-data>'



*   statistics
        WRITE (21,*)'<statistics>'
        WRITE (21,*)'  <all-entries>', 
     +     A(IS+14), '</all-entries>'

        WRITE (21,*)'  <in-range-entries>',
     +     A(IS+14)-A(IS+13)-A(IS+15), '</in-range-entries>'

        RmsX=-1;
        IF (XVARIN.ge.0) RmsX=SQRT(XVARIN)
        RmsY=-1;
        IF (YVARIN.ge.0) RmsY=SQRT(YVARIN)


        WRITE (21,*)'  <x-mean>', 
     +     XAVGE, '</x-mean>'
        WRITE (21,*)'  <x-rms>', 
     +     RmsX, '</x-rms>'
        WRITE (21,*)'  <y-mean>', 
     +     YAVGE, '</y-mean>'
        WRITE (21,*)'  <y-rms>', 
     +     RmsY, '</y-rms>'
         WRITE (21,*)'</statistics>'


         WRITE (21,*)'<bincontents order="x,y">'
         WRITE (21,*)'  bin,height,error,entries'
         WRITE (21,*)'</bincontents>'

         WRITE (21,*)'<data>'
      jj=0


* 110  FORMAT (I5,A1,I5,A1,D15.7,A1,D15.7,A1,D15.7)

      DO IX=1, NX
      DO IY=1, NY
         X=(IX-1)*A(IS+4)+A(IS+2)
         Y=(IY-1)*A(IS+8)+A(IS+6)
         Z(IX,IY)=A(IS+18+NX*(IY-1)+IX)
         WRITE (21,*)IX-1,',',IY-1,',',
     +    Z(IX,IY),',',sqrt(Z(IX,IY)),',',Z(IX,IY)
         jj=jj+1
      ENDDO
      ENDDO

         WRITE (21,*)'</data>'
         WRITE (21,*)'</histogram-h2d>'
         WRITE (21,*)''

C        WRITE (21,*) 'NEW=2D ',ID,
C     +  INT(A(IS+1)),SNGL(A(IS+2)),SNGL(A(IS+3)), 
C     +  INT(A(IS+5)), SNGL(A(IS+6)),SNGL(A(IS+7)) 
C        WRITE (21,*) 'ID=',ID
C        WRITE (21,*) 'TITLE=',TITLE(1:LENTIT)
C      WRITE (21,1000)
C      WRITE (21,1030) (A(IS+J), J=16,18)
C      WRITE (21,1020) (A(IS+J), J=13,15)
C      WRITE (21,1010) (A(IS+J), J=10,12)
C      WRITE (21,1040) XAVGE
C      IF (XVARIN.GT.0) WRITE (21,1050) SQRT(XVARIN)
C      WRITE (21,1060) YAVGE
C      IF (YVARIN.GT.0) WRITE (21,1070) SQRT(YVARIN)
C       WRITE (21,*) 'LIMITS X',SNGL(A(IS+2)),SNGL(A(IS+3)),
C     &    ' Y',SNGL(A(IS+6)),SNGL(A(IS+7))
C      ENDIF
        ENDIF
C  PLOT...
C      WRITE (21,*) 'ORDER: BinX  BinY   Z   Error'
C      WRITE (21,*) 'BEGIN'
C      NN=0
C      DO IX=1, NX
C      DO IY=1, NY
C         X=(IX-1)*A(IS+4)+A(IS+2)
C         Y=(IY-1)*A(IS+8)+A(IS+6)
C         Z(IX,IY)=A(IS+18+NX*(IY-1)+IX)
C      WRITE (21,'(I7,I7,2E14.7)')IX,IY,Z(IX,IY),sqrt(Z(IX,IY)) 
C      ENDDO
C      ENDDO  








*         DO 200 IZ=1, NINT(20*Z(IX,IY)/ZMAX)
*            X=(IX-RAN(ISEED))*A(IS+4)+A(IS+2)
*            Y=(IY-RAN(ISEED))*A(IS+8)+A(IS+6)
*            IF ((ABS(X).GE.1E-5.AND.ABS(X).LT.1E3.OR.X.EQ.0).AND.
*     &          (ABS(Y).GE.1E-5.AND.ABS(Y).LT.1E3.OR.Y.EQ.0)) THEN
*               WRITE (21,'(2F14.7)') X,Y
*            ELSE
*               WRITE (21,'(2(1PE14.4))') X,Y
*            ENDIF
*            NN=NN+1
* 200     CONTINUE
C         IF (NN.GT.500) THEN
C            WRITE (21,*) 'PLOT'
C            NN=0
C          ENDIF
C  210  CONTINUE
C      WRITE (21,*) 'END'
C      WRITE (21,*) ' '
C      IF (NN.GT.0) WRITE (21,*) 'PLOT'

C      WRITE (6,1000)
C      WRITE (6,1030) (A(IS+J), J=16,18)
C      WRITE (6,1020) (A(IS+J), J=13,15)
C      WRITE (6,1010) (A(IS+J), J=10,12)
C      WRITE (6,1040) XAVGE
C      IF (XVARIN.GT.0) WRITE (6,1050) SQRT(XVARIN)
C      WRITE (6,1060) YAVGE
C      IF (YVARIN.GT.0) WRITE (6,1070) SQRT(YVARIN)
 1000 FORMAT (20X,'   X-under',5X,' X-on-grid',5X,'    X-over')
 1010 FORMAT (5X,'   Y-under',3(5X,G10.3))
 1020 FORMAT (5X,' Y-on-grid',3(5X,G10.3))
 1030 FORMAT (5X,'    Y-over',3(5X,G10.3))
 1040 FORMAT (5X,'Mean x value        =',G10.3)
 1050 FORMAT (5X,'X standard deviation=',G10.3)
 1060 FORMAT (5X,'Mean y value        =',G10.3)
 1070 FORMAT (5X,'Y standard deviation=',G10.3)
 2000 FORMAT (5X,'WARNING: Histogram too big, using first',I4,' bins')
 2010 FORMAT (5X,'WARNING: Dividing by ',G10.3)
      END
