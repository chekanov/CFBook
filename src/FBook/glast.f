C*********************************************************************
      INTEGER FUNCTION GLAST(STRING)
C  LOOK FOR LAST NON-SPACE IN STRING
      CHARACTER STRING*108
      I=108
 10   IF (STRING(I:I).EQ.' ') THEN
        I=I-1
        IF (I.GT.1) GOTO 10
      ENDIF
      GLAST=I
      END
