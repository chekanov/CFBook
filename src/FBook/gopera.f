

C*********************************************************************
      SUBROUTINE GOPERA(ID1,OPER,ID2,ID3,F1,F2)
      IMPLICIT INTEGER (I-N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSIZE=200000,NMAX=2000)
      COMMON /GBOOK/ A(NSIZE)
      CHARACTER OPER*1
      IF (ID1.GT.NMAX .OR. ID2.GT.NMAX .OR. ID3.GT.NMAX) RETURN
      IS1=A(ID1+2)+0.5
      IS2=A(ID2+2)+0.5
      IS3=A(ID3+2)+0.5
      NC=INT(A(IS3+1)+0.5)*INT(A(IS3+5)+0.5)
      IF(OPER.EQ.'+'.OR.OPER.EQ.'-'.OR.OPER.EQ.'*'.OR.OPER.EQ.'/')
     &A(IS3+9)=A(IS1+9)+A(IS2+9)
      IF(OPER.EQ.'A'.OR.OPER.EQ.'S'.OR.OPER.EQ.'L') A(IS3+9)=A(IS1+9)
      IF(OPER.EQ.'+') THEN
      DO 100 IC=10,18+NC
  100 A(IS3+IC)=F1*A(IS1+IC)+F2*A(IS2+IC)
      ELSEIF(OPER.EQ.'-') THEN
      DO 110 IC=10,18+NC
  110 A(IS3+IC)=F1*A(IS1+IC)-F2*A(IS2+IC)
      ELSEIF(OPER.EQ.'*') THEN
      DO 120 IC=10,18+NC
  120 A(IS3+IC)=F1*A(IS1+IC)*F2*A(IS2+IC)
      ELSEIF(OPER.EQ.'/') THEN
      DO 130 IC=10,18+NC
      FA2=F2*A(IS2+IC)
      IF(ABS(FA2).LE.1E-10) A(IS3+IC)=0.
  130 IF(ABS(FA2).GT.1E-10) A(IS3+IC)=F1*A(IS1+IC)/FA2
      ELSEIF(OPER.EQ.'A') THEN
      DO 140 IC=10,18+NC
  140 A(IS3+IC)=F1*A(IS1+IC)+F2
      ELSEIF(OPER.EQ.'S') THEN
      ZERO=0
      DO 150 IC=10,18+NC
  150 A(IS3+IC)=F1*SQRT(MAX(ZERO,A(IS1+IC)))+F2
      ELSEIF(OPER.EQ.'L') THEN
      ZMIN=1E30
      DO 160 IC=19,18+NC
  160 IF(A(IS1+IC).LT.ZMIN.AND.A(IS1+IC).GT.1E-20) ZMIN=0.8*A(IS1+IC)
      DO 170 IC=10,18+NC
  170 A(IS3+IC)=F1*LOG10(MAX(A(IS1+IC),ZMIN))+F2
      ELSEIF(OPER.EQ.'M') THEN
      DO 180 IC=10,18+NC
      IF(ABS(A(IS1+IC)).LE.1E-10) A(IS2+IC)=0.
      IF(ABS(A(IS1+IC)).GT.1E-10) A(IS2+IC)=A(IS2+IC)/A(IS1+IC)
      IF(ID3.NE.0.AND.ABS(A(IS1+IC)).LE.1E-10) A(IS3+IC)=0.
      ZERO=0
      IF(ID3.NE.0.AND.ABS(A(IS1+IC)).GT.1E-10) A(IS3+IC)=
     &SQRT(MAX(A(IS3+IC)/A(IS1+IC)-A(IS2+IC)**2,ZERO))
  180 A(IS1+IC)=F1*A(IS1+IC)
      ENDIF
      RETURN
      END
