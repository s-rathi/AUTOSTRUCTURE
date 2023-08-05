C                                                               20/11/14
      PROGRAM CEVAL_ADAS
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     EVALUATE NON-LINEAR LEAST-SQUARES CFIT TO THE RATE COEFFICIENT,
C     USING PAIRS OF COEFFICIENTS OF FORM SUM_I A_I*EXP(-B_I/T)/T**1.5
C     TEMPERATURES ARE ADAS STANADARD.
C     READ cfout FILE.  *** CURRENTLY, MUST BE KELVIN, NO TEST IF EV ***
C
      PARAMETER (NUMT=19)
C
      CHARACTER*1 CARD(60)
C
      DIMENSION DUM(NUMT),A(NUMT),THTIC(NUMT)
C
      DATA 
     XTHTIC/1.0D1,2.0D1,5.0D1,1.0D2,2.0D2,5.0D2,1.0D3,2.0D3,5.0D3,1.0D4
     X     ,2.0D4,5.0D4,1.0D5,2.0D5,5.0D5,1.0D6,2.0D6,5.0D6,1.0D7/
C
      OPEN(8,FILE='cfout')
      OPEN(9,FILE='cfout_adas')
C
      NZ=0
      DO N=1,100
        READ(8,101)CARD
        WRITE(9,101)CARD
        IF(CARD(1).EQ.'Z')THEN
          BACKSPACE(8)
          READ(8,102)NZ,NE
        ENDIF
        IF(CARD(3).EQ.'N')GO TO 2
      ENDDO
C
   2  IF(NZ.EQ.0)STOP 'REQUIRE NZ, NE TO SET ADAS TEMPS'
C
      NZA2=NZ-NE
      NZA2=NZA2*NZA2
C
      DO I=1,NUMT,2
        READ(8,100,END=10)I0,A(I),A(I+1)
        IF(I0.LE.0)GO TO 10
        WRITE(9,100)I0,A(I),A(I+1)
      ENDDO
  10  BACKSPACE(8)
      NFIT=I-1
C
      WRITE(9,201)
C
      NT=0
   5  NT=NT+1
      TT=THTIC(NT)*NZA2
C
      CALL FUNCS(TT,A,ADRF,DUM,NFIT)
C
      ADRF=ADRF/(TT*SQRT(TT))
C
      WRITE(9,200)TT,ADRF
      IF(NT.LT.NUMT)GO TO 5
C
  20  WRITE(*,*)'OUTPUT IN cfout_adas'
C
 100  FORMAT(I3,1PE11.3,1X,E11.3)
 101  FORMAT(60A1)
 102  FORMAT(2X,I2,3X,I2)
 200  FORMAT(1PE10.2,E11.2)
 201  FORMAT(//3X,'TEMP(K)',3X,'ADR(FIT)')
      END
C******************************************************************
      SUBROUTINE FUNCS(X,A,Y,DYDA,NA)
      INTEGER NA
      REAL*8 X,Y,A(NA),DYDA(NA)
      INTEGER I
      REAL*8 ARG,EX
C
      Y=0.
      DO I=1,NA,2
        ARG=A(I+1)/X
        EX=EXP(-ARG)
        Y=Y+A(I)*EX
c        DYDA(I)=EX
c        DYDA(I+1)=-A(I)*EX/X
      ENDDO
C
      RETURN
      END
