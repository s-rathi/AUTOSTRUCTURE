C                                                                 20/11/14
      PROGRAM CEVALRR_ADAS
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     EVALUATE NON-LINEAR LEAST-SQUARES CFIT TO THE RATE COEFFICIENT,
C     USING RR FIT FORM OF VERNER & FERLAND, APJS V103 PP467-73 (1996).
C     TEMPERATURES ARE ADAS STANDARD.
C     READ cfout FILE.  *** CURRENTLY, MUST BE KELVIN, NO TEST IF EV ***
C
      PARAMETER (NUMT=19)
C
      CHARACTER*1 CARD(70)
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
      NWR=4
      IF(CARD(42).NE.' ')NWR=6
      DO N=0,NUMT,NWR
        READ(8,100,END=10)I0,(A(N+I),I=1,NWR)
        IF(I0.LE.0)GO TO 10
        WRITE(9,100)I0,(A(N+I),I=1,NWR)
      ENDDO
  10  BACKSPACE(8)
      NFIT=N
C
      WRITE(9,201)
C
      NT=0
   5  NT=NT+1
      TT=THTIC(NT)*NZA2
C
      CALL FUNCS(TT,A,ARRF,DUM,NFIT,NWR,IFLAG)
C
      ARRF=EXP(ARRF)
C
      WRITE(9,200)TT,ARRF
      IF(NT.LT.NUMT)GO TO 5
C
  20  STOP 'OUTPUT IN cfout_adas'
C
 100  FORMAT(I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)
 101  FORMAT(70A1)
 102  FORMAT(2X,I2,3X,I2)
 200  FORMAT(1PE10.2,2E11.2,E15.2)
 201  FORMAT(3X,'TEMP(K)',3X,'ARR(ORG)',3X,'ARR(FIT)',9X,'DIFF')
      END
C**********************************************************************
      SUBROUTINE FUNCS(X,A,Y,DYDA,NA,NWR,IFLAG)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER NA
      REAL*8 X,Y,A(NA),DYDA(NA)
      INTEGER I
      REAL*8 T0,T1
C
      IFLAG=0
      DO I=1,NA
        IF(A(I).LT.0)THEN
          IFLAG=-I
          write(99,*)'coeff index',i,'  .lt. 0'
          RETURN
        ENDIF
      ENDDO
      Y=0.
      DO I=1,NA,NWR
        T0=SQRT(X/A(I+2))
        T1=SQRT(X/A(I+3))
        B=A(I+1)
        IF(NWR.EQ.6)B=B+A(I+4)*EXP(-A(I+5)/X)
        Y=Y+LOG(A(I))-LOG(T0)-(1.D0-B)*LOG(1.D0+T0)
     X                       -(1.D0+B)*LOG(1.D0+T1)
c        DYDA(I)=1.D0/A(I)
c        DYDA(I+1)=LOG(1.D0+T0)-LOG(1.D0+T1)
c        DYDA(I+2)=.5D0/A(I+2)+.5D0*(1.D0-A(I+1))*T0/((1.D0+T0)*A(I+2))
c        DYDA(I+3)=.5D0*(1.D0+A(I+1))*T1/((1.D0+T1)*A(I+3))
      ENDDO
C
      RETURN
      END
