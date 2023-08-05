C                                                               20/11/14
      PROGRAM CEVAL
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     EVALUATE NON-LINEAR LEAST-SQUARES CFIT TO THE RATE COEFFICIENT,
C     USING PAIRS OF COEFFICIENTS OF FORM SUM_I A_I*EXP(-B_I/T)/T**1.5
C     AT USER SPECIFIED NT LOG SPACED TEMPERATURES BETWEEN TMIN AND TMAX
C     IN UNITS OF THE cfout FILE PRODUCED BY CFIT (REQUIRED).
C     CAN ALLOW FOR NEW USER COMMENTS AT THE TOP OF THE cfout FILE.
C     NT.GT.0 OUTPUT TO SCREEN, .LT. 0 TO FILE 'gout', .EQ. 0 EXITS.
C
      PARAMETER (NUMT=200)
C
      CHARACTER*1 CARD(60)
C
      LOGICAL BOUT
C
      DIMENSION DUM(NUMT),A(NUMT)
C
      OPEN(8,FILE='cfout')
      OPEN(9,FILE='gout')
C
      DO N=1,100
        READ(8,101)CARD
        IF(CARD(3).EQ.'N')GO TO 1
      ENDDO
   1  DO I=1,NUMT,2
        READ(8,100,END=10)I0,A(I),A(I+1)
        IF(I0.LE.0)GO TO 10
      ENDDO
  10  NFIT=I-1
C
   5  WRITE(*,*)'INPUT NO T, TMIN, TMAX (IN UNITS USED BY cfout)'
      READ(*,*)NT,TMIN,TMAX
      IF(NT.EQ.0)STOP 'BYE'
C
      BOUT=NT.GT.0
      NT=IABS(NT)
      IF(NT.GT.NUMT)STOP 'INCREASE NUMT TO NT'
C
      TMIN=LOG10(TMIN)
      IF(NT.GT.1)THEN
        TMAX=LOG10(TMAX)
        DT=(TMAX-TMIN)/(NT-1)
      ELSE
        DT=0.0
      ENDIF
C
      DO I=1,NT
        TT=(I-1)*DT+TMIN
        TT=10**TT
        CALL FUNCS(TT,A,ADR,DUM,NFIT)
        ADR=ADR/(TT*SQRT(TT))
        IF(BOUT)THEN
          WRITE(*,*)'TEMP= ',TT,'  RATE COEF.= ',ADR
        ELSE
          WRITE(9,200)TT,ADR
        ENDIF
      ENDDO
      if(.not.bout)call flush(9)
      GO TO 5
C
 100  FORMAT(I3,1PE11.3,1X,E11.3)
 101  FORMAT(60A1)
 200  FORMAT(1P2D10.2)
C
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
