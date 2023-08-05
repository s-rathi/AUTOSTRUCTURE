C                                                               20/11/14
      PROGRAM CEVALRR
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     EVALUATE NON-LINEAR LEAST-SQUARES CFIT TO THE RATE COEFFICIENT,
C     USING RR FIT FORM OF VERNER & FERLAND, APJS V103 PP467-73 (1996).
C     AT USER SPECIFIED NT LOG SPACED TEMPERATURES BETWEEN TMIN AND TMAX
C     IN UNITS OF THE cfout FILE PRODUCED BY CFITRR (REQUIRED).
C     CAN ALLOW FOR NEW USER COMMENTS AT THE TOP OF THE cfout FILE.
C     NT.GT.0 OUTPUT TO SCREEN, .LT. 0 TO FILE 'gout', .EQ. 0 EXITS.
C
      PARAMETER (NUMT=200)
C
      CHARACTER*1 CARD(70)
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
        IF(CARD(3).EQ.'N')GO TO 2
      ENDDO
   2  NWR=4
      IF(CARD(42).NE.' ')NWR=6
      DO N=0,NUMT,NWR
        READ(8,100,END=10)I0,(A(N+I),I=1,NWR)
        IF(I0.LE.0)GO TO 10
      ENDDO
  10  NFIT=N
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
        CALL FUNCS(TT,A,ARR,DUM,NFIT,NWR,IFLAG)
        ARR=EXP(ARR)
        IF(BOUT)THEN
          WRITE(*,*)'TEMP= ',TT,'  RATE COEF.= ',ARR
        ELSE
          WRITE(9,200)TT,ARR
        ENDIF
      ENDDO
      GO TO 5
C
 100  FORMAT(I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)
 101  FORMAT(70A1)
 200  FORMAT(1P2D10.2)
C
      END
C*********************************************************************
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
