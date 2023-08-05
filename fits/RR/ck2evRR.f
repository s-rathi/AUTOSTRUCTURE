C                                                                 26/09/05
      PROGRAM CK2EVRR
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     CONVERT A cfout FILE FROM K TO EV.
C
      PARAMETER (NUMT=20)
C
      CHARACTER*1 CARD(70)
C
      DIMENSION DUM(NUMT),A(NUMT)
C
      OPEN(8,FILE='cfout')
      OPEN(9,FILE='cfoutev')
C
   1  DO N=1,100
        READ(8,101)CARD
        WRITE(9,101)CARD
        IF(CARD(3).EQ.'N')GO TO 2
      ENDDO
   2  NWR=4
      IF(CARD(42).NE.' ')NWR=6
      DO N=0,NUMT,NWR
        READ(8,100,END=50)I0,(A(N+I),I=1,NWR)
        IF(I0.LE.0)GO TO 10
        A(N+3)=A(N+3)*8.6174E-5
        A(N+4)=A(N+4)*8.6174E-5
        IF(NWR.EQ.6)A(N+6)=A(N+6)*8.6174E-5
        WRITE(9,100)I0,(A(N+I),I=1,NWR)
      ENDDO
  10  BACKSPACE(8)
      NFIT=N
C
      DO N=1,2
        READ(8,101)CARD
        WRITE(9,101)CARD
        IF(CARD(1).EQ.'Z')GO TO 1
      ENDDO
      READ(8,101)CARD
      WRITE(9,201)
C
   5  READ(8,101,END=20)CARD
      IF(CARD(1).EQ.'Z'.OR.CARD(4).EQ.' ')THEN     !CHECK FOR NEW HEADER
        WRITE(9,101)CARD
        GO TO 1
      ELSE
        BACKSPACE(8)
      ENDIF        
      READ(8,*)TK,ARR,ARRF0,DIFF0
C
      TEV=TK*8.6174E-5
C
      CALL FUNCS(TEV,A,ARRF,DUM,NFIT,NWR,IFLAG)
C
      ARRF=EXP(ARRF)
C
      DIFF=0.0
      IF(ARR.GT.0.0)DIFF=ABS(ARR-ARRF)/ARR
      WRITE(9,200)TEV,ARR,ARRF,DIFF
      GO TO 5
C
  20  STOP 'OUTPUT IN cfoutev'
C
  50  STOP 'UNEXPECTED END-OF-FILE cfout'
C
 100  FORMAT(I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)
 101  FORMAT(70A1)
 200  FORMAT(1PE10.2,2E11.2,E15.2)
 201  FORMAT(2X,'TEMP(EV)',3X,'ARR(ORG)',3X,'ARR(FIT)',9X,'DIFF')
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
