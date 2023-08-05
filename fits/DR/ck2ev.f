C                                                                 13/09/05
      PROGRAM CK2EV
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     CONVERT A cfout FILE FROM K TO EV.
C
      PARAMETER (NUMT=20)
C
      CHARACTER*1 CARD(60)
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
   2  DO I=1,NUMT,2
        READ(8,100,END=50)I0,A(I),A(I+1)
        IF(I0.LE.0)GO TO 10
        A(I)=A(I)*8.E-7
        A(I+1)=A(I+1)*8.6174E-5
        WRITE(9,100)I0,A(I),A(I+1)
      ENDDO
  10  BACKSPACE(8)
      NFIT=I-1
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
      READ(8,*)TK,ADR,ADRF0,DIFF0
C
      TEV=TK*8.6174E-5
C
      CALL FUNCS(TEV,A,ADRF,DUM,NFIT)
C
      ADRF=ADRF/(TEV*SQRT(TEV))
C
      DIFF=0.0
      IF(ADR.GT.0.0)DIFF=ABS(ADR-ADRF)/ADR
      WRITE(9,200)TEV,ADR,ADRF,DIFF
      GO TO 5
C
  20  STOP 'OUTPUT IN cfoutev'
C
  50  STOP 'UNEXPECTED END-OF-FILE cfout'
C
 100  FORMAT(I3,1PE11.3,1X,E11.3)
 101  FORMAT(60A1)
 200  FORMAT(1PE10.2,2E11.2,E15.2)
 201  FORMAT(2X,'TEMP(EV)',3X,'ADR(ORG)',3X,'ADR(FIT)',9X,'DIFF')
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
