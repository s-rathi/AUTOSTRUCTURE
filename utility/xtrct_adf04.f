      PROGRAM XTRCTADF04                                      ! 02/08/14
      IMPLICIT REAL*8(A-H,O-Z)
C
C     EXTRACT AN UPSILON/OMEGA FROM A STANDARD ADAS ADF04 FILE 
C     (TYPE 1, 3, 4 OR 5)
C
C     THE SCALED OUTPUT REQUIRES THE DIPOLE LIMIT TO BE FLAGGED NEGATIVE
C     TO DETERMINE A MEANINGFUL SCALING SINCE THERE IS NO USER OVERRIDE.
C
C     N.B. NO LONGER RESTRICTED TO .LE. 20 TEMPS/ENERGIES.
C
C     JT.LT.0 FLAGS ELASTIC PRESENT (JUST TO AID READING)
C     IT.LT.0 FLAGS FULL EXPONENT PRESENT= X.XXD-XX 
C 
      PARAMETER (NMTMP=1001)
      PARAMETER (ICARD=200)
C
      PARAMETER (TEN=10)
      PARAMETER (D0P8=0.8D0)
      PARAMETER (CM2RYD=109737.43D0)
      PARAMETER (TK2RYD=.15789D6)
C
      REAL*8 MANT(NMTMP+1)
C
      LOGICAL BEXP
C
      CHARACTER(LEN=19) C300
      CHARACTER(LEN=35) F300
      CHARACTER(LEN=43) F200
      CHARACTER(LEN=50) F101
      CHARACTER(LEN=1) CARD(ICARD)
      CHARACTER(LEN=8) CANT(21),CBLNK8
      CHARACTER(LEN=50) CFGLAB
C
      DIMENSION IEXP(NMTMP+1),TEMP(NMTMP)
C
      DATA CBLNK8/'        '/
C
      OPEN(1,FILE='adf04',STATUS='OLD')
      OPEN(7,FILE='x4out',STATUS='UNKNOWN')
C
      WRITE(*,*)'ENTER TRANSITION REQUIRED'
      READ(*,*)JT,IT
C
      IF(JT.LT.0)THEN
        JT=-JT
        IONE=0
      ELSE
        IONE=1
      ENDIF
C
      BEXP=IT.LT.0
      IT=IABS(IT)
C
      READ(1,*)                                             !SKIP HEADER
      READ(1,'(200A1)',err=10)(CARD(I),I=1,ICARD)
C
  10  REWIND(1)
      READ(1,*)                                             !RE-POSITION
C
      CALL SCANE4(CARD,ICARD,F101)                          !FIND FORMAT
C
      DO I=1,10000
C
        READ(1,F101,ERR=20)LEV,CFGLAB,IS,IL,TJ,E
c        write(*,F101)LEV,CFGLAB,IS,IL,TJ,E
C
        IF(LEV.EQ.-1)GO TO 20                             !END OF LEVELS
        NLEV=LEV
        ELEV=E
        IF(I.EQ.IT)EI=E
        IF(I.EQ.JT)EJ=E
C
      ENDDO
C
  20  DE=(EJ-EI)/CM2RYD
      DE=ABS(DE)
      WRITE(7,100)JT,IT,DE
C
      READ(1,*)DUM,ITYPE
      BACKSPACE(1)
C
      IF(ITYPE.NE.1.AND.ITYPE.NE.3.AND.ITYPE.NE.4.AND.ITYPE.NE.5)THEN
        ITYPE=0
        WRITE(0,*)'ILLEGAL VALUE FOR ITYPE=',ITYPE
        STOP
      ENDIF
C
      IF(IT.GT.NLEV.OR.JT.GT.NLEV)THEN
        WRITE(*,*)' ERROR: YOU HAVE REQUESTED TRANSIION',JT,' -',IT
     X           ,' BUT THERE ARE ONLY',NLEV,' STATES ON FILE...'
        STOP
      ENDIF
C
      IF(ITYPE.NE.4.AND.JT.LT.IT)THEN
        I=IT
        IT=JT
        JT=I
      ENDIF
C
      IF(ITYPE.GT.0)THEN                     !ALLOW FOR MULTIPLE FORMATS
C
        NTMP=0
        READ(1,*)
  22    NTMP=NTMP+20
        READ(1,'(I5)')NTEST
        IF(NTEST.EQ.0)GO TO 22
        IF(NTMP.GT.NMTMP)STOP 'INCREASE NMTMP'
        BACKSPACE(1)
        READ(1,'(I4)')ITEST
        BACKSPACE(1)
        NTLINE=NTMP/20
        DO N=1,NTLINE
          BACKSPACE(1)
        ENDDO
C
      ELSE        !FIXED LENGTH .LE. 20 AND E/D FORMAT FOR UNKNOWN ITYPE
C
        ITEST=1
        BEXP=.TRUE.
        NTLINE=1
        NTMP=20
C
      ENDIF
C
      IF(ITEST.NE.0)THEN                                   !NLEV.LT.1000
        IF(NTMP.LE.20)THEN
          IF(BEXP)THEN
            F200='(F5.2,I5,8X,20(1PE10.2))'
            F300='(2I4,22(1PE10.2))'
          ELSE
            F200='(F5.2,I5,6X,20(F5.2,I3))'
            F300='(2I4,22(F5.2,I3))'
          ENDIF
        ELSE
          IF(BEXP)THEN
            F200='(F5.2,I5,8X,20(1PE10.2)/(18X,20(1PE10.2)))'
            F300='(2I4,21(1PE10.2)/(18X,20(1PE10.2)))'  !WRAP LAST POINT
          ELSE
            F200='(F5.2,I5,6X,20(F5.2,I3)/(16X,20(F5.2,I3)))'
            F300='(2I4,21(F5.2,I3)/(16X,20(F5.2,I3)))'  !WRAP LAST POINT
          ENDIF
        ENDIF
        IF(BEXP)THEN
          C300='(2I4,10X,21(2X,A8))'
        ELSE
          C300='(2I4, 8X,21(   A8))'
        ENDIF
      ELSE
        IF(NTMP.LE.20)THEN
          IF(BEXP)THEN
            F200='(F5.2,I5,10X,20(1PE10.2))'
            F300='(2I5,22(1PE10.2))'
          ELSE
            F200='(F5.2,I5, 8X,20(F5.2,I3))'
            F300='(2I5,22(F5.2,I3))'
          ENDIF
        ELSE
          IF(BEXP)THEN
            F200='(F5.2,I5,10X,20(1PE10.2)/(20X,20(1PE10.2)))'
            F300='(2I5,21(1PE10.2)/(20X,20(1PE10.2)))'  !WRAP LAST POINT
          ELSE
            F200='(F5.2,I5, 8X,20(F5.2,I3)/(18X,20(F5.2,I3)))'
            F300='(2I5,21(F5.2,I3)/(18X,20(F5.2,I3)))'  !WRAP LAST POINT
          ENDIF
        ENDIF
        IF(BEXP)THEN
          C300='(2I5,10X,21(2X,A8))'
        ELSE
          C300='(2I5, 8X,21(   A8))'
        ENDIF
      ENDIF
C
      IF(BEXP)THEN
        READ(1,F200,err=25)DUM,IDUM,(MANT(I),I=1,NTMP)
c        write(*,F200)DUM,IDUM,(MANT(I),IEXP(I),I=1,NTMP)
      ELSE
        READ(1,F200,err=25)DUM,IDUM,(MANT(I),IEXP(I),I=1,NTMP)
c        write(*,F200)DUM,IDUM,(MANT(I),IEXP(I),I=1,NTMP)
      ENDIF
C
  25  CONTINUE
C
C RE-CONSTRUCT ENERGY/TEMP
C
      DO I=1,NTMP
C
        TEMP(I)=MANT(I)
        IF(.NOT.BEXP)TEMP(I)=TEMP(I)*TEN**IEXP(I)
C
        IF(MANT(I).EQ.0.AND.I.GT.1)THEN                  !FOR DW ITYPE=5
          NTEMP=I-1
          GO TO 30
        ENDIF
C
      ENDDO
C
      NTEMP=NTMP
C
  30  CONTINUE
C
C ESTIMATE MAX NUMBER OF TRANSITIONS TO BE READ/SEARCHED
C
      IF(ITYPE.EQ.4)THEN
        N=NLEV*NLEV
      ELSE
        N=(NLEV*(NLEV+1-2*IONE))/2
      ENDIF
C
C CHECK FOR REQUESTED TRANSTION
C
  35  N=N*NTLINE
C
      DO NN=1,N
C
        READ(1,F300,END=999)J,I
C
        IF(I.EQ.IT.AND.J.EQ.JT)THEN                          !WE HAVE IT
          BACKSPACE(1)
          IF(ITYPE.EQ.0)GO TO 70
          GO TO 40
        ENDIF
C
        IF(J.LT.0)GO TO 36                            !NOT FOUND ON FILE
C
      ENDDO
C
  36  READ(1,F300,END=999)J,I
      IF(I.LT.0)THEN
        WRITE(*,*)'REQUESTED TRANSITION NOT FOUND ON FILE...'
        STOP
      ELSEIF(ITYPE.NE.4.AND.IONE.EQ.1)THEN
        BACKSPACE(1)
        N=(NLEV*(NLEV+1))/2               !NOT FOUND, MAYBE NON-STANDARD
        GO TO 35
      ELSE                                           !GOT LOST SOMEWHERE
        WRITE(*,*)'PROBLEM READING FILE, CANNOT FIND TRANSITION...'
        STOP
      ENDIF
C
C WE HAVE THE REQUIRED TRANSITION
C
  40  I0=I
      J0=J
C
C NOW FIND LENGTH (CASE VARIABLE NUMBER E/T)
C
      NTEMP=0
      NT=1
      NRD=21
C
  45  READ(1,C300)JJ,II,(CANT(I),I=1,NRD)
C
      IF(JJ*JJ.NE.JJ*J0.OR.II*II.NE.II*I0)THEN  !HAVE MOVED TO NEXT TRAN
        I=2                              !INF WAS IN FINAL POSITION READ
        IF(CANT(21).EQ.CBLNK8)I=1                     !BUT NOT IN POS 21
        GO TO 50
      ENDIF
C
C FIRST BLANK TERMINATES
C
      NRD=20
      DO I=1,NRD
        IF(CANT(I).EQ.CBLNK8)GO TO 50
      ENDDO
      NTEMP=NTEMP+NRD
      NT=NT+1
      GO TO 45
C
  50  NTEMP=NTEMP+I-2                             !& EXC. INFINITE POINT
      DO N=1,NT
        BACKSPACE(1)
      ENDDO
C
C WE HAVE THE CORRECT LENGTH
C
  70  NTEMP=NTEMP+1                        !ALLOW FOR INFINITE E/T POINT
      TEMP(NTEMP)=1.D99
C
      IF(BEXP)THEN
        READ(1,F300)JJ,II,DUM,(MANT(I),I=1,NTEMP)
      ELSE
        READ(1,F300)JJ,II,DUM,IDUM,(MANT(I),IEXP(I),I=1,NTEMP)
      ENDIF
C
      IF(II.NE.IT.OR.JJ.NE.JT)then           !SHOULDN'T HAPPEN...
        write(*,*)ii,jj,i0,j0
        STOP 'OOPS!'
      endif
C
C WRITE-OUT ORIGINAL AND SCALED ENERGY/TEMP OMEGA/UPSILON (TYPE 1,3,5)
C
      IF(ITYPE.EQ.0)THEN
C
        DO I=1,NTEMP
          OMUPS=MANT(I)
          IF(.NOT.BEXP)OMUPS=OMUPS*TEN**IEXP(I)
          WRITE(7,400)LOG10(TEMP(I)),TEMP(I),TEMP(I)/TK2RYD,OMUPS
        ENDDO
C
      ELSE
C                                                       !FORBIDDEN POWER
        IE=2                                          !USER COULD MODIFY
        IF(ITYPE.EQ.3.OR.ITYPE.EQ.4)IE=1               !AS UPS NOT OMEGA
C
        IF(DE.EQ.0)DE=1.D-4
C
        IF(ITYPE.EQ.1)THEN
          X2=TEMP(2)-1
        ELSEIF(ITYPE.EQ.3.OR.ITYPE.EQ.4)THEN
          X2=TEMP(2)/(DE*TK2RYD)
        ELSEIF(ITYPE.EQ.5)THEN
          X2=e2/(3*DE*CM2RYD)                           !ELEV->e2
        ENDIF
        IF(MANT(NTEMP).LT.0)THEN
          C0=3
          ic=1
  90      C=(X2+C0)**D0P8
c          WRITE(*,*)C0,C
          c0=c
          ic=ic+1
          if(ic.lt.5)go to 90
        ELSE
          C=6*X2
        ENDIF
C
        DO I=1,NTEMP
C
          OMUPS=MANT(I)
          IF(.NOT.BEXP)OMUPS=OMUPS*TEN**IEXP(I)
C
          IF(IT.EQ.JT)THEN                                !ELASTIC FIX
            XOMUPS=OMUPS
            XX=0
          ELSE
C
            IF(TEMP(I).GT.1.D20)THEN                           !INFINITE
              XX=1
              XOMUPS=ABS(OMUPS)
            ELSE
C
              IF(ITYPE.EQ.1)THEN
                X=TEMP(I)-1
              ELSEIF(ITYPE.EQ.3.OR.ITYPE.EQ.4)THEN
                X=TEMP(I)/(DE*TK2RYD)
              ELSEIF(ITYPE.EQ.5)THEN
                X=TEMP(I)/DE
              ENDIF
C
              IF(MANT(NTEMP).LT.0)THEN                          !DIPOLE
                XX=LOG(X+EXP(1.D0))
                XOMUPS=OMUPS/XX
                XX=LOG(X+C)
                XX=(XX-LOG(C))/XX
              ELSE
                XX=X/(X+C)
                IF(MANT(NTEMP).GT.0)THEN                        !ALLOWED
                  XOMUPS=OMUPS
                ELSE                                          !FORBIDDEN
                  XOMUPS=OMUPS*(X+1)**IE
                ENDIF
              ENDIF
C
            ENDIF
C
          ENDIF
C
          WRITE(7,401)TEMP(I),OMUPS,XX,XOMUPS
C
        ENDDO
C
      ENDIF
C
      IF(ITYPE.EQ.1)STOP 'OMEGA IN x4out'
      IF(ITYPE.EQ.3)STOP 'UPSILON IN x4out'
      IF(ITYPE.EQ.4)THEN
        IF(JT.GT.IT)STOP 'DOWNSILON IN x4out'
        IF(JT.LT.IT)STOP 'UPSILON IN x4out'
      ENDIF
      IF(ITYPE.EQ.5)STOP 'OMEGA IN x4out'
      STOP 'OMEGA/UPSILON IN x4out'
C
 999  WRITE(*,*)'TRANSITION',JT,IT,' NOT FOUND!!!'
C
C
 100  FORMAT('#',I3,' -',I4,' TRANSITION, ENERGY=',F12.6,' RYD')
COLD 101  FORMAT(I5,32X,F16.0)
COLD 201  FORMAT(17X,21(E9.2))
COLD 301  FORMAT(2I4,9X,21(E9.2))
 400  FORMAT(F5.2,1PE10.2,2E10.2)
 401  FORMAT(1P,2E10.2,5X,2E10.2)
C
      END
C
C***********************************************************************
C
      SUBROUTINE SCANE4(CARD,ICARD,FORM)
C
C SCAN AN ENERGY LEVEL OF ADF04 TO FIND WHERE THE FIELDS START/END
C THEN SET FORMAT
C
      CHARACTER(LEN=50) FORM
      CHARACTER(LEN=1) CARD,BLNK,LRB,RRB,LSB,RSB,LBR,RBR,DP,LIT
C
      DIMENSION CARD(ICARD),LIT(0:9)
C
      DATA BLNK,LRB,RRB,LSB,RSB,LBR,RBR,DP
     X     /' ','(',')','[',']','{','}','.'/
      DATA LIT/'0','1','2','3','4','5','6','7','8','9'/
C
C WE SEEK TO DEFINE (OMIT CHECK BOX):
C
      FORM='(I? ,A?? ,1X,I? ,1X,I? ,1X,F??.? ,1X,F??.? )'
C
C FIRST, FIND WHERE ENERGY INDEX STARTS/ENDS
C
      IS=1
      IE0=0
      DO I=1,ICARD
        IF(CARD(I).NE.BLNK)THEN
          IF(IE0.EQ.0)IE0=I
        ELSE
          IF(IE0.NE.0)THEN
            IE=I-1
            GO TO 10
          ENDIF
        ENDIF
      ENDDO
      WRITE(*,*)'CANNOT FIND ENERGY INDEX IN CARD:'
      WRITE(*,*)(CARD(I),I=1,ICARD)
      STOP
C
  10  I5=IE-IS+1
c      write(*,*)is,ie,i5
      WRITE(FORM(3:4),'(I2)')I5
C
C SECOND, FIND CONFIG FIELD
C
      IS=IE+1
      DO I=IS,ICARD
        IF(CARD(I).EQ.LRB)THEN               !START OF STAT WEIGHT FIELD
          IE=I-1
          GO TO 20
        ENDIF
      ENDDO
      WRITE(*,*)'CANNOT FIND CONFIG FIELD IN CARD:'
      WRITE(*,*)(CARD(I),I=1,ICARD)
      STOP
C
  20  IA20=IE-IS+1
c      write(*,*)is,ie,IA20
      WRITE(FORM(7:9),'(I3)')IA20
C
C THIRD, STAT WEIGHT FIELD
C
      IS=IE+1
      IS=IS+1                                              !SKIP BRACKET
      DO I=IS,ICARD
        IF(CARD(I).EQ.RRB)THEN                    !END OF S WEIGHT FIELD
          IE=I-1
          GO TO 30
        ENDIF
      ENDDO
C
  30  IWS=IE-IS+1
c      write(*,*)is,ie,IWS
      WRITE(FORM(15:16),'(I2)')IWS
C
      IS=IE+1
      IS=IS+1                                              !SKIP BRACKET
      DO I=IS,ICARD
        IF(CARD(I).EQ.LRB)THEN                    !END OF L WEIGHT FIELD
          IE=I-1
          GO TO 40
        ENDIF
      ENDDO
C
  40  IWL=IE-IS+1
c      write(*,*)is,ie,IWL
      WRITE(FORM(22:23),'(I2)')IWL
C
      IEE=0
      IS=IE+1
      IS=IS+1                                              !SKIP BRACKET
      DO I=IS,ICARD
        IF(CARD(I).EQ.DP)THEN                 !WE HAVE THE DECIMAL POINT
          IEE=I-1
          GO TO 45
        ENDIF
      ENDDO
      WRITE(*,*)'CANNOT FIND J WEIGHT DECIMAL POINT IN CARD:'
      WRITE(*,*)(CARD(I),I=1,ICARD)
      STOP

  45  ISS=IEE+1
      ISS=ISS+1                                      !SKIP DECIMAL POINT
      DO I=IS,ICARD
        IF(CARD(I).EQ.RRB)THEN                    !END OF J WEIGHT FIELD
          IE=I-1
          GO TO 50
        ENDIF
      ENDDO
C
  50  IWJ=IE-IS+1
c      write(*,*)is,ie,IWJ
      WRITE(FORM(29:30),'(I2)')IWJ
      IWJ=IE-ISS+1
c      write(*,*)iss,ie,IWJ
      WRITE(FORM(32:33),'(I2)')IWJ
C
C FOURTH, FIND ENERGY FIELD
C
      IEE=0
      IS=IE+1
      IS=IS+1                                              !SKIP BRACKET
      DO I=IS,ICARD
        IF(CARD(I).EQ.DP)THEN                 !WE HAVE THE DECIMAL POINT
          IEE=I-1
          GO TO 55
        ENDIF
      ENDDO
      WRITE(*,*)'CANNOT FIND ENERGY DECIMAL POINT IN CARD:'
      WRITE(*,*)(CARD(I),I=1,ICARD)
      STOP
C
  55  ISS=IEE+1
      ISS=ISS+1                                      !SKIP DECIMAL POINT
      DO I=ISS,ICARD
        DO J=0,9                                          !SEE IF NUMBER
          IF(CARD(I).EQ.LIT(J))GO TO 57                        !CONTINUE
        ENDDO
C        IF(CARD(I).EQ.BLNK.OR.CARD(I).EQ.LBR)THEN   !IF NO GAP TO X-BOX
C COULD FLAG SAFE TO CONTINUE, I.E., NOT END OF LINE.
C        ENDIF
        IE=I-1
        GO TO 60
  57    CONTINUE
      ENDDO
      WRITE(*,*)'CANNOT FIND END OF CARD - ICARD=',ICARD,' TOO SHORT?'
      WRITE(*,*)(CARD(I),I=1,ICARD)
      STOP
C
  60  IEN=IE-IS+1
c      write(*,*)is,ie,IEN
      WRITE(FORM(39:40),'(I2)')IEN
      IEN=IE-ISS+1
c      write(*,*)iss,ie,IEN
      WRITE(FORM(42:43),'(I2)')IEN
C
CC FINALLY, LOOK FOR ANY CHECK BOX (MAY NOT EXIST)
CC
C      IS=IE+1
C      DO I=IS,ICARD
C        IF(CARD(I).EQ.LBR)THEN                          !OPEN CHECK BOX
C          IX=I+1
C          GO TO 70
C        ENDIF
C      ENDDO
CC
C  70  CONTINUE
C
C WE HAVE FORMAT
C
c      write(*,*)FORM
C
      RETURN
C
      END
