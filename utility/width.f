C     PROGRAM WIDTH                                             14/06/23
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  DETERMINE TOTAL AUTOIONIZATION & RADIATIVE WIDTHS 
C              AND AUGER & FLUORESCENCE YIELDS, OR RADIATIVE LIFETIMES
C
      PARAMETER (NDIM4=5000)
      PARAMETER (NDIM7=10000000)
      PARAMETER (NDIM12=250000)
      PARAMETER (NDIM13=20000)
      PARAMETER (NDIM14=999)
      PARAMETER (HBAR=4.837769E-17)     !RYD.SEC
      PARAMETER (DZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
C
      INTEGER SS,QS0,QL0,QL,QN
      LOGICAL BFORM,BCA,BLS,BIC,BFIRST
C
      CHARACTER CMBLNK*4
C
      DIMENSION ICA(NDIM12),JCA(NDIM12),ITA(NDIM12),AA(NDIM12)
     X,         EION(NDIM12),JTA(NDIM12)
     X,         JTR(NDIM7),ICR(NDIM7),JCR(NDIM7),IWA(NDIM12),EC(NDIM12)
     X,         IWR(NDIM7),ITR(NDIM7),AR(NDIM7)
     X,         IK(NDIM13),SS(NDIM13),LL(NDIM13),JJ(NDIM13) !,JK(NDIM13)
     X,         LCF(NDIM13)
     X,         ENERG(NDIM13),NG(NDIM14)
     X,         TLIFE(NDIM13)
      DIMENSION AYLD(NDIM4),AWIDTH(NDIM4),RWIDTH(NDIM4),EII(NDIM4)
      DIMENSION KYLD(NDIM4),JYLD(NDIM4),IYLD(NDIM13),LSPJ(NDIM4)
      DIMENSION QS0(10),QL0(10),QN(30),QL(30)
C
      DATA CMBLNK/'    '/
C
C FIX FOR FORTRAN 90 COMPILERS THAT DON'T ALLOW ASSIGNMENT OF CHARACTERS
C TO INTEGER VARIABLES, REQUIRED FOR HISTORIC BACKWARDS COMPATIBILITY
C
      OPEN(90,STATUS='SCRATCH',FORM='FORMATTED')
      WRITE(90,1111)CMBLNK
 1111 FORMAT(A4)
      BACKSPACE(90)
      READ(90,1111)MBLNK
      CLOSE(90)
C
      WRITE(*,*)
      MXDIM4=0
      MXDIM7=0
      MXDIM12=0
      MXDIM13=0
      MXDIM14=0
C
      JCFY=99999       !MAY USE IN FUTURE TO ELIMINATE CF'S
      TOLD=DZERO       !RESOLVE DEGENERATE AUTOIONIZING STATES
      BFIRST=.TRUE.
C
C POSSIBLE UNIT NOS TO BE CHECKED FOR DATA
C
      MR=40
      MRU=41
      INQUIRE (FILE='o1',EXIST=BFORM)
      IF(BFORM)THEN
        OPEN(MR,file='o1')
      ELSE
        INQUIRE (FILE='o1u',EXIST=BFORM)
        IF(BFORM)THEN
          OPEN(MRU,file='o1u',FORM='UNFORMATTED')
          BFORM=.FALSE.
        ELSE
          STOP'***ERROR: NO INPUT RATE FILE FOUND!!!'
        ENDIF
      ENDIF
C
C READ RYDBERG DEFINITION
C
 310  CONTINUE                             !RE-ENTRY POINT FOR NEW NV LV
C
      NV=1
      IF(BFORM)READ(MR,380,END=1000,ERR=1000)NV,LV
      IF(.NOT.BFORM)READ(MRU,END=1000,ERR=1000)NV,LV
C
      IF(BFORM.AND.NV.EQ.0)GO TO 310                 !GO TO 1000
      IF(.NOT.BFORM.AND.NV.EQ.0)GO TO 310
C
      LV0=LV
      NV0=NV
C
C READ HEADER, AND MAYBE ORBITAL CODE
C
      IF(BFORM)READ(MR,101) NCF,NZ0,NE,(QN(I),QL(I),I=1,30)
      IF(.NOT.BFORM)READ(MRU,ERR=300) NCF,NZ0,NE,(QN(I),QL(I),I=1,30)
 101  FORMAT(I3,12X,I2,6X,I2,4X,30(I3,I2))
      GO TO 302
 300  IF(BFIRST)THEN
        REWIND(MRU)
        READ(MRU)
        READ(MRU)
      ELSE
c        BACKSPACE(MRU)  !COMPILER DEPENDENT, SHOULD SET MXORB ON BFIRST
      ENDIF
C
 302  IF(NCF.GT.NDIM14)THEN
        WRITE(*,136)NCF
 136    FORMAT(' DIMENSION EXCEEDED IN AWIDTH, INCREASE NDIM14 TO ',I5)
        STOP
      ENDIF
      MXDIM14=MAX(NCF,MXDIM14)
C
C READ CONFIGURATION DATA
C
      DO 102 I=1,NCF
        IF(BFORM)READ(MR,179)II,NGR,MA0,MB0,(QS0(J),QL0(J),J=1,10)
        IF(.NOT.BFORM)READ(MRU)II,NGR,MA0,MB0,(QS0(J),QL0(J),J=1,10)
 179    FORMAT(2I5,2X,I3,I2,1X,10(I2,A1))
        IN=IABS(II)
        NG(IN)=NGR
 102  CONTINUE
C
      IF(BFORM)READ(MR,103)
      IF(.NOT.BFORM)READ(MRU)NZTEST,NDUME
      IF(BFORM)READ(MR,103)
 103  FORMAT(A1)
      I=0
 111  I=I+1
C
C READ AUTOIONIZATION RATES AND ENERGIES
C
      IF(BFORM)READ(MR,112)ICA(I),ITA(I),IWA(I),JCA(I),JTA(I),AA(I),EC(I
     X),EION(I)
      IF(.NOT.BFORM)READ(MRU)ICA(I),ITA(I),IWA(I),JCA(I),JTA(I),AA(I),EC
     X(I),EION(I)
 112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))
      I=I-1
      IF(ICA(I+1).GT.JCFY)GO TO 111
      I=I+1
      AA(I)=ABS(AA(I))
      IF(ITA(I).NE.0) GO TO 111
      EFMIN=EION(I)
      NUMA=I-1
      IF(NUMA.GE.NDIM12)STOP 'INCREASE NDIM12'
      MXDIM12=MAX(NUMA,MXDIM12)
C
      IF(BFORM)READ(MR,121) NENG,ECORE
      IF(.NOT.BFORM)READ(MRU) NENG,ECORE
 121  FORMAT(10X,I5,45X,F15.6)
      IF(BFORM)READ(MR,105)MTEST
      IF(.NOT.BFORM)READ(MRU)MTEST
 105  FORMAT(26X,A4)
      IF(NENG.GT.NDIM13)THEN
      WRITE(*,369)NENG
 369  FORMAT(' NUMBER OF LEVELS EXCEEDS STORAGE,INCREASE NDIM13 TO',I6)
      STOP 'INCREASE NDIM13'
      ENDIF
      MXDIM13=MAX(NENG,MXDIM13)
C
      BCA=NENG.EQ.NCF
      IF(BCA)THEN
        BLS=.FALSE.
        BIC=.FALSE.
      ELSE
        BIC=MTEST.NE.MBLNK
        BLS=.NOT.BIC
      ENDIF
C
C READ ENERGIES
C
      WND0=-DONE
      NYLD=0
      JB=0
C
      DO 122 I=1,NENG
      IF(BFORM)READ(MR,123)IK(I),ITTTT,SS(I),LL(I),JJ(I),LCF(I),ENERG(I)
      IF(.NOT.BFORM)READ(MRU)IK(I),ITTTT,SS(I),LL(I),JJ(I),LCF(I),ENERG(
     XI)
 123  FORMAT(5X,6I5,F15.6)
      M=IK(I)
      M=IABS(M)
CT      JK(M)=I
      IYLD(M)=0
      IF(LCF(I).GT.0)JB=JB+1                   !N+1 STATE
C
C SET-UP AUGER BINS AND INDEXING
C
      IF(LCF(I).LT.0)GO TO 122                 !CONTINUUM
      IF((ECORE+ENERG(I)).LT.EFMIN)GO TO 122   !TRUE BOUND
      IF(LCF(I).GT.JCFY)GO TO 122              !DROP
      IF(IK(I).LT.0)GO TO 122                  !CORRELATION
C
C                N.B.  SET TOLD=0.0 TO RESOLVE DEGENERATE STATES
      IF(ENERG(I).GE.(WND0+TOLD))THEN
        NYLD=NYLD+1
        IF(NYLD.GT.NDIM4)THEN
          WRITE(*,777)
 777      FORMAT(' SR.AYIELD: INCREASE NDIM4')
          STOP
        ENDIF
      ELSE
        T=0.9*(ENERG(I)-WND0)
        WRITE(*,124)NYLD,IK(I-1),WND0,IK(I),ENERG(I),T
 124    FORMAT(/' ***WARNING, ENERGY DEGENERACY,',
     X  ' POSSIBLE MIS-MATCH WITH R-MATRIX STATES'/I5,2(I5,F15.6)/
     X  ' TRY DECREASING TOLD TO ',F15.8,' RY')
        STOP
      ENDIF
C
      IYLD(M)=NYLD
      JYLD(NYLD)=JB
      KYLD(NYLD)=I
C
      IF(BCA)THEN
        LSPJ(NYLD)=0
        IF(BFORM)SS(I)=SS(I)+100000*ITTTT
      ELSEIF(BLS)THEN
        LSPJ(NYLD)=100*LL(I)+10000*IABS(SS(I))
      ELSEIF(BIC)THEN
        LSPJ(NYLD)=100*IABS(JJ(I))
      ENDIF
      IF(SS(I).LT.0)LSPJ(NYLD)=LSPJ(NYLD)+1
C
      WND0=ENERG(I)
      EII(NYLD)=ECORE+ENERG(I)
C
 122  CONTINUE
C
      MXDIM4=MAX(NYLD,MXDIM4)
C
      IF(BFORM)READ(MR,104)NZTEST
      IF(.NOT.BFORM)READ(MRU)NZTEST,NDUME
 104  FORMAT(66X,I2)
      IF(NZTEST.LT.1)THEN
        NUMR=0
      ELSE
        IF(BFORM)READ(MR,103)
C
C READ RADIATIVE RATES AND ENERGIES
C
        I=0
C
 131    I=I+1
        IF(BFORM)READ(MR,132) ICR(I),ITR(I),IWR(I),JCR(I),JTR(I),AR(I),
     X  DEL0,EATOM0
        IF(.NOT.BFORM)READ(MRU) ICR(I),ITR(I),IWR(I),JCR(I),JTR(I),JWR,
     X  AR(I),DEL0,EATOM0
 132    FORMAT(5I5,5X,1PE15.5,2(0PF15.6))
C
        AR(I)=ABS(AR(I))
        IF(ITR(I).NE.0) GO TO 131
        NUMR=I-1
        IF(NUMR.GE.NDIM7) STOP 'INCREASE NDIM7'
        MXDIM7=MAX(NUMR,MXDIM7)
      ENDIF
C
C FORM LIFETIMES
C
      IF(NYLD.EQ.0)THEN
        WRITE(*,*)'***WARNING: NO AUTOIONIZING STATES FOUND!'
        IF(NUMR.EQ.0)STOP '*** NO RADIATIVE DATA FOOUND!'
        IF(BFIRST)OPEN(16,FILE='LIFETIME')
        WRITE(16,151)NV,LV,HBAR
        DO I=1,NENG
          TLIFE(I)=DZERO
        ENDDO
        TLIF=AR(1)
        DO I=2,NUMR
          IF(ITR(I).EQ.ITR(I-1))THEN
           TLIF=TLIF+AR(I)
          ELSE
            M=ITR(I-1)
CT            WRITE(16,152)JK(M),M,1.0/TLIF       !PER MULTIPOLE
            TLIFE(M)=TLIFE(M)+TLIF              !SUM MULTIPOLE
            TLIF=AR(I)
          ENDIF
        ENDDO
        M=ITR(NUMR)
CT        WRITE(16,152)JK(M),M,1.0/TLIF
        TLIFE(M)=TLIFE(M)+TLIF
        DO I=1,NENG
          M=IK(I)
          TLIF=TLIFE(M)
          IF(TLIF.NE.DZERO)THEN
            TLIF=DONE/TLIF
            WRITE(16,152)I,M,TLIF           !SUM OF MULTIPOLES
          ENDIF
        ENDDO
        IF(BFIRST)WRITE(*,*) 'LIFETIME WRITTEN'
        GO TO 800
      ENDIF
C
C FORM WIDTHS
C
      DO I=1,NYLD
        AWIDTH(I)=DZERO
      ENDDO
C
      IF(NUMA.GT.0)THEN
        IF(BFIRST)OPEN(15,FILE='AWIDTH')
        WRITE(15,151)NV,LV,DONE/HBAR
        DO I=1,NUMA
          MS=IYLD(ITA(I))
          IF(MS.GT.0)THEN
            AWIDTH(MS)=AWIDTH(MS)+AA(I)
          ENDIF
        ENDDO
        DO I=1,NYLD
          IF(AWIDTH(I).GT.DZERO)WRITE(15,150)JYLD(I),AWIDTH(I)
        ENDDO
        IF(BFIRST)WRITE(*,*) 'AWIDTH WRITTEN'
      ENDIF
C
      DO I=1,NYLD
        RWIDTH(I)=DZERO
      ENDDO
C
      IF(NUMR.GT.0)THEN
        IF(BFIRST)OPEN(16,FILE='RWIDTH')
        WRITE(16,151)NV,LV,DONE/HBAR
        DO I=1,NUMR
          MS=IYLD(ITR(I))
          IF(MS.GT.0)THEN
            RWIDTH(MS)=RWIDTH(MS)+AR(I)
          ENDIF
        ENDDO
        DO I=1,NYLD
          IF(RWIDTH(I).GT.DZERO)WRITE(16,150)JYLD(I),RWIDTH(I)
        ENDDO
        IF(BFIRST)WRITE(*,*) 'RWIDTH WRITTEN'
      ENDIF
C
      IF(NUMR*NUMA.LE.0)GO TO 800
C
C FORM AUGER YIELDS
C
      IF(BFIRST)THEN
        OPEN(17,FILE='YIELDS')
        WRITE(17,933)
      ENDIF
      WRITE(17,151)NV,LV
C
      DO I=1,NYLD
        AYLD(I)=DZERO
      ENDDO
C
      DO I=1,NYLD
        IF(AWIDTH(I).EQ.DZERO)THEN
          IF(RWIDTH(I).EQ.DZERO.AND.NZ0-NE.LT.35)AYLD(I)=DONE
        ELSE
          AYLD(I)=AWIDTH(I)/(AWIDTH(I)+RWIDTH(I))
        ENDIF
      ENDDO
C
C WRITE YIELDS
C
      DO I=1,NYLD
        M=KYLD(I)
        WRITE(17,900)I,KYLD(I),IK(M),JYLD(I),LSPJ(I)
     X ,EII(I)-EFMIN,AWIDTH(I),RWIDTH(I),AYLD(I),DONE-AYLD(I)
      ENDDO
      EFMIN=EFMIN-ECORE
      WRITE(17,934)EFMIN
C
      IF(BFIRST)WRITE(*,*) 'YIELDS WRITTEN'
  800 BFIRST=.FALSE.
C
      GO TO 310                                   !GO READ NEW NV LV
C
 1000 CONTINUE
C
C WRITE SOME INFO ON ACTUAL DIMENSION USAGE
C
      WRITE(*,*)
      WRITE(*,"(' DIMENSION',9X,'SET',6X,'USED')")
      WRITE(*,"(' NDIM4 ',5X,2I10)")NDIM4,MXDIM4
      WRITE(*,"(' NDIM7 ',5X,2I10)")NDIM7,MXDIM7
      WRITE(*,"(' NDIM12',5X,2I10)")NDIM12,MXDIM12
      WRITE(*,"(' NDIM13',5X,2I10)")NDIM13,MXDIM13
      WRITE(*,"(' NDIM14',5X,2I10)")NDIM14,MXDIM14
      WRITE(*,*)
C
      STOP 'NORMAL END'
C
 150  FORMAT(I5,1PE15.4)
 151  FORMAT(2X,'NV=',I5,2X,'LV=',I5,1PE15.7)
 152  FORMAT(2I5,1PE15.4)
 380  FORMAT(5X,I5,5X,I5)
 900  FORMAT(4I5,I10,F12.6,1P,2E15.4,0P,2F12.6)
 933  FORMAT(' NYLD','    K','    I','   JB','      LSPJ','  (ECONT)/RY'
     X,'       AWIDTH   ','      RWIDTH   ','     AYLD    '
     X,'     RYLD    ')
 934  FORMAT(25X,'EION=',F12.6)
      END
