C N. R. BADNELL     PROGRAM DLA_EMISS       UoS v1.9            12/09/16
C
C***********************************************************************
C
C         POST-PROCESSOR FOR  ** AUTOSTRUCTURE **
C         ***************************************
C
C  CALCULATES LS/IC DETAILED LEVEL ACCOUTING "EMISSIVITIES"
C
C***********************************************************************
C
      PROGRAM MAIN
C
C UNIX-F77
CF77      REAL*4 TARRY,TIME,TTIME                                   !F77
CF77      DIMENSION TARRY(2)                                        !F77
      CALL CPU_TIME(TTIME)                                          !F95
C
C      OPEN(5,FILE='dlain')                 !STDIN
      OPEN(6,FILE='dlaout')                !STDOUT
C      OPEN(7,FILE='ocs')                   !BINNED "EMISSIVITIES"
C      OPEN(13,FILE='DLA_EMISS_RAW')        !BINNED "EMISSIVITIES"
C      OPEN(14,FILE='DLA_EMISS_GAUSS')      !CONVOLUTED "EMISSIVITIES"
C      OPEN(70,FILE='on')                   !AUTOS DATA FILE (FORMATTED)
C OR
C      OPEN(70,FILE='onu',FORM='UNFORMATTED') !AUTOS DATA FILE (UNFORM)
C
      CALL POSTP
C
C UNIX-F77
CF77      DUM=DTIME(TARRY)                                          !F77
CF77      TIME=TARRY(1)                                             !F77
C
C UNIX-F95
      CALL CPU_TIME(TIME)                                           !F95
C
      TIME=TIME/60.0
      WRITE(6,999)TIME
 999  FORMAT(//1X,'CPU TIME=',F9.3,' MIN')
C
      WRITE(0,*) 'PROGRAM DLA_EMISS: NORMAL END'
C
      CLOSE(6)
C      CLOSE(13)
C      CLOSE(14)
C      CLOSE(70)
C
      END
C***********************************************************************
      SUBROUTINE POSTP
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C UNCOMMENT THE NEXT LINE IF COMPILER/ARCHITECTURE ALLOWS IT:
C      REAL*16 QT,QT0,QT1,QCMU,QMU,QZERO                        !128-BIT
C
      PARAMETER (NDIM1=50001)                        !MAX NO. OF BINS +1
CNORM      PARAMETER (NDIM2=2*NDIM1+1)               !MAX NO. GAUSS E
      PARAMETER (NDIM3=20)                           !MAX NO. OF MOMENTS
C
      PARAMETER (QZERO=0)
      PARAMETER (ZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
      PARAMETER (DTWO=2.0D0)
      PARAMETER (DFOUR=4.0D0)
C
      PARAMETER (TINY=1.D-5)
C
      LOGICAL BVARY
C
      DIMENSION EBIN(NDIM1),SBIN(NDIM1),QMU(0:NDIM3),ALF(0:NDIM3)
CNORM     X         ,TCC(NDIM2)
C
      NAMELIST/ONE/UNITS,IWGHT,nc,MUMAX,NXTRP,NXTRP0,NMAX,LMAX,JCFJ
C
      NAMELIST/TWO/NBIN,EMIN,EMAX,EWIDTH,ERES,TEAPOT,NGAUSS,DELTAF
C
      BVARY=.FALSE.
C
C**********************NAMELIST-ONE*************************************
C
C ***NOTHING IS COMPULSORY:
C
C  THEN THE WEIGHTING FACTOR FOR THE ENERGY MOMENTS IS THE LINE STRENGTH
C  AND ANY SUBSEQUENT USER INPUT ENERGIES ARE ASSUMED TO BE IN UNITS, 
C  AS IS THE OUTPUT.
C
C-----------------------------------------------------------------------
C
C     UNITS--ENERGY UNITS USED FOR SUBSEQUENT EMIN, EMAX, EWIDTH ETC:
C            1.0 FOR RYDBERGS (DEFAULT).
C            13.606 FOR EV, 
C            TO GET *WAVELENGTH*, SET UNITS .LT.0:
C            -109737.37   FOR CM
C            -.010973737  FOR NM
C            -.0010973737 FOR ANGSTROMS
C
C     IWGHT  IS THE WEIGHTING FACTOR FOR THE ENERGY MOMENTS
C          = 0 S (LINE STRENGTH) - DEFAULT
C          > 0   USES ADDITIONAL E**IWGHT FACTOR:
C          = 1 c.f. OPACITY
C          = 3 c.f. PHOTON EMISSIVITY COEFFICIENT (PEC)
C          = 4 c.f. TOTAL INTEGRATED EMISSIVITY
C          < 0   AS > 0, BUT THE E USED IS THE WEIGHTED MEAN, c.f. UTA.
C
C     MUMAX IS THE LARGEST ENERGY MOMENT DETERMINED, DEFAULT=4 KURTOSIS.
C
C     NC .GT. 0 READ OLD ocs FILE, DEFAULT =0.
C
C     NXTRP .GT. 0 EXTRAPOLATE FROM THE LAST N ON FILE UP TO N=NXTRP.
C       *** IF LOOPING OVER N THEN MUST ALSO SET
C     NXTRP0 TO THE LARGEST N ON FILE. 
C           ELSE JUST TAKEN FROM FILE.
C
C     NMAX .GT.0 IS THE MAX NL FOR THE UPPER STATES.
C                IN ADDITION, IF NXTRP.GT.0 THEN 
C          .GE.100 COMPLETES SUM NXTRP TO INFINITY BY QUADRATURE,
C          .GE.1000 CONTINUES INTO THE CONTINUUM.
C
C     LMAX .GE.0 IS THE MAX ORBITAL L FOR THE UPPER STATES.
C
C     JCFJ .GT. 0 IS THE MAX CONFIG NO. CONSIDERED FOR TRANSITION DATA.
C                 I.E. SUCH CONFIGS ONLY AFFECT FINAL RESULT VIA CI.
C
C****************************END-ONE************************************
C
C
      IWGHT=0
      UNITS=DONE
      MUMAX=4
      NXTRP=0
      NXTRP0=-1
      NMAX=-1
      LMAX=-1
      JCFJ=-1
      nc=0
C
C
      READ(5,ONE)
C
C
      IWGHT0=IWGHT
      IWGHT=IABS(IWGHT0)
C
      IF(UNITS.LT.ZERO)THEN
        IUNITS=-1
        UNITS=-UNITS
      ELSE
        IUNITS=1
      ENDIF
C
      WRITE(6,1000)IWGHT0,UNITS
C
      IF(MUMAX.GT.NDIM3)THEN
        WRITE(6,*)' ***DIMENSION EXCEEDED, INCREASE NDIM3 TO: ',MUMAX
        STOP ' ***DIMENSION EXCEEDED, INCREASE NDIM1'
      ENDIF
C
      IF(NMAX.LT.0)NMAX=-999
      IF(LMAX.LT.0)LMAX=9999
      IF(JCFJ.LT.0)JCFJ=9999
C
      WRITE(6,1005)NXTRP,NMAX,LMAX,JCFJ
C
C
C*************************NAMELIST-TWO**********************************
C
C ***NOTHING IS COMPULSORY:
C
C  THEN DEFAULTS JUST GIVE TOTAL LINE STRENGTH, WEIGHTED MEAN LINE 
C  ENERGY, SKEWNESS AND KURTOSIS.
C
C  MINIMUM INPUT FOR BINNED LINE EMISSION IS EMIN, EMAX AND AT LEAST ONE
C  OF NBIN, ERES OR EWIDTH - GAUSSIAN CONVOLUTED REQUIRES EWIDTH.
C  *CONVOLUTED* PREFERRED: ONLY SPECIFY EWIDTH (NOT NBIN/ERES) THEN CODE
C  CHOOSES SUITABLE ENERGY BINS, WHICH MAY HAVE VARIABLE BIN WIDTH.
C 
C-----------------------------------------------------------------------
C 
C     EMIN -- MINIMUM PHOTON ENERGY/WAVELENGTH IN UNITS (DEFAULT ZERO)
C             N.B. ANY LINE EMISSION LESS THAN EMIN IS OMITTED 
C                  FROM ocs, DLA_EMISS_RAW, DLA_EMISS_GAUSS.
C
C     EMAX -- MAXIMUM PHOTON ENERGY/WAVELENGTH IN UNITS (DEFAULT HUGE!)
C             N.B. ANY LINE EMISSION GREATER THAN EMAX IS OMITTED
C                  FROM ocs, DLA_EMISS_RAW, DLA_EMISS_GAUSS.
C
C     EWIDTH--WIDTH IN UNITS FOR GAUSSIAN CONVOLUTION OF BINNED EMISSION
C             IF .LT. 0 THEN -EWIDTH IS THE DIMENSIONLESS FRACTIONAL
C             WIDTH, SO THE ACTUAL GAUSSIAN WIDTH IS THEN -EWIDTH*ENERGY
C             IN THIS CASE, IF NBIN/ERES ARE NOT SPECIFIED THEN THE BIN
C             WIDTH WILL VARY WITH BIN ENERGY SO AS TO MATCH THE VARYING
C             GAUSSIAN WIDTH.
C             DEFAULT, ZERO - NO CONVOLUTION.
C
C     NBIN -- THE NUMBER OF LINE EMISSION BIN ENERGIES FOR PHOTON
C                  ENERGIES/WAVELENGTHS (LINEARLY) BETWEEN EMIN & EMAX.
C             IF .EQ. 0 (DEFAULT) THEN SET INTERNALLY, UNLESS:
C
C     ERES -- INSTEAD OF NBIN, CAN SPECIFY BIN WIDTH. USEFUL IF NEED
C             FIXED BIN WIDTH FOR SEVERAL IONS OR IF EWIDTH IS SMALL.
C
C-----------------------------------------------------------------------
C
C  ***VARIABLES HERE-ON ARE NORMALLY FOR TESTING ETC. ONLY***
C
C     NGAUSS .GT. 0 NGAUSS CONVOLUTION POINTS BETWEEN EMIN AND EMAX.
C            .LT. 0 -NGAUSS CONVOLUTION POINTS PER EWIDTH.
C            .EQ. 0 2*nbin+1 CONVOLUTION POINTS BETWEEN EMIN AND EMAX.
C
C     DELTAF -- THE MINIMUM VALUE OF THE OSCILLATOR STRENGTH RETAINED
C               DEFAULT, ALL.
C
C     TEAPOT -- TOTAL GROUND STATE ENERGY OF THE NEXT IONIZATION STAGE.
C               SET THIS IF NECESSARY SO THAT THE CODE CAN DIFFERENTIATE
C               BETWEEN TRUE BOUND AND AUTOIONIZING STATES. IF THE UPPER
C               STATE IS AUTOIONIZING THEN IT IS OMITTED FROM ALL OUTPUT
c
c    note: nbin.lt.0 can be used to increase the number of bins per 
c          ewidth, default -nbin=5, good enough for government work but
c          higher numerical accuracy with 10.
C
C***************************END-TWO*************************************
C
      TEAPOT=1.D99
      NBIN=-5                                                !AGGRESSIVE
      EMIN=ZERO
      EMAX=99999.0D0
      EWIDTH=ZERO
      ERES=-DONE
      NGAUSS=0
      DELTAF=-DONE
C
C
      READ(5,TWO)
C
C
      WRITE(6,1010)EMIN,EMAX,DELTAF,TEAPOT
C
C SET-UP EWIDTH, MIN POSSIBLE IF FRACTIONAL.
C
      EWIDTH0=EWIDTH
      IF(EWIDTH0.LT.ZERO)EWIDTH=-EWIDTH0*EMIN
C
      ERES0=ERES
C
C SET-UP BINS: ELECTRON ENERGY/WAVELENGTH BINS DEFINED BY EMIN, EMAX
C              IN USER UNITS (RESTRICTS *ALL* OUTPUT)
C
      NBIN0=NBIN
C
      BVARY=NBIN0.LE.1.AND.ERES0.LE.ZERO
      IF(.NOT.BVARY.OR.EWIDTH.GT.ZERO)THEN
C
        OPEN(13,FILE='DLA_EMISS_RAW')
C
        OPEN(7,FILE='ocs')                                    !OPEN FILE
C
        IF(EMAX.LT.EMIN.OR.EMIN.LT.ZERO)THEN
          WRITE(6,*)'EMIN, EMAX NOT SET SUITABLY FOR BINNING:',EMIN,EMAX
          STOP 'EMIN, EMAX NOT SET SUITABLY FOR BINNING!'
        ENDIF
C
        IF(BVARY)THEN                                        !USE EWIDTH
          BVARY=EWIDTH0.LT.ZERO                !ALLOW VARIABLE BIN WIDTH
          npres=max(-nbin,5)                        !no. bins per ewidth
          npres=min(npres,20)                              !don't go mad
          ERES=EWIDTH/npres
          NBIN0=NINT((EMAX-EMIN)/ERES)+1
          IF(NBIN0.LE.501)THEN                         !ENSURE A MINIMUM
            ERES=(EMAX-EMIN)/500
            IF(BVARY)THEN
              npres=nint(ewidth/eres)
              ERES=EWIDTH/npres
            ENDIF
          ENDIF
        ELSEIF(ERES.GT.ZERO)THEN                               !USE ERES
          IF(NBIN0.GT.1)
     X    WRITE(0,*)'YOU HAVE SET ERES & NBIN .GT. 0, IGNORING NBIN!'
          NBIN0=NINT((EMAX-EMIN)/ERES)+1
        ENDIF
C
        IF(BVARY)THEN                                !VARIABLE BIN WIDTH
C
          E5=-EWIDTH0/npres
          T=EMIN-ERES
          DO N=1,NDIM1
            T=T+ERES
            EBIN(N)=T
            IF(T.GT.EMAX)GO TO 10                           !WE ARE DONE
            ERES=E5*T
          ENDDO
C                                        !NOT ENOUGH POINTS, allocate...
          WRITE(6,*)' ***DIMENSION EXCEEDED, INCREASE NDIM1'
          STOP ' ***DIMENSION EXCEEDED, INCREASE NDIM1'
C
  10      NBIN=N
          NBIN1=NBIN-1
          EMAX=T
          ERES=ERES0
C
        ELSE                                            !FIXED BIN WIDTH
C
          NBIN=ABS(NBIN0)                                      !USE NBIN
          NBIN1=NBIN-1
          ERES=(EMAX-EMIN)/NBIN1
C
          IF(NBIN.GT.NDIM1)THEN
            WRITE(6,*)' ***DIMENSION EXCEEDED, INCREASE NDIM1 TO: ',NBIN
            STOP ' ***DIMENSION EXCEEDED, INCREASE NDIM1'
          ENDIF
C
          DO N=1,NBIN
            T=N-1
            T=EMIN+T*ERES
            EBIN(N)=T
          ENDDO
C
        ENDIF
C
C
C INITIALIZE
C
        WRITE(6,1020)NBIN,ERES
C
        IF(EWIDTH.GT.ZERO)THEN
          WRITE(6,1030)EWIDTH0
          OPEN(14,FILE='DLA_EMISS_GAUSS')
C
          IF(ERES.GT.EWIDTH/DTWO)THEN
            WRITE(6,*)'BIN WIDTH IS TOO LARGE FOR EWIDTH - TRY AGAIN!'
            STOP 'BIN WIDTH IS TOO LARGE FOR EWIDTH - TRY AGAIN!'
          ELSEIF(ERES.GT.EWIDTH/DFOUR)THEN
            WRITE(6,*)'WARNING BIN WIDTH IS RATHER LARGE FOR EWIDTH...'
            WRITE(0,*)'WARNING BIN WIDTH IS RATHER LARGE FOR EWIDTH...'
          ENDIF
        ENDIF
C
        DO N=1,NBIN1
          SBIN(N)=ZERO
        ENDDO
C
      ENDIF
c
c read previous ocs files, and convolute with Gaussian.
c
      if(nc.gt.0)then
        if(ngauss.le.0)ngauss=2*nbin+1
        read(7,1060,end=78)nbin
        nbin1=nbin-1
        read(7,1070)(ebin(n),n=1,nbin)
        read(7,1070)(sbin(n),n=1,nbin1)
        smax=zero
        do n=1,nbin1
          smax=max(smax,sbin(n))
        enddo
  78    close(7)
        go to 77
      endif
C
C SUM OVER LINE EMISSION AND GENERATE ENERGY MOMENTS
C
      IF(MUMAX.GT.8)THEN
        WRITE(6,*)' '
        WRITE(0,*)'HIGH REDUCED CENTRED MOMENTS SUBJECT TO LARGE '
     X ,'CANCELLATION ERROR IF NOT USING 128-BIT/REAL*16/QUAD PRECISION'
        WRITE(6,*)'HIGH REDUCED CENTRED MOMENTS SUBJECT TO LARGE '
     X ,'CANCELLATION ERROR IF NOT USING 128-BIT/REAL*16/QUAD PRECISION'
      ENDIF
C
C CONVERT TO INTERNAL UNSCALED RYDBERGS
C
      NUMR=1                                          !FLAG FOR SR,EMISS
      IF(IUNITS.LT.0.AND.NBIN.GT.0)THEN     !FIRST REVERSE IF WAVELENGTH
        NUMR=-1
        IF(EMIN.EQ.ZERO)EMIN=TINY
        IF(EMAX.EQ.ZERO)EMAX=TINY
        E=EMIN
        EMIN=DONE/EMAX
        EMAX=DONE/E
        IF(EBIN(1).EQ.ZERO)EBIN(1)=(EBIN(3)-EBIN(2))/1000
        NBIN2=NBIN/2
        DO N=1,NBIN2
          NN=NBIN-N+1
          E=EBIN(NN)
          EBIN(NN)=DONE/EBIN(N)
          EBIN(N)=DONE/E
        ENDDO
        IF(2*NBIN2.NE.NBIN)THEN
          N=NBIN2+1
          EBIN(N)=DONE/EBIN(N)
        ENDIF
      ENDIF
C
      EMIN=EMIN/UNITS
      EMAX=EMAX/UNITS
      DO N=1,NBIN
        EBIN(N)=EBIN(N)/UNITS
      ENDDO
C
      DO M=0,MUMAX
        QMU(M)=QZERO
      ENDDO
C
C
      CALL EMISS(IWGHT0,DELTAF,TEAPOT,NBIN,EMIN,EMAX,EBIN,SBIN,QMU
     X          ,MUMAX,NXTRP0,NXTRP,NMAX,LMAX,JCFJ,NUMR)
C
C
      WRITE(6,1050)NUMR
C
      IF(QMU(0).EQ.QZERO)RETURN
C
C NORMALIZE THE MOMENTS
C
      QT0=UNITS
      QT=1
      IF(IUNITS.LT.0)QT0=QT/QT0
C
      DO M=1,MUMAX
        QT=QT*QT0
        QMU(M)=QT*QMU(M)/QMU(0)
      ENDDO
C                                                   !SINCE S*E**|IWGHT0|
      IF(IWGHT0.GT.0)QMU(0)=QMU(0)*UNITS**IWGHT0
      IF(IWGHT0.LT.0)QMU(0)=QMU(0)/QMU(1)**(IWGHT0*IUNITS)
C
C MU_0 (UNNORMED) IS THE TOTAL LINE EMISSION 
C
      IF(QMU(0).LT.1.E6)THEN
        WRITE(6,1023)IWGHT,QMU(0)
      ELSE
        WRITE(6,1033)IWGHT,QMU(0)
      ENDIF
C
C MU_1 IS THE MEAN STRENGTH-WEIGHTED LINE ENERGY (MSWLE)
C
      IF(QMU(1).LT.1.E6)THEN
        WRITE(6,1024)QMU(1)
      ELSE
        WRITE(6,1034)QMU(1)
      ENDIF
C
C SIGMA=SQRT(MU^C_2) IS THE RMS DEVIATION OF THE MSWLE, WHERE 
C MU^C_2=MU_2-MU_1^2 (THE 2ND CENTRED MOMENT) IS THE VARIANCE
C
      CMU2=QMU(2)-QMU(1)*QMU(1)
c
      IF(CMU2.GE.ZERO)THEN
        SIGMA=SQRT(CMU2)
      ELSE
        WRITE(6,*)'VARIANCE IS NEGATIVE:',CMU2
        STOP 'VARIANCE IS NEGATIVE'
      ENDIF
C
      IF(CMU2.LT.1.E6)THEN
        WRITE(6,1025)CMU2,SIGMA
      ELSE
        WRITE(6,1035)CMU2,SIGMA
      ENDIF
C
C ALPHA_3=MU^C_3/SIGMA^3 IS THE SKEWNESS
C
      CMU3=2*QMU(1)**3-3*QMU(1)*QMU(2)+QMU(3)
      ALF3=CMU3/SIGMA**3
C
      IF(ALF3.GE.ZERO.AND.ALF3.LT.1.E6 .OR.
     X   ALF3.LT.ZERO.AND.ALF3.GT.-1.E5)THEN
        WRITE(6,1026)ALF3
      ELSE
        WRITE(6,1036)ALF3
      ENDIF
C
C ALPHA_4=MU^C_4/SIGMA^4 IS THE KURTOSIS
C
      CMU4=-3*QMU(1)**4+6*QMU(1)**2*QMU(2)-4*QMU(3)*QMU(1)+QMU(4)
      ALF4=CMU4/SIGMA**4
C
      IF(ALF4.LT.1.E6)THEN
        WRITE(6,1027)ALF4
      ELSE
        WRITE(6,1037)ALF4
      ENDIF
C
C HIGHER REDUCED CENTRED MOMENTS
C
      IF(MUMAX.GT.4)WRITE(6,1028)
C
      DO M=5,MUMAX
        QCMU=QMU(M)
c          write(6,*)cmu
        QT1=-QMU(1)
        INOM=1
        IM=0
        IP=0
        DO I=M-1,1,-1
          IP=IP+1
          INOM=((I+1)*INOM)/IP
          QT=INOM*QMU(I)*QT1
          QCMU=QCMU+QT
          QT1=-QT1*QMU(1)
c          write(6,*)qt,qcmu
        ENDDO
        QCMU=QCMU+QT1
c          write(6,*)qt1,qcmu
C
        ALF(M)=QCMU/SIGMA**M
C
        WRITE(6,1029)M,ALF(M)
      ENDDO
C
C
      IF(NBIN0.LE.1)RETURN
C
C
C CONVERT BACK TO USER UNITS - DON'T FORGET TO REVERSE SBIN AS WELL.
C
      DO N=1,NBIN
        EBIN(N)=EBIN(N)*UNITS
      ENDDO
C
      IF(IWGHT0.NE.0)THEN                           !SINCE S*E**|IWGHT0|
        TU=DONE
        IF(IWGHT0.GT.0)TU=TU*UNITS**IWGHT0
        IF(IWGHT0.LT.0)TU=TU/QMU(1)**(IWGHT0*IUNITS)
        DO N=1,NBIN1
          SBIN(N)=SBIN(N)*TU
        ENDDO
      ENDIF
C
      IF(IUNITS.LT.0)THEN        
        DO N=1,NBIN2
          NN=NBIN-N+1
          E=EBIN(NN)
          EBIN(NN)=DONE/EBIN(N)
          EBIN(N)=DONE/E
          S=SBIN(NN)
          SBIN(NN)=SBIN(N)
          SBIN(N)=S
        ENDDO
        IF(2*NBIN2.NE.NBIN)THEN
          N=NBIN2+1
          EBIN(N)=DONE/EBIN(N)
        ENDIF
      ENDIF
C
C ENERGY-AVERAGE OVER BIN WIDTH (O.K. SINCE WILL NORM TO CONVOLUTED)
C
      SMAX=ZERO
      DO N=1,NBIN1
        SBIN(N)=SBIN(N)/(EBIN(N+1)-EBIN(N))
        SMAX=MAX(SMAX,SBIN(N))
      ENDDO
C
C CONVOLUTE WITH GAUSSIAN (USE A LINEAR ENERGY MESH)
C
  77  IF(EWIDTH.GT.ZERO)THEN
C
        IF(NGAUSS.LT.0)THEN
          NGAUSS=-NGAUSS-1
          IF(NGAUSS.LT.5)NGAUSS=20
          DEG=ABS(EWIDTH)/NGAUSS
          T=(EBIN(NBIN)-EBIN(1))/DEG
          NT=NINT(T)
        ELSEIF(NGAUSS.LT.50)THEN
          IF(EWIDTH.GT.ZERO)THEN
            NT=2*NBIN1
          ELSE                                      !NOT ACCESSIBLE/USED
            T=(EBIN(NBIN)-EBIN(1))/EWIDTH
            NT=NINT(-T)
            NT=MAX(200,5*NT)
          ENDIF
        ELSE
          NT=NGAUSS-1
        ENDIF
C
        NT=NT+1
        WRITE(14,1040)IWGHT,EWIDTH0,NT
C
C ONLY NEEDED IF NORMALIZING CONVOLUTED TO BINNED - THEN UNCOMMENT ALL
C             CNORM STATEMENTS BOTH BELOW AND PARAMETER/DIMENSION ABOVE.
C
CNORM        IF(NT.GT.NDIM2)THEN
CNORM          WRITE(6,*)' ***DIMENSION EXCEEDED, INCREASE NDIM2 TO:',NT
CNORM          STOP ' ***DIMENSION EXCEEDED, INCREASE NDIM2'
CNORM        ENDIF
C
        T=NT-1
        DEG=(EBIN(NBIN)-EBIN(1))/T
        IF(EWIDTH0.LT.ZERO)EWIDTH=-EWIDTH0*EBIN(NBIN)     !NOW MAX POSS.
        DE=(EBIN(NBIN)-EBIN(NBIN1))
        T=2*ABS(EWIDTH)/DE                      !O.K. EVEN IF NON-LINEAR
        N0=NINT(T)                              !*2   !TEST
c        W=DEG/DE                               !for old linear ebin
        nn=1
        E0=EBIN(1)
        TMAX=ZERO
C
        DO N=1,NT
          E=E0+(N-1)*DEG
          IF(EWIDTH0.LT.ZERO)EWIDTH=-EWIDTH0*E
c          NN=NINT(N*W)                         !for old linear ebin
          if(e.gt.ebin(nn))nn=nn+1
          N1=MAX(1,NN-N0)
          N2=MIN(NBIN1,NN+N0)
C
          T=CONVOLG(E,EWIDTH,EBIN,SBIN,N1,N2)
C
          TMAX=MAX(TMAX,T)
CNORM          TCC(N)=T
          WRITE(14,1090)E,T                       !COMMENT-OUT FOR CNORM
        ENDDO
C                                          NORMALZE CONVOLUTED TO BINNED
CNORM        TNORM=SMAX/TMAX
CNORM        DO N=1,NT
CNORM          E=E0+(N-1)*DEG
CNORM          WRITE(14,1090)E,TCC(N)*TNORM
CNORM        ENDDO
C
      ENDIF
C
      WRITE(13,1039)IWGHT,NBIN1
C
      IF(EWIDTH.LE.ZERO)THEN
        SNORM=DONE
      ELSE
        SNORM=TMAX/SMAX
        WRITE(13,1038)SNORM
        CLOSE(14)
      ENDIF
C                                         NORMALIZE BINNED TO CONVOLUTED
      DO N=1,NBIN1
        WRITE(13,1090)EBIN(N+1),SBIN(N)*SNORM
      ENDDO
C
C WRITE-OUT (UNNORMED) BINNED LINE EMISSION
C
      if(nc.le.0)then
        WRITE(7,1060)NBIN
        WRITE(7,1070)(EBIN(N),N=1,NBIN)
        WRITE(7,1070)(SBIN(N),N=1,NBIN1)
      endif
C
      CLOSE(7)
      CLOSE(13)
C
      RETURN
C
 1000 FORMAT(' IWGHT=',I3,5X,'UNITS=',1PE14.7)
 1005 FORMAT(/' NXTRP=',I3,5X,'NMAX=',I5,3X,'LMAX=',I5,5X,'JCFJ=',I5)
 1010 FORMAT(/' EMIN=',F10.3,3X,'EMAX=',F10.3,3X
     X,'DELTAF=',1PD8.1,3X,'TEAPOT=',E12.5)
 1020 FORMAT(/' BINNED CROSS SECTIONS WRITTEN: NBIN=',I6,
     X' WITH BIN WIDTH=',1PE12.4)
 1023 FORMAT(//' TOTAL LINE EMISSION:'//' S*E^',I1,3X,'=',F10.3)
 1024 FORMAT(//' MU_1 MOMENT: '//' MEAN E  =',F10.4)
 1025 FORMAT(//' MU^C_2  = MU_2-MU_1^2 = SIGMA^2: '//' VARIANCE=',F10.6
     X/' RMS     =',F10.6)
 1026 FORMAT(//' ALPHA_3 = MU^C_3/SIGMA^3: '//' SKEWNESS=',F10.3)
 1027 FORMAT(//' ALPHA_4 = MU^C_4/SIGMA^4: '//' KURTOSIS=',F10.3)
 1028 FORMAT(//' HIGHER REDUCED CENTRED MOMENTS: '//5X,'N',6X,'ALPHA_N')
 1029 FORMAT(I6,4X,1PE10.3)
 1030 FORMAT(/' AND CONVOLUTED WITH EWIDTH=',1PE10.3,
     X    ' FWHM GAUSSIAN DISTRIBUTION')
 1033 FORMAT(//' TOTAL LINE EMISSION:'//' S*E^',I1,3X,'=',1PE10.3)
 1034 FORMAT(//' MU_1 MOMENT: '//' MEAN E  =',1PE10.4)
 1035 FORMAT(//' MU^C_2  = MU_2-MU_1^2 = SIGMA^2: '//' VARIANCE='
     X,1PE10.4/' RMS     =',1PE10.4)
 1036 FORMAT(//' ALPHA_3 = MU^C_3/SIGMA^3: '//' SKEWNESS=',1PE10.3)
 1037 FORMAT(//' ALPHA_4 = MU^C_4/SIGMA^4: '//' KURTOSIS=',1PE10.3)
 1038 FORMAT('#',' NORMALIZED TO CONVOLUTED BY FACTOR',1PD12.5)
 1039 FORMAT('# TOTAL LINE EMISSION: S*E^',I1/'#',20X,'AT',I7
     X,' ENERGIES')
 1040 FORMAT('# TOTAL LINE EMISSION: S*E^',I1/'#       CONVOLUTED WITH '
     X,F6.3,' FWHM GAUSSIAN DISTRIBUTION'/'#',20X,'AT',I7,' ENERGIES')
 1050 FORMAT(///' THERE ARE',I12,' TRANSITION LINES'/)
 1060 FORMAT(I5)
 1070 FORMAT(6(1PE12.6))
 1090 FORMAT(1PE16.8,E14.4)
      END
C***********************************************************************
      REAL*8 FUNCTION CONVOLG(E,EWIDTH,EBIN,SBIN,N1,N2)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  CONVOLUTE CROSS SECTIONS WITH GAUSSIAN DISTRIBUTION
C
      PARAMETER (ZERO=0.0D0)
      PARAMETER (DTWO=2.0D0)
C
      DIMENSION EBIN(*),SBIN(*)
C
      A=1.6651092D0/EWIDTH                                !2*sqrt(ln(2))
      SUM=ZERO
C
      IF(EWIDTH.GT.ZERO)THEN                                     !BINNED
        DO I=N1,N2
          IF(SBIN(I).GT.ZERO)THEN
            XI=EBIN(I)
            XI1=EBIN(I+1)
            SUM=SUM+SBIN(I)*(ERF(A*(E-XI))-ERF(A*(E-XI1)))/DTWO
          ENDIF
        ENDDO
      ELSE                                       !LORENTZIANS (NOT USED)
        A=-A
        DO I=N1,N2                                        !ASSUME LINEAR
          IF(SBIN(I).GT.ZERO)THEN
            T=A*(EBIN(I)-E)
            T=T*T
            SUM=SUM+SBIN(I)*EXP(-T)
          ENDIF
        ENDDO
        SUM=SUM*(EBIN(2)-EBIN(1))*A*0.5641895D0              !1/sqrt(pi)
      ENDIF
C
      CONVOLG=SUM
      RETURN
      END
C***********************************************************************
      SUBROUTINE EMISS(IWGHT0,DELTAF,TEAPOT,NBIN,EMIN,EMAX,EBIN,SBIN,QMU
     X                ,MUMAX,NXTRP0,NXTRP,NMAX,LMAX,JCFJ,NUMR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C UNCOMMENT THE NEXT LINE IF COMPILER/ARCHITECTURE ALLOWS IT:
C      REAL*16 QMU,QDEL,QWGHT                                   !128-BIT
C
      PARAMETER (NDIM1=50001)                       !MAX NO. OF BINS +1
      PARAMETER (NDIM14=500)                        !NO OF CONFIGS
      PARAMETER (MXORB0=50)                         !NO. NL READ FROM AS
      PARAMETER (NDIM26=MXORB0)
      PARAMETER (NLIT=60)
C
      PARAMETER (ZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
C
      PARAMETER (CON1=8.0325D9)                     !ALPHA^3/2*TAU_0
C
      LOGICAL BFORM,EX,BFAST,BXTRP,BXTRP0,bxtrp1,bxtrp2
C
      CHARACTER FILNAM*4,CMBLNK*4        !,CMSTAR*4
      CHARACTER*1 CLIT(NLIT)             !,CLABL(20)
C
      INTEGER QNV(NDIM14),QLV(NDIM14),QS0(10),QL0(10)
     X,QN(NDIM26),QL(NDIM26),QND(NDIM26),QLD(NDIM26)
C
      DIMENSION EBIN(*),SBIN(*),QMU(0:*)
      DIMENSION LIT(NLIT),TBIN(NDIM1)
C
      DATA CLIT /'1','2','3','4','5','6','7','8','9','A','B','C','D','E'
     X,'F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U',
     X 'V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k',
     X 'l','m','n','o','p','q','r','s','t','u','v','w','x','y'/
      DATA CMBLNK/'    '/
C
C FIX FOR FORTRAN 90 COMPILERS THAT DON'T ALLOW ASSIGNMENT OF CHARACTERS
C TO INTEGER VARIABLES, REQUIRED FOR HISTORIC BACKWARDS COMPATIBILITY
C
      OPEN(90,STATUS='SCRATCH',FORM='FORMATTED')
      WRITE(90,1111)CMBLNK,(CLIT(I),I=1,NLIT)
 1111 FORMAT(A4,80A1)
      BACKSPACE(90)
      READ(90,1111)MBLNK,(LIT(I),I=1,NLIT)
C      WRITE(90,1111)CMSTAR,(CLABL(I),I=1,20)
C      BACKSPACE(90)
C      READ(90,1111)MSTAR,(LABL(I),I=1,20)
      CLOSE(90)
C
      IUNITS=NUMR
      NUMR=0
      NBIN1=NBIN-1
C
      BXTRP=NXTRP.GT.0
      IF(BXTRP)THEN
        DO N=1,NBIN1
          TBIN(N)=ZERO
        ENDDO
        bxtrp1=nmax.ge.100
        bxtrp2=nmax.ge.1000
      ENDIF

C SKIP TESTS FOR PRODUCTION RUN, SAVE BIN EMIN,EMAX.
C (THESE TESTS ARE JUST THAT, NOT NORMALLY USED FOR PRODUCTION RUN.)
C
      BFAST=iabs(NMAX).GE.100.AND.LMAX.GT.100.AND.TEAPOT.GT.1.D5.AND.
     X      DELTAF.LE.ZERO.AND.JCFJ.GT.1000
C
      NZOLD=0
      NEOLD=0
      MXORB=MIN(50,MXORB0,NLIT)
C
C******************************************************
C POSSIBLE UNIT NOS TO BE CHECKED FOR DATA: READ NV, LV
C******************************************************
C
      BFORM=.FALSE.
      MR=70
      MRU=MR
      IFILE=1
      FILNAM='o1'
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF(EX)THEN
        OPEN(MR,FILE=FILNAM)
        READ(MR,38,END=332)MDUM1,MDUM2
        BFORM=.TRUE.
        BACKSPACE(MR)
      ENDIF
 332  IF(.NOT.BFORM)THEN
        FILNAM='o1u'
        INQUIRE(FILE=FILNAM,EXIST=EX)
        IF(EX)THEN
          OPEN(MRU,FILE=FILNAM,FORM='UNFORMATTED')
        ELSE
          WRITE(6,*)'NO RATE INPUT DATA ON FILE o1 OR o1u!!!'
          STOP 'ERROR: NO RATE INPUT DATA ON FILE o1 OR o1u!!!'
        ENDIF
      ENDIF
C
C*************************
C ENTRY POINT FOR NEW UNIT
C*************************
C
 331  CONTINUE
C
C***************
C START A NEW NL
C***************
C
  31  CONTINUE
C
      IF(BFORM)READ(MR,38,END=1000)NV,LV
      IF(.NOT.BFORM)READ(MRU,END=1000)NV,LV
  38  FORMAT(5X,I5,5X,I5)
C
      IF(NV.EQ.0)GO TO 1000                       !END OF FILE
C
      IF(BXTRP)THEN
        IF(LV.EQ.-1)THEN
          NXTRP0=NV
        ELSEIF(NXTRP0.LT.NV)THEN
          WRITE(0,*)'*** ERROR: N-LOOP OPERATION, NXTRP0 .LT. NV'
     X   ,NXTRP0,NV
          STOP '*** ERROR: N-LOOP OPERATION, BUT NXTRP0 .LT. NV!'
        ENDIF
C
        IF(NV.GT.NXTRP)THEN
          WRITE(0,*)'*** NOTE: USER NXTRP .LT. NV - RE-SETTING TO NV:'
     X   ,NXTRP,NV
          NXTRP=NV
        ENDIF
C
        BXTRP0=NV.EQ.NXTRP0
        TSUM=DONE
C
C UNCOMMENT CLX TO GIVE ESTIMATE OF EXTRAPOLATION OF LOWER N=NV+1
C TO NXTRP, I.E. B-B ONLY, ALL USING THE SAME BIN.
C
CLX        IF(BXTRP0)THEN
CLX          DO NX=NV+1,NXTRP
CLX            T=NV
CLX            T=T/NX
CLX            DO NXP=NX,NXTRP
CLX              TP=NV
CLX              TP=TP/NXP
CLX              TSUM=TSUM+(T*TP)**3
CLX            ENDDO
CLX          ENDDO
CLXc          tsum=1.
CLXc          write(0,*)tsum
CLX        ENDIF
C
      ELSE
        BXTRP0=.FALSE.
      ENDIF
C
C************************************
C READ HEADER, AND MAYBE ORBITAL CODE
C************************************
C
      DO I=1,MXORB
        QND(I)=0
      ENDDO
      IF(BFORM)THEN
 299    READ(MR,101,END=1002) NCFD,NZ0D,NED,(QND(I),QLD(I),I=1,MXORB)
        if(kfpm.eq.0.and.qnd(mxorb).ne.0.and.mxorb.lt.mxorb0)then
          mxorb=mxorb+20
          mxorb=min(mxorb,mxorb0)
          backspace(mr)
          go to 299
        endif
      ELSE
       READ(MRU,END=1002,ERR=300)NCFD,NZ0D,NED,(QND(I),QLD(I),I=1,MXORB)
       GO TO 302
 300   IF(EX)THEN                                !START OF A FILE
         REWIND(MRU)                             !SO REWIND
         READ(MRU)
         READ(MRU)
       ELSE
         STOP 'UNABLE TO READ ORBITAL HEADER...' !SHOULD NOT GET HERE
       ENDIF                                 
      ENDIF
 101  FORMAT(I3,12X,I2,6X,I2,4X,50(I3,I2))
C
 302  NCF=NCFD                     !NOT EOF SO SAFE TO RELABEL
      IF(NCF.EQ.0)RETURN
      NZ0=NZ0D
      NE=NED
      DO I=1,MXORB
        QN(I)=QND(I)
        QL(I)=QLD(I)
      ENDDO
C
      IF(NZOLD.NE.0.AND.NZ0.NE.NZOLD)THEN
        WRITE(6,*)'*** ERROR: DIFFERENT ELEMENTS ON TWO FILES, NZ='
     X            ,NZOLD,NZ0
        STOP '*** ERROR: DIFFERENT ELEMENTS ON TWO FILES'
      ENDIF
      NZOLD=NZ0
C
      IF(NEOLD.NE.0.AND.NE.NE.NEOLD)THEN
        WRITE(6,*)'*** ERROR: DIFFERENT IONS ON TWO FILES, NE='
     X            ,NEOLD,NE
        STOP '*** ERROR: DIFFERENT IONS ON TWO FILES'
      ENDIF
      NEOLD=NE
C
      DO I=1,MXORB                 !SHORT ORBITAL LIST
        IF(QN(I).LE.0)GO TO 301
      ENDDO
      I=MXORB+1
 301  MXORB=I-1
C
      IF(NCF.GT.NDIM14)THEN
        WRITE(6,136)NCF
 136  FORMAT(' DIMENSION EXCEEDED IN SR.EMISS, INCREASE NDIM14 TO',I5)
        STOP 'ERROR: DIMENSION EXCEEDED IN SR.EMISS, INCREASE NDIM14'
      ENDIF
C
      DZ=NZ0-NE+1
C
C************************
C READ CONFIGURATION DATA
C************************
C
      DO I=1,NCF
C
        IF(BFORM)READ(MR,179,END=1002)
     X                NII,NGR,MA0,MB0,(QS0(J),QL0(J),J=1,10)
        IF(.NOT.BFORM)READ(MRU,END=1002)
     X                NII,NGR,MA0,MB0,(QS0(J),QL0(J),J=1,10)
 179    FORMAT(2I5,2X,I3,I2,1X,10(I2,A1))
C
        DO 16 J=10,1,-1
C          QSB(I,J)=MBLNK
          IF(QL0(J).NE.MBLNK)THEN
C            LMX(I)=J
C            M=MOD(QS0(J),50)
C            IF(M.GT.0)QSB(I,J)=LIT(M)
            DO K=1,MXORB
              IF(QL0(J).EQ.LIT(K))GO TO 19
            ENDDO
            STOP 'UNABLE TO DECODE VALENCE ORBITAL; INCREASE MXORB READ'
C            QLB(I,J)=0
C            GO TO 16
C  19        QLB(I,J)=K
C            IF(NII.LT.0)MXOCC(K)=MAX(M,MXOCC(K))
          ENDIF
  16    CONTINUE
C
  19    QNV(I)=QN(K)
        QLV(I)=QL(K)
C
      ENDDO
C
C**************************
C READ AUTOIONIZATION RATES
C**************************
C
      IF(BFORM)READ(MR,103,END=1002)
      IF(.NOT.BFORM)READ(MRU,END=1002)
      IF(BFORM)READ(MR,103,END=1002)
 103  FORMAT(A1)
      I=0
 111  I=I+1
C
      IF(BFORM)READ(MR,112,END=1002)I1,I2,IWA,JCA,I3,T1,T2,EION
      IF(.NOT.BFORM)READ(MRU,END=1002)I1,I2,IWA,JCA,I3,T1,T2,EION
 112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))
C
      IF(I2.EQ.0) GO TO 113
C
      GO TO 111
C
 113  NUMA=I-1
C
C**************
C READ ENERGIES
C**************
C
      IF(BFORM)READ(MR,121,END=1002)NENG,ECORE
      IF(.NOT.BFORM)READ(MRU,END=1002)NENG,ECORE
 121  FORMAT(10X,I5,45X,F15.6)
C
      IF(BFORM)READ(MR,105,END=1002)MTEST
      IF(.NOT.BFORM)READ(MRU,END=1002)MTEST
 105  FORMAT(26X,A4)
C
      DO I=1,NENG
C
        IF(BFORM)READ(MR,123,END=1002)
C     X          IK(I),IT(I),SS(I),LL(I),JJ(I),LCF(I),ENERG(I)
        IF(.NOT.BFORM)READ(MRU,END=1002)
C    X           IK(I),IT(I),SS(I),LL(I),JJ(I),LCF(I),ENERG(I)
 123    FORMAT(5X,6I5,F15.6)
C
      ENDDO
C
C*********************
C READ RADIATIVE RATES
C*********************
C
      IF(BFORM)READ(MR,104,END=1002)NZTEST
      IF(.NOT.BFORM)READ(MRU,END=1002)NZTEST,NDUME
 104  FORMAT(66X,I2)
C
      IF(BFORM)READ(MR,103,END=1002)
C
      I=0
 131  I=I+1
      IF(BFORM)READ(MR,132,END=1002) ICR,I1,IWR,JCR,I3,JWR,T1,DEL,ELOW
      IF(.NOT.BFORM)READ(MRU,END=1002)ICR,I1,IWR,JCR,I3,JWR,T1,DEL,ELOW
 132  FORMAT(6I5,1PE15.5,2(0PF15.6))
C
      IF(I1.EQ.0)THEN
        NUMR=NUMR+I-1
        EX=.FALSE.
        GO TO 31                               !GO AND READ NEW NL BLOCK
      ENDIF
C
C***********************
C PROCESS RADIATIVE DATA
C***********************
C
      I=I-1
      IF(DEL.LT.EMIN.OR.DEL.GT.EMAX)GO TO 131       !OUTSIDE RANGE
C
      IF(BFAST)THEN                                 !SKIP FURTHER TESTS
C
        S=3*ABS(T1)*IWR/(CON1*DEL**3)               !LINE STRENGTH
C
      ELSE
C
        EUP=DEL+ELOW                                !ONLY TEST NON-EXTRP
        IF(EUP.GT.TEAPOT)GO TO 131                  !UPPER AUTOIONIZING
        IF(QNV(ICR).GT.iabs(NMAX))GO TO 131         !OUTSIDE RANGE
        IF(QLV(ICR).GT.LMAX)GO TO 131               !   "      "
        IF(ICR.GT.JCFJ)GO TO 131                    !   "      "
        IF(JCR.GT.JCFJ)GO TO 131                    !   "      "
        T1=ABS(T1)                                  !RATE
        GF=T1*IWR/(CON1*DEL*DEL)                    !SYMM OSCILLATOR
        IF(GF.LT.DELTAF)GO TO 131                   !TOO SMALL
        S=3*GF/DEL
C
      ENDIF
C
      I=I+1
C
      WGHT=S
      IF(IWGHT0.GT.0)WGHT=WGHT*DEL**IWGHT0
      if(iunits.lt.0)del=done/del                 !wavelength moments
C
      QWGHT=WGHT
      QDEL=DEL
C
      DO MU=0,MUMAX
        QMU(MU)=QMU(MU)+QWGHT
        QWGHT=QDEL*QWGHT
      ENDDO
C
      if(nbin1.le.0)go to 131
      if(iunits.lt.0)del=done/del                 !back to energy
c
      CALL BISECT(EBIN,NBIN,0,NBIN+1,DEL,N)
C
      IF(BXTRP0.AND.QNV(ICR).EQ.NXTRP0)THEN
        TBIN(N)=TBIN(N)+S
        IF(QNV(JCR).EQ.NXTRP0)WGHT=WGHT*TSUM
      ENDIF
      SBIN(N)=SBIN(N)+WGHT                        !NO BIN AVERAGE
      GO TO 131
C
C  ABORT
 1002 CONTINUE
      WRITE(6,1107)
 1107 FORMAT(/' ******WARNING, UNEXPECTED END OF DATA IN SR.EMISS !!!!'
     X,/)
C      GO TO 1001
C
 1000 CONTINUE
C
C**********************
C  GO AND READ NEW FILE
C**********************
C
      CLOSE(MR)                !MRU=MR
      IFILE=IFILE+1
      IC1=IFILE/10
      IC2=IFILE-10*IC1
      IC0=ICHAR('0')
      IC1=IC1+IC0
      IC2=IC2+IC0
C
      IF(BFORM)THEN
        FILNAM='o'//CHAR(IC2)
        IF(IFILE.GE.10)FILNAM='o'//CHAR(IC1)//CHAR(IC2)
        INQUIRE(FILE=FILNAM,EXIST=EX)
        IF(EX)OPEN(MR,FILE=FILNAM)
      ELSE
        FILNAM='o'//CHAR(IC2)//'u'
        IF(IFILE.GE.10)FILNAM='o'//CHAR(IC1)//CHAR(IC2)//'u'
        INQUIRE(FILE=FILNAM,EXIST=EX)
        IF(EX)OPEN(MRU,FILE=FILNAM,FORM='UNFORMATTED')
      ENDIF
      IF(EX)GO TO 331
C
      IF(.NOT.BXTRP)GO TO 1001
C
C NOW EXTRAP
C
      NV1=NXTRP0+1
C
      DO 135 M=1,NBIN1
C
      IF(TBIN(M).EQ.DZERO)GO TO 135
C
      DEL=EBIN(M+1)
C
      S=TBIN(M)*NXTRP0**3
C
      N1=M
      DE2=DZ/NXTRP0
      DE2=DE2*DE2
C
      DO 231 NX=NV1,NXTRP
C
        DE1=DE2
        DE2=DZ/NX
        DE2=DE2*DE2
        DEL=DEL+DE1-DE2
C
        WGHT=S/NX**3
C
        IF(IWGHT0.GT.0)WGHT=WGHT*DEL**IWGHT0
        if(iunits.lt.0)del=done/del                  !wavelength moments
C
        QWGHT=WGHT
        QDEL=DEL
C
        DO MU=0,MUMAX
          QMU(MU)=QMU(MU)+QWGHT
          QWGHT=QDEL*QWGHT
        ENDDO
C
        if(nbin1.le.0)go to 231
        if(iunits.lt.0)del=done/del                      !back to energy
c
        IF(DEL.GT.EBIN(NBIN))GO TO 135
C
        DO N=N1,NBIN1
          IF(DEL.GT.EBIN(N).AND.DEL.LE.EBIN(N+1))THEN
            SBIN(N)=SBIN(N)+WGHT
            N1=N
            if(bxtrp1.and.de2.lt.ebin(n+1)-ebin(n))go to 232
            GO TO 231
          ENDIF 
        ENDDO
C
 231  CONTINUE
c
      if(bxtrp1)then
        nx=nxtrp
        del=del+de2
        if(del.gt.ebin(nbin))go to 135
        do n=n1,nbin1
          if(del.gt.ebin(n).and.del.le.ebin(n+1))then
            n1=n
            go to 232
          endif
        enddo
        stop 'why?'
      endif
c
 232  if(bxtrp1)then                                 !complete bound sum
c
        sbin(n1)=sbin(n1)+(nx-1)*wght/2
c
        if(bxtrp2)then                            !extrap into continuum
          wght0=s*ebin(n1)**4/(2*dz*dz)
          iwght=-4
          if(iwght0.gt.0)iwght=iwght+iwght0
          do n=n1,nbin1
            wght=wght0*ebin(n)**iwght
            sbin(n)=sbin(n)+wght*(ebin(n+1)-ebin(n))
          enddo
        endif
c
      endif
C
 135  ENDDO
C
C
 1001 CONTINUE
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE BISECT(X,N,IL,IU,P,J)
C
C FIND INDEX J OF POINT P IN MONOTONIC ARRAY X(I:I=1,N).
C
C IL,IU CAN BE USED TO RESTRICT INITIAL BOUNDS
C IF WE HAVE INFORMATION FROM A PREVIOUS RELATED CALL, SAY.
C ELSE
C SET IL=0 AND IU=N+1
C THEY ARE NOT REDEFINED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      LOGICAL BUP
C
      DIMENSION X(N)
C
      IF(P.LE.X(1))THEN
        J=0
        IF(P.EQ.X(1))J=1
        RETURN
      ELSEIF(P.GE.X(N))THEN
        J=N
        IF(P.EQ.X(N))J=N-1
        RETURN
      ENDIF
C
      BUP=X(N).GE.X(1)
C
      JL=MAX(IL,0)
      JU=MIN(IU,N+1)
C
  1   IF(JU-JL.GT.1)THEN
        JM=(JU+JL)/2
        IF(BUP.EQV.(P.GE.X(JM)))THEN
          JL=JM
        ELSE
          JU=JM
        ENDIF
        GO TO 1
      ENDIF
C
      J=JL
C
      RETURN
C
      END
