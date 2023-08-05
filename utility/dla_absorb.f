C N. R. BADNELL     PROGRAM DLA_ABSORB       UoS v1.10          21/02/23
C
C***********************************************************************
C
C         POST-PROCESSOR FOR  ** AUTOSTRUCTURE **
C         ***************************************
C
C  CALCULATES LS/IC DETAILED LEVEL ACCOUTING "OPACITIES":
C  CROSS SECTIONS PER *ION* I.E. INC. UPPER/LOWER BOLTZMANN FACTOR.
C  (*** PRIOR TO V1.10, C-S WERE PER GROUND-STATE ION.***)
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
C      OPEN(7,FILE='ocs')                   !BINNED "OPACITIES"
C      OPEN(13,FILE='DLA_ABSORB_RAW')       !BINNED "OPACITIES"
C      OPEN(14,FILE='DLA_ABSORB_GAUSS')     !CONVOLUTED "OPACITIES"
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
      WRITE(0,*) 'PROGRAM DLA_ABSORB: NORMAL END'
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
      PARAMETER (NDIM1=50001)                        !MAX NO. OF BINS +1
      PARAMETER (NDIMM=NDIM1-1)
CNORM      PARAMETER (NDIM2=2*NDIM1+1)               !MAX NO. GAUSS E
C
      PARAMETER (DZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
      PARAMETER (DTWO=2.0D0)
      PARAMETER (DFOUR=4.0D0)
C
      PARAMETER (TINY=1.D-5)
C
      PARAMETER (TKAY=1.5789D5)         !KELVIN TO RYDBERGS
      PARAMETER (BOHRA=5.29177D-9)      !CM TO A.U.
C
      LOGICAL BVARY
C
      DIMENSION EBIN(NDIM1),SBIN(NDIM1),TBIN(NDIM1)
     X         ,U(0:NDIMM),V(0:NDIMM)
CNORM     X         ,TCC(NDIM2)
C
      EQUIVALENCE (EBIN(1),U(0))
C
      NAMELIST/ONE/UNITS,IWGHT,nc,NXTRP,NXTRP0,NMAX,LMAX,JCFJ,BKT
     x            ,TEE,DENE,NBOLTZ,ups
C
      NAMELIST/TWO/NBIN,EMIN,EMAX,EWIDTH,ERES,TEAPOT,NGAUSS,DELTAF
C
      PI=ACOS(-DONE)
      BVARY=.FALSE.
C                                                            !OPEN FILES
      OPEN(13,FILE='DLA_ABSORB_RAW')
C
      OPEN(7,FILE='ocs')
C
C**********************NAMELIST-ONE*************************************
C
C ***NOTHING IS COMPULSORY:
C
C  THEN THERE IS NO WEIGHTING FACTOR ON THE CROSS SECTIONS.
C  ANY SUBSEQUENT USER INPUT ENERGIES ARE ASSUMED TO BE IN UNITS, 
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
C     IWGHT  FLAGS THE WEIGHTING FACTOR FOR CROSS SECTION
C          = 0 NONE - DEFAULT
C          = 3 FOR PLANCK MEAN
C          = 4 FOR ROSSELAND MEAN (*NOT* VERY MEANINGFUL)
C          < 0 BY U(=E/BKT)**(-IWGHT) - ONLY MEANINGFUL FOR B-B, WHERE
C
C     TEE  IS THE BOLTZMANN TEMPERATURE IN KELVIN
C          THE DEFAULT IS LARGE I.E. EXP=1.
C
C     NBOLTZ=NO. OF INITIAL STATES BOLTZMANN POPULATED.
C            DEFAULT ALL.
C
C     DENE IS THE ELECTRON DENSITY IN /CM^3.
C          ONLY USED IN LINE PROFILES FOR ROSSELAND MEAN
C          DEFAULT IS "NONE', NOT GOOD FOR RM.
C
C     NC .GT. 0 READ OLD ocs FILE, DEFAULT =0.
C           SINCE THIS IS T-DEPENDENT, ONLY FOR GAUSSIAN STILL.
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
      BKT=-1.D30
      TEE=BKT
      DENE=-1
      NBOLTZ=-1
      IWGHT=0
      UNITS=DONE
      NXTRP=0
      NXTRP0=-1
      NMAX=-1
      LMAX=-1
      JCFJ=-1
      nc=0
      ups=0
C
C
      READ(5,ONE)
C
C
      DEN0=DENE
      DENE=DENE*BOHRA**3
C
      IF(TEE.GT.DZERO)THEN
        BKT=ABS(UNITS)*TEE/TKAY                              !K TO UNITS
      ELSE
        TEE=TKAY*BKT/ABS(UNITS)                              !UNITS TO K
      ENDIF
      BKT0=BKT
      IF(BKT0.LE.DZERO)THEN
        IF(IWGHT.LT.0)THEN
          WRITE(0,*)'*** ERROR: MUST SET BKT > 0 FOR IWGHT < 0'
     X              ,IWGHT,BKT
          STOP '*** ERROR: MUST SET BKT > 0 FOR IWGHT < 0'
        ENDIF
        BKT=-BKT                                          !LARGE DEFAULT
      ENDIF
C
      IF(IWGHT.GT.0.AND.IWGHT.NE.3.AND.IWGHT.NE.4)THEN
        WRITE(0,*)'*** ERROR: ILLEGAL VALUE FOR IWGHT > 0; ONLY 3 (PM)'
     X           ,' OR 4 (RM) ALLOWED:',IWGHT
        STOP '*** ERROR: ILLEGAL VALUE FOR IWGHT > 0'
      ENDIF
C
      IWGHT0=IWGHT
      IWGHT=IABS(IWGHT0)
      IF(IWGHT.EQ.4)WRITE(0,*)
     X   'THE "ROSSELAND MEAN" DETERMINED HERE USES A CRUDE BROADENING!'
C
      IF(UNITS.LT.DZERO)THEN
        IUNITS=-1
        UNITS=-UNITS
        BKT=DONE/BKT
      ELSE
        IUNITS=1
      ENDIF
      BKT=BKT/UNITS
C
      WRITE(6,1000)IWGHT0,TEE,DEN0,NBOLTZ
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
C  MINIMUM INPUT FOR BINNED LINE ABSORBPTION IS EMIN, EMAX AND AT LEAST
C  ONE OF NBIN, ERES OR EWIDTH - GAUSSIAN CONVOLUTED REQUIRES EWIDTH.
C  THEN (PREFERRED): ONLY SPECIFY EWIDTH (NOT NBIN/ERES) AND CODE
C  CHOOSES SUITABLE ENERGY BINS, WHICH MAY HAVE VARIABLE BIN WIDTH.
C 
C-----------------------------------------------------------------------
C 
C     EMIN -- MINIMUM PHOTON ENERGY/WAVELENGTH IN UNITS (DEFAULT ZERO)
C             N.B. ANY LINE EMISSION LESS THAN EMIN IS OMITTED 
C                  FROM ocs, DLA_ABSORB_RAW, DLA_ABSORB_GAUSS.
C
C     EMAX -- MAXIMUM PHOTON ENERGY/WAVELENGTH IN UNITS (DEFAULT HUGE!)
C             N.B. ANY LINE EMISSION GREATER THAN EMAX IS OMITTED
C                  FROM ocs, DLA_ABSORB_RAW, DLA_ABSORB_GAUSS.
C
C     EWIDTH--WIDTH IN UNITS FOR GAUSSIAN CONVOLUTION OF BINNED ABSORPTN
C             IF .LT. 0 THEN -EWIDTH IS THE DIMENSIONLESS FRACTIONAL
C             WIDTH, SO THE ACTUAL GAUSSIAN WIDTH IS THEN -EWIDTH*ENERGY
C             IN THIS CASE, IF NBIN/ERES ARE NOT SPECIFIED THEN THE BIN
C             WIDTH WILL VARY WITH BIN ENERGY SO AS TO MATCH THE VARYING
C             GAUSSIAN WIDTH.
C             DEFAULT, DZERO - NO CONVOLUTION.
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
C
      TEAPOT=1.D99
      NBIN=-5                                                !AGGRESSIVE
      EMIN=DZERO
      EMAX=99999.0D0
      EWIDTH=DZERO
      ERES=-DONE
      NGAUSS=0
      DELTAF=-DONE
C
C
      READ(5,TWO)
C
C
      WRITE(6,1010)UNITS,EMIN,EMAX,DELTAF,TEAPOT
C
C SET-UP EWIDTH, MIN POSSIBLE IF FRACTIONAL.
C
      EWIDTH0=EWIDTH
      IF(EWIDTH0.LT.DZERO)EWIDTH=-EWIDTH0*EMIN
C
C SET-UP BINS: ELECTRON ENERGY/WAVELENGTH BINS DEFINED BY EMIN, EMAX
C              IN USER UNITS (RESTRICTS *ALL* OUTPUT)
C
      NBIN0=NBIN
      ERES0=ERES
C
      BVARY=NBIN0.LE.1.AND.ERES0.LE.DZERO
      IF(.NOT.BVARY.OR.EWIDTH.GT.DZERO)THEN
C
        IF(EMAX.LT.EMIN.OR.EMIN.LT.DZERO)THEN
          WRITE(6,*)'EMIN, EMAX NOT SET SUITABLY FOR BINNING:',EMIN,EMAX
          STOP 'EMIN, EMAX NOT SET SUITABLY FOR BINNING!'
        ENDIF
C
        IF(BVARY)THEN                                        !USE EWIDTH
          BVARY=EWIDTH0.LT.DZERO               !ALLOW VARIABLE BIN WIDTH
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
        ELSEIF(ERES.GT.DZERO)THEN                              !USE ERES
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
        WRITE(6,1020)NBIN,ERES
C
        IF(EWIDTH.GT.DZERO)THEN
          WRITE(6,1030)EWIDTH0
          OPEN(14,FILE='DLA_ABSORB_GAUSS')
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
          SBIN(N)=DZERO
        ENDDO
C
        V(0)=-DONE                                        !FLAG E/U-MESH
C
      ELSE
C
        IF(IWGHT0.LE.0.OR.BKT0.LE.DZERO)THEN
          WRITE(6,*) 'ABSORPTION REQUIRES A BINNED ENERGY SET-UP'
          IF(BKT0.LE.DZERO)THEN
            WRITE(6,*)'SET A BOLTZMANN TEMP, BKT, TO OBTAIN V-MESH'
          ELSE
            WRITE(6,*)'SET NBIN, EMIN,EMAX'
          ENDIF
          STOP 'ABSORPTION REQUIRES A BINNED ENERGY SET-UP'
        ENDIF
        IF(UNITS.LT.DZER0)THEN
          WRITE(6,*)'V-MESH REQUIRES UNITS.GT.0'
          STOP 'V-MESH REQUIRES UNITS.GT.0'
        ENDIF
C
C SET-UP OP V-MESH
C
        VDEX=1.E-4                                   !OP CONSTANT V-MESH
        UMIN=0.001
        UMAX=20.*(IWGHT/2)
        UDEX=UMIN
        UMAX=UMAX+0.1*UDEX
C
        MU=IWGHT
        MS=MU-2           !INC STIM IN SIGMA, VMAX=1.0
        MS=MS+2*MU-7      !EXC STIM IN SIGMA, VMAX=0.92394,1.0553 MU=3,4
C
        PI=ACOS(-DONE)
        C=15/PI**4
        IF(MU.EQ.4)C=C/4

        U(0)=DZERO
        F0=DZERO
        V(0)=DZERO
        M=1
        U(1)=UMIN
C
 200    CONTINUE
C
        DEU=EXP(-U(M))
C
        F=C*U(M)**MU*DEU/(DONE-DEU)**MS
        V(M)=V(M-1)+F*(U(M)-U(M-1))
c
c        write(66,1001)u(m),f,v(m)
c 1001 format(2(1pe12.4),e14.6)
C
        M=M+1
        IF(M.GE.NDIM1)STOP 'INCREASE NDIM1'
C                                       !ESTIMATE DU TO GIVE CONSTANT DV
        T=(F-F0)/UDEX
        UDEX=-F
        TT=F*F+2*T*VDEX
        IF(TT.GT.DZERO)UDEX=UDEX+SQRT(TT)       !to catch endgame v=vmax
        UDEX=UDEX/T
        U(M)=U(M-1)+min(u(m-1),UDEX)              !to catch start-up u=0
        F0=F
C
        IF(U(M).LE.UMAX)GO TO 200
C
        NBIN=M
        NBIN1=NBIN-1
        DO N=1,NBIN
          EBIN(N)=EBIN(N)*BKT0                   !AS INTERNAL BIN IS RYD
        ENDDO
        EMIN=EBIN(1)
        EMAX=EBIN(NBIN)
C
        WRITE(6,1019)
        WRITE(6,1020)NBIN,VDEX
c
c        call flush(66)
c        stop 'v-mesh test'
C
      ENDIF
C
c
c read previous ocs files, and convolute with Gaussian only.
c
      if(nc.gt.0)then
        if(ngauss.le.0)ngauss=2*nbin+1
        read(7,1060,end=78)nbin
        nbin1=nbin-1
        read(7,1070)(ebin(n),n=1,nbin)
        read(7,1070)(sbin(n),n=1,nbin1)
        smax=dzero
        do n=1,nbin1
          smax=max(smax,sbin(n))
        enddo
  78    close(7)
        go to 77
      endif
C
C CONVERT TO INTERNAL UNSCALED RYDBERGS
C
      NUMR=1                                         !FLAG FOR SR.ABSORB
      IF(IUNITS.LT.0.AND.NBIN.GT.0)THEN     !FIRST REVERSE IF WAVELENGTH
        NUMR=-1
        IF(EMIN.EQ.DZERO)EMIN=TINY
        IF(EMAX.EQ.DZERO)EMAX=TINY
        E=EMIN
        EMIN=DONE/EMAX
        EMAX=DONE/E
        IF(EBIN(1).EQ.DZERO)EBIN(1)=(EBIN(3)-EBIN(2))/1000
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
C
      CALL ABSORB(DELTAF,TEAPOT,NBIN,EMIN,EMAX,EBIN,SBIN
     X           ,NXTRP0,NXTRP,NMAX,LMAX,JCFJ,NUMR,BKT,NBOLTZ)
C
C
      WRITE(6,1050)NUMR
C
C BROADEN
C
      IF(IWGHT.EQ.4)THEN                    !.or.iwght.eq.3, test planck
       if(ups.gt.dzero)then
        DO N=1,NBIN1
          TBIN(N)=SBIN(N)*(EBIN(N+1)-EBIN(N))
          SBIN(N)=DZERO
        ENDDO
        GAM=DENE*2*SQRT(PI/BKT)*UPS
        WRITE(6,1007)GAM*UNITS
        DE=EBIN(NBIN)-EBIN(NBIN1)
        IF(GAM/DE.LT.5)THEN
          WRITE(0,*)'*** WARNING: BIN WIDTH TOO LARGE FOR LORENTZ WIDTH'
        ENDIF
        GAM2=GAM*GAM
        PIG=GAM/PI
        DO M=1,NBIN1
          T=TBIN(M)
          IF(T.GT.DZERO)THEN
            E0=EBIN(M)
            DO N=1,NBIN1                      !tbd: restrict based on e0
              PHI=((EBIN(N)-E0)**2+GAM2)/PIG
              SBIN(N)=SBIN(N)+T/PHI
            ENDDO
          ENDIF
        ENDDO
       elseif(iwght.eq.4)then                       !case planck allowed
        DO N=1,NBIN1
          SBIN(N)=SBIN(N)*(EBIN(N+1)-EBIN(N))
        ENDDO
       endif
       if(iwght.eq.4)then                           !case planck allowed
        DO N=1,NBIN1
          IF(SBIN(N).GT.DZERO)SBIN(N)=DONE/SBIN(N)
        ENDDO
       endif
      ENDIF
C
C ABSORPTION PER ION (SO, CM^2 STILL)
C
      IF(V(0).LT.DZERO)THEN                                      !U-MESH
        OP=CONVOLP(IWGHT0,BKT,EBIN,SBIN,1,NBIN1)
      ELSE                                                       !V-MESH
        OP=DZERO
        DO N=1,NBIN1
          SBIN(N)=SBIN(N)*(V(N)-V(N-1))                   !ABSORB WEIGHT
          OP=OP+SBIN(N)
          SBIN(N)=SBIN(N)*BKT/(EBIN(N+1)-EBIN(N))        !=(U(N)-U(N-1))
        ENDDO
        OP=OP/DTWO
      ENDIF
C
      IF(IWGHT0.EQ.3)THEN
        WRITE(6,1041)OP
      ELSEIF(IWGHT0.EQ.4)THEN
        IF(OP.GT.DZERO)OP=DONE/OP
        WRITE(6,1042)OP
      ELSE
        WRITE(6,1033)IWGHT,OP
      ENDIF
C
C CONVERT BACK TO USER UNITS - DON'T FORGET TO REVERSE SBIN AS WELL.
C
      DO N=1,NBIN
        EBIN(N)=EBIN(N)*UNITS
      ENDDO
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
C FIND SMAX FOR RE-NORM TO CONVOLUTED
C
      SMAX=DZERO
      DO N=1,NBIN1
        SMAX=MAX(SMAX,SBIN(N))
      ENDDO
C
C CONVOLUTE WITH GAUSSIAN (USE A LINEAR ENERGY MESH)
C
  77  IF(EWIDTH.GT.DZERO)THEN
C
        IF(NGAUSS.LT.0)THEN
          NGAUSS=-NGAUSS-1
          IF(NGAUSS.LT.5)NGAUSS=20
          DEG=ABS(EWIDTH)/NGAUSS
          T=(EBIN(NBIN)-EBIN(1))/DEG
          NT=NINT(T)
        ELSEIF(NGAUSS.LT.50)THEN
          IF(EWIDTH.GT.DZERO)THEN
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
        WRITE(14,1040)BKT0,EWIDTH0,NT
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
        IF(EWIDTH0.LT.DZERO)EWIDTH=-EWIDTH0*EBIN(NBIN)    !NOW MAX POSS.
        DE=(EBIN(NBIN)-EBIN(NBIN1))
        T=2*ABS(EWIDTH)/DE                      !O.K. EVEN IF NON-LINEAR
        N0=NINT(T)                              !*2   !TEST
c        W=DEG/DE                               !for old linear ebin
        nn=1
        E0=EBIN(1)
        TMAX=DZERO
C
        DO N=1,NT
          E=E0+(N-1)*DEG
          IF(EWIDTH0.LT.DZERO)EWIDTH=-EWIDTH0*E
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
      WRITE(13,1039)BKT0,NBIN1
C
      IF(EWIDTH.LE.DZERO)THEN
        SNORM=DONE
      ELSE
        IF(SMAX.NE.DZERO)THEN
          SNORM=TMAX/SMAX
        ELSE
          SNORM=TMAX
        ENDIF
        WRITE(13,1038)SNORM
        CLOSE(14)
      ENDIF
C                                         NORMALIZE BINNED TO CONVOLUTED
      DO N=1,NBIN1
        WRITE(13,1090)EBIN(N+1),SBIN(N)*SNORM
      ENDDO
C
C WRITE-OUT (UNNORMED) BINNED ABSORPTION
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
 1000 FORMAT(' IWGHT=',I3,5X,'T(K)=',1PE10.3,5X,'N_E(/CM^3)=',1PE10.3
     X,5X,'NBOLTZ=',I7)
 1005 FORMAT(/' NXTRP=',I3,5X,'NMAX=',I5,3X,'LMAX=',I5,5X,'JCFJ=',I5)
 1007 FORMAT(/' LORENTZ BROADENING WIDTH = ',1PE12.4)
 1010 FORMAT(/' UNITS=',1PE14.7,5X,'EMIN=',F10.3,3X,'EMAX=',F10.3,3X
     X,'DELTAF=',1PD8.1,3X,'TEAPOT=',E12.5)
 1019 FORMAT(/' V-MESH IN USE:')
 1020 FORMAT(/' BINNED CROSS SECTIONS WRITTEN: NBIN=',I6/22X
     X,' WITH BIN WIDTH=',1PE12.4)
 1030 FORMAT(/' AND CONVOLUTED WITH EWIDTH=',1PE10.3,
     X    ' FWHM GAUSSIAN DISTRIBUTION')
 1033 FORMAT(//' TOTAL LINE ABSORPTION:'//' SIG*E^',I1,3X,'=',1PE10.3)
 1038 FORMAT('#',' NORMALIZED TO CONVOLUTED BY FACTOR',1PD12.5)
 1039 FORMAT('# TOTAL PHOTOABSORPTION FOR KT=',1PE10.3/
     X'#',20X,'AT',I7,' ENERGIES')
 1040 FORMAT('# TOTAL PHOTOABSORPTION FOR KT=',1PE10.3/
     X'#       CONVOLUTED WITH ',1PE10.3,' FWHM GAUSSIAN DISTRIBUTION'/
     X'#',20X,'AT',I7,' ENERGIES')
 1041 FORMAT(/' PLANCK MEAN / ION (DENSITY) =',1PE10.3,' CM^2')
 1042 FORMAT(/' ROSSELAND MEAN / ION (DENSITY) =',1PE10.3,' CM^2')
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
      PARAMETER (DZERO=0.0D0)
      PARAMETER (DTWO=2.0D0)
C
      DIMENSION EBIN(*),SBIN(*)
C
      A=1.6651092D0/EWIDTH                                !2*sqrt(ln(2))
      SUM=DZERO
C
      IF(EWIDTH.GT.DZERO)THEN                                    !BINNED
        DO I=N1,N2
          IF(SBIN(I).GT.DZERO)THEN
            XI=EBIN(I)
            XI1=EBIN(I+1)
            SUM=SUM+SBIN(I)*(ERF(A*(E-XI))-ERF(A*(E-XI1)))/DTWO
          ENDIF
        ENDDO
      ELSE                                       !LORENTZIANS (NOT USED)
        A=-A
        DO I=N1,N2                                        !ASSUME LINEAR
          IF(SBIN(I).GT.DZERO)THEN
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
      REAL*8 FUNCTION CONVOLP(IWGHT0,BKT,EBIN,SBIN,N1,N2)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  IWGHT0=3 : CONVOLUTE CROSS SECTIONS WITH PLANCK MEAN DISTRIB AT T=BKT
C  IWGHT0=4 : DITTO, BUT FOR CRUDE ROSSLAND MEAN.
C
C   THE CROSS SECTIONS ALREADY INCLUDE BOLTZMANN AND SO ARE FOR SAID 
C   T=BKT ONLY. SO THE FINAL RESULT IS FOR A SINGLE TEMP ONLY. THUS,
C   WE ABSORB APPROPRIATE PLANCK FUNCTION INTO SBIN FOR RAW PRINT FILE.
C
C  IWGHT0<0 : CONVOLUTE CROSS SECTIONS WITH (E/T)**(-IWGHT0) (B-B ONLY).
C
      PARAMETER (DZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
      PARAMETER (DTWO=2.0D0)
C
      DIMENSION EBIN(*),SBIN(*)
C
      C=15/ACOS(-DONE)**4
C
      SUM=DZERO
C
      IF(IWGHT0.EQ.3)THEN                                   !PLANCK MEAN
C
        DO N=N1,N2
          IF(SBIN(N).GT.DZERO)THEN
            TU=EBIN(N+1)/BKT
            WGHT=C*TU**3*EXP(-TU)                   !PLANCK CANCELS S.E.
            SBIN(N)=WGHT*SBIN(N)                          !ABSORB PLANCK
            SUM=SUM+SBIN(N)*(EBIN(N+1)-EBIN(N))
          ENDIF
        ENDDO
C
      ELSEIF(IWGHT0.EQ.4)THEN                          !"ROSSELAND MEAN"
C
        C=C/4
C
        DO N=N1,N2
          IF(SBIN(N).GT.DZERO)THEN
            TU=EBIN(N+1)/BKT
            DEU=EXP(-TU)
            WGHT=C*TU**4*DEU/(DONE-DEU)**3
            SUM=SUM+WGHT*SBIN(N)*(EBIN(N+1)-EBIN(N))
          ENDIF
        ENDDO
C
      ELSEIF(IWGHT0.LT.0)THEN                   !MULT BY ENERGY U-FACTOR
C
        IWGHT=-IWGHT0
        DO N=N1,N2
          TU=EBIN(N+1)/BKT
          SBIN(N)=SBIN(N)*TU**IWGHT                       !ABSORB WEIGHT
          SUM=SUM+SBIN(N)*(EBIN(N+1)-EBIN(N))
        ENDDO
C
      ELSEIF(IWGHT0.EQ.0)THEN                                !UNWEIGHTED
C
        DO N=N1,N2
          SUM=SUM+SBIN(N)*(EBIN(N+1)-EBIN(N))
        ENDDO
C
      ENDIF
C
      SUM=SUM/(DTWO*BKT)
C
      CONVOLP=SUM
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE ABSORB(DELTAF,TEAPOT,NBIN,EMIN,EMAX,EBIN,SBIN
     X                 ,NXTRP0,NXTRP,NMAX,LMAX,JCFJ,NUMR,BKT,NBOLTZ)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (NDIM1=50001)                       !MAX NO. OF BINS +1
      PARAMETER (NDIM14=5000)                       !NO OF CONFIGS
      PARAMETER (MXORB0=50)                         !NO. NL READ FROM AS
      PARAMETER (NDIM26=MXORB0)
      PARAMETER (NLIT=60)
C
      PARAMETER (DZERO=0.0D0)
      PARAMETER (DONE=1.0D0)
      PARAMETER (DTWO=2.0D0)
C
      PARAMETER (CON1=8.0325D9)                     !ALPHA^3/(2*TAU_0)
      PARAMETER (CON2=8.06728D-18)                  !ALPHA*(2*PI*A0)^2
C
      LOGICAL BFORM,BUNFORM,EX,BFAST,BXTRP,BXTRP0,bxtrp1,bxtrp2
C
      CHARACTER FILNAM*4,CMBLNK*4        !,CMSTAR*4
      CHARACTER*1 CLIT(NLIT)             !,CLABL(20)
C
      INTEGER QNV(NDIM14),QLV(NDIM14),QS0(10),QL0(10)
     X,QN(NDIM26),QL(NDIM26),QND(NDIM26),QLD(NDIM26)
C
      ALLOCATABLE :: JK(:)
C
      DIMENSION EBIN(*),SBIN(*)
      DIMENSION LIT(NLIT),TBIN(0:9,NDIM1)
C
      DATA CLIT /'1','2','3','4','5','6','7','8','9','A','B','C','D','E'
     X,'F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U',
     X 'V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k',
     X 'l','m','n','o','p','q','r','s','t','u','v','w','x','y'/
      DATA CMBLNK/'    '/
C
      PI=ACOS(-DONE)
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
      IF(NBOLTZ.LE.0)NBOLTZ=1 999 999 999
      NBOLTZ0=NBOLTZ
      NBZ=1
      ALLOCATE(JK(NBZ))
C
      IUNITS=NUMR
      NUMR=0
      NBIN1=NBIN-1
C
      BXTRP=NXTRP.GT.0
      IF(BXTRP)THEN
        DO N=1,NBIN1
          do l=0,9
            TBIN(l,N)=DZERO
          enddo
        ENDDO
        lx0=0
        bxtrp1=nmax.ge.100
        bxtrp2=nmax.ge.1000
      ENDIF
C
C SKIP TESTS FOR PRODUCTION RUN, SAVE BIN EMIN,EMAX.
C (THESE TESTS ARE JUST THAT, NOT NORMALLY USED FOR PRODUCTION RUN.)
C
      BFAST=iabs(NMAX).Ge.100.AND.LMAX.GT.100.AND.TEAPOT.GT.1.D5.AND.
     X      DELTAF.LE.DZERO.AND.JCFJ.GT.1000
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
      BUNFORM=.NOT.BFORM
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
      IF(BUNFORM)READ(MRU,END=1000)NV,LV
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
C TO NXTRP, I.E. B-B ONLY, ALL USING THE SAME BKT/BIN.
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
        IF(BUNFORM)READ(MRU,END=1002)
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
        lx0=max(lx0,ql(k))
C
      ENDDO
C
C**************************
C READ AUTOIONIZATION RATES
C**************************
C
      IF(BFORM)READ(MR,103,END=1002)
      IF(BUNFORM)READ(MRU,END=1002)
      IF(BFORM)READ(MR,103,END=1002)
 103  FORMAT(A1)
      I=0
 111  I=I+1
C
      IF(BFORM)READ(MR,112,END=1002)I1,I2,IWA,JCA,I3,T1,T2,EION
      IF(BUNFORM)READ(MRU,END=1002)I1,I2,IWA,JCA,I3,T1,T2,EION
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
      IF(BFORM)READ(MR,121,END=1002)NENG,EZERO
      IF(BUNFORM)READ(MRU,END=1002)NENG,EZERO
 121  FORMAT(10X,I5,45X,F15.6)
C
      IF(NENG.GT.NBZ)THEN
        NBZ=NENG
        DEALLOCATE(JK)
        ALLOCATE(JK(NBZ))
        JK=NENG+1
      ENDIF
      NBOLTZ=MIN(NENG,NBOLTZ0)
C
      IF(BFORM)READ(MR,105,END=1002)MTEST
      IF(BUNFORM)READ(MRU,END=1002)MTEST
 105  FORMAT(26X,A4)
C
      WSUM=DZERO
      DO I=1,NBOLTZ
C
        IF(BFORM)READ(MR,123,END=1002)IK,IT,ISS,LL,JJ,LCF,ENERG
        IF(BUNFORM)READ(MRU,END=1002)IK,IT,ISS,LL,JJ,LCF,ENERG
C
        M=IK
        IF(M.GT.0)THEN                              !SKIP ANY CORR.
          JK(M)=I
          IF(MTEST.EQ.MBLNK)THEN                    !LS/CA
            W=IABS(ISS)*(2*LL+1)                    !CA: LL=0, W=SS
          ELSE                                      !IC
            W=JJ+1
          ENDIF
          WSUM=WSUM+W*EXP(-ENERG/BKT)
        ENDIF
C
      ENDDO
C
      DO I=NBOLTZ+1,NENG
C
        IF(BFORM)READ(MR,123,END=1002)
        IF(BUNFORM)READ(MRU,END=1002)
 123    FORMAT(5X,6I5,F15.6)
C
      ENDDO
C
C*********************
C READ RADIATIVE RATES
C*********************
C
      IF(BFORM)READ(MR,104,END=1002)NZTEST
      IF(BUNFORM)READ(MRU,END=1002)NZTEST,NDUME
 104  FORMAT(66X,I2)
C
      IF(BFORM)READ(MR,103,END=1002)
C
      I=0
 131  I=I+1
      IF(BFORM)READ(MR,132,END=1002) ICR,I1,IWR,JCR,I3,JWR,T1,DEL,ELOW
      IF(BUNFORM)READ(MRU,END=1002)ICR,I1,IWR,JCR,I3,JWR,T1,DEL,ELOW
 132  FORMAT(6I5,1PE15.5,2(0PF15.6))
C
      IF(I1.EQ.0)THEN
        if(iwr.eq.0)then
          NUMR=NUMR+I-1
          EX=.FALSE.
          GO TO 31                             !GO AND READ NEW NL BLOCK
        else                                !read Patrick's *.osc format
          read(mr,133)del,ielow,wj,ieup,wi,t1
  133     format(f12.3,i11,4x,f5.1,i11,4x,f5.1,8x,e11.2)
          bform=.false.
          iwr=nint(2*wi+1)
          jwr=nint(2*wj+1)
          t1=t1/iwr
          elow=ielow/109737.37
c          eup=ieup/109737.37
          if(del.gt.dzero)then
            del=911.26667/del
          else
            numr=i-1
            go to 1001                      !we are done
          endif
        endif
      ENDIF
C
C***********************
C PROCESS RADIATIVE DATA
C***********************
C
      I=I-1
      IF(DEL.LT.EMIN.OR.DEL.GT.EMAX)GO TO 131       !OUTSIDE RANGE
      IF(JK(I3).GT.NBOLTZ)GO TO 131                 !UNPOPULATED
C
      IF(BFAST)THEN                                 !SKIP FURTHER TESTS
C
        GF=ABS(T1)*IWR/(CON1*DEL*DEL)               !SYMM OSCILLATOR
C
      ELSE
C
        EUP=DEL+ELOW                                !ONLY TEST NON-EXTRP
        IF(EUP.GT.TEAPOT)GO TO 131                  !UPPER AUTOIONIZING
        IF(QNV(ICR).GT.iabs(NMAX))GO TO 131         !OUTSIDE RANGE
        IF(QLV(ICR).GT.LMAX)GO TO 131               !   "      "
        IF(ICR.GT.JCFJ)GO TO 131                    !   "      "
        IF(JCR.GT.JCFJ)GO TO 131                    !   "      "
        GF=ABS(T1)*IWR/(CON1*DEL*DEL)               !SYMM OSCILLATOR
        IF(GF.LT.DELTAF)GO TO 131                   !TOO SMALL
C
      ENDIF
C
      I=I+1
      SIG=CON2*GF/WSUM                             !PABS (CM^2)*JWR/W0
      BEXP=EXP((EZERO-ELOW)/BKT)                    !BOLTZMANN FACTOR
      T=SIG*BEXP
C
      CALL BISECT(EBIN,NBIN,0,NBIN+1,DEL,N)
C
      IF(BXTRP0.AND.QNV(ICR).EQ.NXTRP0)THEN
        l=qlv(jcr)
        TBIN(l,N)=TBIN(l,N)+T
        IF(QNV(JCR).EQ.NXTRP0)T=T*TSUM
      ENDIF
      SBIN(N)=SBIN(N)+T
C
      GO TO 131
C
C  ABORT
 1002 CONTINUE
      WRITE(6,1107)
 1107 FORMAT(/' ******WARNING, UNEXPECTED END OF DATA IN SR.ABSORB !!!!'
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
      DO M=1,NBIN1
      do 135 l=0,lx0
C
      IF(TBIN(l,M).EQ.DZERO)GO TO 135
C
      DEL=EBIN(M+1)
C
      SIG0=TBIN(l,M)*NXTRP0**3/DEL
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
        IF(DEL.GT.EBIN(NBIN))GO TO 135
C
        SIG=SIG0*DEL/NX**3
C
        DO N=N1,NBIN1
          IF(DEL.GT.EBIN(N).AND.DEL.LE.EBIN(N+1))THEN
            SBIN(N)=SBIN(N)+SIG
            N1=N
            de=ebin(n+1)-ebin(n)
            if(bxtrp1.and.de2.lt.de)go to 232
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
      endif
c
 232  if(bxtrp1)then                                 !complete bound sum
c
        np=n1+1
        sbin(n1)=sbin(n1)+(nx-1)*sig/dtwo
c
        if(bxtrp2)then                            !extrap into continuum
          lx=l+3
          sig=sig0/(2*dz*dz)
          sig=sig*ebin(np)
          wght0=sqrt(ebin(np))*ebin(np)**lx
          do n=n1,nbin1
            np=n+1
            wght=wght0/(sqrt(ebin(np))*ebin(np)**lx)
            t=wght*sig
            de=ebin(np)-ebin(n)
            sbin(n)=sbin(n)+t*de
          enddo
        endif
c
      endif
C
 135  continue
      ENDDO
C
C
 1001 CONTINUE
C
      DEALLOCATE(JK)
C
C FINALIZE CROSS SECTION AS AN ENERGY-AVERAGE (FOR FORMAL QUADRATURE)
C
      DO N=1,NBIN1
        DE=EBIN(N+1)-EBIN(N)
        SBIN(N)=SBIN(N)/DE
      ENDDO
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
