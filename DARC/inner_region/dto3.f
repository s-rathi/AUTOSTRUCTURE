C NRB                          PROGRAM DTO3                  11/09/09
C
c based-on kab's d2h of Dec 2002.
c
c  Converts DARC to RMBP file format: DSTG2.DAT to RECUPH.DAT and RECUPD.DAT.

c  ifile = unit number for DSTG2.DAT input
c  INAST = number of N+1 electron symmetries on file: needs to be defined
C          but can be set large, e.g. 999, to obtain all (stg3 moans a bit) NRB.
c  nproc = number of RECUPH.DAT files to be written (=1 for serial codes).
c
      INCLUDE 'PARAM'
c
      logical ok
      real*8 jtot
      data ifile/30/, INAST/mzslp/, nproc/1/
      namelist/SYM/jtot,npty,ncfgp
c
c if DSTG2.INP exists, then look to see how many symmetries present
c
      INQUIRE (FILE='DSTG2.INP',EXIST=ok)
      if (ok) then
        print*,' '
        print*,'Checking symmetries on file DSTG2.INP...'
        print*,' '
        OPEN(UNIT=ifile,STATUS='OLD',FORM='FORMATTED',FILE='DSTG2.INP')
        inast=0
        do i=1,mzslp
   1      jtot=-1.
          read(ifile,SYM,end=100)
          if(jtot.ge.0.)then
            inast=inast+1
            print *,'Symmmetry',inast,'  JTOT=',jtot,' NPTY=',npty
            go to 1
          else
            go to 100                   !exit at first empty SYM
          endif
        enddo
      endif
  100 continue
      print*,' '
c
c interactive (comment-out for batch)
c
      print*,'Enter number of N+1 electron symmetries on DSTG2.DAT and'
      print*,'number of RECUPH.DAT files to write (=1 for serial codes)'
      print*,'>'
c
      read(5,*)INAST,nproc
c
c batch (can be commented-out)
c
c      print*,'This is the batch version '
c     x      ,'(uncomment read*,INAST,nproc for interactive use)'
c      print*,' '
c      if(.not.ok)
c     xstop 'Cannot find DSTG2.INP, user must set INAST manually'
c      print*,'Taking',inast,' N+1 electron symmetries on DSTG2.DAT and'
c      print*,nproc,' RECUPH.DAT files to write (=1 for serial codes)'
c
      if( INAST.le.0 .or. nproc.le.0 ) stop 'inast and/or nproc invalid'

c Open dstg2.f file for reading:

      INQUIRE (FILE='DSTG2.DAT',EXIST=ok)
      if (.not.ok) then
        print*,'STOP: file DSTG2.DAT does not exist'
        stop
      endif
      OPEN(UNIT=ifile,STATUS='OLD',FORM='UNFORMATTED',FILE='DSTG2.DAT')

      CALL DTO3(INAST,nproc,ifile)
      stop
      end

c-----------------------------------------------------------------------
      SUBROUTINE DTO3(INAST,nproc,IFILE)

c  Converts DARC to RMBP file format: DSTG2.DAT to RECUPH.DAT and RECUPD.DAT.

c  The following need to be predefined:
c  INAST = number of N+1 electron symmetries on file
c  ifile = unit number for DSTG2.DAT input
c  nproc = number of RECUPH.DAT files to be written (=1 for serial codes).

      IMPLICIT REAL*8(A-H,O-Z)

c  DSTG2.DAT is the file from the DARC program dstg2.f, 
c  and can contain both Hamiltonian and dipole matrix elements.

c  The DTO3 routine reads this file and creates file RECUPH.DAT with
c  the Hamiltonians, and RECUPD.DAT with the dipoles if present,
c  in the format required by the BPRM program stg3r.f
c  (the format was defined by Berrington et al 1995 in CPC 92,290-420).

c  It also converts the Buttle correction to 'MJS' style.
c  It also detects any external-region contributions to the dipole matrix
c  (these can now be calculated in dstg2.f with some mods by kab).

c   DARC style Buttle correction written to DBUT.DAT (case MJS=0) for
c   automatic read by stgf.

c Include the dimension parameters MZLR2,MZNR2,MZNC2,MZLMX,MZCHF,MZSLP,MZTAR
c using the Badnell (stg3r.f etc) convention:

      INCLUDE 'PARAM'

c This could be done using the DARC dstg2.f dimension plants as follows:
c      PARAMETER (MZLR2={NK})
c      PARAMETER (MZNR2={NB})
c      PARAMETER (NXNC2={NC})
c      PARAMETER (MZCHF={CH})
c      PARAMETER (MZTAR={NL})
c      PARAMETER (MZLMX=4)
c      PARAMETER (MZSLP=100)

c Derive further parameters to set dimensions.
c The biggest array here is work, whose size=MZNR2*MZNC2*MZCHF*MZCHF/2.

      PARAMETER (INML=MZNR2/MZNC2, JNML=MZNC2/MZNR2, KNML=INML+JNML)
      PARAMETER (ND24=MZNR2*INML/KNML+MZNC2*JNML/KNML)
      PARAMETER (kccc=(MZCHF*(MZCHF+1))/2)
      PARAMETER (NBUT=2*(MZNR2-1))
      parameter (zero=0.0)

      CHARACTER IHED*80,RECORD*20
      CHARACTER*1 NUM(0:9)

      logical outer,bnew

      DIMENSION 
     *          MAXFUL(MZLR2), LAG(MZLR2), KAG(MZLR2), JAG(MZLR2),
     *          NCONAT(MZTAR), K2P(MZCHF), L2P(MZCHF),
     *          MBUT(MZLR2), ksym(MZCHF,MZCHF),
     *          ILSPI(3,MZSLP), temp(MZLR2)
      DIMENSION AC(MZCHF,MZCHF), BLC(MZCHF,MZCHF), BVC(MZCHF,MZCHF),
     *          EBUT(NBUT,MZLR2),CBUT(NBUT,MZLR2),CF(MZCHF,MZCHF,MZLMX),
     *          e(NBUT), f(NBUT), eigmax(MZLR2),wrkcc(MZNR2,MZNR2,kccc),
     *          wrkcb(MZNR2,MZNC2,MZCHF),wrkbb(MZNC2),wrkd(ND24,ND24,2)

      COMMON/COM1/ENDS(MZNR2,MZLR2),ENAT(MZTAR),
     *            EIGENS(MZNR2,MZLR2),COEFF(3,MZLR2)
      COMMON/COM2/JAT(MZTAR),LPTY(MZTAR), MAXNLG(MZLR2),
     *            MAXNQN(MZLR2), MINNQN(MZLR2)
      COMMON/COM3/NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,ICODE,LAM,IZESP,
     *            JRELOP,maxnc,NAST,isat,NIX,RA,BSTO,HINT,eta

c  MJS   = 1 (default) for producing Buttle fit;
c        = 0 for dump on DBUT.DAT (this also occurs if fit fails).
c  outer = true (default) for outer region contributions, if any;
c        = false for inserting zeros.
c  jfile = 32 (default) unit number for RECUPD.DAT output.
c  kfile = 33 (default) unit number for RECUPH.DAT output.
c  iprint= 1 (default) for printing useful stuff on standard output;
c        = 0 for emergency printing only.
c  The other parameters set here should be acceptable for BPRM.

      data MJS/1/, outer/.TRUE./, jfile/32/, kfile/33/, iprint/1/
      data ICODE/25/, IZESP/0/, JRELOP/1/, maxnc/0/, isat/0/, NSPN/0/
      data ABUTL/0.0/, ABUTV/0.0/, BBUTL/0.0/, BBUTV/0.0/, eta/0.0/
      DATA NUM/'0','1','2','3','4','5','6','7','8','9'/

c  Check dimensions for number of symmetries input

      if( INAST.gt.MZSLP ) then
        print*,'STOP! increase dimension MZLSP to:',MZSLP
        stop
      endif

c
c nrec=number of symmetries per file
c
      nrec=inast/nproc
      if(nrec.le.0)nrec=1

c  Open Hamiltonian output file and info file
      if(nproc.eq.1)then
        OPEN( UNIT=jfile, STATUS='UNKNOWN', FORM='UNFORMATTED',
     *      FILE='RECUPH.DAT')
      endif
      OPEN(99,file='sizeBP.dat',status='unknown',form='formatted')

c-----------------------------------------------------------------------
c Read header from DSTG2.DAT file

c Some of the variables are DARC-specific and do not seem to correspond
c to anything in BPRM, eg speed of light CL, so we will try to ignore them.

c On the other hand, there are some variables that BPRM would like,
c which the DARC file does not have, eg. LAMAX, so we will try to derive them.

c Along the way, we will also check dimensions.

      rewind ifile
      READ (ifile) IHED,RECORD
      if(iprint.ge.1)print*,  IHED,RECORD
      print*,' '
      READ (ifile) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      READ (ifile) LAMBB,LAMBC,LAMCC

      LRANG1 = KX
      LRANG2 = KCMAX                ! but starts at KCMIN
      LAMAX  = LAMBB
      LAM    = 1
      if( LAMBC.gt.0 ) LAM = 2
      if( LAMCC.gt.0 ) LAM = 3

      if( LRANG1.gt.MZLR2 .or. LRANG2.gt.MZLR2  ) then
        print*,'STOP! Increase dimension MZLR2 to:',max(LRANG1,LRANG2)
        stop
      endif
      if( LAMAX.gt.MZLMX ) then
        print*,'STOP! Increase dimension MZLMX to:',LAMAX
        stop
      endif
      if( NRANG2.gt.MZNR2 ) then
        print*,'STOP! Increase dimension MZNR2 to:',NRANG2
        stop
      endif

      IF( KBMAX.GT.0 ) THEN
        READ (ifile) (MINNQN(L),L=1,KX), (MAXNQN(L),L=1,KX),
     *               (MAXNLG(L),L=1,KX), (MAXFUL(L),L=1,KBMAX),
     *               (LAG(L),L=1,KX), (KAG(L),L=1,KX), (JAG(L),L=1,KX)
      ELSE
        READ (ifile) (MINNQN(L),L=1,KX), (MAXNQN(L),L=1,KX),
     *               (MAXNLG(L),L=1,KX), (MAXFUL(L),L=1,KX),
     *               (LAG(L),L=1,KX),(KAG(L),L=1,KX), (JAG(L),L=1,KX)
      ENDIF

      READ (ifile) RA,BSTO,HINT,CL,DR1
      READ (ifile) NIX
      READ (ifile) (IRX,I=1,NIX)
      READ (ifile) NUMORB

c Write header to standard RECUPH.DAT hamilitonian output file

      if(nproc.eq.1)then
        WRITE(jfile) NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,ICODE,LAM,IZESP,
     *               JRELOP,JRELOP,JRELOP
        WRITE(jfile) (MAXNqn(L),L=1,LRANG1), (MAXNLG(L),L=1,LRANG1),
     *               (minnqn(l)-1,L=1,LRANG1)
      endif

      IF (KCMIN.GT.1) THEN
        DO L = 1,KCMIN-1
          DO N=1,NRANG2
            ENDS(N,L)=ZERO
            EIGENS(N,L)=ZERO
          ENDDO
          if(nproc.eq.1)then
            WRITE(jfile) (EIGENS(N,L),N=1,NRANG2)
            WRITE(jfile) (ENDS(N,L),N=1,NRANG2)
          endif
        ENDDO
      ENDIF
c Finish reading header

      DO L = KCMIN,KCMAX
        READ (ifile) (EIGENS(N,L),N=1,NRANG2)
        if(nproc.eq.1)WRITE(jfile) (2*EIGENS(N,L),N=1,NRANG2) !a.u. to Ryd
        eigmax(L) = EIGENS(NRANG2,L)
        READ (ifile) (ENDS(N,L),N=1,NRANG2)
        if(nproc.eq.1)WRITE(jfile) (ENDS(N,L),N=1,NRANG2)
        READ (ifile) MBUT(L)
        READ (ifile) (EBUT(I,L),I=1,MBUT(L))
        READ (ifile) (CBUT(I,L),I=1,MBUT(L))
      ENDDO

      READ (ifile) NAST
      if(NAST.gt.MZTAR)then
        print *,'STOP! Increase dimension MZTAR to:',NAST
        stop
      endif
      READ (ifile) (JAT(I),I=1,NAST)
      READ (ifile) (ENAT(I),I=1,NAST)

c Print target states

      if( iprint.ge.1 ) then
        print*,'NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,RA,NAST:',
     *          NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,RA,NAST
        print*,' '
        print*,'Target states:'
        do i = 1,nast
          LPTY(i) = 0
          if( JAT(I).lt.0 ) LPTY(i) = 1
          JAT(i) = abs(JAT(i))-1
          aj = 0.5*JAT(i)
          if( LPTY(i).eq.1 ) aj = -aj
          ee = 2.0*(enat(i)-enat(1))
          if( i.eq.1 ) write(6,1001) aj,enat(1)
          if( i.gt.1 ) write(6,1002) aj,ee,ee*13.6057
        enddo
 1001   format(f5.1,'    0.0',1pe17.9,' au')
 1002   format(f5.1,2f12.6,' eV')
      endif

C  FIT BUTTLE CORRECTION AND STORE FIT PARAMETERS IN  COEFF(I,L),I=1,3

      IF (KCMIN.GT.1) THEN
        DO L = 1,KCMIN-1
          COEFF(1,L) = zero
          COEFF(2,L) = zero
          COEFF(3,L) = zero
        ENDDO
      ENDIF

      IF (BSTO.NE.ZERO .OR. MJS.EQ.0) GOTO 90

C  IF MJS=1 and BSTO=0 USE IMPROVED FITTING PROCEDURE 
c  (M J SEATON, J.PHYS.B20(1987)L69-72) - remember to convert to Ryd!

      COEFF(3,1)=-30000.      !FLAG NEWBUT CASE KCMIN.GT.1
      DO L = KCMIN,KCMAX
        imax = MBUT(L)
        do i=1,imax
          e(i) = 2*EBUT(i,L)  !BECAUSE BUTFIT REDEFINES E
          f(i) = CBUT(i,L)    !UNNECESSARY AS F NOT REDEFINED
        enddo
        emax = 2*eigmax(L)
        CALL BUTFIT(imax,e,f,RA,emax,ALPHA,BETA,nNBUT,DELTA)
        if(iprint.gt.1)print*,'Buttle',L,nNBUT,ALPHA,BETA
        IF (nNBUT.le.0) THEN
          print*,'NEW BUTFIT DIVERGING, USE SUBROUTINE INTBUT IN STGF/B'
          goto 90
        ENDIF
        COEFF(1,L) = ALPHA
        COEFF(2,L) = BETA
        COEFF(3,L) = -10000*nNBUT
      ENDDO
      goto 120

C  IF MJS=0 OR IF BUTFIT DIVERGING, dump Buttle corrections into DBUT.DAT 
C  (STGF CANNOT HANDLE A MIXTURE OF OLD AND NEW BUTTLE FITS): keep as a.u.

   90 OPEN(31,FILE='DBUT.DAT',FORM='UNFORMATTED')
C
      IF (KCMIN.GT.1) THEN
        DO L = 1,KCMIN-1
          MBUT(L) = 1
          EBUT(1,L) = zero
          CBUT(1,L) = zero
          eigmax(L) = zero
        ENDDO
      ENDIF
C
      DO L = 1, LRANG2
        COEFF(1,L) = zero
        COEFF(2,L) = zero
        COEFF(3,L) = zero
        WRITE (31) MBUT(L)
        WRITE (31) (EBUT(N,L),N=1,MBUT(L))
        WRITE (31) (CBUT(N,L),N=1,MBUT(L))
      ENDDO
c 1031 format(1p100e20.12)
      if(iprint.ge.0)print*,
     *  'Note, Buttle correction for INTBUT routine put in DBUT.DAT'
  120 continue

c Finish writing header
      NIX=0                                       !TO SKIP READ OF WERNERS'
      if(nproc.eq.1)then
        WRITE(jfile) RA,BSTO,HINT,eta,eta,NIX
        WRITE(jfile) ((COEFF(I,L),I=1,3),L=1,LRANG2)
        WRITE(jfile) NAST
        WRITE(jfile) (ENAT(I),I=1,NAST), (JAT(I),I=1,NAST),
     *               (lpty,I=1,NAST), (LPTY(I),I=1,NAST)
      endif

      IPOLPH = 1
      if(iprint.ge.1)then
        print*,' '
        print*,'N+1 electron symmetries:'
      endif
c-----------------------------------------------------------------------
c  Loop over each total symmetry:
      iam=-1
      do loop = 1, INAST

        bnew=nproc.gt.1.and.mod(loop-1,nrec).eq.0
        if(bnew)then
          if(iam.ge.0)close(jfile)
          iam=iam+1
          i1=iam/100
          i2=(iam-100*(iam/100))/10
          i3=iam-(100*(iam/100))-i2*10
          OPEN(UNIT=jfile, STATUS='UNKNOWN', FORM='UNFORMATTED',
     *    FILE='RECUPH'//NUM(i1)//NUM(i2)//NUM(i3))
          CALL HEAD(jfile)                       !write header to each file
        endif


        READ(ifile,end=99) J2P1,NPTY,NCFGP

c  The following IF structure allows automatic detection of where the
c  dipole matrices start: it is only used if you have set INAST too big.

        if( IPOLPH.eq.2 .and. J2P1.ge.0 ) then
          if(iprint.ge.1)print*, J2P1,NPTY,NCFGP
          backspace ifile        
          go to 10
        endif

        if( NCFGP.gt.MZNC2 ) then
          print*,'STOP! Increase dimension MZNC2 to:', NCFGP
          stop
        endif

        READ(ifile) MNP1,NCHAN
        if(iprint.ge.1)print*,loop,') J2P,NPTY,NCFGP,MNP1,NCHAN:',
     *                             iabs(J2P1)-1,NPTY,NCFGP,MNP1,NCHAN
        if( NCHAN.gt.MZCHF ) then
          print*,'STOP! Increase dimension MZCHF to:', NCHAN
          stop
        endif

C   CPB  create  sizeBP.dat for pstg3r allocations
        write(99,*)NCHAN,MNP1-NCFGP,MNP1,
     A            ' 2J= ',abs(J2P1)-1,' PI ',NPTY

c Initialize ksym pointer to symmetric hamilitonian matrix elements

        kcc = 0
        do i = 1, NCHAN
          do j = 1,i
            kcc = kcc + 1
            ksym(i,j) = kcc
            ksym(j,i) = kcc
          enddo
        enddo

        READ(ifile) (NCONAT(I),I=1,NAST)
        READ(ifile) (K2P(I),I=1,NCHAN)          !kappa

        MORE   = 1
        if( loop.eq.INAST ) MORE = 0            !case INAST/nproc.ne.integer
        if(mod(loop,nrec).eq.0)MORE=0

        if (J2P1.lt.0) then
          IPOLPH = 2
          J2P1 = -J2P1
        endif
        do i=1,NCHAN                            !NRB
          L2P(I) = 2*K2P(I)                     !2*kappa
          if(K2P(I).lt.0) L2P(I) = -L2P(I)-1    !K, analogue of L
          L2P(I)=L2P(I)-1                       !K-1
        enddo
        if(iprint.gt.1)print*,'L2P:',(L2P(i),i=1,NCHAN)

        ILSPI(1,loop) = J2P1*NPTY
        ILSPI(2,loop) = MNP1
        ILSPI(3,loop) = NCHAN
        if( NPTY.gt.0 ) NPTY = 0
        if( NPTY.lt.0 ) NPTY = 1

        WRITE(jfile) J2P1-1,NSPN,NPTY,NCFGP,IPOLPH
        WRITE(jfile) MNP1,NRANG2*NCHAN,NCHAN
        WRITE(jfile) (NCONAT(I),I=1,NAST)
        WRITE(jfile) (L2P(I),I=1,NCHAN),(K2P(I),I=1,NCHAN) !kappa not used
        WRITE(jfile) MORE

c Asymptotic coeffs

        READ (ifile) IKMAX
        IF (IKMAX.GT.0) THEN
          DO LAM = 1,IKMAX
            READ (ifile) ((CF(J,I,LAM),J=1,NCHAN),I=1,NCHAN)
          ENDDO
        ENDIF
        IF(IKMAX.LT.LAMAX)THEN
          DO LAM=IKMAX+1,LAMAX
            DO I=1,NCHAN
              DO J=I,NCHAN
                CF(J,I,LAM)=ZERO
              ENDDO
            ENDDO
          ENDDO
        ENDIF

c  Hamiltonian matrix

C  Continuum-continuum blocks

        DO ICH = 1,NCHAN
          DO JCH = ICH,NCHAN
            READ (ifile) I,J
            kcc = ksym(I,J)
            IF (I.EQ.J) THEN
              READ (ifile) ((wrkcc(M,N,kcc),N=M,NRANG2),M=1,NRANG2)
c  Note, the diagonal block needs to be symmetrised
              do M=1,NRANG2
                do N=M,NRANG2
                  wrkcc(N,M,kcc)=wrkcc(M,N,kcc)
                enddo
              enddo
c              print*,kcc,i,j,ich,jch,(wrkcc(M,M,kcc),M=1,NRANG2)
            ELSE
              READ (ifile) ((wrkcc(M,N,kcc),N=1,NRANG2),M=1,NRANG2)
            ENDIF
          ENDDO
        ENDDO
c  Note, transpose is written out, because input is on one side of the
c  triangle (JCH = ICH,NCHAN) and output is on other side (JCH = 1,ICH)
        DO ICH = 1,NCHAN
          DO JCH = 1,ICH
            kcc = ksym(JCH,ICH)
            WRITE(jfile) ((wrkcc(N,M,kcc),N=1,NRANG2),M=1,NRANG2)
          ENDDO
        ENDDO

        IF (NCFGP.GT.0) THEN

C  Continuum-bound blocks

          DO ICH = 1,NCHAN
            READ (ifile) I,((wrkcb(M,N,I),N=1,NCFGP),M=1,NRANG2)
          ENDDO
          DO ICH = 1,NCHAN
            WRITE(jfile) ((wrkcb(M,N,ICH),N=1,NCFGP),M=1,NRANG2)
          ENDDO

C  Bound-bound elements

          DO M = 1,NCFGP
            READ (ifile) (wrkbb(N),N=M,NCFGP)
            WRITE(jfile) (wrkbb(N),N=M,NCFGP)
          ENDDO
        ENDIF

c  Asymptotic coefficients

        if( LAMAX.GT.0 ) then
          do I = 1, NCHAN
            WRITE(jfile) ((2*CF(J,I,K),K=1,LAMAX),J=I,NCHAN)  !2* NRB
          enddo
        endif

        INAST2 = loop
      enddo

c-----------------------------------------------------------------------
c  Dipole matrix

      if( IPOLPH.le.1 ) goto 99
   10 continue

c    To be implemented...
      if(nproc.gt.1)then
        print *,'Using a single RECUPD.DAT dipole file....'
        print *,'Re-run with 1 RECUPH.DAT file if necessary.'
      endif

c  Open dipole matrix output file
 
      OPEN( UNIT=kfile, STATUS='UNKNOWN', FORM='UNFORMATTED',
     *      FILE='RECUPD.DAT')

c Loop over pairs of symmetries for dipole

      do loop2 = 2, INAST2
       do loop1 = 1,loop2-1
        MJ    = ILSPI(1,loop1)
        MMNP2 = ILSPI(2,loop1)
        MCHAN = ILSPI(3,loop1)
        NJ    = ILSPI(1,loop2)
        MNP2  = ILSPI(2,loop2)
        NCHAN = ILSPI(3,loop2)
        mcfg  = MMNP2-MCHAN*NRANG2
        ncfg  = MNP2 -NCHAN*NRANG2
        jdif  = (abs(MJ)-abs(NJ))/2
        if( MJ*NJ.lt.0 .and. abs(jdif).le.1 ) then
          if(iprint.ge.1)print*,'Dipole transition 2J+1:',mj,nj

c Selection rules ok.  read/write dipole matrix elements

          I = -2
          J = -2

c Continuum-continuum dipole

          do nch = 1, NCHAN
            m2 = 0
            do mch = 1, MCHAN
              if( I.eq.-2 .and. J.eq.-2 ) READ (ifile,end=98) I,J,K,L
c              print*,i,j,k,l
              IF (I.EQ.-1 .AND. J.EQ.-1) GOTO 20
              if( I.eq.nch .and. J.eq.mch ) then
                I = -2
                J = -2
                READ (ifile) ((wrkd(M,N,1),M=1,K),N=1,L)
                READ (ifile) ((wrkd(M,N,2),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,1),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,2),M=1,K),N=1,L)
              else
                WRITE(kfile) (zero,M=1,NRANG2*NRANG2)
                WRITE(kfile) (zero,M=1,NRANG2*NRANG2)
              endif
              m2 = m2 + NRANG2
            enddo
            if( m2.lt.MMNP2 ) then
              if( I.eq.-2 .and. J.eq.-2 ) READ (ifile,end=98) I,J,K,L
c              print*,i,j,k,l
              IF (I.EQ.-1 .AND. J.EQ.-1) GOTO 20
              if( I.eq.nch .and. J.eq.0 ) then
                if( mcfg.ne.L ) then
                  print *,'STOP: mcfg and L must be the same:',mcfg,L
                  stop
                endif
                I = -2
                J = -2
                READ (ifile) ((wrkd(M,N,1),M=1,K),N=1,L)
                READ (ifile) ((wrkd(M,N,2),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,1),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,2),M=1,K),N=1,L)
              else
                WRITE(kfile) (zero,M=1,NRANG2*mcfg)
                WRITE(kfile) (zero,M=1,NRANG2*mcfg)
              endif
            endif
            WRITE(kfile) ((ABUTL,M=1,NRANG2),N=1,MCHAN)
            WRITE(kfile) ((ABUTV,M=1,NRANG2),N=1,MCHAN)
          enddo
 
c Bound-continuum dipole

          if( ncfg.gt.0 ) then
            do mch = 1, MCHAN
              if( I.eq.-2 .and. J.eq.-2 ) READ (ifile,end=98) I,J,K,L
c              print*,i,j,k,l
              IF (I.EQ.-1 .AND. J.EQ.-1) GOTO 20
              if( I.eq.0 .and. J.eq.mch ) then
                if( ncfg.ne.K ) then
                  print *,'STOP: ncfg and K must be the same:',ncfg,K
                  stop
                endif
                I = -2
                J = -2
                READ (ifile) ((wrkd(M,N,1),M=1,K),N=1,L)
                READ (ifile) ((wrkd(M,N,2),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,1),M=1,K),N=1,L)
                WRITE(kfile) ((wrkd(M,N,2),M=1,K),N=1,L)
              else
                WRITE(kfile) (zero,M=1,ncfg*NRANG2)
                WRITE(kfile) (zero,M=1,ncfg*NRANG2)
              endif
            enddo

c Bound-bound dipole

            if( mcfg.gt.0 ) then
              if( I.eq.-2 .and. J.eq.-2 ) READ (ifile,end=98) I,J,K,L
c              print*,i,j,k,l
              IF (I.EQ.-1 .AND. J.EQ.-1) GOTO 20
              if( I.eq.0 .and. J.eq.0 ) then
                if( mcfg.ne.L .or. ncfg.ne.K ) then
                  print *,'STOP: mcfg,ncfg and L,K=',mcfg,L,ncfg,K
                  stop
                endif
                I = -2
                J = -2
                READ (ifile) ((wrkd(M,N,1),M=1,K),N=1,L)
                READ (ifile) ((wrkd(M,N,2),M=1,K),N=1,L)
              else
                do N=1,mcfg
                  do M=1,ncfg
                    wrkd(M,N,1)=zero
                    wrkd(M,N,2)=zero
                  enddo
                enddo
              endif
              NDIMEN = NRANG2
              NTIMES = (ncfg-1)/NDIMEN + 1
              n2 = 0
              do nt = 1,NTIMES
                n22 = min(n2+NDIMEN,ncfg)
                WRITE(kfile) ((wrkd(M,N,1),M=n2+1,n22),N=1,mcfg)
                WRITE(kfile) ((wrkd(M,N,2),M=n2+1,n22),N=1,mcfg)
                n2 = n22
              enddo
            endif
          endif

c Write zeros for dipole matrix Buttle correction

   20     READ (ifile,end=98) I,J,K,L
c          print*,i,j,k,l
          if( I.ne.-1 .and. J.ne.-1 .and. K.ne.-1 .and. L.ne.-1 ) then
            print*,'WARNING: dipole matrices not finished off:',i,j,k,l
          endif
          n2 = NCHAN*NRANG2
          if( n2.LT.MNP2 ) then
            WRITE(kfile) ((ABUTL,J=1,MNP2-n2),I=1,MCHAN)
            WRITE(kfile) ((ABUTV,J=1,MNP2-n2),I=1,MCHAN)
          endif
          do MCH = 1,MCHAN
            WRITE(kfile) ((BBUTL,J=1,NCHAN),I=1,NRANG2)
            WRITE(kfile) ((BBUTV,J=1,NCHAN),I=1,NRANG2)
          enddo
          m2 = MCHAN*NRANG2
          if( m2.LT.MMNP2 ) then
            WRITE(kfile) ((BBUTL,J=1,NCHAN),I=1,MMNP2-m2)
            WRITE(kfile) ((BBUTV,J=1,NCHAN),I=1,MMNP2-m2)
          endif
          WRITE(kfile) ((BBUTL,J=1,NCHAN),I=1,MCHAN)
          WRITE(kfile) ((BBUTV,J=1,NCHAN),I=1,MCHAN)

c CLEBSCH GORDAN COEFFICIENTS FOR THE POLARIZABILITY CALC

          READ (ifile) MAXM1
          if(MAXM1.gt.MZLR2)then
            print *,'STOP! Increase dimension MZLR2 to:',MAXM1
            stop
          endif

          READ (ifile) (temp(M),M=1,MAXM1)
          WRITE(kfile) MAXM1,(temp(M),M=1,MAXM1)

c ANGULAR INTEGRALS NEEDED FOR OUTER REGION INTEGRATION

          if(outer) then
            READ (ifile,end=96) ((AC(I,J),J=1,MCHAN),I=1,NCHAN)
            READ (ifile) ((BLC(I,J),J=1,MCHAN),I=1,NCHAN)
            READ (ifile) ((BVC(I,J),J=1,MCHAN),I=1,NCHAN)
            if(iprint.ge.1)print*,'outer region contributions found'
            go to 97
          endif
   96     print*,'outer region contributions not found: zeros inserted'
          do j=1,MZCHF
            do i=1,MZCHF
              AC(i,j)  = zero
              BLC(i,j) = zero
              BVC(i,j) = zero
            enddo
          enddo
   97     WRITE(kfile) ((AC (I,J),J=1,MCHAN),I=1,NCHAN)
          WRITE(kfile) ((BLC(I,J),J=1,MCHAN),I=1,NCHAN)
          WRITE(kfile) ((BVC(I,J),J=1,MCHAN),I=1,NCHAN)
        endif
       enddo
      enddo

   98 if(iprint.ge.1)then
        print*,' '
        print*,'RECUPD.DAT created'
      endif
      close(kfile)
   99 if(iprint.ge.1)then
        print*,' '
        if(nproc.eq.1)print*,'RECUPH.DAT created'
        if(nproc.gt.1)print*,'RECUPH.XXX created'
      endif
      close(jfile)
      close(ifile)

      stop 'Normal End'
      end

C-----------------------------------------------------------------------

      SUBROUTINE BUTFIT(IMAX,E,F,RA,EMAX,ALPHA,BETA,NBUT,DELTA)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  FITTING OF BUTTLE CORRECTIONS -- M J SEATON, J.PHYS.B20(1987)L69-72.
C
C  IMAX = NUMBER OF POINTS FOR WHICH CORRECTION CALCULATED.
C  E(I) = ENERGY POINTS
C  F(I) = BUTTLE CORRECTION
C  RA = BOUNDARY RADIUS
C  EMAX = EIGENS(NRANG2,L)
C  ALPHA, BETA = FIT PARAMETERS, NBUT =...
C  DELTA = ACCURACY ACHIEVED
C
      LOGICAL POLE
C
      DIMENSION E(IMAX),F(IMAX)
C-----------------------------------------------------------------------
C
C  INITIALISATIONS
C
      D = RA*RA
      DO 10 I = 1,IMAX
        E(I) = E(I)*D
   10 CONTINUE
      T = 0.5D0
      IF(EMAX.GT.0.0D0)T=T + RA*SQRT(EMAX)/3.141592654D0
      NBUT = T
      DELTA = 1.0D30
      ALPHA = 1.0D0
      BETA = 0.0D0
C
C START ITERATIONS FOR FIT
C
      DO 50 KK = 1,15
C
        X11 = 0.0D0
        X12 = 0.0D0
        X22 = 0.0D0
        Y1 = 0.0D0
        Y2 = 0.0D0
        DELTA0 = DELTA
        DELTA = 0.0D0
C
C    START SUM OVER POINTS I
C
        DO 40 I = 1,IMAX
          U = BETA + E(I)
C
C    CALCULATE FUNCTIONS B(U) AND C(U)
C
          B = 0.0D0
          C = 0.0D0
C    CASE OF U.GT.0.04
          IF (U.GT.0.04D0) THEN
            FK = SQRT(U)
            POLE = .FALSE.
            G = -1.5707963D0
            DO 20 N = 0,NBUT
              G = G + 3.141592654D0
              IF (ABS(FK-G).GT.0.3D0) THEN
                A = 1.0D0/ (U-G*G)
                B = B + A
                C = C - A*A
C
              ELSE
                POLE = .TRUE.
                D1 = FK - G
              ENDIF
C
   20       CONTINUE
            IF (POLE) THEN
              D2 = D1*D1
              D = 0.33333333D0*
     A            D1* (1.0D0+0.066666667D0*
     B            D2* (1.0D0+0.0952381D0*D2))
              A = 1.0D0/ (2.0D0*FK-D1)
              BB = (D+A)/FK
              D = 0.33333333D0* (1.0D0+
     A            D2* (0.2D0+0.031746032D0*D2))
              C = 2.0D0*C + 0.5D0* (D-A*A-BB)/U
              B = 2.0D0*B + BB
C
            ELSE
              T = TAN(FK)
              TK = T/FK
              B = 2.0D0*B + TK
              C = 2.0D0*C + 0.5D0*
     A            (1.0D0+T*T-TK)/U
            ENDIF
C
C    SUM FOR U.LE..04
          ELSE
            G = -1.5707963D0
            DO 30 N = 0,NBUT
              G = G + 3.141592654D0
              A = 1.0D0/ (U-G*G)
              B = B + A
              C = C - A*A
   30       CONTINUE
C
C    CASE OF U.LT..04 AND U.GT.-.04
            IF (U.GT.-0.04D0) THEN
              B = (0.4D0*U+1.0D0)
     A            *U*0.33333333D0
     A            + B*2.0D0 + 1.0D0
              C = ((0.48571429D0*U+0.8D0)*U
     A            +1.0D0)*0.33333333D0
     B            + C*2.0D0
C
C    CASE OF U.LT.-.04
            ELSE
              FK = SQRT(-U)
              T = TANH(FK)
              TK = T/FK
              B = 2.0D0*B + TK
              C = 2.0D0*C + 0.5D0
     A            * (1.0D0-T*T-TK)/U
            ENDIF
C
          ENDIF
C
C    INCREMENT MATRICES X AND Y
          DF = F(I) - ALPHA*B
          DD = ABS(DF)
          IF (DELTA.LT.DD) DELTA = DD
          X11 = X11 + B*B
          X12 = X12 + B*C
          X22 = X22 + C*C
          Y1 = Y1 + DF*B
          Y2 = Y2 + DF*C
C
   40   CONTINUE
C
C  SOLVE EQUATIONS AND INCREMENT ALPHA AND BETA
C
C+++ MODIFICATIONS MADE BY MJS, 22.12.86.
CJZ    X12=ALPHA*X12
        DET = 1.0D0/ (X11*X22-X12*X12)
CJZ    BETA=BETA+DET*(-X12*Y1+X11*Y2)
        BETA = BETA + DET* (-X12*Y1+X11*Y2)/ALPHA
C+++ END MODIFICATIONS
        ALPHA = (X22*Y1-X12*Y2)*DET + ALPHA
C
C    CHECK CONVERGENCE
C
        IF (DELTA.LT.1.0D-4) GOTO 60
C
C  END ITERATIONS
C
   50 CONTINUE
      NBUT = -NBUT
      IF (DELTA.GT.DELTA0) NBUT = 0
C     AS PROCEDURE NOT CONVERGED OR DISTINCTLY DIVERGING - WE'88NOV17.
C
   60 CONTINUE
C
      END

C-----------------------------------------------------------------------

      SUBROUTINE HEAD(jfile)
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'PARAM'

      COMMON/COM1/ENDS(MZNR2,MZLR2),ENAT(MZTAR),
     *            EIGENS(MZNR2,MZLR2),COEFF(3,MZLR2)
      COMMON/COM2/JAT(MZTAR),LPTY(MZTAR), MAXNLG(MZLR2),
     *            MAXNQN(MZLR2), MINNQN(MZLR2)
      COMMON/COM3/NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,ICODE,LAM,IZESP,
     *            JRELOP,maxnc,NAST,isat,NIX,RA,BSTO,HINT,eta
C
c Write header to standard RECUPH.DAT hamilitonian output file
C
      WRITE(jfile) NELC,NZ,LRANG1,LRANG2,NRANG2,LAMAX,ICODE,LAM,IZESP,
     *             JRELOP,JRELOP,JRELOP
      WRITE(jfile) (MAXNqn(L),L=1,LRANG1), (MAXNLG(L),L=1,LRANG1),
     *             (minnqn(l)-1,L=1,LRANG1)
C
      DO L = 1,LRANG2
        WRITE(jfile) (2*EIGENS(N,L),N=1,NRANG2)     !a.u. to Ryd - nrb
        WRITE(jfile) (ENDS(N,L),N=1,NRANG2)
      ENDDO
C
      WRITE(jfile) RA,BSTO,HINT,eta,eta,NIX
      WRITE(jfile) ((COEFF(I,L),I=1,3),L=1,LRANG2)
      WRITE(jfile) NAST
      WRITE(jfile) (ENAT(I),I=1,NAST), (JAT(I),I=1,NAST),
     *             (isat,I=1,NAST), (LPTY(I),I=1,NAST)
C
      RETURN
      END
