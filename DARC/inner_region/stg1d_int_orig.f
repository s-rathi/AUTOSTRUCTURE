CEND--------------------------------------------------------------------
CEND    INTS.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      PROGRAM INTS
CRCS
CRCS $Source: /home/phn/DARC/RCS/dstg1int.mak,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:21:20 $
CRCS $Revision: 11.2 $
CRCS
C ======================================================================
C
C DARC has been developed mainly by Patrick Norrington in collaboration
C with Prof Ian Grant FRS. Any inquiries or comments on the package can
C be directed to Dr Norrington at
C
C email     : p.norrington@qub.ac.uk
C homepage  : www.am.qub.ac.uk/users/p.norrington
C
C ======================================================================
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Local variables
C
      CHARACTER*10 CODE
      CHARACTER*4 FILE
      CHARACTER*60 AUT
      CHARACTER*60 DAT
      CHARACTER*60 REV
      CHARACTER*60 SOU
      CHARACTER*8 FILE1
      CHARACTER*8 FILE2
      CHARACTER EMPTY
      CHARACTER INPREC(80)
      INTEGER I
      INTEGER INPUNI
      INTEGER IREAD
      INTEGER IWRITE
      INTEGER LENREC
      INTEGER IDMTST(30)
      INTEGER NDIMAX(30)
      LOGICAL EX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA EMPTY/' '/
      DATA CODE/'DSTG1-INTS'/
      DATA FILE/'INTS'/
      DATA INPUNI/9/
C-----------------------------------------------------------------------
C
C  Set the stream numbers for :
C
C  (1) reading from input data (IREAD)
C  (2) writing to lineprinter  (IWRITE)
C
      IREAD = 5
      IWRITE = 7
C
      FILE1 = FILE//'.OUT'
      FILE2 = FILE//'.INP'
C
C  Open IWRITE file
C
      OPEN (UNIT=IWRITE,FILE=FILE1,STATUS='UNKNOWN')
C
C  Set version information
C
      SOU = '$Source: /home/phn/DARC/RCS/dstg1int.mak,v $'
      AUT = '$Author: phn $'
      DAT = '$Date: 2001/12/05 19:21:20 $'
      REV = '$Revision: 11.2 $'
C
C  Write out title
C
      PRINT 3000,SOU,AUT,DAT,REV,CODE
      WRITE (IWRITE,3000) SOU,AUT,DAT,REV,CODE
C
      PRINT 3010
C
C  Check if the input data file is available
C
      INQUIRE (FILE=FILE2,EXIST=EX)
C
      IF (.NOT.EX) THEN
        PRINT 3060,FILE2
        CALL DMSET1(IWRITE, IDMTST, NDIMAX)
        WRITE (IWRITE,4010)
        WRITE (IWRITE,4020)
        WRITE (IWRITE,4030)
        WRITE (IWRITE,4040)
        WRITE (IWRITE,4060)
        WRITE (IWRITE,4070)
        WRITE (IWRITE,4080)
        WRITE (IWRITE,4090)
        WRITE (IWRITE,4100)
        WRITE (IWRITE,4110)
        WRITE (IWRITE,4120)
        STOP
      ENDIF
C
C  Open the input data file
C
      OPEN (UNIT=IREAD,FILE=FILE2,STATUS='OLD',FORM='FORMATTED')
C
C  Copy the input file to a scratch file
C  This allows the input data to be printed
C
      OPEN (UNIT=INPUNI,STATUS='SCRATCH',FORM='FORMATTED',
     +ACCESS='SEQUENTIAL')
C
      WRITE (IWRITE,3040)
   10 CONTINUE
      READ (IREAD,3020,END=30) INPREC
      DO I = 80,1,-1
        IF (INPREC(I).NE.EMPTY) THEN
          LENREC = I
          GOTO 20
        ENDIF
      ENDDO
      LENREC = 1
   20 CONTINUE
      WRITE (INPUNI,3020) (INPREC(I),I=1,LENREC)
      WRITE (IWRITE,3030) (INPREC(I),I=1,LENREC)
      GOTO 10
   30 CONTINUE
      REWIND INPUNI
      WRITE (IWRITE,3050)
C
C  Use the scratch file for the input data
C
      IREAD = INPUNI
C-----------------------------------------------------------------------
C
C  Now call the rest of the program
C
      CALL AAINTS(IREAD, IWRITE)
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/1X,A60/1X,A60/1X,A60/1X,A60/1X,71('*')//
     +' DARC, the Dirac Atomic R-matrix Codes.'/
     +' homepage: www.am.qub.ac.uk/DARC/'/1X,A10,' module'/
     +' Coded in double precision FORTRAN'//
     +' Any inquiries or comments on the code can be directed to'/
     +' Dr Patrick Norrington at :'//
     +' email     : p.norrington@qub.ac.uk'/
     +' homepage  : www.am.qub.ac.uk/users/p.norrington')
 3010 FORMAT (/' start of program')
 3020 FORMAT (80A1)
 3030 FORMAT (1X,80A1)
 3040 FORMAT (/1X,71('*')/' Input data follows'/1X,71('*'))
 3050 FORMAT (1X,71('*'))
 3060 FORMAT (/1X,A9,' does not exist ... STOPPING.')
 4010 FORMAT (
     + ' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +/' DSTG1-INTS evaluates the radial integrals.'
     +/' '
     +/' Routine GEN controls the calculation of the radial integrals.'
     +/' These have the following types:'
     +/' '
     +/' Multipole'
     +/' ---------'
     +/' These are one-electron integrals which have 3 kinds:'
     +)
 4020 FORMAT (
     + ' bound-bound, bound-continuum and continuum-continuum.'
     +/' '
     +/' The bound-bound integrals are needed for',
     + ' the calculation of the'
     +/' long-range scattering potentials. These ',
     + 'potentials determine the'
     +/' solution in the external (r>RA) region.'
     +/' '
     +/' All three types are required in the calculation of'
     +/' photo-ionisation matrix elements. In this case there is a'
     +/' further subdivision of radial integral ',
     + 'into length and velocity'
     +/' form. Only dipole matrix elements are required for'
     +/' photo-ionisation so that lambda is one.'
     +)
 4030 FORMAT (
     + ' '
     +/' Continuum-continuum one-electron and Slater two-electron'
     +/' --------------------------------------------------------'
     +/' '
     +/' These are used in the evaluation of the ',
     + 'continuum-continuum part'
     +/' of the internal (r<RA) (N_e+1)-electron ',
     + 'Hamiltonian, where N_e is'
     +/' the number of target electrons.'
     +/' '
     +/' Bound-continuum one-electron and Slater two-electron'
     +/' ----------------------------------------------------'
     +/' '
     +/' These are used in the evaluation of the ',
     + 'bound-continuum part of'
     +)
 4040 FORMAT (
     + ' the internal (r<RA) (N_e+1)-electron Hamiltonian.'
     +/' '
     +/' Bound-bound one-electron and Slater two-electron'
     +/' ------------------------------------------------'
     +/' '
     +/' These are used in the evaluation of the target Hamiltonian and'
     +/' the bound-bound part of the internal (r<RA) (N_e+1)-electron'
     +/' Hamiltonian.'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +)
 4060 FORMAT (
     + ' File usage'
     +/' ----------'
     +/' '
     +/' The files are defined internally. Messages are also written to'
     +/' output stream 6 using the PRINT command.'
     +/' '
     +/' The following files are used:'
     +/' '
     +/' |------------|-----------------------------|',
     + '------------------|'
     +/' |file        |description                  |',
     + 'stream stream FORM|'
     +/' |name        |                             |',
     + 'name   number     |'
     +)
 4070 FORMAT (
     + ' |------------|-----------------------------|',
     + '------------------|'
     +/' |INTS.INP    |Input.                       |',
     + 'IREAD    5    F   |'
     +/' |INTS.OUT    |Output.                      |',
     + 'IWRITE   7    F   |'
     +/' |ORBITALS.DAT|DA file for orbitals.        |',
     + '---     11    U   |'
     +/' |INTEGRAL.DAT|DA file for radial integrals.|',
     + '---     12    U   |'
     +/' |ORBS.DAT    |DSTG1-ORBS dump.             |',
     + 'IDISC4  14    U   |'
     +/' |DSTG1.DAT   |DSTG1 dump.                  |',
     + 'IDISC5  15    U   |'
     +/' |---         |Scratch file for input data. |',
     + '---      9    F   |'
     +)
 4080 FORMAT (
     + ' |------------|-----------------------------|',
     + '------------------|'
     +/' '
     +/' DA == direct access'
     +/' F  == formatted'
     +/' U  == unformatted'
     +/' '
     +/'                        ORBITALS.DAT (created by dstg1-orbs)'
     +/'                        ORBS.DAT     (created by dstg1-orbs)'
     +/'                           |'
     +/'                           |'
     +/'                           |'
     +/'          INTS.INP ---> dstg1-ints ---> INTS.OUT'
     +/'          input            |          output'
     +)
 4090 FORMAT (
     + '                           |'
     +/'                           |'
     +/'                        INTEGRAL.DAT (created by dstg1-ints,'
     +/'                                      used by dstg2)'
     +/'                        DSTG1.DAT    (created by dstg1-ints,'
     +/'                                      used by dstg2)'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +/' Input data on stream IREAD (file: INTS.INP)'
     +/' --------------------------'
     +)
 4100 FORMAT (
     + ' '
     +/' Input record 1'
     +/' --------------'
     +/' '
     +/' NAMELIST / INTS / JTARG, LAMBB, LAMBC, LAMCC, OPT'
     +/' '
     +/' The default values are given in square brackets.'
     +/' '
     +/' JTARG  [-1] ... maximum 2J-value for the target states.'
     +/' LAMBB  [4]  ... maximum lambda-value for bound-bound multipole'
     +/'                 integrals.'
     +/' LAMBC  [1]  ... maximum lambda-value for bound-continuum'
     +/'                 multipole integrals.'
     +/' LAMCC  [1]  ... maximum lambda-value for continuum-continuum'
     +)
 4110 FORMAT (
     + '                 multipole integrals.'
     +/' OPT    [0]  ... (array) options.'
     +/' '
     +/' Print options.'
     +/' '
     +/' 15 ... print multipole radial integrals in routines INTRD and'
     +/'        INTRDV.'
     +/' 16 ... print one-electron radial integrals in routine INTRD1.'
     +/' 17 ... print two-electron radial integrals in routine INTRD2.'
     +/' 27 ... print from routine GENCC.'
     +/' '
     +/' ===================================================',
     + '=============='
     +)
 4120 FORMAT (
     + ' ===================================================',
     + '=============='
     +/' '
     +)
      END
CEND--------------------------------------------------------------------
CEND    AAINTS.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AAINTS(IREAD, IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AAINTS.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
      INTEGER NIRX
      PARAMETER (NIRX=20)
      INTEGER NBUT
      PARAMETER (NBUT=2*(MXNB-1))
C
C  Argument variables
C
      INTEGER IREAD
      INTEGER IWRITE
C
C  Local variables
C
      CHARACTER*2 LAB(MXNK)
      CHARACTER*2 ORBLAB(16)
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION CBUT(MXNK,NBUT)
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(MXP1)
      DOUBLE PRECISION CQ(MXP1)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION DUMMY(1)
      DOUBLE PRECISION EBUT(MXNK,NBUT)
      DOUBLE PRECISION EIGENS(MXNK,MXNB)
      DOUBLE PRECISION ENDS(MXNK,MXNB)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(MXNP)
      DOUBLE PRECISION TIME2
      DOUBLE PRECISION WEIGHT(MXNP)
      DOUBLE PRECISION ZCORE(MXNP)
      DOUBLE PRECISION ZNUCL(MXNP)
      DOUBLE PRECISION ZSTAT(MXNP)
      INTEGER I
      INTEGER IDISC4
      INTEGER IDISC5
      INTEGER IDMTST(30)
      INTEGER IPOS(ND15,MXNK)
      INTEGER IREC1
      INTEGER IREC2
      INTEGER IRX(NIRX)
      INTEGER ITC(50)
      INTEGER JAG(MXNK)
      INTEGER JTARG
      INTEGER KAG(MXNK)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KX
      INTEGER LAG(MXNK)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNLG(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MBUT(MXNK)
      INTEGER MINNQN(MXNK)
      INTEGER NDIMAX(30)
      INTEGER NELC
      INTEGER NIX
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
      INTEGER NZ
      LOGICAL EX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA ORBLAB/'S ','P-','P ','D-','D ','F-','F ','G-','G ','H-','H '
     +,'I-','I ','J-','J ','**'/
C-----------------------------------------------------------------------
C
C  Now make calls to:
C
C   QUARTZ ... initialise timing
C   CALEN  ... get RECORD of current time/date
C   DMSET1 ... set dimensions
C
      CALL QUARTZ(-1, IWRITE, TIME2)
      CALL CALEN(IWRITE, RECORD)
      CALL DMSET1(IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
C
C  Read the input data
C
C-----------------------------------------------------------------------
      PRINT 3050
      CALL RDINTS (IREAD, IWRITE, ITC, JTARG, LAMBB, LAMBC, LAMCC)
C-----------------------------------------------------------------------
      IREC1 = 0
      CALL DA1('ORBITALS.DAT', 1, IREC1, 11, 0, DUMMY)
C
      IREC2 = 0
      CALL DA1('INTEGRAL.DAT', 2, IREC2, 12, 0, DUMMY)
C
      INQUIRE (FILE='ORBS.DAT',EXIST=EX)
      IF (.NOT.EX) THEN
        PRINT 3070
        WRITE (IWRITE,3070)
        STOP
      ENDIF
      IDISC4 = 14
      OPEN (UNIT=IDISC4,STATUS='OLD',FORM='UNFORMATTED',FILE='ORBS.DAT')
C
      IDISC5 = 15
      OPEN (UNIT=IDISC5,STATUS='UNKNOWN',FORM='UNFORMATTED',
     + FILE='DSTG1.DAT')
C
      WRITE (IWRITE,3060)
C-----------------------------------------------------------------------
C
C  Read information from ORBS.DAT and write 1st part of DSTG1.DAT
C
C-----------------------------------------------------------------------
      PRINT 3030
      CALL RDISC4 (
     + IDISC4, IDISC5, BSTO, CBUT, CL, DR1, EBUT, EIGENS, ENDS,
     + HINT, IDMTST, IHED, IRX, IWRITE, JAG, KAG, KBMAX, KCMAX,
     + KCMIN, KX, LAG, LAMBB, LAMBC, LAMCC, MAXFUL, MAXNLG,
     + MAXNQN, MBUT, MINNQN, NDIMAX, NELC, NIX,
     + NRANG2, NUMORB, NZ, RA, RECORD)
C
C   Set the variable NPTS.
C
      NPTS = IRX(NIX)
C
C   Set the array LAB for the current case.
C
      DO I = 1,KX
        IF (I.LT.16) THEN
          LAB(I) = ORBLAB(I)
        ELSE
          LAB(I) = ORBLAB(16)
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C
C  Read information from ORBITALS.DAT
C
C  1. Store the bound orbitals in the CP and CQ arrays.
C  2. Read arrays RMESH,WEIGHT,ZSTAT,ZNUCL,ZCORE
C
C-----------------------------------------------------------------------
      PRINT 3020
      CALL RDISC1 (
     + IDMTST, IWRITE, KBMAX, LAG, MAXNQN, NDIMAX, NPTS, CP, CQ, IPOS,
     + RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT)
C-----------------------------------------------------------------------
C
C  Evaluate the radial integrals.
C
C-----------------------------------------------------------------------
      PRINT 3040
      CALL GEN (
     + CL, CP, CQ, DR1, EIGENS, HINT, IDISC5, IDMTST, IPOS, IREC2,
     + IRX, ITC, IWRITE, JAG, JTARG, KAG, KBMAX, KCMAX, KCMIN, LAB,
     + LAG, LAMBB, LAMBC, LAMCC, MAXFUL, MAXNLG, MAXNQN, MINNQN,
     + NDIMAX, NIX, NPTS, NRANG2, NUMORB, RMESH, WEIGHT, ZCORE,
     + ZNUCL, ZSTAT)
C-----------------------------------------------------------------------
      PRINT 3000
      CALL DMPRT1 (IWRITE, IDMTST, NDIMAX)
      PRINT 3010
C-----------------------------------------------------------------------
 3000 FORMAT (/' calling routine DMPRT1')
 3010 FORMAT (/' STOPPING normally')
 3020 FORMAT (/' calling routine RDISC1')
 3030 FORMAT (/' calling routine RDISC4')
 3040 FORMAT (/' calling routine GEN')
 3050 FORMAT (/' reading input data in routine RDINTS')
 3060 FORMAT (/1X,71('*')/' I/O files'/1X,71('*')/
     +' INTS.INP    : IREAD  (input data..................FORMATTED)  ',
     +' :     5'/
     +' INTS.OUT    : IWRITE (printer output..............FORMATTED)  ',
     +' :     7'/
     +'                      (scratch for input data......FORMATTED)  ',
     +' :     9'/
     +' ORBITALS.DAT:        (direct access orbitals......UNFORMATTED)',
     +' :    11'/
     +' INTEGRAL.DAT:        (direct access integrals.....UNFORMATTED)',
     +' :    12'/
     +' ORBS.DAT    : IDISC4 (DSTG1-ORBS dump.............UNFORMATTED)',
     +' :    14'/
     +' DSTG1.DAT   : IDISC5 (DSTG1 dump..................UNFORMATTED)',
     +' :    15'/1X,71('*'))
 3070 FORMAT (/' ORBS.DAT does not exist....STOPPING.')
      END
CEND--------------------------------------------------------------------
CEND    DDET5.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DDET5(F,DF,HINT,IRX,NIX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DDET5.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   F      ... array containing the function
C o DF     ... array containing the first derivative
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   NIX    ... number of subintervals
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION DH1
      PARAMETER (DH1=1.D0/12.D0)
      DOUBLE PRECISION DB1
      PARAMETER (DB1=-25.D0)
      DOUBLE PRECISION DB2
      PARAMETER (DB2=48.D0)
      DOUBLE PRECISION DB3
      PARAMETER (DB3=-36.D0)
      DOUBLE PRECISION DB4
      PARAMETER (DB4=16.D0)
      DOUBLE PRECISION DB5
      PARAMETER (DB5=-3.D0)
      DOUBLE PRECISION DC1
      PARAMETER (DC1=-3.D0)
      DOUBLE PRECISION DC2
      PARAMETER (DC2=-10.D0)
      DOUBLE PRECISION DC3
      PARAMETER (DC3=18.D0)
      DOUBLE PRECISION DC4
      PARAMETER (DC4=-6.D0)
      DOUBLE PRECISION DC5
      PARAMETER (DC5=1.D0)
      DOUBLE PRECISION EIGHT
      PARAMETER (EIGHT=8.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
C
C  Argument variables
C
      DOUBLE PRECISION F(*),DF(*)
      DOUBLE PRECISION HINT
      INTEGER IRX(*),NIX
C
C  Local variables
C
      DOUBLE PRECISION HH
      INTEGER I,IN,FIRST
      INTEGER LAST
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      FIRST = 1
      HH = DH1/HINT
C
C  loop over the subintervals
C
      DO IN = 1,NIX
C
        LAST = IRX(IN)
C
        DF(FIRST) = HH*(DB1*F(FIRST)+DB2*F(FIRST+1)+DB3*F(FIRST+2)+DB4*F!
     +(FIRST+3)+DB5*F(FIRST+4))
        DF(FIRST+1) = HH*(DC1*F(FIRST)+DC2*F(FIRST+1)+DC3*F(FIRST+2)+DC4!
     +*F(FIRST+3)+DC5*F(FIRST+4))
C
        DO I = FIRST+2,LAST-2
          DF(I) = HH*(F(I-2)+EIGHT*(-F(I-1)+F(I+1))-F(I+2))
        ENDDO
C
        DF(LAST-1) = -HH*(DC1*F(LAST)+DC2*F(LAST-1)+DC3*F(LAST-2)+DC4*F(!
     +LAST-3)+DC5*F(LAST-4))
        DF(LAST) = -HH*(DB1*F(LAST)+DB2*F(LAST-1)+DB3*F(LAST-2)+DB4*F(LA!
     +ST-3)+DB5*F(LAST-4))
C
        FIRST = LAST+1
        HH = HH*HALF
C
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    DMCHK1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMCHK1(I,ID, IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMCHK1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   I      ... is the position in the IDMTST array, which holds the
C              current array dimensions
C   ID     ... is the array size required by the data
C   IWRITE ... stream number for printed output
C   IDMTST ... array containing maximum dimensions set in the code
C o NDIMAX ... array containing maximum dimensions used in the code
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER I,ID
      INTEGER IWRITE
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (ID.GT.NDIMAX(I)) NDIMAX(I) = ID
C
      IF (ID.LE.IDMTST(I)) RETURN
C
      PRINT 3000,I,IDMTST(I),I,ID
      WRITE (IWRITE,3000) I,IDMTST(I),I,ID
C
      IF (I.EQ.3) THEN
        PRINT 3010
        WRITE (IWRITE,3010)
        GOTO 10
      ENDIF
C
      IF (I.EQ.4) THEN
        PRINT 3020
        WRITE (IWRITE,3020)
        GOTO 10
      ENDIF
C
      IF (I.EQ.6) THEN
        PRINT 3030
        WRITE (IWRITE,3030)
        GOTO 10
      ENDIF
C
      IF (I.EQ.8) THEN
        PRINT 3040
        WRITE (IWRITE,3040)
        GOTO 10
      ENDIF
C
      IF (I.EQ.11) THEN
        PRINT 3050
        WRITE (IWRITE,3050)
        GOTO 10
      ENDIF
C
      IF (I.EQ.12) THEN
        PRINT 3060
        WRITE (IWRITE,3060)
        GOTO 10
      ENDIF
C
      IF (I.EQ.13) THEN
        PRINT 3070
        WRITE (IWRITE,3070)
        GOTO 10
      ENDIF
C
      IF (I.EQ.21) THEN
        PRINT 3080
        WRITE (IWRITE,3080)
        GOTO 10
      ENDIF
C
      IF (I.EQ.22) THEN
        PRINT 3090
        WRITE (IWRITE,3090)
        GOTO 10
      ENDIF
C
      IF (I.EQ.23) THEN
        PRINT 3100
        WRITE (IWRITE,3100)
        GOTO 10
      ENDIF
C
      IF (I.EQ.24) THEN
        PRINT 3110
        WRITE (IWRITE,3110)
        GOTO 10
      ENDIF
C
   10 CONTINUE
      CALL DMPRT1(IWRITE, IDMTST, NDIMAX)
      PRINT 3120
      WRITE (IWRITE,3120)
      STOP
C
 3000 FORMAT (
     +/'               **********************'
     +/'               *** ARRAY OVERFLOW ***'
     +/'               **********************'
     +/' You must increase dimensions given by IDMTST(',I2,')=',I9
     +/'                           to at least IDMTST(',I2,')=',I9
     +)
 3010 FORMAT (
     +/' IDMTST(3) contains the dimension of the array'
     +/' RKSTO used to store integrals.'
     +/' It is set by variable MXI1 in file darc.inc.'
     +)
 3020 FORMAT (
     +/' IDMTST(4) contains the dimension of the arrays'
     +/' ISTX1 and ISTX2.'
     +/' It is set by variable MXI2 in file darc.inc.'
     +)
 3030 FORMAT (
     +/' IDMTST(6) contains the maximum number of continuum'
     +/' orbitals per angular momentum.'
     +/' It is set by variable MXNB in file darc.inc.'
     +)
 3040 FORMAT (
     +/' IDMTST(8) contains the maximum value of KX,'
     +/' the number of bound/continuum KAPPA values.'
     +/' It is set by variable MXNK in file darc.inc.'
     +)
 3050 FORMAT (
     +/' IDMTST(11) contains the dimension of arrays used to'
     +/' tabulate the mesh, orbitals and potentials.'
     +/' It is set by variable MXNP in file darc.inc.'
     +)
 3060 FORMAT (
     +/' IDMTST(12) contains the maximum number of target orbitals.'
     +/' It is set by variable MXNW in file darc.inc.'
     +)
 3070 FORMAT (
     +/' IDMTST(13) contains the maximum value of points'
     +/' that can be used for storing orbitals.'
     +/' It is set by variable MXP1 in file darc.inc.'
     +)
 3080 FORMAT (
     +/' IDMTST(21) contains the maximum value of NBT.'
     +/' This is the number of bound orbitals to which the'
     +/' continuum orbitals can be orthogonalised using'
     +/' Lagrange multipliers.'
     +/' It is set by variable ND11.'
     +)
 3090 FORMAT (
     +/' IDMTST(22) contains the maximum number of'
     +/' bound orbitals to which a continuum orbital'
     +/' can be Schmidt orthogonalised.'
     +/' It is set by variable NSMIT.'
     +)
 3100 FORMAT (
     +/' IDMTST(23) is the maximum pseudo principal quantum'
     +/' number allowed in array IPOS.'
     +/' It is set by variable ND15 which equals MXNB+MXNK/2.'
     +/' Either increase MXNB or MXNK in darc.inc or manually'
     +/' change the value of ND15 in the code.'
     +)
 3110 FORMAT (
     +/' IDMTST(24) contains the dimension of IRX and is'
     +/' the maximum number of sub-intervals allowed in'
     +/' the regular mesh.'
     +/' It is set by variable NIRX.'
     +)
 3120 FORMAT (
     +/'      *******************************************'
     +/'      *** Module terminates in routine DMCHK1 ***'
     +/'      *******************************************'
     +)
      END
CEND--------------------------------------------------------------------
CEND    DMPRT1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMPRT1(IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMPRT1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... stream number for printed output
C   IDMTST ... array containing maximum dimensions set in the code
C   NDIMAX ... array containing maximum dimensions used in the code
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IWRITE
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
      WRITE (IWRITE,3010) IDMTST(03),NDIMAX(03)
      WRITE (IWRITE,3020) IDMTST(04),NDIMAX(04)
      WRITE (IWRITE,3030) IDMTST(06),NDIMAX(06)
      WRITE (IWRITE,3040) IDMTST(08),NDIMAX(08)
      WRITE (IWRITE,3050) IDMTST(11),NDIMAX(11)
      WRITE (IWRITE,3060) IDMTST(12),NDIMAX(12)
      WRITE (IWRITE,3070) IDMTST(13),NDIMAX(13)
      WRITE (IWRITE,3080) IDMTST(21),NDIMAX(21)
      WRITE (IWRITE,3090) IDMTST(22),NDIMAX(22)
      WRITE (IWRITE,3100) IDMTST(23),NDIMAX(23)
      WRITE (IWRITE,3110) IDMTST(24),NDIMAX(24)
C-----------------------------------------------------------------------
 3000 FORMAT (/31X,'Routine DMPRT1'/31X,'--------------'//              !
     +' The following information relates to the dimensions set in the',!
     +' code.'/)
 3010 FORMAT (' IDMTST(03)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXI1')
 3020 FORMAT (' IDMTST(04)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXI2')
 3030 FORMAT (' IDMTST(06)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNB')
 3040 FORMAT (' IDMTST(08)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNK')
 3050 FORMAT (' IDMTST(11)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNP')
 3060 FORMAT (' IDMTST(12)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNW')
 3070 FORMAT (' IDMTST(13)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXP1')
 3080 FORMAT (' IDMTST(21)   set to : ',I9,'   maximum used : ',I9)
 3090 FORMAT (' IDMTST(22)   set to : ',I9,'   maximum used : ',I9)
 3100 FORMAT (' IDMTST(23)   set to : ',I9,'   maximum used : ',I9)
 3110 FORMAT (' IDMTST(24)   set to : ',I9,'   maximum used : ',I9)
      END
CEND--------------------------------------------------------------------
CEND    DMSET1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMSET1(IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMSET1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... stream number for printed output
C o IDMTST ... array containing maximum dimensions set in the code
C o NDIMAX ... array containing maximum dimensions used in the code
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER ND11
      PARAMETER (ND11=12)
      INTEGER NSMIT
      PARAMETER (NSMIT=10)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
      INTEGER NIRX
      PARAMETER (NIRX=20)
C
C  Argument variables
C
      INTEGER IWRITE
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
C
C  Local variables
C
      INTEGER I
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO I = 1,30
        NDIMAX(I) = -1
        IDMTST(I) = -1
      ENDDO
C-----------------------------------------------------------------------
C
C   Write out information on the dimensions set.
C
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
      WRITE (IWRITE,3010)
      WRITE (IWRITE,3020) MXI1
      WRITE (IWRITE,3030) MXI2
      WRITE (IWRITE,3040) MXNB
      WRITE (IWRITE,3050) MXNK
      WRITE (IWRITE,3060) MXNP
      WRITE (IWRITE,3070) MXNW
      WRITE (IWRITE,3080) MXP1
      WRITE (IWRITE,3090)
C-----------------------------------------------------------------------
      IDMTST(03) = MXI1
      IDMTST(04) = MXI2
      IDMTST(06) = MXNB
      IDMTST(08) = MXNK
      IDMTST(11) = MXNP
      IDMTST(12) = MXNW
      IDMTST(13) = MXP1
      IDMTST(21) = ND11
      IDMTST(22) = NSMIT
      IDMTST(23) = ND15
      IDMTST(24) = NIRX
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/                                              !
     +' routine DMSET1 : the code has been dimensioned as follows'/1X,71!
     +('*'))
 3010 FORMAT (/' The following dimensions have been set :'/)
 3020 FORMAT (1X,I9,' = I1  radial integrals')
 3030 FORMAT (1X,I9,' = I2  marks position of radial integrals')
 3040 FORMAT (1X,I9,' = NB  continuum orbitals per KAPPA value')
 3050 FORMAT (1X,I9,' = NK  KAPPA values')
 3060 FORMAT (1X,I9,' = NP  points in radial mesh')
 3070 FORMAT (1X,I9,' = NW  target orbitals')
 3080 FORMAT (1X,I9,' = P1  mesh points used to store orbitals')
 3090 FORMAT (/1X,71('*'))
      END
CEND--------------------------------------------------------------------
CEND    DQSF.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DQSF(K,X,Y,ASY,RMESH,DR1,HINT,IRX,NIX,NPTS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DQSF.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   K      ... is the order
C   X      ... is the function in (1/r) Y^k(X;r)
C o Y      ... is the result i.e. (1/r) Y^k(X;r)
C   ASY    ... is the asymptotic form of the function (1/r) Y^k(X;r)
C              when k=0. ASY is used to correct the outward integration.
C              The inward integration is corrected by requiring the
C              inward function to be zero for the last two points.
C   RMESH  ... array containing regular radial mesh
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION THIRD
      PARAMETER (THIRD=1.D0/3.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION FOUR
      PARAMETER (FOUR=4.D0)
C
C  Argument variables
C
      DOUBLE PRECISION ASY
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION Y(*)
      INTEGER IRX(*)
      INTEGER K
      INTEGER NIX
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION CORR,HT,S
      INTEGER I,FIRST,J,LAST
C
      DOUBLE PRECISION RK(MXNP)
      DOUBLE PRECISION RK1(MXNP)
      DOUBLE PRECISION TMP(MXNP)
      DOUBLE PRECISION YA(MXNP)
      DOUBLE PRECISION YB(MXNP)
      INTEGER KLAST
C
      SAVE RK,RK1,KLAST
      DATA KLAST/-1/
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Store the radial arrays r^k and 1/r^(k+1)
C
      IF (K.NE.KLAST) THEN
        DO J = 1,NPTS
          RK(J) = RMESH(J)**K
        ENDDO
        DO J = 1,NPTS
          RK1(J) = ONE/(RK(J)*RMESH(J))
        ENDDO
        KLAST = K
      ENDIF
C-----------------------------------------------------------------------
C
C  First part of the integration
C
C  y_A(r) = int_0^r X(s) s^k ds
C
C-----------------------------------------------------------------------
C
C  Form the integrand
C
      DO J = 1,NPTS
        TMP(J) = X(J)*RK(J)
      ENDDO
C
      HT = HINT*THIRD
C
C  The integrand is zero at the origin
C  First point uses trapezoidal rule
C  The other points use Simpson's rule
C
      YA(1) = DR1*TMP(1)*HALF
      YA(2) = HT*(FOUR*TMP(1)+TMP(2))
      FIRST = 1
C
      DO I = 1,NIX
C
        LAST = IRX(I)
C
        DO J = FIRST+2,LAST
          YA(J) = YA(J-2)+HT*(TMP(J-2)+FOUR*TMP(J-1)+TMP(J))
        ENDDO
C
        IF (I.LT.NIX) THEN
          HT = HT+HT
          YA(LAST+1) = YA(LAST-2)+HT*(TMP(LAST-2)+FOUR*TMP(LAST)+TMP(LAS!
     +T+1))
          YA(LAST+2) = YA(LAST)+HT*(TMP(LAST)+FOUR*TMP(LAST+1)+TMP(LAST+!
     +2))
          FIRST = LAST+1
        ENDIF
C
      ENDDO
C
C  Correct the integration when K=0
C
      IF (K.EQ.0) THEN
        CORR = ASY-YA(NPTS)
        DO J = 2,NPTS,2
          YA(J) = CORR+YA(J)
        ENDDO
        CORR = ASY-YA(NPTS-1)
        DO J = 1,NPTS-1,2
          YA(J) = CORR+YA(J)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C  Second part of the integration
C
C  y_B(r) = int_r^RA             X(s) 1/s^(k+1) ds
C         = (int_0^RA - int_0^r) X(s) 1/s^(k+1) ds
C
C-----------------------------------------------------------------------
C
C  Form the integrand
C
      DO J = 1,NPTS
        TMP(J) = X(J)*RK1(J)
      ENDDO
C
      HT = HINT*THIRD
C
C  The integrand is zero at the origin
C  First point uses trapezoidal rule
C  The other points use Simpson's rule
C
      YB(1) = DR1*TMP(1)*HALF
      YB(2) = HT*(FOUR*TMP(1)+TMP(2))
      FIRST = 1
C
      DO I = 1,NIX
C
        LAST = IRX(I)
C
        DO J = FIRST+2,LAST
          YB(J) = YB(J-2)+HT*(TMP(J-2)+FOUR*TMP(J-1)+TMP(J))
        ENDDO
C
        IF (I.LT.NIX) THEN
          HT = HT+HT
          YB(LAST+1) = YB(LAST-2)+HT*(TMP(LAST-2)+FOUR*TMP(LAST)+TMP(LAS!
     +T+1))
          YB(LAST+2) = YB(LAST)+HT*(TMP(LAST)+FOUR*TMP(LAST+1)+TMP(LAST+!
     +2))
          FIRST = LAST+1
        ENDIF
C
      ENDDO
C
C  y_B(r) = (int_0^RA - int_0^r) X(s) 1/s^(k+1) ds
C         = int_r^RA X(s) 1/s^(k+1) ds
C
      S = YB(NPTS)
      DO J = 1,NPTS
        YB(J) = S-YB(J)
      ENDDO
C
C  Correct the integration
C  YB must be zero at the end-points
C
      CORR = -YB(NPTS-1)
      DO J = 1,NPTS-1,2
        YB(J) = CORR+YB(J)
      ENDDO
C-----------------------------------------------------------------------
C
C   Form the Y function i.e. (1/r) Y^k(X;r)
C
C      int_0^r X(s) s^k/r^(k+1) ds + int_r^RA X(s) r^k/s^(k+1) ds
C
C-----------------------------------------------------------------------
      DO J = 1,NPTS
        Y(J) = YA(J)*RK1(J)+YB(J)*RK(J)
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    GEN.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GEN(
     +CL, CP, CQ, DR1, EIGENS, HINT, IDISC5, IDMTST, IPOS,
     +IREC2, IRX, ITC, IWRITE, JAG, JTARG, KAG, KBMAX, KCMAX, KCMIN,
     +LAB, LAG, LAMBB, LAMBC, LAMCC, MAXFUL, MAXNLG, MAXNQN, MINNQN,
     +NDIMAX, NIX, NPTS, NRANG2, NUMORB, RMESH, WEIGHT, ZCORE, ZNUCL,
     +ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GEN.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:20:04 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   HINT   ... basic step-size
C   IDISC5 ... stream number of DSTG1.DAT (dstg1 dump file)
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   IREC2  ... pointer to next available DA record in INTEGRAL.DAT
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   JTARG  ... maximum 2J-value for the target states
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   LAB    ... array of orbital labels for each K-value
C   LAG    ... array of orbital l quantum numbers
C   LAMBB  ... maximum lambda value allowed for bound-bound
C              multipole integrals
C   LAMBC  ... maximum lambda value allowed for bound-continuum
C              multipole integrals
C   LAMCC  ... maximum lambda value allowed for continuum-continuum
C              multipole integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZCORE  ... core potential, tabulated on the regular radial mesh
C   ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C-----------------------------------------------------------------------
C
C   IBBPOL ... array giving positions of the b-b integrals
C   IBCPOL ... array giving positions of the b-c integrals
C   ICCPOL ... array giving positions of the c-c integrals
C   ICTX   ... array ICTX(K1,K1P,LAM/2+1) gives direct c-c Slater int.
C   ICTX   ... array ICTX(K1,K2,K3) gives index in ISTX1/ISTX2 arrays
C   ICTY   ... array ICTY(K1,K1P,LAM/2+1) gives exchange c-c Slater int.
C   IRK1   ... number of integrals stored in RKSTO
C   IRK2   ... number of combinations in ISTX1/ISTX2 arrays
C   ISTX   ... array ISTX(K1) gives position of one-electron int.
C   ISTX1  ... array stores LAM for b-c Slater int.
C   ISTX1  ... array stores LAM*(KBMAX+1)+K4 for b-b Slater int.
C   ISTX1  ... array stores combination LAM*(KBMAX+1)+K4 for Slater int.
C   ISTX2  ... array gives position of Slater int. corresponding to
C              ISTX1
C   RKSTO  ... array containing the radial integrals
C
C   IFLAG  ... flag indicating that a dimension has been exceeded
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NDY
      PARAMETER (NDY=(NDX+MXNK)/4+1)
      INTEGER NDZ
      PARAMETER (NDZ=5)
C
C  Argument variables
C
      CHARACTER*2 LAB(*)
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IREC2
      INTEGER IRX(*)
      INTEGER IDISC5
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER JTARG
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NIX
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
C
C  Local variables
C
      DOUBLE PRECISION TIME2
      DOUBLE PRECISION TIMTOT
      INTEGER I,I1,I2,ICOUNT
      INTEGER IPTS,IFLAG,ITOT1,ITOT2
      INTEGER ITOT3,ITOT4,ITOT5,J
      INTEGER K,KC
      INTEGER KCP,LAMIND
C
      DOUBLE PRECISION RKSTO(MXI1)
      INTEGER ICTX(NDX,NDX,NDX)
      INTEGER ICTY(NDX,NDX,NDY)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISTX(NDX)
      INTEGER ISTX1(MXI2)
      INTEGER ISTX2(MXI2)
      INTEGER IBBPOL(NDX,NDX,NDZ)
      INTEGER IBCPOL(NDX,MXNK,NDZ)
      INTEGER ICCPOL(MXNK,MXNK,NDZ)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      ITOT2 = 0
      ITOT3 = 0
      ITOT4 = 0
      TIMTOT = ZERO
      ICOUNT = 0
C
      WRITE (IWRITE,3000)
C
C  dimension check
C
      IPTS = NPTS*(NUMORB+2*NRANG2)
      CALL DMCHK1(13, IPTS, IWRITE, IDMTST, NDIMAX)
C
C  initialise flag that indicates a dimension exceeded
C
      IFLAG = 0
C=======================================================================
C
C  Generate and store multipole integrals.
C
C=======================================================================
      WRITE (IWRITE,3010)
C
      IRK1 = 0
C
C  bound-bound
C
      IF (LAMBB.GT.0) THEN
        WRITE (IWRITE,3020)
        PRINT 3150
        CALL GENMBB(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, LAG, LAMBB, MAXNQN, NPTS, RMESH, WEIGHT,
     +IBBPOL, IRK1, RKSTO)
      ENDIF
C
C  bound-continuum
C
      IF (KCMAX.GT.0) THEN
        IF (LAMBC.GT.0) THEN
          WRITE (IWRITE,3030)
          PRINT 3160
          CALL GENMBC(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, KCMAX, KCMIN, LAG, LAMBC, MAXNQN, NPTS,
     +NRANG2, NUMORB, RMESH, WEIGHT, IBCPOL, IRK1, RKSTO)
        ENDIF
      ENDIF
C
C  continuum-continuum
C
      IF (KCMAX.GT.0) THEN
        IF (LAMCC.GT.0) THEN
          WRITE (IWRITE,3040)
          PRINT 3170
          CALL GENMCC(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, KCMAX, KCMIN, LAG, LAMCC, MAXNQN, NPTS,
     +NRANG2, NUMORB, RMESH, WEIGHT, ICCPOL, IRK1, RKSTO)
        ENDIF
      ENDIF
C
      IF (IFLAG.EQ.1) THEN
        IF (IRK1.GT.NDIMAX(3)) NDIMAX(3) = IRK1
        IF (IRK1.GT.0) THEN
          ICOUNT = ICOUNT+1
          WRITE (IWRITE,3111) IRK1,ICOUNT
        ENDIF
      ELSE
        CALL DMCHK1(3, IRK1, IWRITE, IDMTST, NDIMAX)
C
        WRITE (IDISC5) IRK1,IREC2
C
        IF (IRK1.GT.0) THEN
          IF (LAMBB.GT.0) THEN
            LAMIND = (LAMBB+1)/2
            WRITE (IDISC5)
     +(((IBBPOL(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,LAMIND)
          ENDIF
          IF (KCMAX.GT.0) THEN
            IF (LAMBC.GT.0) THEN
              LAMIND = (LAMBC+1)/2
              WRITE (IDISC5)
     +(((IBCPOL(I,J,K),I=1,KBMAX),J=1,KCMAX),K=1,LAMIND)
            ENDIF
          ENDIF
          IF (KCMAX.GT.0) THEN
            IF (LAMCC.GT.0) THEN
              LAMIND = (LAMCC+1)/2
              WRITE (IDISC5)
     +(((ICCPOL(I,J,K),I=1,KCMAX),J=1,KCMAX),K=1,LAMIND)
            ENDIF
          ENDIF
          CALL DA1('INTEGRAL.DAT', 2, IREC2, 12, IRK1, RKSTO)
          ICOUNT = ICOUNT+1
          CALL QUARTZ(2, IWRITE, TIME2)
          TIMTOT = TIMTOT+TIME2
          WRITE (IWRITE,3110) IRK1,ICOUNT,TIME2
        ENDIF
C
      ENDIF
C
      ITOT1 = IRK1
C=======================================================================
C
C  Generate and store continuum-continuum integrals.
C
C=======================================================================
      IF (KCMAX.GT.0) THEN
C
        WRITE (IWRITE,3070)
C
        DO KC = KCMIN,KCMAX
          DO KCP = KC,KCMAX
C
            PRINT 3180,KC,KCP
C
C  get radial functions
C
            IF (IFLAG.EQ.0) THEN
              CALL SKIPER(
     +KC, KCP, KCMIN, NPTS, NRANG2, KBMAX,
     +LAG, MAXNQN, NUMORB, IPOS, CP, CQ)
            ENDIF
C
            CALL GENCC(
     +IFLAG, KC, KCP, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST,
     +IPOS, IRX, ITC, IWRITE, JAG, JTARG, KAG, KBMAX, LAB, LAG,
     +MAXFUL, MAXNLG, MAXNQN, MINNQN, NIX, NPTS, NRANG2, RMESH,
     +WEIGHT, ZCORE, ZNUCL, ZSTAT, ICTX, ICTY, IRK1, RKSTO)
C
            IF (IFLAG.EQ.1) THEN
              IF (IRK1.GT.NDIMAX(3)) NDIMAX(3) = IRK1
              IF (IRK1.GT.0) THEN
                ICOUNT = ICOUNT+1
                WRITE (IWRITE,3081) IRK1,KC,KCP,ICOUNT
              ENDIF
            ELSE
              CALL DMCHK1(3, IRK1, IWRITE, IDMTST, NDIMAX)
C
              WRITE (IDISC5) IRK1,KC,KCP,IREC2
C
              IF (IRK1.GT.0) THEN
                I1 = MIN(KBMAX+KBMAX,KC+KCP)
                I2 = MIN(KBMAX+KC,KBMAX+KCP)
                I1 = I1/4+1
                I2 = I2/4+1
                WRITE (IDISC5)
     +(((ICTX(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I1),
     +(((ICTY(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I2)
                CALL DA1('INTEGRAL.DAT', 2, IREC2, 12, IRK1, RKSTO)
                ICOUNT = ICOUNT+1
                CALL QUARTZ(2, IWRITE, TIME2)
                TIMTOT = TIMTOT+TIME2
                WRITE (IWRITE,3080) IRK1,KC,KCP,ICOUNT,TIME2
              ENDIF
C
            ENDIF
C
            ITOT2 = ITOT2+IRK1
C
          ENDDO
        ENDDO
C
      ENDIF
C=======================================================================
C
C  Generate and store bound-continuum integrals.
C
C=======================================================================
        IF (KCMAX.GT.0) THEN
C
          WRITE (IWRITE,3060)
C
          DO KC = KCMIN,KCMAX
C
            PRINT 3190,KC
C
C  get radial functions
C
            IF (IFLAG.EQ.0) THEN
              CALL SKIPER(
     +KC, 0, KCMIN, NPTS, NRANG2, KBMAX,
     +LAG, MAXNQN, NUMORB, IPOS, CP, CQ)
            ENDIF
C
            CALL GENBC(
     +IFLAG, KC, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST, IPOS,
     +IRX, ITC, IWRITE, JAG, KAG, KBMAX, MAXFUL, MAXNLG, MAXNQN,
     +MINNQN, NIX, NPTS, NRANG2, RMESH, WEIGHT, ZCORE, ZNUCL,
     +ZSTAT, ICTX, IRK1, IRK2, ISTX1, ISTX2, RKSTO)
C
            IF (IFLAG.EQ.1) THEN
              IF (IRK1.GT.NDIMAX(3)) NDIMAX(3) = IRK1
              IF (IRK2.GT.NDIMAX(4)) NDIMAX(4) = IRK2
              IF (IRK1.GT.0) THEN
                ICOUNT = ICOUNT+1
                WRITE (IWRITE,3091) IRK1,IRK2,KC,ICOUNT
              ENDIF
            ELSE
              CALL DMCHK1(3, IRK1, IWRITE, IDMTST, NDIMAX)
              CALL DMCHK1(4, IRK2, IWRITE, IDMTST, NDIMAX)
C
              WRITE (IDISC5) IRK1,IRK2,KC,IREC2
C
              IF (IRK1.GT.0) THEN
                WRITE (IDISC5)
     +(((ICTX(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +(ISTX1(I),I=1,IRK2),(ISTX2(I),I=1,IRK2)
                CALL DA1('INTEGRAL.DAT', 2, IREC2, 12, IRK1, RKSTO)
                ICOUNT = ICOUNT+1
                CALL QUARTZ(2, IWRITE, TIME2)
                TIMTOT = TIMTOT+TIME2
                WRITE (IWRITE,3090) IRK1,IRK2,KC,ICOUNT,TIME2
              ENDIF
C
            ENDIF
C
            ITOT3 = ITOT3+IRK1
C
          ENDDO
C
        ENDIF
C=======================================================================
C
C  Generate and store bound-bound integrals
C
C=======================================================================
      WRITE (IWRITE,3050)
C
      PRINT 3200
C
      CALL GENBB(
     +IFLAG, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST, IPOS, IRX,
     +ITC, IWRITE, JAG, KAG, KBMAX, MAXFUL, MAXNLG, MAXNQN,
     +MINNQN, NIX, NPTS, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT,
     +ICTX, IRK1, IRK2, ISTX, ISTX1, ISTX2, RKSTO)
C
      IF (IFLAG.EQ.1) THEN
        IF (IRK1.GT.NDIMAX(3)) NDIMAX(3) = IRK1
        IF (IRK2.GT.NDIMAX(4)) NDIMAX(4) = IRK2
        IF (IRK1.GT.0) THEN
          ICOUNT = ICOUNT+1
          WRITE (IWRITE,3101) IRK1,IRK2,ICOUNT
        ENDIF
      ELSE
        CALL DMCHK1(3, IRK1, IWRITE, IDMTST, NDIMAX)
        CALL DMCHK1(4, IRK2, IWRITE, IDMTST, NDIMAX)
C
        WRITE (IDISC5) IRK1,IRK2,IREC2
C
        IF (IRK1.GT.0) THEN
          WRITE (IDISC5)
     +(ISTX(I),I=1,KBMAX),
     +(((ICTX(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +(ISTX1(I),I=1,IRK2),
     +(ISTX2(I),I=1,IRK2)
          CALL DA1('INTEGRAL.DAT', 2, IREC2, 12, IRK1, RKSTO)
          ICOUNT = ICOUNT+1
          CALL QUARTZ(2, IWRITE, TIME2)
          TIMTOT = TIMTOT+TIME2
          WRITE (IWRITE,3100) IRK1,IRK2,ICOUNT,TIME2
        ENDIF
C
      ENDIF
C
      ITOT4 = IRK1
C=======================================================================
C
C  Integral evaluation is complete.
C
C=======================================================================
      REWIND IDISC5
      WRITE (IWRITE,3120) ICOUNT
      ITOT5 = ITOT1+ITOT2+ITOT3+ITOT4
      WRITE (IWRITE,3130) ITOT1,ITOT2,ITOT3,ITOT4,ITOT5
      WRITE (IWRITE,3140) TIMTOT
C=======================================================================
 3000 FORMAT (/31X,'Routine GEN'/31X,'-----------')
 3010 FORMAT (/' generate the multipole integrals'/)
 3020 FORMAT (' generate the bound-bound multipole integrals')
 3030 FORMAT (' generate the bound-continuum multipole integrals')
 3040 FORMAT (' generate the continuum-continuum multipole integrals')
 3050 FORMAT (/' generate the bound-bound integrals'/)
 3060 FORMAT (/' generate the bound-continuum integrals'/)
 3070 FORMAT (/' generate the continuum-continuum integrals'/)
 3080 FORMAT (' IRK1=',I7,' K=',I5,' KP=',I5,2X,'  block=',I6,
     +'  CPU=',F7.2,' sec')
 3081 FORMAT (' IRK1=',I7,' K=',I5,' KP=',I5,2X,'  block=',I6,
     +'  dimension error')
 3090 FORMAT (' IRK1=',I7,' IRK2=',I5,' K=',I5,'  block=',I6,
     +'  CPU=',F7.2,' sec')
 3091 FORMAT (' IRK1=',I7,' IRK2=',I5,' K=',I5,'  block=',I6,
     +'  dimension error')
 3100 FORMAT (' IRK1=',I7,' IRK2=',I5,8X,'  block=',I6,
     +'  CPU=',F7.2,' sec')
 3101 FORMAT (' IRK1=',I7,' IRK2=',I5,8X,'  block=',I6,
     +'  dimension error')
 3110 FORMAT (' IRK1=',I7,19X,'  block=',I6,'  CPU=',F7.2,' sec')
 3111 FORMAT (' IRK1=',I7,19X,'  block=',I6,'  dimension error')
 3120 FORMAT (
     +/' **** write to file completed'
     +/' **** number of blocks = ',I6)
 3130 FORMAT (
     + /' Total numbers of radial integrals stored'
     +//1X,I9,' multipole'
     + /1X,I9,' continuum-continuum'
     + /1X,I9,' bound-continuum'
     + /1X,I9,' bound-bound'
     +//1X,I9,' total')
 3140 FORMAT (/' Total time spent in routine GEN is ',F11.2,' sec')
 3150 FORMAT (' ... calling GENMBB')
 3160 FORMAT (' ... calling GENMBC')
 3170 FORMAT (' ... calling GENMCC')
 3180 FORMAT (' ... calling GENCC for ',2I5)
 3190 FORMAT (' ... calling GENBC for ',I5)
 3200 FORMAT (' ... calling GENBB')
      END
CEND--------------------------------------------------------------------
CEND    GENBB.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENBB(
     +IFLAG, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST, IPOS, IRX,
     +ITC, IWRITE, JAG, KAG, KBMAX, MAXFUL, MAXNLG, MAXNQN, MINNQN, NIX,
     +NPTS, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT, ICTX, IRK1, IRK2, ISTX,
     +ISTX1, ISTX2, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENBB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C x IFLAG  ... flag indicating dimension exceeded
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   HINT   ... basic step-size
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZCORE  ... core potential, tabulated on the regular radial mesh
C   ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C o ICTX   ... array ICTX(K1,K2,K3) gives index in ISTX1/ISTX2 arrays
C o IRK1   ... number of integrals stored in RKSTO
C o IRK2   ... number of combinations in ISTX1/ISTX2 arrays
C o ISTX   ... array ISTX(K1) gives position of one-electron int.
C o ISTX1  ... array stores LAM*(KBMAX+1)+K4 for b-b Slater int.
C o ISTX2  ... array gives position of Slater int. corresponding to
C              ISTX1
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER ICTX(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IFLAG
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER IRX(*)
      INTEGER ISTX(*)
      INTEGER ISTX1(*)
      INTEGER ISTX2(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NIX
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION RESULT
      INTEGER I,ITEST,J
      INTEGER J1,J2,J3,J4
      INTEGER K,K1,K2,K3
      INTEGER K4,KAP1,KAP2,KAP3
      INTEGER KAP4,LAM,LMAX,LMIN
      INTEGER MMAX,MMIN,MODE,N1
      INTEGER N1MAX,N1MIN,N2,N2MAX
      INTEGER N2MIN,N3,N3MAX,N3MIN
      INTEGER N4,N4MAX,N4MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IRK1 = 0
      IRK2 = 0
C
C  Evaluate one-electron integrals
C
      DO K1 = 1,KBMAX
        ISTX(K1) = IRK1+1
        N1MIN = MINNQN(K1)
        N1MAX = MAXNQN(K1)
        N2MIN = MINNQN(K1)
        DO N1 = N1MIN,N1MAX
          MODE = 0
          N2MAX = N1
          DO N2 = N2MIN,N2MAX
            IF (N1.GT.MAXFUL(K1)) GOTO 10
            IF (N2.GT.MAXFUL(K1)) GOTO 10
            IF (N1.EQ.N2) GOTO 10
            GOTO 20
   10       CONTINUE
            IRK1 = IRK1+1
            IF (IRK1.GT.IDMTST(3)) IFLAG = 1
            IF (IFLAG.EQ.0) THEN
              CALL INTRD1(
     +MODE, N2, N1, K1, RESULT, IWRITE, ITC, NIX, IRX, NPTS,
     +KAG, KBMAX, MAXNLG, MAXNQN, IPOS, HINT, RMESH, WEIGHT,
     +CL, EIGENS, ZCORE, ZNUCL, ZSTAT, CP, CQ)
              RKSTO(IRK1) = RESULT
              MODE = 1
            ENDIF
   20       CONTINUE
          ENDDO
        ENDDO
      ENDDO
C
C  Calculate the Slater integrals
C
      DO I = 1,KBMAX
        DO J = 1,KBMAX
          DO K = 1,KBMAX
            ICTX(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
      DO K1 = 1,KBMAX
        J1 = JAG(K1)
        KAP1 = KAG(K1)
        N1MIN = MINNQN(K1)
        N1MAX = MAXNQN(K1)
C
        DO K2 = K1,KBMAX
          J2 = JAG(K2)
          KAP2 = KAG(K2)
          N2MIN = MINNQN(K2)
          N2MAX = MAXNQN(K2)
C
          DO K3 = K1,KBMAX
            J3 = JAG(K3)
            KAP3 = KAG(K3)
            N3MIN = MINNQN(K3)
            N3MAX = MAXNQN(K3)
C
            LMIN = ABS(J1-J3)/2
            IF (KAP1*KAP3.LT.0) LMIN = LMIN + 1
            LMAX = (J1+J3)/2
            ICTX(K1,K2,K3) = IRK2+1
C
            DO K4 = K2,KBMAX
              J4 = JAG(K4)
              KAP4 = KAG(K4)
              N4MIN = MINNQN(K4)
              N4MAX = MAXNQN(K4)
C
C  Set the value of lambda
C
              LAM = LMIN
              MMAX = (J2+J4)/2
              IF (LAM.GT.MMAX) GOTO 70
              ITEST = MMAX+LAM
              IF (KAP2*KAP4.GT.0) ITEST = ITEST + 1
              IF (MOD(ITEST,2).NE.0) GOTO 70
              MMIN = ABS(J2-J4)/2
   30         CONTINUE
              IF (LAM.GE.MMIN) GOTO 40
              LAM = LAM+2
              GOTO 30
C
   40         CONTINUE
              IRK2 = IRK2+1
              IF (IRK2.GT.IDMTST(4)) IFLAG = 1
              IF (IFLAG.EQ.0) THEN
                ISTX1(IRK2) = LAM*(KBMAX+1)+K4
                ISTX2(IRK2) = IRK1+1
              ENDIF
C
C  Loop over the principal quantum numbers of bound orbitals
C
              DO N1 = N1MIN,N1MAX
                IF (K3.EQ.K1) N3MAX = N1
                IF (K2.EQ.K1) N2MAX = N1
                DO N3 = N3MIN,N3MAX
                  MODE = 0
                  DO N2 = N2MIN,N2MAX
                    IF (K4.EQ.K2) N4MAX = N2
                    DO N4 = N4MIN,N4MAX
                      IF (N1.GT.MAXFUL(K1)) GOTO 50
                      IF (N2.GT.MAXFUL(K2)) GOTO 50
                      IF (N3.GT.MAXFUL(K3)) GOTO 50
                      IF (N4.GT.MAXFUL(K4)) GOTO 50
                      IF (N1.EQ.N2 .AND. K1.EQ.K2 .AND.
     +                    N3.EQ.N4 .AND. K3.EQ.K4) GOTO 50
                      IF (N1.EQ.N3 .AND. K1.EQ.K3 .AND.
     +                    N2.EQ.N4 .AND. K2.EQ.K4) GOTO 50
                      GOTO 60
   50                 CONTINUE
                      IRK1 = IRK1+1
                      IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                      IF (IFLAG.EQ.0) THEN
                        CALL INTRD2(
     +N1, K1, N2, K2, N3, K3, N4, K4, LAM, RESULT,
     +MODE, IWRITE, ITC, WEIGHT, RMESH, DR1,
     +HINT, IRX, NIX, NPTS, IPOS, CP, CQ)
                        RKSTO(IRK1) = RESULT
                        MODE = 1
                      ENDIF
   60                 CONTINUE
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
C
C  Increment the value of lambda
C
              LAM = LAM+2
              IF (LAM.GT.LMAX .OR. LAM.GT.MMAX) GOTO 70
              GOTO 40
C
   70         CONTINUE
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    GENBC.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENBC(
     +IFLAG, KC, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST, IPOS, IRX, ITC,
     +IWRITE, JAG, KAG, KBMAX, MAXFUL, MAXNLG, MAXNQN, MINNQN, NIX,
     +NPTS, NRANG2, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT, ICTX, IRK1,
     +IRK2, ISTX1, ISTX2, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENBC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C x IFLAG  ... flag indicating a dimension exceeded
C   KC     ... continuum K-value
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   HINT   ... basic step-size
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZCORE  ... core potential, tabulated on the regular radial mesh
C   ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C o ICTX   ... array ICTX(K1,K2,K3) gives index in ISTX1/ISTX2 arrays
C o IRK1   ... number of integrals stored in RKSTO
C o IRK2   ... number of combinations in ISTX1/ISTX2 arrays
C o ISTX1  ... array stores LAM for b-c Slater int.
C o ISTX2  ... array gives position of Slater int. corresponding to
C              ISTX1
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER ICTX(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IFLAG
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER IRX(*)
      INTEGER ISTX1(*)
      INTEGER ISTX2(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KC
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NIX
      INTEGER NPTS
      INTEGER NRANG2
C
C  Local variables
C
      DOUBLE PRECISION RESULT
      INTEGER I,ITEST,J
      INTEGER J1,J2,J3,JC
      INTEGER K,K1,K2,K3
      INTEGER KAP1,KAP2,KAP3,KAPC
      INTEGER LAM,LMAX,LMIN,MMAX
      INTEGER MMIN,MODE,N,N1
      INTEGER N1MAX,N1MIN,N2,N2MAX
      INTEGER N2MIN,N3,N3MAX,N3MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IRK1 = 0
      IRK2 = 0
C
C  Evaluate one-electron integrals
C
      IF (KC.LE.KBMAX) THEN
        N1MAX = MAXNQN(KC)
        N1 = MINNQN(KC)
C
   10   CONTINUE
        N2 = 1
        MODE = 0
C
   20   CONTINUE
        IRK1 = IRK1+1
        N3 = N1MAX+N2
        IF (IRK1.GT.IDMTST(3)) IFLAG = 1
        IF (IFLAG.EQ.0) THEN
          CALL INTRD1(
     +MODE, N3, N1, KC, RESULT, IWRITE, ITC, NIX, IRX, NPTS, KAG,
     +KBMAX, MAXNLG, MAXNQN, IPOS, HINT, RMESH, WEIGHT, CL,
     +EIGENS, ZCORE, ZNUCL, ZSTAT, CP, CQ)
          RKSTO(IRK1) = RESULT
        ENDIF
C
        N2 = N2+1
        MODE = 1
        IF (N2.LE.NRANG2) GOTO 20
C
        N1 = N1+1
        IF (N1.LE.N1MAX) GOTO 10
      ENDIF
C
C  Evaluate two-electron integrals
C
      DO I = 1,KBMAX
        DO J = 1,KBMAX
          DO K = 1,KBMAX
            ICTX(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
C  Set the initial angular momenta and read the continuum orbitals
C
      JC = JAG(KC)
      KAPC = KAG(KC)
C
      DO K1 = 1,KBMAX
        J1 = JAG(K1)
        KAP1 = KAG(K1)
        N1MIN = MINNQN(K1)
        N1MAX = MAXNQN(K1)
C
        DO K2 = 1,KBMAX
          J2 = JAG(K2)
          KAP2 = KAG(K2)
          N2MIN = MINNQN(K2)
          N2MAX = MAXNQN(K2)
C
          LMIN = ABS(J2-JC)/2
          IF (KAP2*KAPC.LT.0) LMIN = LMIN + 1
          LMAX = (J2+JC)/2
C
          DO K3 = K1,KBMAX
            J3 = JAG(K3)
            KAP3 = KAG(K3)
            N3MIN = MINNQN(K3)
            N3MAX = MAXNQN(K3)
C
            ICTX(K1,K2,K3) = IRK2+1
C
            LAM = LMIN
            MMAX = (J1+J3)/2
            IF (LAM.GT.MMAX) GOTO 70
            ITEST = MMAX+LAM
            IF (KAP1*KAP3.GT.0) ITEST = ITEST + 1
            IF (MOD(ITEST,2).NE.0) GOTO 70
            MMIN = ABS(J1-J3)/2
   30       CONTINUE
            IF (LAM.GE.MMIN) GOTO 40
            LAM = LAM+2
            GOTO 30
C
   40       CONTINUE
            IRK2 = IRK2+1
            IF (IRK2.GT.IDMTST(4)) IFLAG = 1
            IF (IFLAG.EQ.0) THEN
              ISTX1(IRK2) = LAM
              ISTX2(IRK2) = IRK1+1
            ENDIF
C
C  Loop over the principal quantum numbers
C
            DO N1 = N1MIN,N1MAX
              IF (K3.EQ.K1) N3MAX = N1
              DO N3 = N3MIN,N3MAX
                MODE = 0
                DO N2 = N2MIN,N2MAX
                  IF (N1.GT.MAXFUL(K1)) GOTO 50
                  IF (N2.GT.MAXFUL(K2)) GOTO 50
                  IF (N3.GT.MAXFUL(K3)) GOTO 50
                  IF (N1.EQ.N2 .AND. K1.EQ.K2) GOTO 50
                  IF (N1.EQ.N3 .AND. K1.EQ.K3) GOTO 50
                  GOTO 60
   50             CONTINUE
                  DO N = 1,NRANG2
                    IRK1 = IRK1+1
                    IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                    IF (IFLAG.EQ.0) THEN
                      CALL INTRD2(
     +N1, K1, N2, K2, N3, K3, MAXNQN(KC)+N, KC, LAM,
     +RESULT, MODE, IWRITE, ITC, WEIGHT, RMESH,
     +DR1, HINT, IRX, NIX, NPTS, IPOS, CP, CQ)
                      RKSTO(IRK1) = RESULT
                      MODE = 1
                    ENDIF
                  ENDDO
   60             CONTINUE
                ENDDO
              ENDDO
            ENDDO
C
            LAM = LAM+2
            IF (LAM.GT.LMAX .OR. LAM.GT.MMAX) GOTO 70
            GOTO 40
C
   70       CONTINUE
          ENDDO
        ENDDO
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    GENCC.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENCC(
     +IFLAG, KC, KCP, CL, CP, CQ, DR1, EIGENS, HINT, IDMTST, IPOS,
     +IRX, ITC, IWRITE, JAG, JTARG, KAG, KBMAX, LAB, LAG,
     +MAXFUL, MAXNLG, MAXNQN, MINNQN, NIX, NPTS, NRANG2,
     +RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT, ICTX, ICTY, IRK1, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENCC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:20:04 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C x IFLAG  ... flag indicating a dimension exceeded
C   KC     ... continuum K-value
C   KCP    ... continuum K-value
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   HINT   ... basic step-size
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   JTARG  ... maximum 2J-value for the target states
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   LAB    ... array of orbital labels for each K-value
C   LAG    ... array of orbital l quantum numbers
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZCORE  ... core potential, tabulated on the regular radial mesh
C   ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C o ICTX   ... array ICTX(K1,K1P,LAM/2+1) gives direct c-c Slater int.
C o ICTY   ... array ICTY(K1,K1P,LAM/2+1) gives exchange c-c Slater int.
C o IRK1   ... number of integrals stored in RKSTO
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Statement functions
C
      LOGICAL IPAR,ITRG
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      CHARACTER*2 LAB(*)
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER ICTX(NDX,NDX,*)
      INTEGER ICTY(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IFLAG
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER JTARG
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KC
      INTEGER KCP
      INTEGER LAG(*)
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NIX
      INTEGER NPTS
      INTEGER NRANG2
C
C  Local variables
C
      DOUBLE PRECISION RESULT
      INTEGER I,IFIRST,IK,IL
      INTEGER ILAST,J,J1
      INTEGER J1P,J2,J3,JL
      INTEGER JLP,JTEST,JTOTAL,K1
      INTEGER K1P,KL,L1,L1P
      INTEGER L2,L3,LAM,LAMS
      INTEGER LL,LLP,LMAX,LMIN
      INTEGER MAXHF,MODE,N,N1
      INTEGER N1P,N1PMAX,N1PMIN,N2
      INTEGER N3,N4,NP,NST
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C   This function tests the triangular condition.
C   The arguments are 2J (J is half-integral).
C
      ITRG(J1,J2,J3) = ABS(J1-J2).LE.J3 .AND.
     +                         J3.LE.J1+J2 .AND.
     +                 MOD(J1+J2+J3,2).EQ.0
C
C   This function tests the (even) parity condition.
C   The arguments are L (integers).
C
      IPAR(L1,L2,L3) = MOD(L1+L2+L3,2).EQ.0
C-----------------------------------------------------------------------
      IRK1 = 0
C
      LL = LAG(KC)
      JL = JAG(KC)
      LLP = LAG(KCP)
      JLP = JAG(KCP)
C
      IF (JTARG.GE.0) THEN
        IF (JLP.GE.JTARG) THEN
          JTEST = JLP-JL
          JTEST = JTEST/2
          IF (JTEST.GT.JTARG) RETURN
        ENDIF
      ENDIF
C
C  Evaluate one-electron integrals
C
      IF (KC.EQ.KCP) THEN
        MAXHF = MAXNQN(KC)
        DO N1 = 1,NRANG2
          N3 = MAXHF+N1
          N2 = 1
          MODE = 0
          DO N2 = 1,N1
            IRK1 = IRK1+1
            N4 = MAXHF+N2
            IF (IRK1.GT.IDMTST(3)) IFLAG = 1
            IF (IFLAG.EQ.0) THEN
              CALL INTRD1(
     +MODE, N4, N3, KC, RESULT, IWRITE, ITC, NIX, IRX, NPTS, KAG,
     +KBMAX, MAXNLG, MAXNQN, IPOS, HINT, RMESH, WEIGHT, CL, EIGENS,
     +ZCORE, ZNUCL, ZSTAT, CP, CQ)
              RKSTO(IRK1) = RESULT
            ENDIF
            MODE = 1
          ENDDO
        ENDDO
      ENDIF
C
C  Slater integrals
C
      IK = MIN(KBMAX+KBMAX,KC+KCP)
      IL = MIN(KBMAX+KC,KBMAX+KCP)
      IK = IK/4+1
      IL = IL/4+1
C
      DO I = 1,KBMAX
        DO J = 1,KBMAX
          DO KL = 1,IK
            ICTX(I,J,KL) = 0
          ENDDO
          DO KL = 1,IL
            ICTY(I,J,KL) = 0
          ENDDO
        ENDDO
      ENDDO
C
C  Direct integrals
C
      LMIN = ABS(JL-JLP)/2
      IF (MOD(LL+LLP+LMIN,2).EQ.1) LMIN = LMIN + 1
      LMAX = (JL+JLP)/2
C
C  loop over LAM, K1 and K1P
C  using triangular and parity conditions to restrict possible values
C
      DO LAM = LMIN,LMAX,2
        DO K1 = 1,KBMAX
          L1 = LAG(K1)
          J1 = JAG(K1)
          DO K1P = K1,KBMAX
            L1P = LAG(K1P)
            J1P = JAG(K1P)
            IF (ITRG(J1,J1P,2*LAM)) THEN
              IF (IPAR(L1,L1P,LAM)) THEN
C
                IF (IFLAG.EQ.0) THEN
                  LAMS = LAM/2+1
                  ICTX(K1,K1P,LAMS) = IRK1+1
                ENDIF
C
C  loop over N1 ... bound
C
                DO N1 = MINNQN(K1),MAXNQN(K1)
C
                  IF (N1.LE.MAXFUL(K1)) THEN
                    IF (K1.EQ.K1P) THEN
                      N1PMIN = N1
                      N1PMAX = N1
                    ELSE
                      N1PMIN = MAXFUL(K1P)+1
                      N1PMAX = MAXNQN(K1P)
                    ENDIF
                  ELSE
                    IF (K1.EQ.K1P) THEN
                      N1PMIN = MINNQN(K1P)
                      N1PMAX = N1
                    ELSE
                      N1PMIN = MINNQN(K1P)
                      N1PMAX = MAXNQN(K1P)
                    ENDIF
                  ENDIF
C
                  IF (N1PMIN.LE.N1PMAX) THEN
C
C  loop over N1P ... bound
C
                    DO N1P = N1PMIN,N1PMAX
C
                      MODE = 0
                      IFIRST = IRK1+1
C
C  loop over N ... continuum
C
                      DO N = 1,NRANG2
C
                        IF (KC.EQ.KCP) THEN
                          NST = N
                        ELSE
                          NST = NRANG2
                        ENDIF
C
C  loop over NP ... continuum
C
                        DO NP = 1,NST
                          IRK1 = IRK1+1
                          IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                          IF (IFLAG.EQ.0) THEN
                            CALL INTRD2(
     +N1, K1, MAXNQN(KC)+N, KC, N1P, K1P, MAXNQN(KCP)+NP, KCP,
     +LAM, RESULT, MODE, IWRITE, ITC, WEIGHT, RMESH, DR1, HINT,
     +IRX, NIX, NPTS, IPOS, CP, CQ)
                            RKSTO(IRK1) = RESULT
                          ENDIF
                          MODE = 1
                        ENDDO
C
                      ENDDO
C
                      IF (ITC(27).EQ.1) THEN
                        ILAST = IRK1
                        JTOTAL = ILAST-IFIRST+1
                        WRITE (IWRITE,3000)
     +N1,LAB(K1),KC,N1P,LAB(K1P),KCP,LAM,IFIRST,ILAST,JTOTAL
                      ENDIF
C
                    ENDDO
C
                  ENDIF
C
                ENDDO
C
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C  Exchange integrals
C
C  loop over K1, LAM and K1P
C  using triangular and parity conditions to restrict possible values
C
      DO K1 = 1,KBMAX
        L1 = LAG(K1)
        J1 = JAG(K1)
        LMIN = ABS(JL-J1)/2
        IF (MOD(LL+L1+LMIN,2).EQ.1) LMIN = LMIN + 1
        LMAX = (JL+J1)/2
        DO LAM = LMIN,LMAX,2
          DO K1P = 1,KBMAX
            L1P = LAG(K1P)
            J1P = JAG(K1P)
            IF (ITRG(JLP,J1P,2*LAM)) THEN
              IF (IPAR(LLP,L1P,LAM)) THEN
C
                IF (IFLAG.EQ.0) THEN
                  LAMS = LAM/2+1
                  ICTY(K1,K1P,LAMS) = IRK1+1
                ENDIF
C
C  loop over N1 ... bound
C
                DO N1 = MINNQN(K1),MAXNQN(K1)
C
                  IF (N1.LE.MAXFUL(K1)) THEN
                    IF (K1.EQ.K1P) THEN
                      N1PMIN = N1
                      N1PMAX = N1
                    ELSE
                      N1PMIN = MAXFUL(K1P)+1
                      N1PMAX = MAXNQN(K1P)
                    ENDIF
                  ELSE
                    N1PMIN = MINNQN(K1P)
                    N1PMAX = MAXNQN(K1P)
                  ENDIF
C
                  IF (N1PMIN.LE.N1PMAX) THEN
C
C  loop over N1P ... bound
C
                    DO N1P = N1PMIN,N1PMAX
                      IFIRST = IRK1+1
C
C  loop over N ... continuum
C
                      DO N = 1,NRANG2
C
                        MODE = 0
C
                        IF (KC.EQ.KCP) THEN
                          NST = N
                        ELSE
                          NST = NRANG2
                        ENDIF
C
C  loop over NP ... continuum
C
                        DO NP = 1,NST
                          IRK1 = IRK1+1
                          IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                          IF (IFLAG.EQ.0) THEN
                            CALL INTRD2(
     +N1, K1, N1P, K1P, MAXNQN(KC)+N, KC, MAXNQN(KCP)+NP, KCP,
     +LAM, RESULT, MODE, IWRITE, ITC, WEIGHT, RMESH, DR1, HINT,
     +IRX, NIX, NPTS, IPOS, CP, CQ)
                            RKSTO(IRK1) = RESULT
                          ENDIF
C
                          MODE = 1
                        ENDDO
C
                      ENDDO
C
                      IF (ITC(27).EQ.1) THEN
                        ILAST = IRK1
                        JTOTAL = ILAST-IFIRST+1
                        WRITE (IWRITE,3010)
     +N1,LAB(K1),N1P,LAB(K1P),KC,KCP,LAM,IFIRST,ILAST,JTOTAL
                      ENDIF
C
                    ENDDO
C
                  ENDIF
C
                ENDDO
C
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C=======================================================================
 3000 FORMAT ('  D  ',I2,A2,4X,I3,1X,I2,A2,4X,I3,2X,I3,4X,3I9)
 3010 FORMAT ('  E  ',I2,A2,2X,I2,A2,4X,I3,3X,I3,2X,I3,4X,3I9)
      END
CEND--------------------------------------------------------------------
CEND    GENMBB.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENMBB(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, LAG, LAMBB, MAXNQN, NPTS, RMESH, WEIGHT, IBBPOL, IRK1,
     +RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENMBB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C x IFLAG  ... flag indicating that a dimension has been exceeded
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   LAG    ... array of orbital l quantum numbers
C   LAMBB  ... maximum lambda value allowed for bound-bound
C              multipole integrals
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C o IBBPOL ... array giving positions of the b-b integrals
C o IRK1   ... number of integrals stored in RKSTO
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NDZ
      PARAMETER (NDZ=5)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IBBPOL(NDX,NDX,NDZ)
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IFLAG
      INTEGER IWRITE
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER MAXNQN(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION X1
      INTEGER I,J,K,K1
      INTEGER K2,L1,L2,LAM
      INTEGER LAMIND,LAMST,LMAX,LMIN
      INTEGER N1,N2,N2MAX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      LAMIND = (LAMBB+1)/2
C
      DO I = 1,KBMAX
        DO J = 1,KBMAX
          DO K = 1,LAMIND
            IBBPOL(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
C  Loop over the bound angular momenta
C
      DO K1 = 1,KBMAX
        L1 = LAG(K1)
        DO K2 = K1,KBMAX
          L2 = LAG(K2)
C
C  Determine the range for LAM
C  Loop over LAM
C
          LMIN = ABS(L1-L2)
          IF (LMIN.EQ.0) LMIN = 2
          LMAX = MIN(L1+L2,LAMBB)
          IF (LMIN.LE.LMAX) THEN
            DO LAM = LMIN,LMAX,2
C
              LAMST = (LAM+1)/2
              IBBPOL(K1,K2,LAMST) = IRK1+1
C
C  Loop over the principal quantum numbers
C
              DO N1 = L1+1,MAXNQN(K1)
                IF (K1.EQ.K2) THEN
                  N2MAX = N1
                ELSE
                  N2MAX = MAXNQN(K2)
                ENDIF
                DO N2 = L2+1,N2MAX
C
C  Calculate length integral
C
                  IRK1 = IRK1+1
                  IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                  IF (IFLAG.EQ.0) THEN
                    CALL INTRD(
     +N1, K1, N2, K2, LAM, X1, CP, CQ, WEIGHT,
     +RMESH, IWRITE, ITC, NPTS, IPOS)
                    RKSTO(IRK1) = X1
                  ENDIF
C
C  Calculate velocity integral, when LAM = 1
C
                  IF (LAM.EQ.1) THEN
                    IRK1 = IRK1+1
                    IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                    IF (IFLAG.EQ.0) THEN
                      CALL INTRDV(
     +N1, K1, N2, K2, X1, CP, CQ, WEIGHT,
     +IWRITE, ITC, NPTS, IPOS, KAG, CL)
                      RKSTO(IRK1) = X1
                    ENDIF
                  ENDIF
C
                ENDDO
              ENDDO
C
            ENDDO
          ENDIF
C
        ENDDO
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    GENMBC.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENMBC(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, KCMAX, KCMIN, LAG, LAMBC, MAXNQN, NPTS, NRANG2, NUMORB,
     +RMESH, WEIGHT, IBCPOL, IRK1, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENMBC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C x IFLAG  ... flag indicating that a dimension has been exceeded
C   CL     ... speed of light in au
C x CP     ... array containing orbital large components
C x CQ     ... array containing orbital small components
C   IDMTST ... array containing maximum dimensions set in the code
C x IPOS   ... array giving orbital index for CP and CQ arrays
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   LAG    ... array of orbital l quantum numbers
C   LAMBC  ... maximum lambda value allowed for bound-continuum
C              multipole integrals
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C o IBCPOL ... array giving positions of the b-c integrals
C o IRK1   ... number of integrals stored in RKSTO
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NDZ
      PARAMETER (NDZ=5)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IBCPOL(NDX,MXNK,NDZ)
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IFLAG
      INTEGER IWRITE
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER LAG(*)
      INTEGER LAMBC
      INTEGER MAXNQN(*)
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
C
C  Local variables
C
      DOUBLE PRECISION X1
      INTEGER I,J,K,K1
      INTEGER K2,L1,L2,LAM
      INTEGER LAMIND,LAMST,LMAX,LMIN
      INTEGER N1,N2,N2P
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      LAMIND = (LAMBC+1)/2
C
      DO I = 1,KBMAX
        DO J = 1,KCMAX
          DO K = 1,LAMIND
            IBCPOL(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
C  Loop over the continuum and bound angular momenta
C
      DO K2 = KCMIN,KCMAX
        L2 = LAG(K2)
        DO K1 = 1,KBMAX
          L1 = LAG(K1)
C
C  Determine the range for LAM
C  Loop over LAM
C
          LMIN = ABS(L1-L2)
          IF (LMIN.EQ.0) LMIN = 2
          LMAX = MIN(L1+L2,LAMBC)
          IF (LMIN.LE.LMAX) THEN
            DO LAM = LMIN,LMAX,2
C
              LAMST = (LAM+1)/2
              IBCPOL(K1,K2,LAMST) = IRK1+1
C
C  Loop over the principal quantum numbers
C
              DO N1 = L1+1,MAXNQN(K1)
                DO N2 = 1,NRANG2
C
                  N2P = MAXNQN(K2)+N2
C
C  Calculate length integral
C
                  IRK1 = IRK1+1
                  IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                  IF (IFLAG.EQ.0) THEN
                    CALL SKIPER(
     +K2, 0, KCMIN, NPTS, NRANG2, KBMAX, LAG, MAXNQN,
     +NUMORB, IPOS, CP, CQ)
                    CALL INTRD(
     +N1, K1, N2P, K2, LAM, X1, CP, CQ, WEIGHT, RMESH,
     +IWRITE, ITC, NPTS, IPOS)
                    RKSTO(IRK1) = X1
                  ENDIF
C
C  Calculate velocity integral, when LAM = 1
C
                  IF (LAM.EQ.1) THEN
                    IRK1 = IRK1+1
                    IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                    IF (IFLAG.EQ.0) THEN
                      CALL INTRDV(
     +N1, K1, N2P, K2, X1, CP, CQ, WEIGHT,
     +IWRITE, ITC, NPTS, IPOS, KAG, CL)
                      RKSTO(IRK1) = X1
                    ENDIF
                  ENDIF
C
                ENDDO
              ENDDO
C
            ENDDO
          ENDIF
C
        ENDDO
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    GENMCC.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENMCC(
     +IFLAG, CL, CP, CQ, IDMTST, IPOS, ITC, IWRITE, KAG,
     +KBMAX, KCMAX, KCMIN, LAG, LAMCC, MAXNQN, NPTS, NRANG2, NUMORB,
     +RMESH, WEIGHT, ICCPOL, IRK1, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENMCC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C x IFLAG  ... flag indicating that a dimension has been exceeded
C   CL     ... speed of light in au
C x CP     ... array containing orbital large components
C x CQ     ... array containing orbital small components
C   IDMTST ... array containing maximum dimensions set in the code
C x IPOS   ... array giving orbital index for CP and CQ arrays
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   LAG    ... array of orbital l quantum numbers
C   LAMCC  ... maximum lambda value allowed for continuum-continuum
C              multipole integrals
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C o ICCPOL ... array giving positions of the c-c integrals
C o IRK1   ... number of integrals stored in RKSTO
C o RKSTO  ... array containing the radial integrals
C
C o indicates output from routine
C x indicates variables changed in call
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NDZ
      PARAMETER (NDZ=5)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER ICCPOL(MXNK,MXNK,NDZ)
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IFLAG
      INTEGER IWRITE
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER LAG(*)
      INTEGER LAMCC
      INTEGER MAXNQN(*)
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
C
C  Local variables
C
      DOUBLE PRECISION X1
      INTEGER I,J,K,K1
      INTEGER K2,L1,L2,LAM
      INTEGER LAMIND,LAMST,LMAX,LMIN
      INTEGER N1,N1P,N2,N2MAX
      INTEGER N2P
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      LAMIND = (LAMCC+1)/2
C
      DO I = 1,KCMAX
        DO J = 1,KCMAX
          DO K = 1,LAMIND
            ICCPOL(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
C  Loop over the continuum and bound angular momenta
C
      DO K1 = KCMIN,KCMAX
        L1 = LAG(K1)
        DO K2 = K1,KCMAX
          L2 = LAG(K2)
C
C  Determine the range for LAM
C  Loop over LAM
C
          LMIN = ABS(L1-L2)
          IF (LMIN.EQ.0) LMIN = 2
          LMAX = MIN(L1+L2,LAMCC)
          IF (LMIN.LE.LMAX) THEN
            DO LAM = LMIN,LMAX,2
C
              LAMST = (LAM+1)/2
              ICCPOL(K1,K2,LAMST) = IRK1+1
C
C  Loop over the principal quantum numbers
C
              DO N1 = 1,NRANG2
                N1P = MAXNQN(K1)+N1
                IF (K1.EQ.K2) THEN
                  N2MAX = N1
                ELSE
                  N2MAX = NRANG2
                ENDIF
                DO N2 = 1,N2MAX
                  N2P = MAXNQN(K2)+N2
C
C  Calculate length integral
C
                  IRK1 = IRK1+1
                  IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                  IF (IFLAG.EQ.0) THEN
                    CALL SKIPER(
     +K1, K2, KCMIN, NPTS, NRANG2, KBMAX, LAG, MAXNQN,
     +NUMORB, IPOS, CP, CQ)
                    CALL INTRD(
     +N1P, K1, N2P, K2, LAM, X1, CP, CQ, WEIGHT, RMESH,
     +IWRITE, ITC, NPTS, IPOS)
                    RKSTO(IRK1) = X1
                  ENDIF
C
C  Calculate velocity integral, when LAM = 1
C
                  IF (LAM.EQ.1) THEN
                    IRK1 = IRK1+1
                    IF (IRK1.GT.IDMTST(3)) IFLAG = 1
                    IF (IFLAG.EQ.0) THEN
                      CALL INTRDV(
     +N1P, K1, N2P, K2, X1, CP, CQ, WEIGHT, IWRITE,
     +ITC, NPTS, IPOS, KAG, CL)
                      RKSTO(IRK1) = X1
                    ENDIF
                  ENDIF
C
                ENDDO
              ENDDO
C
            ENDDO
          ENDIF
C
        ENDDO
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    INTRD.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE INTRD(N1,K1,N2,K2,KK,RESULT,CP,CQ,WEIGHT,RMESH,IWRITE,I!
     +TC,NPTS,IPOS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/INTRD.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1     ... orbital 1, principal quantum number
C   K1     ... orbital 1, K-value
C   N2     ... orbital 2, principal quantum number
C   K2     ... orbital 2, K-value
C   KK     ... exponent of r in integral
C o RESULT ... value of integral
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   WEIGHT ... array containing radial integration weights
C   RMESH  ... array containing regular radial mesh
C   IWRITE ... stream number for printed output
C   ITC    ... array containing option flags
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   IPOS   ... array giving orbital index for CP and CQ arrays
C
C o indicates output from routine
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION RESULT
      INTEGER K1,K2,KK
      INTEGER N1,N2
C
      DOUBLE PRECISION CP(*),CQ(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION RMESH(*)
      INTEGER IWRITE
      INTEGER ITC(*)
      INTEGER NPTS
      INTEGER IPOS(ND15,*)
C
C  Local variables
C
      DOUBLE PRECISION X(MXNP)
      INTEGER J,J1,J2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  form the integrand including integration weights
C
      J1 = NPTS*(IPOS(N1,K1)-1)
      J2 = NPTS*(IPOS(N2,K2)-1)
C
      IF (KK.EQ.0) THEN
        DO J = 1,NPTS
          X(J) = WEIGHT(J)*(CP(J1+J)*CP(J2+J)+CQ(J1+J)*CQ(J2+J))
        ENDDO
        GOTO 10
      ENDIF
C
      IF (KK.EQ.1) THEN
        DO J = 1,NPTS
          X(J) = WEIGHT(J)*(CP(J1+J)*CP(J2+J)+CQ(J1+J)*CQ(J2+J))*RMESH(J!
     +)
        ENDDO
        GOTO 10
      ENDIF
C
      IF (KK.EQ.2) THEN
        DO J = 1,NPTS
          X(J) = WEIGHT(J)*(CP(J1+J)*CP(J2+J)+CQ(J1+J)*CQ(J2+J))*RMESH(J!
     +)*RMESH(J)
        ENDDO
        GOTO 10
      ENDIF
C
      IF (KK.EQ.-1) THEN
        DO J = 1,NPTS
          X(J) = WEIGHT(J)*(CP(J1+J)*CP(J2+J)+CQ(J1+J)*CQ(J2+J))/RMESH(J!
     +)
        ENDDO
        GOTO 10
      ENDIF
C
      DO J = 1,NPTS
        X(J) = WEIGHT(J)*(CP(J1+J)*CP(J2+J)+CQ(J1+J)*CQ(J2+J))*RMESH(J)*!
     +*KK
      ENDDO
C
C  sum the integrand
C
   10 CONTINUE
      RESULT = ZERO
      DO J = 1,NPTS
        RESULT = RESULT+X(J)
      ENDDO
C
      IF (ITC(15).EQ.1) THEN
        WRITE (IWRITE,3000) N1,K1,N2,K2,KK,RESULT
      ENDIF
C
 3000 FORMAT (1X,2(2I3,2X),2X,I3,4X,1P,E12.5)
      END
CEND--------------------------------------------------------------------
CEND    INTRD1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE INTRD1(MODE,N1,N2,K1,RESULT,IWRITE,ITC,NIX,IRX,NPTS,KAG!
     +,KBMAX,MAXNLG,MAXNQN,IPOS,HINT,RMESH,WEIGHT,CL,EIGENS,ZCORE,ZNUCL,!
     +ZSTAT,CP,CQ)
CRCS
CRCS $Source: /home/phn/DARC/RCS/INTRD1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   MODE   ... flag that indicates parts of integrand can be reused
C   N1     ... orbital 1, principal quantum number
C   N2     ... orbital 2, principal quantum number
C   K1     ... orbital 1, K-value
C o RESULT ... value of integral
C
C   IWRITE ... stream number for printed output
C   ITC    ... array containing option flags
C   NIX    ... number of subintervals
C   IRX    ... defines the radial index at the end of the subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   HINT   ... basic step-size
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   CL     ... speed of light in au
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   ZCORE  ... core potential, tabulated on the regular radial mesh
C   ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION RESULT
      INTEGER K1,MODE,N1,N2
C
      INTEGER IWRITE
      INTEGER ITC(*)
      INTEGER NIX
      INTEGER IRX(*)
      INTEGER NPTS
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER IPOS(ND15,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION CL
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
C
C  Local variables
C
      DOUBLE PRECISION A,AKAP,B,CCL
      DOUBLE PRECISION RCL
      DOUBLE PRECISION P1(MXNP),Q1(MXNP)
      DOUBLE PRECISION P2(MXNP),Q2(MXNP)
      DOUBLE PRECISION DP2(MXNP),DQ2(MXNP)
      DOUBLE PRECISION PART1(MXNP),PART2(MXNP)
      DOUBLE PRECISION STORE(MXNP)
      DOUBLE PRECISION TMP(MXNP)
      INTEGER I1,I2,IQ,ITEST
      INTEGER L,MAXHF
      INTEGER FIRST
C
      SAVE PART1,PART2
      SAVE FIRST
      SAVE STORE
      SAVE CCL,RCL
      DATA FIRST/0/
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   Save some variables on the first call
C
      IF (FIRST.EQ.0) THEN
        DO L = 1,NPTS
          STORE(L) = -WEIGHT(L)*((ZNUCL(L)+ZCORE(L))-ZSTAT(L))/RMESH(L)
        ENDDO
        CCL = CL+CL
        RCL = ONE/CL
        FIRST = 1
      ENDIF
C
C  Get the radial components of orbital 1
C
      I1 = NPTS*(IPOS(N1,K1)-1)
      DO L = 1,NPTS
        IQ = I1+L
        P1(L) = CP(IQ)
        Q1(L) = CQ(IQ)
      ENDDO
C
C  Test which formula to use
C
      MAXHF = MAXNQN(K1)
      IF (N2.LE.MAXHF) GOTO 10
C
      ITEST = 0
      IF (K1.LE.KBMAX) ITEST = MAXHF - MAXNLG(K1)
      IF (ITEST.EQ.0) GOTO 20
C-----------------------------------------------------------------------
C
C  Use the full formula if either
C  1. orbital 2 is bound
C  2. Schmidt orthogonalisation was used
C
   10 CONTINUE
C
      IF (MODE.EQ.0) THEN
C
        AKAP = KAG(K1)
C
C  Get the radial components of orbital 2
C
        I2 = NPTS*(IPOS(N2,K1)-1)
        DO L = 1,NPTS
          IQ = I2+L
          P2(L) = CP(IQ)
          Q2(L) = CQ(IQ)
        ENDDO
C
C  Evaluate the derivatives
C
        CALL DDET5(P2,DP2,HINT,IRX,NIX)
        CALL DDET5(Q2,DQ2,HINT,IRX,NIX)
C
C  Construct the two parts of the integrand
C
C  I_1 = (1/r) [ kappa Q_2 - (1/c) (Z_n+Z_c) P_2 ] - Q_2'
C  I_2 = (1/r) [ kappa P_2 - (1/c) (Z_n+Z_c) Q_2 ] + P_2' - 2c Q_2
C
        DO L = 1,NPTS
          A = (AKAP*Q2(L)-(ZNUCL(L)+ZCORE(L))*P2(L)*RCL)/RMESH(L)
          B = (AKAP*P2(L)-(ZNUCL(L)+ZCORE(L))*Q2(L)*RCL)/RMESH(L)
          PART1(L) = WEIGHT(L)*(A-DQ2(L))
          PART2(L) = WEIGHT(L)*(B+DP2(L)-CCL*Q2(L))
        ENDDO
C
      ENDIF
C
C  Form the integrand
C
C  int_0^RA ( I_1 P_1 + I_2 Q_1 ) dr
C
      DO L = 1,NPTS
        TMP(L) = P1(L)*PART1(L)+Q1(L)*PART2(L)
      ENDDO
C
C  Sum the integrand
C
      RESULT = ZERO
      DO L = 1,NPTS
        RESULT = RESULT+TMP(L)
      ENDDO
C
C  Multiply by the speed of light
C
      RESULT = CL*RESULT
      GOTO 30
C-----------------------------------------------------------------------
   20 CONTINUE
C
C  Construct the two parts of the integrand
C
C  I_1 = - (1/r) (Z_n+Z_c-Z_s) P_2
C  I_2 = - (1/r) (Z_n+Z_c-Z_s) Q_2
C
      IF (MODE.EQ.0) THEN
        I2 = NPTS*(IPOS(N2,K1)-1)
        DO L = 1,NPTS
          IQ = I2+L
          PART1(L) = STORE(L)*CP(IQ)
          PART2(L) = STORE(L)*CQ(IQ)
        ENDDO
      ENDIF
C
C  Form the integrand
C
C  int_0^RA ( I_1 P_1 + I_2 Q_1 ) dr
C
      DO L = 1,NPTS
        TMP(L) = P1(L)*PART1(L)+Q1(L)*PART2(L)
      ENDDO
C
C  Sum the integrand
C
      RESULT = ZERO
      DO L = 1,NPTS
        RESULT = RESULT+TMP(L)
      ENDDO
C
C  Add the eigenvalue in the diagonal case
C
      IF (N1.EQ.N2) RESULT = RESULT + EIGENS(K1,N1-MAXHF)
C-----------------------------------------------------------------------
   30 CONTINUE
      IF (ITC(16).EQ.1) WRITE (IWRITE,3000) N1,K1,N2,K1,RESULT
C
 3000 FORMAT (1X,2(2I3,2X),2X,1P,E12.5)
      END
CEND--------------------------------------------------------------------
CEND    INTRD2.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE INTRD2(N1,K1,N2,K2,N3,K3,N4,K4,K,RESULT,MODE,IWRITE,ITC!
     +,WEIGHT,RMESH,DR1,HINT,IRX,NIX,NPTS,IPOS,CP,CQ)
CRCS
CRCS $Source: /home/phn/DARC/RCS/INTRD2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1     ... orbital 1, principal quantum number
C   K1     ... orbital 1, K-value
C   N2     ... orbital 2, principal quantum number
C   K2     ... orbital 2, K-value
C   N3     ... orbital 3, principal quantum number
C   K3     ... orbital 3, K-value
C   N4     ... orbital 4, principal quantum number
C   K4     ... orbital 4, K-value
C   K      ... order of Slater potential
C o RESULT ... value of integral
C   MODE   ... flag that indicates the Slater potential can be reused
C   IWRITE ... stream number for printed output
C   ITC    ... array containing option flags
C   WEIGHT ... array containing radial integration weights
C   RMESH  ... array containing regular radial mesh
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION RESULT
      INTEGER K,K1,K2,K3,L
      INTEGER K4,MODE,N1,N2
      INTEGER N3,N4
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IPOS(ND15,*)
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER NIX
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION ASY
      DOUBLE PRECISION TMP(MXNP),Y(MXNP)
      INTEGER J,J1,J2,J3
      INTEGER J4
C
      SAVE Y
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Calculate the Slater potential if MODE=0
C
      IF (MODE.EQ.0) THEN
C
C  Locate the positions of orbitals
C
        J1 = NPTS*(IPOS(N1,K1)-1)
        J3 = NPTS*(IPOS(N3,K3)-1)
C
        DO J = 1,NPTS
          TMP(J) = CP(J1+J)*CP(J3+J)+CQ(J1+J)*CQ(J3+J)
        ENDDO
C
        IF (K.EQ.0) THEN
          IF (N1.EQ.N3 .AND. K1.EQ.K3) THEN
            ASY = ONE
          ELSE
            ASY = ZERO
          ENDIF
        ENDIF
C
C  Evaluate (1/r) Y^k(TMP;r)
C
        CALL DQSF(K,TMP,Y,ASY,RMESH,DR1,HINT,IRX,NIX,NPTS)
C
C  Multiply (1/r) Y^k(TMP;r) by the integration weights
C
        DO J = 1,NPTS
          Y(J) = Y(J)*WEIGHT(J)
        ENDDO
C
      ENDIF
C-----------------------------------------------------------------------
C
C  Locate the positions of orbitals
C
      J2 = NPTS*(IPOS(N2,K2)-1)
      J4 = NPTS*(IPOS(N4,K4)-1)
C
C  Form & Sum the integrand
C
      RESULT = ZERO
      DO J = 1,NPTS
        RESULT = RESULT + (CP(J2+J)*CP(J4+J)+CQ(J2+J)*CQ(J4+J))*Y(J)
      ENDDO
C
C  Print the integrand
C

      IF (ITC(17).EQ.1)WRITE (IWRITE,3000) N1,K1,N2,K2,N3,K3,N4,K4,K,RES!
     +ULT
C
 3000 FORMAT (1X,4(2I3,2X),2X,I3,4X,1P,E12.5)
      END
CEND--------------------------------------------------------------------
CEND    INTRDV.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE INTRDV(N1,K1,N2,K2,RESULT,CP,CQ,WEIGHT,IWRITE,ITC,NPTS,!
     +IPOS,KAG,CL)
CRCS
CRCS $Source: /home/phn/DARC/RCS/INTRDV.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1     ... orbital 1, principal quantum number
C   K1     ... orbital 1, K-value
C   N2     ... orbital 2, principal quantum number
C   K2     ... orbital 2, K-value
C o RESULT ... value of integral
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   WEIGHT ... array containing radial integration weights
C   IWRITE ... stream number for printed output
C   ITC    ... array containing option flags
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   KAG    ... array of orbital kappa quantum numbers
C   CL     ... speed of light in au
C
C o indicates output from routine
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION RESULT
      INTEGER K1,K2
      INTEGER N1,N2
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IPOS(ND15,*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KAG(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION AA,AX,BB
      DOUBLE PRECISION X(MXNP)
      INTEGER J,J1,J2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      AX = DBLE(KAG(K2)-KAG(K1))
      AA = ONE+AX
      BB = -ONE+AX
C
C  form the integrand including integration weights
C
      J1 = NPTS*(IPOS(N1,K1)-1)
      J2 = NPTS*(IPOS(N2,K2)-1)
C
      DO J = 1,NPTS
        X(J) = WEIGHT(J)*(AA*CP(J1+J)*CQ(J2+J)+BB*CQ(J1+J)*CP(J2+J))
      ENDDO
C
C  sum the integrand
C
      RESULT = ZERO
      DO J = 1,NPTS
        RESULT = RESULT+X(J)
      ENDDO
C
C  multiply by the speed of light
C
      RESULT = CL*RESULT
C
      IF (ITC(15).EQ.1) THEN
        WRITE (IWRITE,3000) N1,K1,N2,K2,RESULT
      ENDIF
C
 3000 FORMAT (1X,2(2I3,2X),2X,'  1',4X,1P,E12.5)
      END
CEND--------------------------------------------------------------------
CEND    RDINTS.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RDINTS(IREAD, IWRITE, ITC, yJTARG,
     + yLAMBB, yLAMBC, yLAMCC)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RDINTS.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IREAD  ... stream number of file containing input data
C   IWRITE ... stream number for printed output
C o ITC    ... array containing option flags
C o JTARG  ... maximum 2J-value for the target states
C o LAMBB  ... maximum lambda value allowed for bound-bound
C o LAMBC  ... maximum lambda value allowed for bound-continuum
C o LAMCC  ... maximum lambda value allowed for continuum-continuum
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IREAD
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER yJTARG
      INTEGER yLAMBB
      INTEGER yLAMBC
      INTEGER yLAMCC
C
C  Local variables
C
      INTEGER I
      INTEGER OPT(50)
C
C  Namelists
C
      INTEGER JTARG
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
C
      NAMELIST / INTS / JTARG, LAMBB, LAMBC, LAMCC, OPT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
C-----------------------------------------------------------------------
C
C   Input record 1
C
C   Namelist INTS
C
C-----------------------------------------------------------------------
      DO I = 1,50
        OPT(I) = 0
      ENDDO
      JTARG = -1
      LAMBB = 4
      LAMBC = 1
      LAMCC = 1
C-----------------------------------------------------------------------
      READ (IREAD,INTS)
C-----------------------------------------------------------------------
      DO I = 1,50
        ITC(I) = 0
      ENDDO
C
      DO I = 1,50
        IF (OPT(I).GT.0 .AND. OPT(I).LE.50) THEN
          ITC(OPT(I)) = 1
        ENDIF
      ENDDO
C
      IF (LAMBB.LT.1 .OR. LAMBB.GT.10) THEN
        WRITE (IWRITE,3130)
        STOP
      ENDIF
C
      IF (LAMBC.GT.LAMBB .OR. LAMCC.GT.LAMBB .OR. LAMBC.LT.0 .OR. LAMCC.!
     +LT.0) THEN
        WRITE (IWRITE,3140)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      WRITE (IWRITE,3010) JTARG,LAMBB,LAMBC,LAMCC
C
      yJTARG = JTARG
      yLAMBB = LAMBB
      yLAMBC = LAMBC
      yLAMCC = LAMCC
C-----------------------------------------------------------------------
 3000 FORMAT (/31X,'Routine RDINTS'/31X,'--------------'/)
 3010 FORMAT (/                                                         !
     +' JTARG  (max. 2J for target)                          : ',I5/
     +' LAMBB  (bound-bound multipole parameter)             : ',I5/    !
     +' LAMBC  (bound-continuum multipole parameter)         : ',I5/    !
     +' LAMCC  (continuum-continuum multipole parameter)     : ',I5/)
 3130 FORMAT (/' ***************************************************'/  !
     +' ***       WARNING from routine RDINTS           ***'/           !
     +' *** You can only have LAMBB lying between 1 and ***'/           !
     +' ***  10 inclusive with present dimensions       ***'/           !
     +' ***  The parameter NDZ needs to be increased    ***'/           !
     +' ***          Code is STOPPING.                  ***'/           !
     +' ***************************************************'/)
 3140 FORMAT (/' *****************************************************'/!
     +' ***       WARNING from routine RDINTS             ***'/         !
     +' *** LAMBC and LAMCC must not be larger than LAMBB ***'/         !
     +' ***     They should be non-negative also          ***'/         !
     +' ***          Code is STOPPING.                    ***'/         !
     +' *****************************************************'/)
      END
CEND--------------------------------------------------------------------
CEND    RDISC1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RDISC1(IDMTST, IWRITE, KBMAX, LAG, MAXNQN, NDIMAX,NPTS,!
     + CP, CQ, IPOS,RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RDISC1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDMTST ... array containing maximum dimensions set in the code
C   IWRITE ... stream number for printed output
C   KBMAX  ... maximum bound K-value
C   LAG    ... array of orbital l quantum numbers
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o CP     ... array containing orbital large components
C o CQ     ... array containing orbital small components
C o IPOS   ... array giving orbital index for CP and CQ arrays
C o RMESH  ... array containing regular radial mesh
C o WEIGHT ... array containing radial integration weights
C o ZCORE  ... core potential, tabulated on the regular radial mesh
C o ZNUCL  ... nuclear potential, tabulated on the regular radial mesh
C o ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IWRITE
      INTEGER KBMAX
      INTEGER LAG(*)
      INTEGER MAXNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION PA(MXNP),QA(MXNP)
      INTEGER IREC,K,K1,K2
      INTEGER KB,KK,LB,MAXHF
      INTEGER NB,NBOUND,NS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NBOUND = 0
C
      IREC = 1
      CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,RMESH)
      CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,WEIGHT)
C
      DO KB = 1,KBMAX
        LB = LAG(KB)+1
        MAXHF = MAXNQN(KB)
        CALL DMCHK1(23,MAXHF, IWRITE, IDMTST, NDIMAX)
        DO NB = LB,MAXHF
C
          NBOUND = NBOUND+1
          IPOS(NB,KB) = NBOUND
C
          NS = NPTS*(NBOUND-1)
          K1 = NS+1
          K2 = NS+NPTS
          CALL DMCHK1(13,K2, IWRITE, IDMTST, NDIMAX)
C
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,PA)
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,QA)
C
          KK = K1
          DO K = 1,NPTS
            CP(KK) = PA(K)
            CQ(KK) = QA(K)
            KK = KK+1
          ENDDO
C
        ENDDO
      ENDDO
C
      CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,ZSTAT)
      CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,ZNUCL)
      CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,ZCORE)
C-----------------------------------------------------------------------
      END
CEND--------------------------------------------------------------------
CEND    RDISC4.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RDISC4 (
     + IDISC4, IDISC5, BSTO, CBUT, CL, DR1, EBUT, EIGENS, ENDS,
     + HINT, IDMTST, IHED, IRX, IWRITE, JAG, KAG, KBMAX, KCMAX,
     + KCMIN, KX, LAG, LAMBB, LAMBC, LAMCC, MAXFUL, MAXNLG,
     + MAXNQN, MBUT, MINNQN, NDIMAX, NELC, NIX,
     + NRANG2, NUMORB, NZ, RA, RECORD)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RDISC4.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:20:04 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C i IDISC4 ... stream number of ORBS.DAT (DSTG1-ORBS dump file)
C i IDISC5 ... stream number of DSTG1.DAT (dstg1 dump file)
C   BSTO   ... constant that arises in the boundary condition for
C              the continuum orbitals
C   CBUT   ... Buttle corrections
C   CL     ... speed of light in au
C   DR1    ... initial radial point (equals HINT)
C   EBUT   ... Buttle energies (in au)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   ENDS   ... amplitude of large component of the continuum orbitals
C              at the R-matrix boundary RA
C   HINT   ... basic step-size
C i IDMTST ... array containing maximum dimensions set in the code
C   IHED   ... 80-character title for the calculation
C   IRX    ... defines the radial index at the end of the subintervals
C i IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   KX     ... maximum of KBMAX and KCMAX
C   LAG    ... array of orbital l quantum numbers
C i LAMBB  ... maximum lambda value allowed for bound-bound
C              multipole integrals
C i LAMBC  ... maximum lambda value allowed for bound-continuum
C              multipole integrals
C i LAMCC  ... maximum lambda value allowed for continuum-continuum
C              multipole integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MBUT   ... array giving the number of Buttle corrections stored
C              for each K-value
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NELC   ... number of target electrons
C   NIX    ... number of subintervals
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   NZ     ... atomic number
C   RA     ... R-matrix radius
C   RECORD ... 20-character record of time/date at execution
C
C i denotes input to routine (unchanged)
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IDISC4
      INTEGER IDISC5
C
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION CBUT(MXNK,*)
      DOUBLE PRECISION CL
      DOUBLE PRECISION DR1
      DOUBLE PRECISION EBUT(MXNK,*)
      DOUBLE PRECISION EIGENS(MXNK,*)
      DOUBLE PRECISION ENDS(MXNK,*)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      INTEGER IRX(*)
      INTEGER IWRITE
      INTEGER IDMTST(*)
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KX
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MBUT(*)
      INTEGER MINNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NELC
      INTEGER NIX
      INTEGER NRANG2
      INTEGER NUMORB
      INTEGER NZ
C
C  Local variables
C
      INTEGER L
      INTEGER N
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      REWIND IDISC4
      REWIND IDISC5
C
C note change to 3rd record
C
      READ (IDISC4) IHED,RECORD
      READ (IDISC4) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      CALL DMCHK1(8, KX, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK1(6, NRANG2, IWRITE, IDMTST, NDIMAX)
      READ (IDISC4)
C
      WRITE (IDISC5) IHED,RECORD
      WRITE (IDISC5) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      WRITE (IDISC5) LAMBB,LAMBC,LAMCC
C
      READ (IDISC4)
     +(MINNQN(L),L=1,KX), (MAXNQN(L),L=1,KX),
     +(MAXNLG(L),L=1,KX), (MAXFUL(L),L=1,KBMAX),
     +(LAG(L),L=1,KX), (KAG(L),L=1,KX), (JAG(L),L=1,KX)
C
      WRITE (IDISC5)
     +(MINNQN(L),L=1,KX), (MAXNQN(L),L=1,KX),
     +(MAXNLG(L),L=1,KX), (MAXFUL(L),L=1,KBMAX),
     +(LAG(L),L=1,KX), (KAG(L),L=1,KX), (JAG(L),L=1,KX)
C
      READ (IDISC4) RA,BSTO,HINT,CL,DR1
      READ (IDISC4) NIX
      CALL DMCHK1(24, NIX, IWRITE, IDMTST, NDIMAX)
      READ (IDISC4) (IRX(L),L=1,NIX)
      READ (IDISC4) NUMORB
C
      WRITE (IDISC5) RA,BSTO,HINT,CL,DR1
      WRITE (IDISC5) NIX
      WRITE (IDISC5) (IRX(L),L=1,NIX)
      WRITE (IDISC5) NUMORB
C
      WRITE (IWRITE,3000)
C
      DO L = KCMIN,KCMAX
C
        READ (IDISC4) (EIGENS(L,N),N=1,NRANG2)
        READ (IDISC4) (ENDS(L,N),N=1,NRANG2)
        READ (IDISC4) MBUT(L)
        READ (IDISC4) (EBUT(L,N),N=1,MBUT(L))
        READ (IDISC4) (CBUT(L,N),N=1,MBUT(L))
C
        WRITE (IDISC5) (EIGENS(L,N),N=1,NRANG2)
        WRITE (IDISC5) (ENDS(L,N),N=1,NRANG2)
        WRITE (IDISC5) MBUT(L)
        WRITE (IDISC5) (EBUT(L,N),N=1,MBUT(L))
        WRITE (IDISC5) (CBUT(L,N),N=1,MBUT(L))
C
      ENDDO
C
      REWIND IDISC4
C
      WRITE (IWRITE,3010)
C=======================================================================
 3000 FORMAT (
     +/' **** basic data read from stream IDISC4 by routine RDISC4')
 3010 FORMAT (
     + ' **** eigenvalues, amplitudes and Buttle corrections read ok')
      END
CEND--------------------------------------------------------------------
CEND    SKIPER.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SKIPER(K1,K2,KCMIN,NPTS,NRANG2,KBMAX,LAG,MAXNQN,NUMORB,!
     +IPOS,CP,CQ)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SKIPER.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:50:36 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   If only one K1 orbital is required then K2 must be equal to zero.
C
C   K1     ... required K-value
C   K2     ... required K-value
C   KCMIN  ... minimum continuum K-value
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   KBMAX  ... maximum bound K-value
C   LAG    ... array of orbital l quantum numbers
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NUMORB ... number of bound orbitals
C o IPOS   ... array giving orbital index for CP and CQ arrays
C o CP     ... array containing orbital large components
C o CQ     ... array containing orbital small components
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
C
C  Argument variables
C
      INTEGER K1,K2
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      INTEGER IPOS(ND15,*)
      INTEGER KBMAX
      INTEGER KCMIN
      INTEGER LAG(*)
      INTEGER MAXNQN(*)
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
C
C  Local variables
C
      INTEGER IREC,K,KK,KPOS1
      INTEGER KPOS2,LBL,MAXHF,N
      INTEGER NBL,NUMB
C
      DOUBLE PRECISION PA(MXNP),QA(MXNP)
      INTEGER K1STO
      INTEGER K2STO
      INTEGER FIRST
      INTEGER NREC
C
      SAVE K1STO
      SAVE K2STO
      SAVE FIRST
      SAVE NREC
C
      DATA K1STO/0/
      DATA K2STO/0/
      DATA FIRST/0/
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF (FIRST.EQ.0) THEN
        NREC = 1
        CALL DA1('ORBITALS.DAT',1,NREC,11,NPTS,PA)
        NREC = NREC-1
        FIRST = 1
      ENDIF
C-----------------------------------------------------------------------
C
C  Return if the orbitals are already available.
C
C   K1STO  ... indicates the current K-value of orbitals in CP/CQ array
C   K2STO  ... indicates the current K-value of orbitals in CP/CQ array
C
C-----------------------------------------------------------------------
      IF (K2.EQ.0) THEN
        IF (K1.EQ.K1STO .OR. K1.EQ.K2STO) RETURN
      ELSE
        IF (K1.EQ.K2) THEN
          IF (K1.EQ.K1STO .OR. K1.EQ.K2STO) RETURN
        ELSE
          IF (K1.EQ.K1STO .AND. K2.EQ.K2STO) RETURN
          IF (K1.EQ.K2STO .AND. K2.EQ.K1STO) RETURN
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C  Determine the position to replace.
C
C-----------------------------------------------------------------------
      IF (K2.EQ.0) THEN
        KPOS1 = K1
        KPOS2 = K2STO
        GOTO 10
      ENDIF
C
      IF (K1.EQ.K2) THEN
        KPOS1 = K1
        KPOS2 = K2STO
        GOTO 10
      ENDIF
C
      IF (K1.EQ.K1STO) THEN
        KPOS1 = K1STO
        KPOS2 = K2
        GOTO 10
      ENDIF
C
      IF (K1.EQ.K2STO) THEN
        KPOS1 = K2
        KPOS2 = K2STO
        GOTO 10
      ENDIF
C
      IF (K2.EQ.K1STO) THEN
        KPOS1 = K1STO
        KPOS2 = K1
        GOTO 10
      ENDIF
C
      IF (K2.EQ.K2STO) THEN
        KPOS1 = K1
        KPOS2 = K2STO
        GOTO 10
      ENDIF
C
      KPOS1 = K1
      KPOS2 = K2
C-----------------------------------------------------------------------
   10 CONTINUE
C
      IF (KPOS1.NE.K1STO) THEN
C
        NUMB = NUMORB
        KK = NPTS*NUMB+1
        MAXHF = LAG(KPOS1)
        IF (KPOS1.LE.KBMAX) MAXHF = MAXNQN(KPOS1)
        IREC = NREC*(5+2*NUMORB+2*(KPOS1-KCMIN)*NRANG2)+1
C
        DO N = 1,NRANG2
C
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,PA)
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,QA)
C
          DO K = 1,NPTS
            CP(KK) = PA(K)
            CQ(KK) = QA(K)
            KK = KK+1
          ENDDO
C
          NBL = MAXHF+N
          LBL = NUMB+N
          IPOS(NBL,KPOS1) = LBL
C
        ENDDO
C
        K1STO = KPOS1
C
      ENDIF
C
      IF (KPOS2.NE.K2STO) THEN
C
        NUMB = NUMORB+NRANG2
        KK = NPTS*NUMB+1
        MAXHF = LAG(KPOS2)
        IF (KPOS2.LE.KBMAX) MAXHF = MAXNQN(KPOS2)
        IREC = NREC*(5+2*NUMORB+2*(KPOS2-KCMIN)*NRANG2)+1
C
        DO N = 1,NRANG2
C
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,PA)
          CALL DA1('ORBITALS.DAT',1,IREC,11,NPTS,QA)
C
          DO K = 1,NPTS
            CP(KK) = PA(K)
            CQ(KK) = QA(K)
            KK = KK+1
          ENDDO
C
          NBL = MAXHF+N
          LBL = NUMB+N
          IPOS(NBL,KPOS2) = LBL
C
        ENDDO
C
        K2STO = KPOS2
C
      ENDIF
C-----------------------------------------------------------------------
      END
CEND--------------------------------------------------------------------
CEND    DA1.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DA1(FILEN,KEY,IREC,JDISC,LENGTH,ARRAY)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DA1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:57:16 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C    FILEN : filename
C
C    KEY = 1 for read,
C        = 2 for write,
C        = 0 for finding number of DA records a given array takes.
C
C    IREC =  (on call) pointer to first DA record for array,
C         =  0 for opening DA file (by name),
C         = -1 for opening DA file (scratch),
C         =  (on return) pointer to next available DA record.
C
C    JDISC = DA file unit number.
C
C    ARRAY(LENGTH) = array to read or write.
C                    If LENGTH=0 then nothing is read or written.
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER LREC
      PARAMETER (LREC=512)
C
C  Argument variables
C
      CHARACTER*12 FILEN
      DOUBLE PRECISION ARRAY(*)
      INTEGER IREC,JDISC,KEY,LENGTH
C
C  Local variables
C
      INTEGER I,I1,I2,IRECL
      LOGICAL EX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IREC.EQ.-1) THEN
        IRECL = 8*LREC
        OPEN (JDISC,STATUS='SCRATCH',ACCESS='DIRECT',RECL=IRECL)
        IREC = 1
      ENDIF
C
      IF (IREC.EQ.0) THEN
        IRECL = 8*LREC
        IF (KEY.EQ.2) THEN
          OPEN (JDISC,STATUS='UNKNOWN',FILE=FILEN,
     +          ACCESS='DIRECT',RECL=IRECL)
        ELSE
          INQUIRE (FILE=FILEN,EXIST=EX)
          IF (.NOT.EX) THEN
            PRINT 3000,FILEN
            STOP
          ENDIF
          OPEN (JDISC,STATUS='OLD',FILE=FILEN,
     +          ACCESS='DIRECT',RECL=IRECL)
        ENDIF
        IREC = 1
      ENDIF
C
      IF (LENGTH.EQ.0) RETURN
C
      I2 = 0
   10 CONTINUE
      I1 = I2+1
      I2 = MIN(I2+LREC,LENGTH)
C
      IF (KEY.EQ.1) THEN
        READ (JDISC,REC=IREC) (ARRAY(I),I=I1,I2)
      ENDIF
C
      IF (KEY.EQ.2) THEN
        WRITE (JDISC,REC=IREC) (ARRAY(I),I=I1,I2)
      ENDIF
C
      IREC = IREC+1
      IF (I2.LT.LENGTH) GOTO 10
C
 3000 FORMAT (/3X,A12,' does not exist....STOPPING.')
      END
CEND--------------------------------------------------------------------
CEND    CALEN.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CALEN(IWRITE,RECORD)
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      CHARACTER*20 RECORD
      INTEGER IWRITE
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      RECORD = ' not available      '
C
      IF (IWRITE.GT.0) WRITE (IWRITE,3000) RECORD
C
 3000 FORMAT (//' Run at : ',A20/)
      END
CEND--------------------------------------------------------------------
CEND    QUARTZ.f    Wed Jan 21 10:16:16 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE QUARTZ(MODE,IWRITE,TIME2)
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      DOUBLE PRECISION TIME2
      INTEGER IWRITE
      INTEGER MODE
C
C  Local variables
C
      DOUBLE PRECISION TIME1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE TIME1
C-----------------------------------------------------------------------
      IF (MODE.EQ.0 .OR. MODE.EQ.2) THEN
C
       call cpu_time(time2)
       time2=time2-time1
C
        IF (MODE.EQ.0) THEN
          IF (IWRITE.GT.0) WRITE (IWRITE,3000) TIME2
        ENDIF
C
        TIME1 = TIME1+TIME2
        RETURN
C
      ENDIF
C-----------------------------------------------------------------------
      IF (MODE.EQ.-1) THEN
        call cpu_time(time1)
        RETURN
      ENDIF
C-----------------------------------------------------------------------
C
C  Error
C
C-----------------------------------------------------------------------
      IF (IWRITE.GT.0) WRITE (IWRITE,3010) MODE
      STOP
C-----------------------------------------------------------------------
 3000 FORMAT (/' Time used = ',F10.2,' sec')
 3010 FORMAT (/' ERROR in QUARTZ.'/                                     !
     +' Routine was called with parameter MODE = ',I3/                  !
     +' The parameter must be -1, 0 or 2.'/' Code is STOPPING.')
      END
