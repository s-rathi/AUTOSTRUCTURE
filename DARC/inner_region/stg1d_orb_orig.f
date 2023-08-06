CEND--------------------------------------------------------------------
CEND    ORBS.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      PROGRAM ORBS
CRCS
CRCS $Source: /home/phn/DARC/RCS/dstg1orb.mak,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:13:30 $
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
      DATA CODE/'DSTG1-ORBS'/
      DATA FILE/'ORBS'/
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
      SOU = '$Source: /home/phn/DARC/RCS/dstg1orb.mak,v $'
      AUT = '$Author: phn $'
      DAT = '$Date: 2001/12/05 19:13:30 $'
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
        WRITE (IWRITE,4130)
        WRITE (IWRITE,4140)
        WRITE (IWRITE,4150)
        WRITE (IWRITE,4160)
        WRITE (IWRITE,4170)
        WRITE (IWRITE,4180)
        WRITE (IWRITE,4190)
        WRITE (IWRITE,4200)
        WRITE (IWRITE,4210)
        WRITE (IWRITE,4220)
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
      OPEN (UNIT=INPUNI,STATUS='SCRATCH',FORM='FORMATTED',ACCESS='SEQUEN!
     +TIAL')
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
      CALL AAORBS(IREAD,IWRITE)
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
     +/' DSTG1-ORBS evaluates the continuum orbitals.'
     +/' '
     +/' In outline:'
     +/' '
     +/' DSTG1-ORBS reads in bound orbitals from TARGET.INP.'
     +/' '
     +/' Using the bound orbitals DSTG1-ORBS will generate an R-matrix'
     +)
 4020 FORMAT (
     + ' boundary and an appropriate radial mesh. All o',
     + 'rbitals (continuum'
     +/' and bound) are tabulated on this radial mesh. ',
     + 'You can optionally'
     +/' read in the R-matrix boundary and/or the radial mesh.'
     +/' '
     +/' The continuum orbitals are generated using a target potential.'
     +/' This is usually the static potential. DSTG1-ORBS will use the'
     +/' bound orbitals to generate such a potential or it can be'
     +/' optionally read in.'
     +/' '
     +/' Routine BASORB generates the continuum orbitals.'
     +/' '
     +/' This produces a set of energies and boundary a',
     + 'mplitudes for each'
     +)
 4030 FORMAT (
     + ' angular momentum. These are stored on the outp',
     + 'ut file ORBS.DAT.'
     +/' The tabulated wave-function is stored on disc ',
     + '(file INTEGRAL.DAT)'
     +/' for use later in evaluating radial integrals.'
     +/' '
     +/' The numerical solution of the differential equ',
     + 'ations uses a 4th'
     +/' order predictor-corrector method (see routine DFBSFN).'
     +/' '
     +/' The Buttle corrections are also calculated at this time. These'
     +/' compensate for the truncation in the continuum',
     + ' orbital expansion.'
     +)
 4040 FORMAT (
     + ' The Buttle correction is a smoothly varying fu',
     + 'nction of energy.'
     +/' For this reason it is calculated at a small number of energy'
     +/' points and polynomial interpolation can be use',
     + 'd for any energy.'
     +/' The way that this is implemented differs from the current'
     +/' versions of the Breit-Pauli R-matrix codes.'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +/' File usage'
     +)
 4060 FORMAT (
     + ' ----------'
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
     +/' |ORBS.INP    |Input.                       |',
     + 'IREAD    5    F   |'
     +/' |ORBS.OUT    |Output.                      |',
     + 'IWRITE   7    F   |'
     +/' |TARGET.INP  |Input bound orbitals.        |',
     + 'JREAD   10    F   |'
     +/' |ORBITALS.DAT|DA file for orbitals.        |',
     + '---     11    U   |'
     +/' |ORBS.SH     |Output (options 36,37,38).   |',
     + 'IDISC3  13    F   |'
     +/' |ORBS.DAT    |DSTG1-ORBS dump.             |',
     + 'IDISC4  14    U   |'
     +)
 4080 FORMAT (
     + ' |---         |Scratch file for input data. |',
     + '---      9    F   |'
     +/' |------------|-----------------------------|',
     + '------------------|'
     +/' '
     +/' DA == direct access'
     +/' F  == formatted'
     +/' U  == unformatted'
     +/' '
     +/'                        TARGET.INP (created by dstg0)'
     +/'                           |'
     +/'                           |'
     +/'                           |'
     +/'          ORBS.INP ---> dstg1-orbs ---> ORBS.OUT & ORBS.SH'
     +)
 4090 FORMAT (
     + '          input            |          output   ',
     + '  optional output'
     +/'                           |'
     +/'                           |'
     +/'                        ORBITALS.DAT (created by dstg1-orbs,'
     +/'                                      used by dstg1-ints)'
     +/'                        ORBS.DAT     (created by dstg1-orbs,'
     +/'                                      used by dstg1-ints)'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +/' Input data on stream IREAD (file: ORBS.INP)'
     +)
 4100 FORMAT (
     + ' --------------------------'
     +/' '
     +/' Input record 1'
     +/' --------------'
     +/' '
     +/' IHED ... 80-character title'
     +/' '
     +/' Input record 2'
     +/' --------------'
     +/' '
     +/' NAMELIST / ORBS / BSTO,   HINT,   IRX,'
     +/'                   KBMAX,  KCMAX,  KCMIN,'
     +/'                   MAXFUL, MAXNLG, MAXNQN, MINNQN,'
     +/'                   NELC,   NIX,    NRANG2, NZ,'
     +)
 4110 FORMAT (
     + '                   RA,     OPT'
     +/' '
     +/' The default values are given in square brackets.'
     +/' '
     +/' BSTO   [0]  ... boundary condition constant.'
     +/' HINT   [0]  ... basic step-size in the regular mesh.'
     +/' IRX    [-1] ... (array) radial index at the end of the'
     +/'                 subintervals in the regular mesh.'
     +/' KBMAX  [-1] ... largest bound K-value.'
     +/' KCMAX  [0]  ... largest continuum K-value.'
     +/' KCMIN  [1]  ... smallest continuum K-value.'
     +/' MAXFUL [-1] ... (array) determines full subshells. Default: no'
     +/'                 full subshells.'
     +/' MAXNLG [-1] ... (array) determines the method of'
     +)
 4120 FORMAT (
     + '                 orthogonalisation. Default: same as MAXNQN.'
     +/' MAXNQN [-1] ... (array) contains the maximum principal quantum'
     +/'                 number.'
     +/' MINNQN [-1] ... (array) contains the minimum principal quantum'
     +/'                 number. Default: all orbitals are included.'
     +/' NELC   [-1] ... number of electrons in the target.'
     +/' NIX    [-1] ... number of subintervals in the regular mesh.'
     +/' NRANG2 [20] ... continuum orbitals per continuum K-value.'
     +/' NZ     [-1] ... atomic number of the target.'
     +/' RA     [0]  ... R-matrix boundary.'
     +/' OPT    [0]  ... (array) options.'
     +/' '
     +/' Operational options.'
     +/' '
     +)
 4130 FORMAT (
     + ' 31 ... double the step-size of the automatic mesh.'
     +/' 32 ... halve the step-size of the automatic mesh.'
     +/' 36 ... write the bound orbitals to ORBS.SH.'
     +/' 37 ... write the static, nuclear and core pote',
     + 'ntials to ORBS.SH.'
     +/' 38 ... write continuum orbitals to ORBS.SH.'
     +/' 40 ... read in static potential from TARGET.INP.'
     +/' 42 ... read in core potential from TARGET.INP.'
     +/' 43 ... replace the orbital occupation numbers (array UCF).'
     +/'        Default: use those from TARGET.INP.'
     +/' 44 ... radial integration weights correspond to Bode rule.'
     +/'        Default: Simpson rule.'
     +/' '
     +/' Print options.'
     +)
 4140 FORMAT (
     + ' '
     +/'  1 ... Debug print from routine FINDER.'
     +/'  2 ... Debug print from routine DFBSFN.'
     +/'  3 ... Debug print from routine DFBSFN.'
     +/'  4 ... Debug print from routine DFBSFN.'
     +/'  5 ... Print orbital overlap integrals in routine BASORB.'
     +/'  6 ... Print Schmidt coefficients in routine SCHMDT.'
     +/'  8 ... Print Buttle corrections in routine BASORB.'
     +/' 15 ... Print multipole radial integrals in routine INTRD.'
     +/' '
     +/' HINT>0 ---> read in regular mesh.'
     +/' RA=0   ---> a boundary will be determined automatically.'
     +/' '
     +)
 4150 FORMAT (
     + ' Input record 3'
     +/' --------------'
     +/' '
     +/' When option 43 is set, read in the orbital occupation numbers'
     +/' (array UCF). This overwrites the array UCF read from file'
     +/' TARGET.INP.'
     +/' '
     +/' Read the following input records using FORTRAN free-format.'
     +/' '
     +/'  A. (UCF(J),J=1,NW)'
     +/' '
     +/' ===================================================',
     + '=============='
     +)
 4160 FORMAT (
     + ' ===================================================',
     + '=============='
     +/' '
     +/' Input data on stream JREAD (file: TARGET.INP)'
     +/' --------------------------'
     +/' '
     +/' Read in the target orbitals'
     +/' ---------------------------'
     +/' '
     +/' Read the following input records using FORTRAN',
     + ' free-format. The'
     +/' input records D to G are repeated for J=1,NW. Note that the'
     +/' orbitals use a common radial mesh DR.'
     +/' '
     +/'  A. NW,N'
     +)
 4170 FORMAT (
     + '  B. (UCF(J),J=1,NW)'
     +/'  C. (DR(I),I=1,N)'
     +/' '
     +/'  D. NP(J),NAK(J)'
     +/'  E. (CP(I),I=1,N)'
     +/'  F. NP(J),NAK(J)'
     +/'  G. (CQ(I),I=1,N)'
     +/' '
     +/' The following data can be added to the file TARGET.INP.'
     +/' The potentials are defined at the radial mesh DR.'
     +/' '
     +/' When option 40 is set then read in the static potential'
     +/' -------------------------------------------------------'
     +/' '
     +)
 4180 FORMAT (
     + ' Read the following input records using FORTRAN free-format.'
     +/' Note that N and DR here can differ from that used above.'
     +/' '
     +/'  A. N'
     +/'  B. (DR(I),I=1,N)'
     +/'  C. (ZSTAT(I),I=1,N)'
     +/' '
     +/' When option 42 is set then read in the core potential'
     +/' -----------------------------------------------------'
     +/' '
     +/' Read the following input records using FORTRAN free-format.'
     +/' Note that N and DR here can differ from that used above.'
     +/' '
     +/'  A. N'
     +)
 4190 FORMAT (
     + '  B. (DR(I),I=1,N)'
     +/'  C. (ZCORE(I),I=1,N)'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +/' NW --- is the number of orbitals.'
     +/' '
     +/' N --- is the number of radial mesh points.'
     +/' '
     +/' UCF(J) J=1,NW --- is the orbital occupation nu',
     + 'mber for orbital J.'
     +)
 4200 FORMAT (
     + ' '
     +/' DR(I) I=1,N --- is the radial mesh.'
     +/' '
     +/' NP(J) J=1,NW --- is the principal (n) quantum ',
     + 'number for orbital'
     +/'                  J. n=1,,3,...'
     +/' '
     +/' NAK(J) J=1,NW --- is the kappa quantum number for orbital J.'
     +/'                   kappa = -1  for  s+ j=1/2'
     +/'                   kappa = +1  for  p- j=1/2'
     +/'                   kappa = -2  for  p+ j=3/2'
     +/'                   kappa = +2  for  d- j=3/2'
     +/'                   kappa = -3  for  d+ j=5/2'
     +/'                   kappa = +3  for  f- j=5/2'
     +)
 4210 FORMAT (
     + '                   kappa = -4  for  f+ j=7/2'
     +/' '
     +/' CP(I) I=1,N --- is an array containing orbital',
     + ' large components.'
     +/' '
     +/' CQ(I) I=1,N --- is an array containing orbital',
     + ' small components.'
     +/' '
     +/' ZSTAT(I) I=1,N --- is the static potential, tabulated on the'
     +/' radial mesh. The behaviour of the static potential Z_s is'
     +/'      Z_s(r) = Z       when   r --> 0'
     +/'               Z-N_e   when   r --> RA'
     +/' where Z is the atomic number (variable NZ) and',
     + ' N_e is the number'
     +)
 4220 FORMAT (
     + ' of target electrons (variable NELC). This assu',
     + 'mes that radius RA'
     +/' encloses the bound orbitals.'
     +/' '
     +/' ZCORE(I) I=1,N --- is the core potential, tabu',
     + 'lated on the radial'
     +/' mesh.'
     +/' '
     +/' ===================================================',
     + '=============='
     +/' ===================================================',
     + '=============='
     +/' '
     +)
      END
CEND--------------------------------------------------------------------
CEND    AAORBS.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AAORBS(IREAD,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AAORBS.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:12:04 $
CRCS $Revision: 11.2 $
CRCS
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
C
C  Argument variables
C
      INTEGER IREAD
      INTEGER IWRITE
C
C  Local variables
C
      CHARACTER*2 LAB(MXNK)
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(MXP1)
      DOUBLE PRECISION CQ(MXP1)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION DUMMY(1)
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(MXNP)
      DOUBLE PRECISION TIME2
      DOUBLE PRECISION WEIGHT(MXNP)
      DOUBLE PRECISION ZCORE(MXNP)
      DOUBLE PRECISION ZNUCL(MXNP)
      DOUBLE PRECISION ZSTAT(MXNP)
      INTEGER IDISC3
      INTEGER IDISC4
      INTEGER IDMTST(30)
      INTEGER IPOS(ND15,MXNK)
      INTEGER IREC1
      INTEGER IRX(NIRX)
      INTEGER ITC(50)
      INTEGER JAG(MXNK)
      INTEGER JREAD
      INTEGER KAG(MXNK)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KX
      INTEGER LAG(MXNK)
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNLG(MXNK)
      INTEGER MAXNQN(MXNK)
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
C-----------------------------------------------------------------------
C
C  Now make calls to:
C
C   QUARTZ ... initialise timing
C   CALEN  ... get RECORD of current time/date
C   DMSET1 ... set dimensions
C
      CALL QUARTZ(-1,IWRITE,TIME2)
      CALL CALEN(IWRITE,RECORD)
      CALL DMSET1(IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
C
C  Read the input data
C
      PRINT 3070
      CALL RDORBS(
     + IDMTST, IREAD, IWRITE, NDIMAX, RECORD, BSTO, CL, DR1, HINT,
     + IHED, IRX, ITC, JAG, KAG, KBMAX, KCMAX, KCMIN, KX, LAB, LAG,
     + MAXFUL, MAXNLG, MAXNQN, MINNQN, NELC, NIX, NPTS, NRANG2, NZ, RA)
C-----------------------------------------------------------------------
      INQUIRE (FILE='TARGET.INP',EXIST=EX)
      IF (.NOT.EX) THEN
        PRINT 3090
        STOP
      ENDIF
      JREAD = 10
      OPEN (UNIT=JREAD,FILE='TARGET.INP',STATUS='OLD',FORM='FORMATTED')
C
      IREC1 = 0
      CALL DA1('ORBITALS.DAT',2,IREC1,11,0,DUMMY)
C
      IDISC3 = 13
C
C  No formatted output
C
      IF (ITC(36).EQ.0 .AND. ITC(37).EQ.0 .AND. ITC(38).EQ.0 ) THEN
        IDISC3 = 0
      ENDIF
C
      IF (IDISC3.GT.0) THEN
        OPEN (UNIT=IDISC3,FORM='FORMATTED',STATUS='UNKNOWN',
     +   FILE='ORBS.SH')
      ENDIF
C
      IDISC4 = 14
      OPEN (UNIT=IDISC4,FORM='UNFORMATTED',STATUS='UNKNOWN',
     + FILE='ORBS.DAT')
C
      WRITE (IWRITE,3080)
C-----------------------------------------------------------------------
C
C  Obtain the regular mesh, bound orbitals and potentials.
C
        PRINT 3020
        CALL STP0 (
     + IDISC3, IREC1, CP, CQ, DR1, HINT, IDMTST, IREAD, IRX,
     + ITC, IWRITE, JREAD, KAG, KBMAX, LAG, MAXNQN, NDIMAX, NELC,
     + NIX, NPTS, NUMORB, NZ, RA, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT)
C-----------------------------------------------------------------------
C
C  Write the first part of the DSTG1-ORBS dump.
C
      PRINT 3040
      CALL WDISC4 (
     + IDISC4, BSTO, CL, DR1, HINT, IHED, IRX, IWRITE, JAG,
     + KAG, KBMAX, KCMAX, KCMIN, KX, LAG, MAXFUL, MAXNLG,
     + MAXNQN, MINNQN, NELC, NIX, NRANG2, NUMORB, NZ, RA, RECORD)
C-----------------------------------------------------------------------
C
C  Read information from ORBITALS.DAT
C
C  1. Store the bound orbitals in the CP and CQ arrays.
C  2. Read arrays RMESH,WEIGHT,ZSTAT,ZNUCL,ZCORE
C
      PRINT 3050
      CALL RDISC1(
     + IDMTST, IWRITE, KBMAX, LAG, MAXNQN, NDIMAX, NPTS, CP, CQ,
     + IPOS, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT)
C-----------------------------------------------------------------------
C
C  Evaluate the continuum orbitals.
C
      PRINT 3060
      IF (KCMAX.GT.0) CALL BASORB (
     + BSTO, CL, CP, CQ, DR1, HINT, IDISC3, IDISC4, IDMTST, IPOS,
     + IREC1, IRX, ITC, IWRITE, JAG, KAG, KBMAX, KCMAX, KCMIN,
     + LAG, MAXNLG, MAXNQN, NDIMAX, NELC, NPTS, NRANG2, NUMORB,
     + NZ, RA, RMESH, WEIGHT, ZSTAT)
C-----------------------------------------------------------------------
      PRINT 3000
      CALL DMPRT1(IWRITE, IDMTST, NDIMAX)
      PRINT 3010
C=======================================================================
 3000 FORMAT (/' calling routine DMPRT1')
 3010 FORMAT (/' STOPPING normally')
 3020 FORMAT (/' calling routine STP0')
 3040 FORMAT (/' calling routine WDISC4')
 3050 FORMAT (/' calling routine RDISC1')
 3060 FORMAT (/' calling routine BASORB')
 3070 FORMAT (/' reading input data in routine RDORBS')
 3080 FORMAT (/1X,71('*')/' I/O files'/1X,71('*')/
     +' ORBS.INP    : IREAD  (input data..................FORMATTED)  ',
     +' :     5'/
     +' ORBS.OUT    : IWRITE (printer output..............FORMATTED)  ',
     +' :     7'/
     +'                      (scratch for input data......FORMATTED)  ',
     +' :     9'/
     +' TARGET.INP  : JREAD  (target orbitals.............FORMATTED)  ',
     +' :    10'/
     +' ORBITALS.DAT:        (direct access orbitals......UNFORMATTED)',
     +' :    11'/
     +' ORBS.SH     : IDISC3 (options 36,37,38............FORMATTED)  ',
     +' :    13'/
     +' ORBS.DAT    : IDISC4 (DSTG1-ORBS dump.............UNFORMATTED)',
     +' :    14'/1X,71('*'))
 3090 FORMAT (/' TARGET.INP does not exist....STOPPING.'/
     +' You must use module DSTG0 to create it.')
      END
CEND--------------------------------------------------------------------
CEND    BASORB.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE BASORB (BSTO, CL, CP, CQ, DR1,HINT, IDISC3, IDISC4, IDM!
     +TST, IPOS, IREC1, IRX,ITC, IWRITE, JAG, KAG, KBMAX, KCMAX, KCMIN, !
     +LAG, MAXNLG, MAXNQN,NDIMAX, NELC, NPTS, NRANG2, NUMORB, NZ,RA, RME!
     +SH, WEIGHT, ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/BASORB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   BSTO   ... constant that arises in the boundary condition for
C              the continuum orbitals
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IDISC3 ... stream number of file containing formatted output
C   IDISC4 ... stream number of ORBS.DAT (DSTG1-ORBS dump file)
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOS   ... array giving orbital index for CP and CQ arrays
C o IREC1  ... pointer to next available DA record in ORBITALS.DAT
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   LAG    ... array of orbital l quantum numbers
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NELC   ... number of target electrons
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   NZ     ... atomic number
C   RA     ... R-matrix radius
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
C
C   CBUT   ... Buttle corrections
C   EBUT   ... Buttle energies (in au)
C   EIGENS ... eigenvalues of the continuum orbitals (in au)
C   ENDS   ... amplitude of large component of the continuum orbitals
C              at the R-matrix boundary RA
C   MBUT   ... array giving the number of Buttle corrections stored
C              for each K-value
C
C   ND24 - dimension for overlap integrals
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION SMALL
      PARAMETER (SMALL=1.D-3)
      DOUBLE PRECISION DPI
      PARAMETER (DPI=0.3141592653589793D1)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION TENTH
      PARAMETER (TENTH=.1D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      INTEGER ND11
      PARAMETER (ND11=12)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
      INTEGER ND24
      PARAMETER (ND24=MXNB+ND11)
      INTEGER NBUT
      PARAMETER (NBUT=2*(MXNB-1))
C
C  Argument variables
C
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IDISC3
      INTEGER IDISC4
      INTEGER IDMTST(*)
      INTEGER IPOS(ND15,*)
      INTEGER IREC1
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER LAG(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NELC
      INTEGER NPTS
      INTEGER NRANG2
      INTEGER NUMORB
      INTEGER NZ
C
C  Local variables
C
      CHARACTER*3 LABEL
      CHARACTER*6 ORBNAM
      DOUBLE PRECISION AKAP,AZ,AZC,BST1
      DOUBLE PRECISION BSTX,BUT1,BUT2,BUT3
      DOUBLE PRECISION EIGEN2,ETRIAL,GAM,P0
      DOUBLE PRECISION FIRSTP
      DOUBLE PRECISION FIRSTQ
      DOUBLE PRECISION Q0,RESULT,SUM
      DOUBLE PRECISION TEMP(ND24,ND24),TIMTOT,U
      DOUBLE PRECISION V,WBUT,WBUT2,WINT
      DOUBLE PRECISION X
      DOUBLE PRECISION ZB(MXNP)
      DOUBLE PRECISION TIME2
      INTEGER I,IJ,INTV,INTV1
      INTEGER INTV2,INTV3,INTV4,ISMIT
      INTEGER ITMP,J,J1,K1
      INTEGER KC,L1,LP,MAXHF
      INTEGER MAXLG,N,N1,N2
      INTEGER N3,N4,NBS,NBT
      INTEGER NCOUNT,ND,NODE,NODES
      INTEGER NS,NUM,NUM1,NUM2
      INTEGER NUM3,NUM4
C
      DOUBLE PRECISION BVALUE
      DOUBLE PRECISION CBUT(MXNK,NBUT)
      DOUBLE PRECISION EBUT(MXNK,NBUT)
      DOUBLE PRECISION EIGENS(MXNK,MXNB)
      DOUBLE PRECISION ENDS(MXNK,MXNB)
      DOUBLE PRECISION ORBP(MXNP)
      DOUBLE PRECISION ORBQ(MXNP)
      INTEGER MBUT(MXNK)
      INTEGER NORI(ND11)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA TIMTOT/0.D0/
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020)
      CALL QUARTZ(0,IWRITE,TIME2)
C
      DO KC = 1,KCMAX
        DO N = 1,NRANG2
          EIGENS(KC,N) = ZERO
          ENDS(KC,N) = ZERO
        ENDDO
      ENDDO
C
      AZ = ZSTAT(1)
      AZC = AZ/CL
C
      NCOUNT = 0
C=======================================================================
C >>>>> Begin loop over the kappa-values
C
      DO KC = KCMIN,KCMAX
C
        K1 = KAG(KC)
        L1 = LAG(KC)
        J1 = JAG(KC)
        WRITE (IWRITE,3030) K1,L1,J1
C
        AKAP = K1
        GAM = SQRT(AKAP*AKAP-AZC*AZC)
C
        P0 = AZ**(GAM+HALF)
        P0 = P0*TENTH
        IF (K1.GT.0) THEN
          P0 = P0*AZC/CL
          Q0 = P0*(GAM+AKAP)/AZC
        ELSE
          Q0 = P0*AZC/(AKAP-GAM)
        ENDIF
C
        X = DR1**GAM
        FIRSTP = P0*X
        FIRSTQ = Q0*X
C
        BSTX = BSTO+AKAP
C
        LP = L1+1
C
        WRITE (IWRITE,3040)
C
        MAXHF = L1
        ISMIT = 0
        NBT = 0
C
C  bound orbitals are to be included
C
        IF (KC.LE.KBMAX) THEN
C
          MAXLG = MAXNLG(KC)
          MAXHF = MAXNQN(KC)
          ISMIT = MAXHF-MAXLG
          NBT = MAXLG-L1
C
          CALL DMCHK1(21,NBT, IWRITE, IDMTST, NDIMAX)
          CALL DMCHK1(22,ISMIT, IWRITE, IDMTST, NDIMAX)
C
C  set the array NORI
C
          NBS = 0
C
          DO N = LP,MAXLG
            ND = IPOS(N,KC)
            NS = NPTS*(ND-1)
            NBS = NBS+1
            NORI(NBS) = NS
            WRITE (IWRITE,3060) CP(NS+NPTS),CQ(NS+NPTS),N
          ENDDO
C
          IF (ISMIT.GT.0) THEN
            DO N = MAXLG,MAXHF
              ND = IPOS(N,KC)
              NS = NPTS*(ND-1)
              WRITE (IWRITE,3070) CP(NS+NPTS),CQ(NS+NPTS),N
            ENDDO
          ENDIF
C
        ENDIF
C
        CALL DMCHK1(23,MAXHF+NRANG2, IWRITE, IDMTST, NDIMAX)
C
C  find starting values for FINDER
C
        IF (KC.EQ.1) THEN
          IF (NELC.EQ.NZ) THEN
            ETRIAL = DPI/(RA*TWO)
            ETRIAL = ETRIAL*ETRIAL
          ELSE
            ETRIAL = DBLE(NELC-NZ)*DPI/(RA+RA)
          ENDIF
        ELSE
          ETRIAL = EIGENS(KC-1,1)
        ENDIF
C=======================================================================
C >>>>> Begin loop over the continuum orbitals
C  now evaluate the continuum orbitals for a given kappa
C
        DO N = 1,NRANG2
C
          ND = NUMORB+N
          N1 = MAXHF+N
          IPOS(N1,KC) = ND
          NODES = N1-LP-ISMIT
C
          IF (KC.GT.KCMIN) THEN
            IF (K1.LT.0) THEN
              ETRIAL = EIGENS(KC-1,N)
            ENDIF
          ENDIF
C
          PRINT 3130,KC,N
          CALL FINDER(NBT,AKAP,FIRSTP,FIRSTQ,BSTX,ETRIAL,NODES,CL, CP, C!
     +Q, DR1, HINT, ORBP, ORBQ, RA, RMESH,WEIGHT, ZSTAT, IRX, ITC, IWRIT!
     +E, NORI, NPTS)
C
C  For each continuum orbital, store the function in CP/CQ and the
C  eigenvalue in EIGENS. Write out the boundary amplitude, the
C  eigenvalue and the number of nodes.
C
          NS = NPTS*(ND-1)
          CALL DMCHK1(13,NS+NPTS, IWRITE, IDMTST, NDIMAX)
          DO I = 1,NPTS
            IJ = NS+I
            CP(IJ) = ORBP(I)
            CQ(IJ) = ORBQ(I)
          ENDDO
C
          EIGENS(KC,N) = ETRIAL
C
          BST1 = (TWO*CL*RA*ORBQ(NPTS)/ORBP(NPTS))-(AKAP+BSTO)
          EIGEN2 = ETRIAL*TWO
C
          WRITE (IWRITE,3050) ORBP(NPTS),ORBQ(NPTS),BST1,EIGEN2,NODES
C
C  Calculation of a new energy estimate for the next orbital.
C
          IF (ETRIAL.LT.ZERO) THEN
            ETRIAL = ETRIAL+(DPI*DPI)/(RA*RA)
          ELSE
            ETRIAL = (SQRT(ETRIAL)+DPI/RA)**2
          ENDIF
C
        ENDDO
C
C >>>>> end loop over the continuum orbitals
C=======================================================================
C
C  Schmidt orthogonalisation if ISMIT > 0.
C
C=======================================================================
        IF (ISMIT.GT.0) THEN
          CALL SCHMDT(IWRITE,KC,CP, CQ, IPOS, ITC, MAXNLG, MAXNQN, NPTS,!
     + NRANG2, RMESH, WEIGHT)
        ENDIF
C
        DO N = 1,NRANG2
C=======================================================================
C
C  Store the continuum orbitals in the DA file ORBITALS.DAT
C
C=======================================================================
          ND = NUMORB+N
          N1 = MAXHF+N
          NS = NPTS*(ND-1)
          DO I = 1,NPTS
            IJ = NS+I
            ORBP(I) = CP(IJ)
            ORBQ(I) = CQ(IJ)
          ENDDO
          CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,ORBP)
          CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,ORBQ)
C=======================================================================
C
C  If option 38 is set then write the continuum orbitals to file IDISC3.
C
C=======================================================================
          IF (ITC(38).EQ.1) THEN
            NCOUNT = NCOUNT+1
            IF (NCOUNT.EQ.1000) THEN
              WRITE (IWRITE,3180)
              ITC(38) = 0
            ELSE
              CALL IN2CH3 (NCOUNT,LABEL,IWRITE)
              ORBNAM = 'bas'//LABEL
              DO I = 1,NPTS
                ZB(I) = ORBP(I)*ORBP(I)+ORBQ(I)*ORBQ(I)
              ENDDO
              WRITE (IDISC3,3140) ORBNAM,N,K1,NPTS,EIGENS(KC,N)
              WRITE (IDISC3,3150) ORBNAM
              WRITE (IDISC3,3160)(RMESH(I),ORBP(I),ORBQ(I),ZB(I),I=1,NPT!
     +S)
              WRITE (IDISC3,3170)
            ENDIF
          ENDIF
C
        ENDDO
C=======================================================================
C
C  Store the values on the boundary in ENDS.
C
C=======================================================================
        N2 = NPTS*NUMORB
        DO N = 1,NRANG2
          N2 = N2+NPTS
          ENDS(KC,N) = CP(N2)
        ENDDO
C=======================================================================
C
C  Examine the mesh for the last continuum orbital.
C
C=======================================================================
        NUM1 = 0
        NUM2 = 0
        NUM3 = 0
        NUM4 = 0
C
        INTV1 = 0
        INTV2 = 0
        INTV3 = 0
        INTV4 = 0
C
        NUM = 0
        INTV = 0
C
        U = ZERO
C
        DO I = 1,NPTS
          V = ORBP(I)
          NUM = NUM+1
C
          IF (U*V.LT.ZERO) THEN
            INTV = INTV+1
            IF (INTV.EQ.1) THEN
              NUM1 = NUM
              NUM2 = NUM
              NUM3 = NUM
              INTV1 = INTV
              INTV2 = INTV
              INTV3 = INTV
            ELSE
              IF (NUM.LT.NUM2) THEN
                NUM2 = NUM
                INTV2 = INTV
              ENDIF
              IF (NUM.GT.NUM3) THEN
                NUM3 = NUM
                INTV3 = INTV
              ENDIF
            ENDIF
            NUM = 0
          ENDIF
C
          U = V
        ENDDO
C
        NUM4 = NUM
        INTV4 = INTV+1
C=======================================================================
C
C  Buttle correction
C
C=======================================================================
        IF (ITC(8).EQ.1) WRITE (IWRITE,3000)
C
        MBUT(KC) = 0
C
        DO N = 1,NRANG2-1
C
C  set the energy
C
          WINT = EIGENS(KC,N+1)-EIGENS(KC,N)
          WINT = WINT/3
          WBUT = EIGENS(KC,N)
C
          DO ITMP = 1,2
            WBUT = WBUT+WINT
C
            SUM = ZERO
            DO I = 1,NRANG2
              SUM = SUM+ENDS(KC,I)*ENDS(KC,I)/(EIGENS(KC,I)-WBUT)
            ENDDO
            SUM = HALF*SUM/RA
C
            CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,WBUT,BVALUE, CL, CP,!
     + CQ, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH!
     +, WEIGHT, ZSTAT)
C
            BUT1 = BVALUE-BSTX
            BUT2 = ONE/BUT1
            BUT3 = BUT2-SUM
C
            MBUT(KC) = MBUT(KC)+1
            EBUT(KC,MBUT(KC)) = WBUT
            CBUT(KC,MBUT(KC)) = BUT3
C
            IF (ITC(8).EQ.1) THEN
              WBUT2 = WBUT*TWO
              WRITE (IWRITE,3010) MBUT(KC),WBUT2,BUT3
            ENDIF
C
          ENDDO
C
        ENDDO
C=======================================================================
C
C  Write out the continuum orbital data to the DSTG1-ORBS dump.
C
C=======================================================================
        WRITE (IDISC4) (EIGENS(KC,N),N=1,NRANG2)
        WRITE (IDISC4) (ENDS(KC,N),N=1,NRANG2)
        WRITE (IDISC4) MBUT(KC)
        WRITE (IDISC4) (EBUT(KC,I),I=1,MBUT(KC))
        WRITE (IDISC4) (CBUT(KC,I),I=1,MBUT(KC))
C=======================================================================
C
C  Calculate and check the overlap integrals between all
C  the bound and continuum orbitals.
C
C  Print them if option 5 is set.
C
C=======================================================================
        IF (ITC(5).EQ.1) WRITE (IWRITE,3080)
C
        N2 = MAXHF+NRANG2
C
        DO N3 = LP,N2
          I = N3-LP+1
          DO N4 = N3,N2
            CALL INTRD(N3,KC,N4,KC,0,RESULT,CP,CQ,WEIGHT,RMESH,IWRITE,IT!
     +C,NPTS,IPOS)
            J = N4-LP+1
            IF (N3.EQ.N4) THEN
              RESULT = RESULT-ONE
            ENDIF
            IF (ABS(RESULT).GT.SMALL) WRITE (IWRITE,3120) RESULT,N3,N4
            TEMP(I,J) = RESULT
          ENDDO
        ENDDO
C
        IF (ITC(5).EQ.1) CALL MATOUT(IWRITE,TEMP,N2-LP+1,N2-LP+1,ND24,ND!
     +24,5)
C
        WRITE (IWRITE,3110) INTV1,NUM1,INTV2,NUM2,INTV3,NUM3,INTV4,NUM4
C
        CALL QUARTZ(0,IWRITE,TIME2)
        TIMTOT = TIMTOT+TIME2
C
      ENDDO
C
C >>>>> end loop over the kappa-values
C=======================================================================
      WRITE (IWRITE,3090)
      WRITE (IWRITE,3100) TIMTOT
C
 3000 FORMAT (/'           energy (Ryd)       Buttle corrections'/)
 3010 FORMAT (1X,I3,3X,1P,E16.9,6X,E16.9)
 3020 FORMAT (/31X,'Routine BASORB'/31X,'--------------'/               !
     +' This routine controls the calculation of the continuum orbital',!
     +'s.'/' These satisfy the R-matrix boundary condition :'/          !
     +'      BVALUE = BSTO + KAPPA'/                                    !
     +' where      BVALUE = 2 * CL * RA * Q(RA) / P(RA)'/               !
     +'   CL is the speed of light'/'   RA is the R-matrix boundary'/   !
     +'   Q(RA) is the small component amplitude of the orbital at RA'/ !
     +'   P(RA) is the large component amplitude of the orbital at RA'/ !
     +'   KAPPA is the angular kappa-value for the orbital'/            !
     +'   BSTO is an input constant')
 3030 FORMAT (/' Orbitals for KAPPA = ',I3,'    L = ',I3,'    J = ',I3, !
     +'/2'/' ------------------------')
 3040 FORMAT (/                                                         !
     +'   amplitude at RA        BVALUE       eigenvalue (Ryd)  nodes'/ !
     +'   large   small       -(BSTO+KAPPA)'/)
 3050 FORMAT (1P,2E10.2,4X,E10.2,5X,E15.7,2X,I5)
 3060 FORMAT (1P,2E10.2,4X,'n = ',I2,' bound orbital - Lagrange orthog.'!
     +)
 3070 FORMAT (1P,2E10.2,4X,'n = ',I2,' bound orbital - Schmidt  orthog.'!
     +)
 3080 FORMAT (/' Overlap integrals between the orbitals.'/              !
     +' One has been subtracted from the normalisation integral on the',!
     +' diagonal.'/)
 3090 FORMAT (/                                                         !
     +' **** eigenvalues, amplitudes and Buttle corrections written to',!
     +' IDISC4')
 3100 FORMAT (/' Total time spent in routine BASORB is ',F11.2,' sec')
 3110 FORMAT (/' Mesh information for the last continuum orbital.'/     !
     +' This gives the number of mesh points used between'/             !
     +' nodes of the large component.'//'     First interval (',I3,     !
     +') : ',I5/' min. mesh interval (',I3,') : ',I5/                   !
     +' max. mesh interval (',I3,') : ',I5/'      last interval (',I3,  !
     +') : ',I5)
 3120 FORMAT (' WARNING : large overlap (',1P,E9.2,                     !
     +') > .001 between orbitals ',2I4)
 3130 FORMAT (' ... about to solve for   K=',I3,'      N=',I3)
 3140 FORMAT ('cat << eof >> INDEX1'/A6,' : ',I3,1X,I3,' : r (a.u.) : ',!
     +I4,1X,1P,E12.5/'eof')
 3150 FORMAT ('cat << eof >> ',A6)
 3160 FORMAT (1X,1P,4E12.4)
 3170 FORMAT ('eof')
 3180 FORMAT (                                                          !
     +' WARNING : option 38 cannot be used for more than 999 continuum',!
     +' orbitals'/' The option has been disabled.')
      END
CEND--------------------------------------------------------------------
CEND    BZVALU.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE BZVALU(X,IX,ID,RES1,RES2,IFLAG,CP, CQ, NORI, NPTS, RMES!
     +H, ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/BZVALU.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   X      ... value of r required
C   IX     ... estimate of position of X in RMESH array
C   ID     ... position of required orbital in NORI array
C o RES1   ... large component (IFLAG=1) at required r or
C              static potential (IFLAG=2)
C o RES2   ... small component (IFLAG=1) at required r
C   IFLAG  ... flag, =1 for interpolation of wave-functions, =2 for
C              interpolation of static potential
C
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   NORI   ... array giving index of orbitals in CP/CQ arrays
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   RMESH  ... array containing regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Statement functions
C
      DOUBLE PRECISION FX
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION RES1,RES2,X
      INTEGER IX,ID,IFLAG
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER NORI(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION X0,X1,X2,X3
      DOUBLE PRECISION Y0,Y1,Y2,Y3
      INTEGER L
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      FX(X) = ((X-X1)/(X0-X1))*((X-X2)/(X0-X2))*((X-X3)/(X0-X3))*Y0+((X-!
     +X0)/(X1-X0))*((X-X2)/(X1-X2))*((X-X3)/(X1-X3))*Y1+((X-X0)/(X2-X0))!
     +*((X-X1)/(X2-X1))*((X-X3)/(X2-X3))*Y2+((X-X0)/(X3-X0))*((X-X1)/(X3!
     +-X1))*((X-X2)/(X3-X2))*Y3
C-----------------------------------------------------------------------
      L = IX
      IF (L.EQ.1 .AND. X.LT.RMESH(2)) GOTO 40
      IF (L.GE. (NPTS-1) .OR. X.GE.RMESH(NPTS-1)) GOTO 30
C
   10 CONTINUE
      IF (X.LT.RMESH(L+1)) GOTO 20
      L = L+1
      GOTO 10
C
   20 CONTINUE
      L = L-1
      IF (L.EQ.0) GOTO 40
      IF (X.LE.RMESH(L)) GOTO 20
C
   30 CONTINUE
      IF (L.GE.NPTS-2) L = NPTS - 2
      IF (L.GT.1) GOTO 50
C--------------------------
   40 CONTINUE
      IF (IFLAG.EQ.1) THEN
        X0 = ZERO
        X1 = RMESH(1)
        X2 = RMESH(2)
        X3 = RMESH(3)
        L = NORI(ID)
        Y0 = ZERO
        Y1 = CP(L+1)
        Y2 = CP(L+2)
        Y3 = CP(L+3)
        RES1 = FX(X)
        Y1 = CQ(L+1)
        Y2 = CQ(L+2)
        Y3 = CQ(L+3)
        RES2 = FX(X)
      ELSE
        X0 = RMESH(1)
        X1 = RMESH(2)
        X2 = RMESH(3)
        X3 = RMESH(4)
        Y0 = ZSTAT(1)
        Y1 = ZSTAT(2)
        Y2 = ZSTAT(3)
        Y3 = ZSTAT(4)
        RES1 = FX(X)
        RES2 = ZERO
      ENDIF
      RETURN
C--------------------------
   50 CONTINUE
      X0 = RMESH(L-1)
      X1 = RMESH(L)
      X2 = RMESH(L+1)
      X3 = RMESH(L+2)
      IF (IFLAG.EQ.1) THEN
        L = NORI(ID)+L
        Y0 = CP(L-1)
        Y1 = CP(L)
        Y2 = CP(L+1)
        Y3 = CP(L+2)
        RES1 = FX(X)
        Y0 = CQ(L-1)
        Y1 = CQ(L)
        Y2 = CQ(L+1)
        Y3 = CQ(L+2)
        RES2 = FX(X)
      ELSE
        Y0 = ZSTAT(L-1)
        Y1 = ZSTAT(L)
        Y2 = ZSTAT(L+1)
        Y3 = ZSTAT(L+2)
        RES1 = FX(X)
        RES2 = ZERO
      ENDIF
C--------------------------
      END
CEND--------------------------------------------------------------------
CEND    DFBSFN.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,WR,BVALUE, CL, CP, C!
     +Q, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH, !
     +WEIGHT, ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DFBSFN.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NBT    ... number of Lagrange multipliers i.e. (NBT+1) pairs of
C              radial equations are solved
C o NODE   ... number of nodes in the final solution
C   AKAP   ... kappa-value for this continuum orbital
C   FIRSTP ... expansion coefficient
C   FIRSTQ ... expansion coefficient
C   WR     ... eigenvalue of the continuum orbital
C o BVALUE ... logarithmic derivative, (2c RA Q(RA)) / P(RA)
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   NORI   ... array giving position of orbitals in CP/CQ arrays
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o ORBP   ... large component of continuum orbital
C o ORBQ   ... small component of continuum orbital
C   RA     ... R-matrix radius
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
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
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      INTEGER ND11
      PARAMETER (ND11=12)
      INTEGER ND21
      PARAMETER (ND21=ND11+1)
      INTEGER ND22
      PARAMETER (ND22=2*ND21)
C
C  Argument variables
C
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION WR
      DOUBLE PRECISION FIRSTQ
      DOUBLE PRECISION FIRSTP
      INTEGER NBT
      INTEGER NODE
C
      DOUBLE PRECISION BVALUE
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION ORBP(*)
      DOUBLE PRECISION ORBQ(*)
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER NORI(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION ADL(ND21)
      DOUBLE PRECISION ALAMDA(ND21)
      DOUBLE PRECISION AUP(MXNP)
      DOUBLE PRECISION AUQ(MXNP)
      DOUBLE PRECISION CONST1
      DOUBLE PRECISION CONST2
      DOUBLE PRECISION FACTN
      DOUBLE PRECISION PSUM
      DOUBLE PRECISION QSUM
      DOUBLE PRECISION RCL
      DOUBLE PRECISION RESULT
      DOUBLE PRECISION SDELT(ND21,ND21)
      DOUBLE PRECISION SUM
      DOUBLE PRECISION U
      DOUBLE PRECISION UI
      DOUBLE PRECISION UJ
      DOUBLE PRECISION UP(ND21,MXNP)
      DOUBLE PRECISION UQ(ND21,MXNP)
      DOUBLE PRECISION V
      DOUBLE PRECISION X(MXNP)
      DOUBLE PRECISION Y(ND22)
      INTEGER I,IP,IQ,INFO
      INTEGER IPIV(ND21),J,K
      INTEGER KK,KKK,L,NBTP1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NBTP1 = NBT+1
C
C  the boundary conditions at the first point or near origin
C
      DO I = 1,NBTP1
        IQ = I+I
        IP = IQ-1
        Y(IP) = FIRSTP
        Y(IQ) = FIRSTQ
      ENDDO
C
C  variables AKAP,RCL,CONST2,CONST1 are passed to routine FCT via DHPCG
C
      RCL = ONE/CL
      CONST2 = WR*RCL
      CONST1 = CL+CL+CONST2
C
      CALL DHPCG(DR1,HINT,IRX,RA,Y,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPT!
     +S,RCL,RMESH,ZSTAT,UP,UQ)
C-----------------------------------------------------------------------
C
C  evaluate the unnormalised Lagrange multipliers ADL(I)
C
C-----------------------------------------------------------------------
      IF (NBT.GT.0) THEN
C------
        DO I = 1,NBT
          KK = NORI(I)
C
          DO K = 1,NPTS
            KKK = KK+K
            AUP(K) = CP(KKK)
            AUQ(K) = CQ(KKK)
          ENDDO
C
          DO J = 1,NBTP1
C
            DO K = 1,NPTS
              ORBP(K) = UP(J,K)
              ORBQ(K) = UQ(J,K)
            ENDDO
C
            DO K = 1,NPTS
              X(K) = WEIGHT(K)*(AUP(K)*ORBP(K)+AUQ(K)*ORBQ(K))
            ENDDO
            RESULT = ZERO
            DO K = 1,NPTS
              RESULT = RESULT+X(K)
            ENDDO
C
            SDELT(I,J) = RESULT
C
          ENDDO
C
        ENDDO
C-------
        IF (NBT.EQ.1) THEN
          ADL(1) = -SDELT(1,2)/SDELT(1,1)
        ELSE
          DO I = 1,NBT
            ADL(I) = -SDELT(I,NBTP1)
          ENDDO
          CALL DGETRF(NBT,NBT,SDELT,ND21,IPIV,INFO)
          IF (INFO.NE.0) THEN
            WRITE (IWRITE,3070) INFO
            STOP
          ENDIF
          CALL DGETRS('N',NBT,1,SDELT,ND21,IPIV,ADL,ND21,INFO)
          IF (INFO.NE.0) THEN
            WRITE (IWRITE,3080) INFO
            STOP
          ENDIF
        ENDIF
C-------
      ENDIF
C-----------------------------------------------------------------------
      ADL(NBTP1) = ONE
      QSUM = ZERO
      PSUM = ZERO
      DO I = 1,NBTP1
        IQ = I+I
        IP = IQ-1
        PSUM = PSUM+ADL(I)*Y(IP)
        QSUM = QSUM+ADL(I)*Y(IQ)
      ENDDO
C
      BVALUE = (TWO*CL*RA*QSUM)/PSUM
C-----------------------------------------------------------------------
C
C   determine the solution and store in ORBP , ORBQ
C
C-----------------------------------------------------------------------
      IF (NBT.EQ.0) THEN
C
        DO K = 1,NPTS
          AUP(K) = UP(1,K)
          AUQ(K) = UQ(1,K)
        ENDDO
C
        DO K = 1,NPTS
          X(K) = WEIGHT(K)*(AUP(K)*AUP(K)+AUQ(K)*AUQ(K))
        ENDDO
        RESULT = ZERO
        DO K = 1,NPTS
          RESULT = RESULT+X(K)
        ENDDO
C
        FACTN = ONE/SQRT(RESULT)
        DO K = 1,NPTS
          ORBP(K) = AUP(K)*FACTN
          ORBQ(K) = AUQ(K)*FACTN
        ENDDO
C
      ELSE
C
        DO I = 1,NBTP1
C
          DO K = 1,NPTS
            AUP(K) = UP(I,K)
            AUQ(K) = UQ(I,K)
          ENDDO
C
          DO J = 1,I
C
            DO K = 1,NPTS
              ORBP(K) = UP(J,K)
              ORBQ(K) = UQ(J,K)
            ENDDO
C
            DO K = 1,NPTS
              X(K) = WEIGHT(K)*(AUP(K)*ORBP(K)+AUQ(K)*ORBQ(K))
            ENDDO
            RESULT = ZERO
            DO K = 1,NPTS
              RESULT = RESULT+X(K)
            ENDDO
C
            SDELT(I,J) = RESULT
            SDELT(J,I) = RESULT
C
          ENDDO
C
        ENDDO
C
        SUM = ZERO
        DO I = 1,NBTP1
          DO J = 1,NBTP1
            SUM = SUM+ADL(I)*ADL(J)*SDELT(I,J)
          ENDDO
        ENDDO
C
        IF (ITC(3).EQ.1) THEN
          DO I = 1,NBTP1
            WRITE (IWRITE,3030) I
            WRITE (IWRITE,3040) (UP(I,K),K=1,NPTS)
            WRITE (IWRITE,3050) (UQ(I,K),K=1,NPTS)
          ENDDO
        ENDIF
C
        IF (SUM.LE.ZERO .OR. ITC(2).EQ.1) THEN
          WRITE (IWRITE,3010)
          WRITE (IWRITE,3000) (ADL(I),I=1,NBTP1)
          WRITE (IWRITE,3020)
          DO I = 1,NBTP1
            WRITE (IWRITE,3000) (SDELT(I,J),J=1,NBTP1)
          ENDDO
          WRITE (IWRITE,3060) SUM
          IF (SUM.LE.ZERO) THEN
            WRITE (IWRITE,3090)
            STOP
          ENDIF
        ENDIF
C
        FACTN = ONE/SQRT(SUM)
        DO I = 1,NBTP1
          ALAMDA(I) = ADL(I)*FACTN
        ENDDO
C
        DO K = 1,NPTS
          UI = ZERO
          UJ = ZERO
          DO L = 1,NBTP1
            UI = UI+UP(L,K)*ALAMDA(L)
            UJ = UJ+UQ(L,K)*ALAMDA(L)
          ENDDO
          ORBP(K) = UI
          ORBQ(K) = UJ
        ENDDO
C
      ENDIF
C-----------------------------------------------------------------------
      IF (ITC(4).EQ.1) THEN
        WRITE (IWRITE,3040) (ORBP(I),I=1,NPTS)
        WRITE (IWRITE,3050) (ORBQ(I),I=1,NPTS)
      ENDIF
C-----------------------------------------------------------------------
C
C   count the number of nodes in solution
C
C-----------------------------------------------------------------------
      U = ZERO
      NODE = 0
      DO I = 1,NPTS
        V = ORBP(I)
        IF (U*V.LT.ZERO) NODE = NODE + 1
        U = V
      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (1X,1P,5E12.4)
 3010 FORMAT (' DFBSFN : ADL')
 3020 FORMAT (' DFBSFN : SDELT')
 3030 FORMAT (' DFBSFN : ORBITAL ',I4)
 3040 FORMAT (9X,'P',3X,1P,5E12.4/(1X,6E12.4))
 3050 FORMAT (9X,'Q',3X,1P,5E12.4/(1X,6E12.4))
 3060 FORMAT (' DFBSFN : SUM = ',1P,E12.5)
 3070 FORMAT (/' **** DFBSFN: DGETRF returned with INFO =',I2)
 3080 FORMAT (/' **** DFBSFN: DGETRS returned with INFO =',I2)
 3090 FORMAT (/' STOPPING in routine DFBSFN'/                           !
     +' There is a problem with the orthogonalisation of the orbitals'/ !
     +' SUM is non-positive'/                                           !
     +' where SUM = (sum over I and J of) ADL(I)*ADL(J)*SDELT(I,J)')
      END
CEND--------------------------------------------------------------------
CEND    DHPCG.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DHPCG(DR1,HINT,IRX,RA,Y,NBT,AKAP,CONST1,CONST2,CP,CQ,NO!
     +RI,NPTS,RCL,RMESH,ZSTAT,UP,UQ)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DHPCG.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   RA     ... R-matrix radius
C   Y      ... array which starts the solution
C   NBT    ... number of Lagrange multipliers. There are NBT+1 sets
C              of equations to be solved.
C   AKAP   ... kappa-value
C   CONST1 ... constant in equations, 2c+E/c
C   CONST2 ... constant in equations, E/c
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   NORI   ... array giving index of orbitals in CP/CQ arrays
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   RCL    ... 1/c, reciprocal of speed of light
C   RMESH  ... array containing regular radial mesh
C o UP     ... storage of large component solution, 2nd index radial
C o UQ     ... storage of small component solution, 2nd index radial
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS6
      PARAMETER (EPS6=1.D-6)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION FOUR
      PARAMETER (FOUR=4.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      DOUBLE PRECISION THREE
      PARAMETER (THREE=3.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION A1
      PARAMETER (A1=30.25D0/9.D0)
      DOUBLE PRECISION A2
      PARAMETER (A2=8.962962962962963D0)
      DOUBLE PRECISION B1
      PARAMETER (B1=0.125D0)
      DOUBLE PRECISION B2
      PARAMETER (B2=9.D0)
      DOUBLE PRECISION B3
      PARAMETER (B3=0.0743801652892562D0)
      DOUBLE PRECISION C1
      PARAMETER (C1=FOUR/THREE)
      DOUBLE PRECISION C2
      PARAMETER (C2=0.9256198347107438D0)
      DOUBLE PRECISION D1
      PARAMETER (D1=0.4D0)
      DOUBLE PRECISION D2
      PARAMETER (D2=0.29697760924775360D0)
      DOUBLE PRECISION D3
      PARAMETER (D3=0.15875964497103583D0)
      DOUBLE PRECISION D4
      PARAMETER (D4=0.45573725421878943D0)
      DOUBLE PRECISION D5
      PARAMETER (D5=0.21810038822592047D0)
      DOUBLE PRECISION D6
      PARAMETER (D6=3.05096514869293080D0)
      DOUBLE PRECISION D7
      PARAMETER (D7=3.83286476046701030D0)
      DOUBLE PRECISION D8
      PARAMETER (D8=0.17476028226269037D0)
      DOUBLE PRECISION D9
      PARAMETER (D9=0.55148066287873294D0)
      DOUBLE PRECISION D10
      PARAMETER (D10=1.20553559939652350D0)
      DOUBLE PRECISION D11
      PARAMETER (D11=0.17118478121951903D0)
      DOUBLE PRECISION E1
      PARAMETER (E1=ONE/24.0D0)
      DOUBLE PRECISION E2
      PARAMETER (E2=9.0D0)
      DOUBLE PRECISION E3
      PARAMETER (E3=19.0D0)
      DOUBLE PRECISION E4
      PARAMETER (E4=5.0D0)
      DOUBLE PRECISION E5
      PARAMETER (E5=.375D0)
      DOUBLE PRECISION E6
      PARAMETER (E6=ONE/THREE)
      INTEGER ND11
      PARAMETER (ND11=12)
      INTEGER ND21
      PARAMETER (ND21=ND11+1)
      INTEGER ND22
      PARAMETER (ND22=2*ND21)
C
C  Argument variables
C
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION Y(*)
      INTEGER IRX(*)
      INTEGER NBT
C
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION CONST1
      DOUBLE PRECISION CONST2
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RCL
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION UP(ND21,*)
      DOUBLE PRECISION UQ(ND21,*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER NORI(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION AUX(16,ND22),DELT
      DOUBLE PRECISION DERY(ND22),H,HS,X
      DOUBLE PRECISION Z
      INTEGER I,IX,IRAD,IFI
      INTEGER ISTEP,ISW,N,NBTP1
      INTEGER NDIM
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NBTP1 = NBT+1
      NDIM = 2*NBTP1
      IRAD = 1
      IX = IRAD
      N = 1
      ISTEP = 1
      IFI = 0
      X = DR1
      H = HINT
      DO I = 1,NDIM
        AUX(16,I) = ZERO
        AUX(15,I) = ONE
        AUX(1,I) = Y(I)
      ENDDO
C
C  Computation of DERY for starting values
C
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
C
C  Recording of starting values
C
      CALL OUTP(UP,UQ,IRAD,Y,NBTP1)
      DO I = 1,NDIM
        AUX(8,I) = DERY(I)
      ENDDO
      ISW = 1
      GOTO 60
C
C  Computation of AUX(2,I)
C
   10 CONTINUE
      X = X+H
      DO I = 1,NDIM
        AUX(2,I) = Y(I)
      ENDDO
C
C  Increment H is tested by means of bisection
C
      X = X-H
      DO I = 1,NDIM
        AUX(4,I) = AUX(2,I)
      ENDDO
      H = HALF*H
      N = 1
      ISW = 2
      GOTO 60
C
   20 CONTINUE
      X = X+H
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      DO I = 1,NDIM
        AUX(2,I) = Y(I)
        AUX(9,I) = DERY(I)
      ENDDO
      N = 2
      ISW = 3
      GOTO 60
C
   30 CONTINUE
      X = X+H
      IRAD = IRAD+1
      IX = IRAD
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      DO I = 1,NDIM
        AUX(3,I) = Y(I)
        AUX(10,I) = DERY(I)
      ENDDO
      N = 3
      ISW = 4
      GOTO 60
C
   40 CONTINUE
      N = 1
      X = X+H
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
C
C   Return to the original point
C
      X = DR1
      HS = H*E1
      DO I = 1,NDIM
        AUX(11,I) = DERY(I)
        Y(I) = AUX(1,I)+HS*(E2*AUX(8,I)+E3*AUX(9,I)-E4*AUX(10,I)+DERY(I)!
     +)
      ENDDO
C
   50 CONTINUE
      X = X+H
      N = N+1
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      IF (N.EQ.3 .AND. IRAD.EQ.2) CALL OUTP(UP,UQ,IRAD,Y,NBTP1)
      IF (N.GE.4) GOTO 70
      DO I = 1,NDIM
        AUX(N,I) = Y(I)
        AUX(N+7,I) = DERY(I)
      ENDDO
C
      IF (N.EQ.3) THEN
        HS = H*E5
        DO I = 1,NDIM
          Y(I) = AUX(1,I)+HS*(AUX(8,I)+THREE*(AUX(9,I)+AUX(10,I))+AUX(11!
     +,I))
        ENDDO
C
      ELSE
        HS = H*E6
        DO I = 1,NDIM
          Y(I) = AUX(1,I)+HS*(AUX(8,I)+FOUR*AUX(9,I)+AUX(10,I))
        ENDDO
      ENDIF
C
      GOTO 50
C
C  The following part of routine DHPCG computes by means of the
C  Runge-Kutta method starting values for the predictor-corrector
C
   60 CONTINUE
      DO I = 1,NDIM
        Z = H*AUX(N+7,I)
        AUX(5,I) = Z
        Y(I) = AUX(N,I)+D1*Z
      ENDDO
C
C  Z is an auxiliary storage location
C
      Z = X+D1*H
      CALL FCT(Z,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      DO I = 1,NDIM
        Z = H*DERY(I)
        AUX(6,I) = Z
        Y(I) = AUX(N,I)+D2*AUX(5,I)+D3*Z
      ENDDO
C
      Z = X+D4*H
      CALL FCT(Z,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      DO I = 1,NDIM
        Z = H*DERY(I)
        AUX(7,I) = Z
        Y(I) = AUX(N,I)+D5*AUX(5,I)-D6*AUX(6,I)+D7*Z
      ENDDO
C
      Z = X+H
      IX = IX+1
      CALL FCT(Z,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
      DO I = 1,NDIM
        Y(I) = AUX(N,I)+D8*AUX(5,I)-D9*AUX(6,I)+D10*AUX(7,I)+D11*H*DERY(!
     +I)
      ENDDO
      IX = IX-1
C
      IF (ISW.EQ.1) GOTO 10
      IF (ISW.EQ.2) GOTO 20
      IF (ISW.EQ.3) GOTO 30
      IF (ISW.EQ.4) GOTO 40
C
C  Possible break-point for linkage.
C
C  Starting values are computed.
C  Now start Hammings modified predictor-corrector method.
C
C  N=8 causes the rows of AUX to change their storage locations
C
   70 CONTINUE
      IF (N.EQ.8) THEN
        DO N = 2,7
          DO I = 1,NDIM
            AUX(N-1,I) = AUX(N,I)
            AUX(N+6,I) = AUX(N+7,I)
          ENDDO
        ENDDO
        N = 7
      ENDIF
C
C  N less than 8 causes N+1 to get N
C
      N = N+1
C
C  Computation of next vector Y
C
      DO I = 1,NDIM
        AUX(N-1,I) = Y(I)
        AUX(N+6,I) = DERY(I)
      ENDDO
      X = X+H
      IF (MOD(N,2).EQ.1 .OR. N.EQ.8) IRAD = IRAD + 1
      IX = IRAD
      HS = H*C1
      DO I = 1,NDIM
        DELT = AUX(N-4,I)+HS*(TWO*AUX(N+6,I)-AUX(N+5,I)+TWO*AUX(N+4,I))
        Y(I) = DELT-C2*AUX(16,I)
        AUX(16,I) = DELT
      ENDDO
C
C  Predictor is now generated in row 16 of AUX,
C  modified predictor is generated in Y,
C  DELT means an auxiliary storage
C
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
C
C  Derivative of modified predictor is generated in DERY.
C
      HS = H*THREE
      DO I = 1,NDIM
        DELT = B1*(B2*AUX(N-1,I)-AUX(N-3,I)+HS*(DERY(I)+TWO*AUX(N+6,I)-A!
     +UX(N+5,I)))
        AUX(16,I) = AUX(16,I)-DELT
        Y(I) = DELT+B3*AUX(16,I)
      ENDDO
C
C  H must not be halved. That means Y(I) are good.
C
      CALL FCT(X,IX,Y,DERY,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPTS,RCL,RM!
     +ESH,ZSTAT)
C
      IF (N.EQ.6) GOTO 70
C
      CALL OUTP(UP,UQ,IRAD,Y,NBTP1)
C
      IF (ABS(X-RA).LT.EPS6) RETURN
C
      IF (N.EQ.7 .OR. IRAD.EQ.IFI) GOTO 80
C
      IF (N.LE.8) GOTO 70
C
C  Increase step length.
C
   80 CONTINUE
      H = H+H
      HS = H*A1
      DO I = 1,NDIM
        AUX(N-1,I) = AUX(N-2,I)
        AUX(N-2,I) = AUX(N-4,I)
        AUX(N-3,I) = AUX(N-6,I)
        AUX(N+6,I) = AUX(N+5,I)
        AUX(N+5,I) = AUX(N+3,I)
        AUX(N+4,I) = AUX(N+1,I)
        AUX(16,I) = A2*(Y(I)-AUX(N-3,I))-HS*(DERY(I)+THREE*(AUX(N+6,I)+A!
     +UX(N+5,I))+AUX(N+4,I))
      ENDDO
      IFI = IRX(ISTEP)
      ISTEP = ISTEP+1
      GOTO 70
C
      END
CEND--------------------------------------------------------------------
CEND    DMCHK1.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    DMPRT1.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    DMSET1.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    DQSF.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    FCT.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FCT(X,IX,FUN,DFUN,NBT,AKAP,CONST1,CONST2,CP,CQ,NORI,NPT!
     +S,RCL,RMESH,ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FCT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   X      ... value of r required
C   IX     ... estimate of position of X in RMESH array
C   FUN    ... values of p and q at r
C o DFUN   ... values of p' and q' at r
C   NBT    ... number of Lagrange multipliers. There are NBT+1 sets
C              of equations to be solved.
C   AKAP   ... kappa-value
C   CONST1 ... constant in equations, 2c+E/c
C   CONST2 ... constant in equations, E/c
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   NORI   ... array giving index of orbitals in CP/CQ arrays
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   RCL    ... 1/c, reciprocal of speed of light
C   RMESH  ... array containing regular radial mesh
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS8
      PARAMETER (EPS8=1.D-8)
C
C  Argument variables
C
      DOUBLE PRECISION FUN(*),DFUN(*),X
      INTEGER IX,NBT
C
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RCL
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION CONST1
      DOUBLE PRECISION CONST2
      DOUBLE PRECISION ZSTAT(*)
      INTEGER NORI(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION PART1P,PART1Q,PART2P,PART2Q
      DOUBLE PRECISION ZVALUE,ORBP,ORBQ,CONST3
      INTEGER I,IP,IQ,IMARK
      INTEGER LL
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (ABS(X-RMESH(IX)).LE.EPS8) THEN
        ZVALUE = ZSTAT(IX)
        IMARK = 0
      ELSE
        CALL BZVALU(X,IX,0,ZVALUE,ORBQ,2,CP,CQ,NORI,NPTS,RMESH,ZSTAT)
        IMARK = 1
      ENDIF
C
      ZVALUE = -ZVALUE/X
      CONST3 = AKAP/X
C
      IF (NBT.GT.0) THEN
        DO I = 1,NBT
C
          IQ = I+I
          IP = IQ-1
          PART1P = -CONST3*FUN(IP)+CONST1*FUN(IQ)
          PART1Q = +CONST3*FUN(IQ)-CONST2*FUN(IP)
C
          IF (IMARK.EQ.0) THEN
            LL = NORI(I)+IX
            ORBP = CP(LL)
            ORBQ = CQ(LL)
          ELSE
            CALL BZVALU(X,IX,I,ORBP,ORBQ,1,CP,CQ,NORI,NPTS,RMESH,ZSTAT)
          ENDIF
C
          PART2P = ZVALUE*FUN(IQ)
          PART2Q = ZVALUE*FUN(IP)
          DFUN(IP) = PART1P-(PART2P+ORBQ)*RCL
          DFUN(IQ) = PART1Q+(PART2Q+ORBP)*RCL
C
        ENDDO
      ENDIF
C
      I = NBT+1
      IQ = I+I
      IP = IQ-1
      PART1P = -CONST3*FUN(IP)+CONST1*FUN(IQ)
      PART1Q = +CONST3*FUN(IQ)-CONST2*FUN(IP)
      ZVALUE = ZVALUE*RCL
      PART2P = ZVALUE*FUN(IQ)
      PART2Q = ZVALUE*FUN(IP)
      DFUN(IP) = PART1P-PART2P
      DFUN(IQ) = PART1Q+PART2Q
C
      END
CEND--------------------------------------------------------------------
CEND    FINDER.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINDER(NBT,AKAP,FIRSTP,FIRSTQ,BSTX,ETRIAL,NODES,CL, CP,!
     + CQ, DR1, HINT, ORBP, ORBQ, RA, RMESH,WEIGHT, ZSTAT, IRX, ITC, IWR!
     +ITE, NORI, NPTS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINDER.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  A new FINDER routine written at Muenster...August 1983
C  Modified by P.Norrington for the relativistic code.
C
C   NBT    ... the number of bound orbitals included in
C              orthogonalisation (number of Lagrange multipliers)
C              i.e. (NBT+1) pairs of radial equations are solved
C   AKAP   ... kappa value for this case
C   FIRSTP ... expansion coefficient for P
C   FIRSTQ ... expansion coefficient for Q
C   BSTX   ... required logarithmic derivative, i.e. b+kappa
C o ETRIAL ... initial estimate of the eigenenergy, this changes
C   NODES  ... the required number of nodes in the eigenfunction
C   CL     ... speed of light in au
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT) in the regular mesh
C   HINT   ... basic step-size in the regular mesh
C   IRX    ... defines the radial index at the end of the subintervals
C              in the regular mesh
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   NORI   ... array indicating position of bound orbitals in CP/CQ
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o ORBP   ... computed large component, P
C o ORBQ   ... computed small component, Q
C   RA     ... R-matrix radius
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C   ZSTAT  ... static potential, tabulated on the regular radial mesh
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
C
C   Option 1 gives a debug print from this routine.
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION TENTH
      PARAMETER (TENTH=.1D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION THIRD
      PARAMETER (THIRD=1.D0/3.D0)
      DOUBLE PRECISION EPS6
      PARAMETER (EPS6=1.D-6)
      DOUBLE PRECISION DPI
      PARAMETER (DPI=0.3141592653589793D1)
C
C  Argument variables
C
      DOUBLE PRECISION AKAP
      DOUBLE PRECISION BSTX
      DOUBLE PRECISION ETRIAL
      DOUBLE PRECISION FIRSTQ
      DOUBLE PRECISION FIRSTP
      INTEGER NBT,NODES
C
      DOUBLE PRECISION CL
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION ORBP(*)
      DOUBLE PRECISION ORBQ(*)
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER NORI(*)
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION BVALUE
      DOUBLE PRECISION ABSERR,B,BHIGH,BLOW
      DOUBLE PRECISION C,DEL,DEL1,DEL2
      DOUBLE PRECISION DELL,EHIGH,EHOLD,ELOW
      DOUBLE PRECISION ETEST,FT,RNN,RELERR
      DOUBLE PRECISION T
      INTEGER IFLAG,ISUM,NHOLD,NODE
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   Initialise variables.
C
      IF (ITC(1).EQ.1) WRITE (IWRITE,3000) NODES
      DEL = DPI/RA
      DEL2 = DEL*DEL
      DELL = DEL
      ISUM = 0
C-----------------------------------------------------------------------
   10 CONTINUE
      CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,ETRIAL,BVALUE, CL, CP, CQ,!
     + DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH, WE!
     +IGHT, ZSTAT)
      FT = BVALUE-BSTX
      IF (ITC(1).EQ.1) WRITE (IWRITE,3010) NODE,FT,ETRIAL
C-----------------------------------------------------------------------
C
C   Check that the function has the correct number of nodes.
C
C-----------------------------------------------------------------------
      IF (NODE.NE.NODES) THEN
C-----------------------------------------------------------------------
C
C   Function has not the correct number of nodes, modify energy
C   accordingly.
C
C-----------------------------------------------------------------------
        IF (ISUM.GT.0) THEN
C
          IF ((NODE-NODES).EQ.NHOLD) THEN
            DELL = DELL*HALF
            DEL2 = DEL2*HALF
            ETRIAL = EHOLD
          ELSE
            NHOLD = NODES-NODE
          ENDIF
C
        ELSE
          NHOLD = NODES-NODE
        ENDIF
C
        EHOLD = ETRIAL
        ISUM = ISUM+1
        RNN = DBLE(NODES-NODE)
        ETEST = RNN*DELL+SQRT(ABS(ETRIAL))
C
        IF ((ETRIAL.LT.ZERO) .OR. (ETEST.LT.ZERO)) THEN
          ETRIAL = ETRIAL+ABS(RNN)*RNN*DEL2
        ELSE
          ETRIAL = ETEST**2
        ENDIF
C
        IF (ISUM.EQ.100) THEN
          WRITE (IWRITE,3020)
          STOP
        ELSE
          GOTO 10
        ENDIF
C
      ELSE
C-----------------------------------------------------------------------
C
C   Function has the correct number of nodes.
C
C-----------------------------------------------------------------------
        IF (BVALUE.LT.BSTX) THEN
C-----------------------------------------------------------------------
C
C   We have an upper bound to the energy, now find lower bound.
C
C-----------------------------------------------------------------------
          EHIGH = ETRIAL
          BLOW = BVALUE
          DEL1 = DEL*THIRD
          DEL2 = DEL2*THIRD
          EHOLD = ETRIAL
C
   20     CONTINUE
C
          ETEST = SQRT(ABS(ETRIAL))-DEL1
C
          IF ((ETRIAL.LT.ZERO) .OR. (ETEST.LT.ZERO)) THEN
            ETRIAL = ETRIAL-DEL2
          ELSE
            ETRIAL = ETEST**2
          ENDIF
C
          CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,ETRIAL,BVALUE, CL, CP,!
     + CQ, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH!
     +, WEIGHT, ZSTAT)
          FT = BVALUE-BSTX
          IF (ITC(1).EQ.1) WRITE (IWRITE,3010) NODE,FT,ETRIAL
C
          IF (NODE.NE.NODES) THEN
C-----------------------------------------------------------------------
C
C   Energy decreased too far, decrease increment
C
C-----------------------------------------------------------------------
            DEL1 = DEL1*THIRD
            DEL2 = DEL2*THIRD
            ETRIAL = EHOLD
            GOTO 20
C
          ELSE
            IF (BVALUE.LT.BSTX) THEN
C-----------------------------------------------------------------------
C
C   Better upper bound to the energy found, try again for a lower bound.
C
C-----------------------------------------------------------------------
              EHIGH = ETRIAL
              BLOW = BVALUE
              EHOLD = ETRIAL
              GOTO 20
C
            ELSE
C-----------------------------------------------------------------------
C
C   Lower bound to the energy now found.
C
C-----------------------------------------------------------------------
              ELOW = ETRIAL
              BHIGH = BVALUE
            ENDIF
C
          ENDIF
C
        ELSE
C-----------------------------------------------------------------------
C
C   Lower bound to the energy found, now try for an upper bound.
C
C-----------------------------------------------------------------------
          ELOW = ETRIAL
          BHIGH = BVALUE
          DEL1 = DEL*THIRD
          DEL2 = DEL2*THIRD
          EHOLD = ETRIAL
C
   30     CONTINUE
C
          IF (ETRIAL.LT.ZERO) THEN
            ETRIAL = ETRIAL+DEL2
          ELSE
            ETRIAL = (SQRT(ABS(ETRIAL))+DEL1)**2
          ENDIF
C
          CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,ETRIAL,BVALUE, CL, CP,!
     + CQ, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH!
     +, WEIGHT, ZSTAT)
          FT = BVALUE-BSTX
          IF (ITC(1).EQ.1) WRITE (IWRITE,3010) NODE,FT,ETRIAL
C
          IF (NODE.NE.NODES) THEN
C-----------------------------------------------------------------------
C
C   Energy increased too far, decrease increment.
C
C-----------------------------------------------------------------------
            DEL1 = DEL1*THIRD
            DEL2 = DEL2*THIRD
            ETRIAL = EHOLD
            GOTO 30
C
          ELSE
            IF (BVALUE.GE.BSTX) THEN
C-----------------------------------------------------------------------
C
C   Better lower bound to the energy found, try again for the upper
C   bound.
C
C-----------------------------------------------------------------------
              ELOW = ETRIAL
              BHIGH = BVALUE
              EHOLD = ETRIAL
              GOTO 30
C
            ELSE
C-----------------------------------------------------------------------
C
C   Upper bound to the energy found.
C
C-----------------------------------------------------------------------
              EHIGH = ETRIAL
              BLOW = BVALUE
            ENDIF
C
          ENDIF
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
C
C   We now have upper and lower bounds to the eigenenergy, now use root
C   finding routine to get a better estimate.
C
C-----------------------------------------------------------------------
      IFLAG = 1
      ABSERR = ZERO
      RELERR = EPS6*TENTH
      B = ELOW
      C = EHIGH
C
      IF (ETRIAL.EQ.ELOW) THEN
        FT = BHIGH-BSTX
      ELSE
        FT = BLOW-BSTX
      ENDIF
C-----------------------------------------------------------------------
   40 CONTINUE
C
      IF (ITC(1).EQ.1) WRITE (IWRITE,3060) T,FT,B,C,IFLAG
      CALL ROOT(T,FT,B,C,RELERR,ABSERR,IFLAG)
      IF (ITC(1).EQ.1) WRITE (IWRITE,3060) T,FT,B,C,IFLAG
 3060 FORMAT(1p,' ROOT: t=',e12.4,' ft=',e12.4,' b=',e12.4,' c=',e12.4,
     X ' iflag=',i2)
C
      IF (IFLAG.LT.0) THEN
C
C   Still not close enough to the root
C
        IF (T.EQ.ELOW) THEN
          FT = BHIGH-BSTX
          GOTO 40
C
        ELSEIF (T.EQ.EHIGH) THEN
          FT = BLOW-BSTX
          GOTO 40
C
        ELSE
          ETRIAL = T
          CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,ETRIAL,BVALUE, CL, CP,!
     + CQ, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH!
     +, WEIGHT, ZSTAT)
          FT = BVALUE-BSTX
          IF (ITC(1).EQ.1) WRITE (IWRITE,3010) NODE,FT,ETRIAL
C
C   If abs(ft) is less than error ... solution found
C
          IF (ABS(FT).LT.EPS6) GOTO 50
          GOTO 40
C
        ENDIF
C-----------------------------------------------------------------------
      ELSEIF (IFLAG.GT.1) THEN
C
C   Error in root
C
        WRITE (IWRITE,3030) IFLAG
        WRITE (IWRITE,4001)
        WRITE (IWRITE,4002)
        IF (IFLAG.EQ.2) WRITE (IWRITE,4020)
        IF (IFLAG.EQ.3) WRITE (IWRITE,4030)
        IF (IFLAG.EQ.4) WRITE (IWRITE,4040)
        IF (IFLAG.EQ.5) WRITE (IWRITE,4050)
        STOP
C
      ELSE
C
C   IFLAG=1 ... root sucessfully located to required accuracy
C
        ETRIAL = B
        CALL DFBSFN(NBT,NODE,AKAP,FIRSTP,FIRSTQ,ETRIAL,BVALUE, CL, CP, C!
     +Q, DR1, HINT, IRX, ITC, IWRITE,NORI, NPTS, ORBP, ORBQ, RA, RMESH, !
     +WEIGHT, ZSTAT)
        FT = BVALUE-BSTX
        IF (ITC(1).EQ.1) WRITE (IWRITE,3010) NODE,FT,ETRIAL
C
      ENDIF
C-----------------------------------------------------------------------
   50 CONTINUE
C
      IF (NODE.EQ.NODES) THEN
C
C   All is well ... eigenvalue found with correct number of nodes
C
        IF (ITC(1).EQ.1) WRITE (IWRITE,3050)
C
      ELSE
C
C   Incorrect number of nodes ... give up
C
        WRITE (IWRITE,3040)
        STOP
C
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/' >>> FINDER : seeking solution with ',I4,' nodes')
 3010 FORMAT (' >>> NODE = ',I3,1P,'  BVALUE = ',E14.7,'  EIGEN = ',E14.!
     +7)
 3020 FORMAT (
     +/'       **************************'
     +/'       *** STOPPING in FINDER ***'
     +/'       **************************'
     +/' Cannot find the correct number of nodes')
 3030 FORMAT (
     +/'       **************************'
     +/'       *** STOPPING in FINDER ***'
     +/'       **************************'
     +/' ERROR in ROOT with IFLAG = ',I5)
 3040 FORMAT (
     +/'       **************************'
     +/'       *** STOPPING in FINDER ***'
     +/'       **************************'
     +/' DFBSFN entered but incorrect number of nodes found')
 3050 FORMAT (1X)
 4001 FORMAT (
     +/' ROOT computes a root of the nonlinear equation F(X)=0, ',
     + 'where F(X)'
     +/' is a continuous real function of a single real variable',
     + ' X. The'
     +/' method of solution is a combination of bisection and th',
     + 'e secant rule.'
     +/
     +/' Normal input consists of a continuous function F and an',
     + ' interval')
 4002 FORMAT (
     + ' (B,C) such that F(B)*F(C).LE.0.0. Each iteration finds ',
     + 'new values of'
     +/' B and C such that the intervall (B,C) is shrunk and F(B',
     + ')*F(C).LE.0.0.'
     +/
     +/' The stopping criterion is ABS(B-C).LE.2.0*(RELERR*ABS(B',
     + ')+ABSERR)'
     +/' where RELERR=relative error and ABSERR=absolute error a',
     + 're input'
     +/' quantities. Set the flag, IFLAG, positive to initialise',
     + ' the'
     +/' computation. As B, C and IFLAG are used for both input ',
     + 'and output,'
     +/' they must be variables in the calling program.')
 4020 FORMAT (
     +/' IFLAG=2  if a value B is found such that the computed value ',
     + 'F(B)'
     +/'          is exactly zero. The interval (B,C) may not satisfy'
     +/'          the stopping criterion.')
 4030 FORMAT (
     +/' IFLAG=3  if ABS(F(B)) exceeds the input values ABS(F(B)),'
     +/'          ABS(F(C)). In this case it is likely that B is close'
     +/'          to a pole of F.')
 4040 FORMAT (
     +/' IFLAG=4  if no odd order root was found in the interval.'
     +/'          A local mininmum may have been obtained.')
 4050 FORMAT (
     +/' IFLAG=5  if too many function evaluations were made.'
     +/'          (as programmed, 500 are allowed.)')
      END
CEND--------------------------------------------------------------------
CEND    INTRD.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    OUTP.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE OUTP(UP,UQ,IRAD,Y,NBTP1)
CRCS
CRCS $Source: /home/phn/DARC/RCS/OUTP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C o UP     ... storage of large component solution, 2nd index radial
C o UQ     ... storage of small component solution, 2nd index radial
C   IRAD   ... radial index
C   Y      ... array of computed solutions
C   NBTP1  ... =NBT+1, the number of sets of solutions
C
C o indicates output from routine
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER ND11
      PARAMETER (ND11=12)
      INTEGER ND21
      PARAMETER (ND21=ND11+1)
C
C  Argument variables
C
      DOUBLE PRECISION UP(ND21,*)
      DOUBLE PRECISION UQ(ND21,*)
      DOUBLE PRECISION Y(*)
      INTEGER IRAD
      INTEGER NBTP1
C
C  Local variables
C
      INTEGER ISOL,ISOLP,ISOLQ
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO ISOL = 1,NBTP1
        ISOLQ = ISOL+ISOL
        ISOLP = ISOLQ-1
        UP(ISOL,IRAD) = Y(ISOLP)
        UQ(ISOL,IRAD) = Y(ISOLQ)
      ENDDO
      END
CEND--------------------------------------------------------------------
CEND    RDISC1.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    RDORBS.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RDORBS(
     + IDMTST, IREAD, IWRITE, NDIMAX, RECORD, yBSTO, CL,
     + DR1, yHINT, IHED, yIRX, ITC,JAG, KAG, yKBMAX, yKCMAX,
     + yKCMIN, KX, LAB, LAG, yMAXFUL, yMAXNLG, yMAXNQN,
     + yMINNQN, yNELC, yNIX, NPTS, yNRANG2, yNZ, yRA)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RDORBS.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:12:04 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IDMTST ... array containing maximum dimensions set in the code
C   IREAD  ... stream number of file containing input data
C   IWRITE ... stream number for printed output
C   NDIMAX ... array containing maximum dimensions used in the code
C   RECORD ... 20-character record of time/date at execution
C o BSTO   ... constant that arises in the boundary condition for
C              the continuum orbitals
C o CL     ... speed of light in au
C o DR1    ... initial radial point (equals HINT)
C o HINT   ... basic step-size
C o IHED   ... 80-character title for the calculation
C o IRX    ... defines the radial index at the end of the subintervals
C o ITC    ... array containing option flags
C o JAG    ... array of orbital 2j quantum numbers
C o KAG    ... array of orbital kappa quantum numbers
C o KBMAX  ... maximum bound K-value
C o KCMAX  ... maximum continuum K-value
C o KCMIN  ... minimum continuum K-value
C o KX     ... maximum of yKBMAX and KCMAX
C o LAB    ... array of orbital labels for each K-value
C o LAG    ... array of orbital l quantum numbers
C o MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C o MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C o MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C o MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C o NELC   ... number of target electrons
C o NIX    ... number of subintervals
C o NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o NRANG2 ... number of continuum orbitals per K-value
C o NZ     ... atomic number
C o RA     ... R-matrix radius
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
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NIRX
      PARAMETER (NIRX=20)
C
C  Argument variables
C
      CHARACTER*2 LAB(*)
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION yBSTO
      DOUBLE PRECISION CL
      DOUBLE PRECISION DR1
      DOUBLE PRECISION yHINT
      DOUBLE PRECISION yRA
      INTEGER IDMTST(*)
      INTEGER IREAD
      INTEGER yIRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER yKBMAX
      INTEGER yKCMAX
      INTEGER yKCMIN
      INTEGER KX
      INTEGER LAG(*)
      INTEGER yMAXFUL(*)
      INTEGER yMAXNLG(*)
      INTEGER yMAXNQN(*)
      INTEGER yMINNQN(*)
      INTEGER NDIMAX(*)
      INTEGER yNELC
      INTEGER yNIX
      INTEGER NPTS
      INTEGER yNRANG2
      INTEGER yNZ
C
C  Local variables
C
      CHARACTER*2 ORBLAB(16)
      INTEGER I,K,K1,L
      INTEGER MK
C
C  Namelists
C
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION HINT
      INTEGER IRX(NIRX)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNLG(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MINNQN(MXNK)
      INTEGER NELC
      INTEGER NIX
      INTEGER NRANG2
      INTEGER NZ
      INTEGER OPT(50)
      DOUBLE PRECISION RA
C
      NAMELIST / ORBS /BSTO, HINT, IRX, KBMAX, KCMAX, KCMIN,
     + MAXFUL, MAXNLG, MAXNQN, MINNQN, NELC, NIX, NRANG2, NZ, OPT, RA
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA ORBLAB/'S ','P-','P ','D-','D ','F-','F ','G-','G ','H-',
     +'H ','I-','I ','J-','J ','**'/
C-----------------------------------------------------------------------
      WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
C
C   Input record 1
C
C   Read the title (up to 80 characters)
C
C-----------------------------------------------------------------------
      READ (IREAD,3120) IHED
C-----------------------------------------------------------------------
C
C   Input record 2
C
C   Namelist ORBS
C
C-----------------------------------------------------------------------
      DO I = 1,50
        OPT(I) = 0
      ENDDO
      BSTO = ZERO
      HINT = ZERO
      DO I = 1,NIRX
        IRX(I) = -1
      ENDDO
      KBMAX = -1
      KCMAX = 0
      KCMIN = 1
      DO I = 1,MXNK
        MINNQN(I) = -1
        MAXFUL(I) = -1
        MAXNLG(I) = -1
        MAXNQN(I) = -1
      ENDDO
      NELC = -1
      NIX = -1
      NRANG2 = 20
      NZ = -1
      RA = ZERO
C-----------------------------------------------------------------------
      READ (IREAD,ORBS)
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
C-----------------------------------------------------------------------
      IF (KCMIN.LT.1) KCMIN = 1
      IF (KCMAX.LT.0) KCMAX = 0
      IF (KCMAX.GT.0 .AND. KCMAX.LT.KCMIN) THEN
        WRITE (IWRITE,3210)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      IF (KBMAX.LT.1) THEN
        WRITE (IWRITE,3150)
        STOP
      ENDIF
C
      IF (NZ.LT.1) THEN
        WRITE (IWRITE,3160)
        STOP
      ENDIF
C
      IF (NELC.LT.1) THEN
        WRITE (IWRITE,3170)
        STOP
      ENDIF
C
      IF (KBMAX.GT.NDX) THEN
        WRITE (IWRITE,3220) KBMAX,NDX
        STOP
      ENDIF
C
      CALL DMCHK1(8,KBMAX, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK1(6,NRANG2, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
      DO I = 1,KBMAX
        IF (MAXNQN(I).LT.0) THEN
          WRITE (IWRITE,3180)
          STOP
        ENDIF
      ENDDO
C
      IF (MAXNLG(1).LT.0) THEN
        DO I = 1,KBMAX
          MAXNLG(I) = MAXNQN(I)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IF (HINT.GT.ZERO) THEN
C
        IF (NIX.LT.0) THEN
          WRITE (IWRITE,3190)
          STOP
        ENDIF
C
        CALL DMCHK1(24,NIX, IWRITE, IDMTST, NDIMAX)
C
        DO I = 1,NIX
          IF (IRX(I).LT.0) THEN
            WRITE (IWRITE,3200)
            STOP
          ENDIF
        ENDDO
C
        NPTS = IRX(NIX)
        CALL DMCHK1(11,NPTS, IWRITE, IDMTST, NDIMAX)
C
        DR1 = HINT
C
      ENDIF
C-----------------------------------------------------------------------
C
C   Set the variable KX for the current case.
C
C-----------------------------------------------------------------------
      KX = MAX(KCMAX,KBMAX)
C
      CALL DMCHK1(8,KX, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
C
C   Set the arrays JAG, LAG, KAG and LAB for the current case.
C
C-----------------------------------------------------------------------
      DO I = 1,KX
        K = I/2
        IF (MOD(I,2).EQ.1) K = - (I+1)/2
        MK = ABS(K)
        JAG(I) = MK+MK-1
        LAG(I) = MK+(SIGN(1,K)-1)/2
        KAG(I) = K
        IF (I.LT.16) THEN
          LAB(I) = ORBLAB(I)
        ELSE
          LAB(I) = ORBLAB(16)
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C
C   Set the arrays MINNQN, MAXNLG and MAXNQN for the current case.
C
C-----------------------------------------------------------------------
      IF (KCMAX.GT.KBMAX) THEN
        K1 = KBMAX+1
        DO K = K1,KCMAX
          L = LAG(K)
          MINNQN(K) = L
          MAXNLG(K) = L
          MAXNQN(K) = L
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      DO K = 1,KBMAX
        IF (MINNQN(K).LE.LAG(K)) MINNQN(K) = LAG(K) + 1
        IF (MINNQN(K).GT.MAXNQN(K)) MINNQN(K) = MAXNQN(K)
        IF (MAXFUL(K).LT.LAG(K)) MAXFUL(K) = LAG(K)
        IF (MAXFUL(K).GT.MAXNQN(K)) MAXFUL(K) = MAXNQN(K)
        ENDDO
C-----------------------------------------------------------------------
C
C   Set the speed of light
C
C-----------------------------------------------------------------------
      CL = 137.0359895D0
C-----------------------------------------------------------------------
C
C   Write out the basic data.
C
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000) IHED(1:40),IHED(41:80),RECORD
      WRITE (IWRITE,3020) KCMIN,KCMAX
C-----------------------------------------------------------------------
      WRITE (IWRITE,3030) KBMAX,NRANG2
      WRITE (IWRITE,3040) (MINNQN(I),I=1,KBMAX)
      WRITE (IWRITE,3050) (MAXFUL(I),I=1,KBMAX)
      WRITE (IWRITE,3060) (MAXNLG(I),I=1,KBMAX)
      WRITE (IWRITE,3070) (MAXNQN(I),I=1,KBMAX)
      WRITE (IWRITE,3090) (LAG(I),I=1,KBMAX)
      WRITE (IWRITE,3100) (KAG(I),I=1,KBMAX)
      WRITE (IWRITE,3080) (JAG(I),I=1,KBMAX)
      WRITE (IWRITE,3110) RA,BSTO,NZ,NELC,CL
C-----------------------------------------------------------------------
      yBSTO = BSTO
      yHINT = HINT
      DO I = 1,20
        yIRX(I) = IRX(I)
      ENDDO
      yKBMAX = KBMAX
      yKCMAX = KCMAX
      yKCMIN = KCMIN
      DO I = 1,MXNK
        yMINNQN(I) = MINNQN(I)
        yMAXFUL(I) = MAXFUL(I)
        yMAXNLG(I) = MAXNLG(I)
        yMAXNQN(I) = MAXNQN(I)
      ENDDO
      yNELC = NELC
      yNIX = NIX
      yNRANG2 = NRANG2
      yNZ = NZ
      yRA = RA
C=======================================================================
 3000 FORMAT (/1X,A40/1X,A40/1X,A20/)
 3010 FORMAT (/31X,'Routine RDORBS'/31X,'--------------'/)
 3020 FORMAT (
     +/' KCMIN  (smallest continuum angular momentum K-value) : ',I5
     +/' KCMAX  (largest  continuum angular momentum K-value) : ',I5)
 3030 FORMAT (
     +/' KBMAX  (largest bound angular momentum K-value)      : ',I5
     +/' NRANG2 (continuum orbitals per ang.mom.)             : ',I5)
 3040 FORMAT (' MINNQN : ',10I5)
 3050 FORMAT (' MAXFUL : ',10I5)
 3060 FORMAT (' MAXNLG : ',10I5)
 3070 FORMAT (' MAXNQN : ',10I5)
 3080 FORMAT ('    JAG : ',10I5)
 3090 FORMAT ('    LAG : ',10I5)
 3100 FORMAT ('    KAG : ',10I5)
 3110 FORMAT (1P,
     +/' RA     (R-matrix boundary)                           : ',E14.7
     +/' BSTO   (log. derivative parameter)                   : ',E14.7
     +/' NZ     (atomic number)                               : ',I5
     +/' NELC   (number of electrons)                         : ',I5
     +/' CL     (speed of light)                              : ',E14.7)
 3120 FORMAT (A80)
 3150 FORMAT (/
     +' **************************************'/
     +' ***  WARNING from routine RDORBS   ***'/
     +' *** You must set integer KBMAX > 0 ***'/
     +' ***     Code is STOPPING.          ***'/
     +' **************************************'/)
 3160 FORMAT (/
     +' ************************************'/
     +' *** WARNING from routine RDORBS  ***'/
     +' ***  You must set integer NZ > 0 ***'/
     +' ***     Code is STOPPING.        ***'/
     +' ************************************'/)
 3170 FORMAT (/
     +' *************************************'/
     +' ***  WARNING from routine RDORBS  ***'/
     +' *** You must set integer NELC > 0 ***'/
     +' ***     Code is STOPPING.         ***'/
     +' *************************************'/)
 3180 FORMAT (/
     +' ***************************************************'/
     +' ***       WARNING from routine RDORBS           ***'/
     +' *** You must set KBMAX elements of array MAXNQN ***'/
     +' ***          Code is STOPPING.                  ***'/
     +' ***************************************************'/)
 3190 FORMAT (/
     +' ***********************************'/
     +' *** WARNING from routine RDORBS ***'/
     +' *** You must set variable NIX   ***'/
     +' ***     Code is STOPPING.       ***'/
     +' ***********************************'/)
 3200 FORMAT (/
     +' **********************************************'/
     +' ***       WARNING from routine RDORBS      ***'/
     +' *** You must set NIX elements of array IRX ***'/
     +' ***          Code is STOPPING.             ***'/
     +' **********************************************'/)
 3210 FORMAT (/
     +' ******************************************'/
     +' ***      WARNING from routine RDORBS   ***'/
     +' *** You have KCMIN greater than KCMAX. ***'/
     +' ***     The code is STOPPING here.     ***'/
     +' ******************************************'/)
 3220 FORMAT (/
     +' ***********************************************'/
     +' ***        WARNING from routine RDORBS      ***'/
     +' ***    STOPPING due to a dimension ERROR    ***'/
     +' *** The parameter NDX needs to be increased ***'/
     +' ***    KBMAX = ',I3,'  NDX = ',I3,'               ***'/
     +' ***       The code is STOPPING here.        ***'/
     +' ***********************************************'/)
      END
CEND--------------------------------------------------------------------
CEND    ROOT.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ROOT(T,FT,B,C,RELERR,ABSERR,IFLAG)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ROOT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  This routine is taken from the book of: Shampine and Gordon,
C  'Computational Solution of Ordinary Differential Equations'
C
C  ROOT computes a root of the nonlinear equation F(X)=0, where F(X)
C  is a continuous real function of a single real variable X. The
C  method of solution is a combination of bisection and the secant rule.
C
C  Normal input consists of a continuous function F and an interval
C  (B,C) such that F(B)*F(C).LE.0.0. Each iteration finds new values of
C  B and C such that the intervall (B,C) is shrunk and F(B)*F(C).LE.0.0.
C
C  The stopping criterion is
C
C           ABS(B-C).LE.2.0*(RELERR*ABS(B)+ABSERR)
C
C  where RELERR=relative error and ABSERR=absolute error are input
C  quantities. Set the flag, IFLAG, positive to initialise the
C  computation. As B, C and IFLAG are used for both input and output,
C  they must be variables in the calling program.
C  If 0.0 is a possible root, one should not choose ABSERR=0.0.
C
C  The output value of B is the better approximation to a root as
C  B and C are always redefined so that ABS(F(B)).LE.ABS(F(C)).
C
C  To solve the equation, ROOT must evaluate F(X) repeatedly. This is
C  done in the calling program. When an evaluation of F is needed at T,
C  ROOT returns with IFLAG negative. Evaluate FT=F(T) and call ROOT
C  again. Do not alter IFLAG.
C
C  When the computation is complete, ROOT returns to the calling
C  program with IFLAG positive:
C
C     IFLAG=1  if F(B)*F(C).LT.0 and the stopping criterion is met.
C
C          =2  if a value B is found such that the computed value F(B)
C              is exactly zero. The interval (B,C) may not satisfy
C              the stopping criterion.
C
C          =3  if ABS(F(B)) exceeds the input values ABS(F(B)),
C              ABS(F(C)). In this case it is likely that B is close
C              to a pole of F.
C
C          =4  if no odd order root was found in the interval.
C              A local mininmum may have been obtained.
C
C          =5  if too many function evaluations were made.
C              (as programmed, 500 are allowed.)
C
C  This code is a modification of the code ZEROIN which is
C  completely explained and documented in the text, Numerical
C  Computing: An Introduction  by L.F. Shampine and R.C. Allen
C
C  Roundoff error U. It is calculated in the routine MACHIN
C  which must have been called before the first call of ROOT.
C  In this code MACHIN is not called but U is set to 1.0D-12.
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION EIGHT
      PARAMETER (EIGHT=8.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION U
      PARAMETER (U=1.D-12)
C
C  Argument variables
C
      DOUBLE PRECISION ABSERR,B,C,FT
      DOUBLE PRECISION RELERR,T
      INTEGER IFLAG
C
C  Local variables
C
      DOUBLE PRECISION A,ACBS,ACMB,AE
      DOUBLE PRECISION CMB,FA,FB,FC
      DOUBLE PRECISION FX,P,Q,RE
      DOUBLE PRECISION TOL
      INTEGER IC,KOUNT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE RE,AE,ACBS,A,FA,FB,FC,FX
      SAVE IC,KOUNT
C-----------------------------------------------------------------------
      IF (IFLAG.GE.0) THEN
        RE = MAX(RELERR,U)
        AE = MAX(ABSERR,ZERO)
        IC = 0
        ACBS = ABS(B-C)
        A = C
        T = A
        IFLAG = -1
        RETURN
      ENDIF
C-----------------------------------------------------------------------
      IF (IFLAG.EQ.-1) THEN
        FA = FT
        T = B
        IFLAG = -2
        RETURN
      ENDIF
C-----------------------------------------------------------------------
      IF (IFLAG.EQ.-2) THEN
        FB = FT
        FC = FA
        KOUNT = 2
        FX = MAX(ABS(FB),ABS(FC))
        GOTO 10
      ENDIF
C-----------------------------------------------------------------------
      IF (IFLAG.EQ.-3) THEN
        FB = FT
        IF (FB.EQ.ZERO) GOTO 90
        KOUNT = KOUNT+1
        IF (SIGN(ONE,FB).NE.SIGN(ONE,FC)) GOTO 10
        C = A
        FC = FA
        GOTO 10
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
   10 CONTINUE
      IF (ABS(FC).GE.ABS(FB)) GOTO 20
C-----------------------------------------------------------------------
C
C  Interchange B and C so that ABS(F(B)).LE.ABS(F(C))
C
C-----------------------------------------------------------------------
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
C
   20 CONTINUE
      CMB = HALF*(C-B)
      ACMB = ABS(CMB)
      TOL = RE*ABS(B)+AE
C-----------------------------------------------------------------------
C
C  Test stopping criterion and function count
C
C-----------------------------------------------------------------------
      IF (ACMB.LE.TOL) GOTO 80
      IF (KOUNT.GE.500) GOTO 120
C-----------------------------------------------------------------------
C
C  Calculate new iterate implicitly as B+P/Q where we arrange P.GE.0.
C  The implicit form is used to prevent overflow.
C
C-----------------------------------------------------------------------
      P = (B-A)*FB
      Q = FA-FB
      IF (P.GE.ZERO) GOTO 30
      P = -P
      Q = -Q
C-----------------------------------------------------------------------
C
C  Update A, check reduction of the size of bracketing interval is
C  satisfactory. If not, bisect until it is.
C
C-----------------------------------------------------------------------
   30 CONTINUE
      A = B
      FA = FB
      IC = IC+1
      IF (IC.LT.4) GOTO 40
      IF (EIGHT*ACMB.GE.ACBS) GOTO 60
      IC = 0
      ACBS = ACMB
C-----------------------------------------------------------------------
C
C  Test for too small a change
C
C-----------------------------------------------------------------------
   40 CONTINUE
      IF (P.GT.ABS(Q)*TOL) GOTO 50
C-----------------------------------------------------------------------
C
C  Increment by tolerance
C
C-----------------------------------------------------------------------
      B = B+SIGN(TOL,CMB)
      GOTO 70
C-----------------------------------------------------------------------
C
C  Root ought to be between B and (C+B)/2
C
C-----------------------------------------------------------------------
   50 CONTINUE
      IF (P.GE.CMB*Q) GOTO 60
C-----------------------------------------------------------------------
C
C  Use secant rule
C
C-----------------------------------------------------------------------
      B = B+P/Q
      GOTO 70
C-----------------------------------------------------------------------
C
C  Use bisection
C
C-----------------------------------------------------------------------
   60 CONTINUE
      B = HALF*(C+B)
C-----------------------------------------------------------------------
C
C  Have completed computation of new iterate B
C
C-----------------------------------------------------------------------
   70 CONTINUE
      T = B
      IFLAG = -3
      RETURN
C-----------------------------------------------------------------------
C
C  Finished. Set IFLAG
C
C-----------------------------------------------------------------------
   80 CONTINUE
      IF (SIGN(ONE,FB).EQ.SIGN(ONE,FC)) GOTO 110
      IF (ABS(FB).GT.FX) GOTO 100
C-----------------------------------------------------------------------
      IFLAG = 1
      RETURN
C-----------------------------------------------------------------------
   90 CONTINUE
      IFLAG = 2
      RETURN
C-----------------------------------------------------------------------
  100 CONTINUE
      IFLAG = 3
      RETURN
C-----------------------------------------------------------------------
  110 CONTINUE
      IFLAG = 4
      RETURN
C-----------------------------------------------------------------------
  120 CONTINUE
      IFLAG = 5
C-----------------------------------------------------------------------
      END
CEND--------------------------------------------------------------------
CEND    SCHMDT.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SCHMDT(IWRITE,KC,CP, CQ, IPOS, ITC, MAXNLG, MAXNQN, NPT!
     +S, NRANG2, RMESH, WEIGHT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SCHMDT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... stream number for printed output
C   KC     ... K-value
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   IPOS   ... array giving orbital index for CP and CQ arrays
C   ITC    ... array containing option flags
C   MAXNLG ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals for which
C              Lagrange multipliers have been included
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C   NRANG2 ... number of continuum orbitals per K-value
C   RMESH  ... array containing regular radial mesh
C   WEIGHT ... array containing radial integration weights
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
C
C   NSMIT/ND25 - dimension used for Schmidt orthogonalisation
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
      DOUBLE PRECISION EPS
      PARAMETER (EPS=1.D-10)
      INTEGER ND15
      PARAMETER (ND15=MXNB+MXNK/2)
      INTEGER NSMIT
      PARAMETER (NSMIT=10)
      INTEGER ND25
      PARAMETER (ND25=MXNB+NSMIT)
C
C  Argument variables
C
      INTEGER IWRITE,KC
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IPOS(ND15,*)
      INTEGER ITC(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER NPTS
      INTEGER NRANG2
C
C  Local variables
C
      DOUBLE PRECISION ANORM,B(ND25,ND25)
      DOUBLE PRECISION OVRLAP(MXNB,NSMIT),RESULT,SUMP
      DOUBLE PRECISION SUMQ,TEMP(ND25),TEMQ(ND25),XP
      DOUBLE PRECISION XQ
      INTEGER I,I1,I3,ISMIT
      INTEGER J,J1,J2,K
      INTEGER K1,MAXHF,MAXLG,N
      INTEGER N2,N3,N4
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      MAXHF = MAXNQN(KC)
      MAXLG = MAXNLG(KC)
      ISMIT = MAXHF-MAXLG
C
C  Calculate the overlaps
C
      DO I = 1,ISMIT
        N3 = MAXLG+I
        IF (ITC(5).EQ.1) WRITE (IWRITE,3010) N3
        DO N = 1,NRANG2
          N4 = MAXHF+N
          CALL INTRD(N3,KC,N4,KC,0,RESULT,CP,CQ,WEIGHT,RMESH,IWRITE,ITC,!
     +NPTS,IPOS)
          OVRLAP(N,I) = RESULT
        ENDDO
        IF (ITC(5).EQ.1) WRITE (IWRITE,3020) (OVRLAP(N,I),N=1,NRANG2)
      ENDDO
C
C  Set the B-array and the diagonal elements to unity
C
      N = NRANG2+ISMIT
      DO I = 1,N
        DO J = 1,N
          B(I,J) = ZERO
        ENDDO
      ENDDO
      DO I = 1,ISMIT
        B(I,I) = ONE
      ENDDO
C
C  Calculate the Schmidt coefficients
C
      DO I = 1,NRANG2
        N2 = ISMIT+I-1
        N3 = N2+1
C
        ANORM = ONE
C
        DO J = 1,N2
          TEMP(J) = ZERO
          DO K = 1,ISMIT
            TEMP(J) = TEMP(J)-B(J,K)*OVRLAP(I,K)
          ENDDO
          ANORM = ANORM-TEMP(J)*TEMP(J)
        ENDDO
C
C  Check the value of ANORM
C
        IF (ANORM.LT.EPS) THEN
          WRITE (IWRITE,3000) KC,ANORM
          STOP
        ENDIF
C
        ANORM = ONE/SQRT(ANORM)
C
        DO J = 1,N2
          B(N3,J) = ZERO
          DO K = 1,N2
            B(N3,J) = B(N3,J)+TEMP(K)*B(K,J)
          ENDDO
          B(N3,J) = B(N3,J)*ANORM
        ENDDO
        B(N3,N3) = ANORM
C
      ENDDO
C
C  Print the Schmidt coefficients
C
      IF (ITC(6).EQ.1) THEN
        WRITE (IWRITE,3040)
        CALL MATOUT(IWRITE,B,ISMIT+NRANG2,ISMIT+NRANG2,ND25,ND25,4)
      ENDIF
C
C  Schmidt orthogonalise the continuum orbitals
C
      I1 = IPOS(MAXHF+1,KC)-2
C
      DO I = 1,NPTS
        DO J = 1,NRANG2
          J1 = NPTS*(I1+J)+I
          TEMP(J) = CP(J1)
          TEMQ(J) = CQ(J1)
        ENDDO
        DO J = 1,NRANG2
          SUMP = ZERO
          SUMQ = ZERO
          N2 = ISMIT+J
          DO K = 1,N2
            IF (K.GT.ISMIT) THEN
              I3 = K-ISMIT
              XP = TEMP(I3)
              XQ = TEMQ(I3)
            ELSE
              K1 = K+MAXLG
              J2 = NPTS*(IPOS(K1,KC)-1)+I
              XP = CP(J2)
              XQ = CQ(J2)
            ENDIF
            SUMP = SUMP+XP*B(N2,K)
            SUMQ = SUMQ+XQ*B(N2,K)
          ENDDO
          J1 = NPTS*(I1+J)+I
          CP(J1) = SUMP
          CQ(J1) = SUMQ
        ENDDO
      ENDDO
C
      WRITE (IWRITE,3030)
C
 3000 FORMAT (/' STOPPING in routine SCHMDT.'/' For K-value ',I3,       !
     +' the value of ANORM is ',1P,E12.5/                               !
     +' This indicates an ERROR with the Schmidt procedure.'/           !
     +' Reduce the number of bound orbitals used in the procedure'/     !
     +' or the value of NRANG2 (the number of continuum orbitals).')
 3010 FORMAT (/' Overlap integrals between the N =',I3,' bound orbital'/!
     +' and each continuum orbital before Schmidt orthogonalising')
 3020 FORMAT (1X,1P,7E10.2)
 3030 FORMAT (/' Schmidt orthogonalisation has been done')
 3040 FORMAT (/' Schmidt coefficients'/)
      END
CEND--------------------------------------------------------------------
CEND    SETW.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SETW(DR1,HINT,IRX,ITC,IWRITE,NIX,NPTS,RMESH,WEIGHT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SETW.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o RMESH  ... array containing regular radial mesh
C o WEIGHT ... array containing radial integration weights
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION SRW1
      PARAMETER (SRW1=1.D0/3.D0)
      DOUBLE PRECISION SRW2
      PARAMETER (SRW2=4.D0/3.D0)
      DOUBLE PRECISION SRW3
      PARAMETER (SRW3=2.D0/3.D0)
      DOUBLE PRECISION BRW1
      PARAMETER (BRW1=14.D0/45.D0)
      DOUBLE PRECISION BRW2
      PARAMETER (BRW2=64.D0/45.D0)
      DOUBLE PRECISION BRW3
      PARAMETER (BRW3=24.D0/45.D0)
      DOUBLE PRECISION BRW4
      PARAMETER (BRW4=64.D0/45.D0)
      DOUBLE PRECISION BRW5
      PARAMETER (BRW5=28.D0/45.D0)
C
C   Argument variables
C
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER NIX
      INTEGER NPTS
C
C  Local variables
C
      DOUBLE PRECISION H,X,XH
      INTEGER I,I1,I2,IFI
      INTEGER IST,J,K,NOP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Set up the regular mesh.
C
      XH = HINT
      I1 = 1
      X = DR1-XH
C
      DO I = 1,NIX
        I2 = IRX(I)
        DO J = I1,I2
          X = X+XH
          RMESH(J) = X
        ENDDO
        I1 = I2+1
        XH = XH+XH
      ENDDO
C
C  Print info on the regular mesh.
C
      WRITE (IWRITE,3020) HINT,NIX
      WRITE (IWRITE,3030) (IRX(I),I=1,NIX)
      WRITE (IWRITE,3040)
C
      I1 = 1
      XH = HINT
C
      DO I = 1,NIX
        I2 = IRX(I)
        NOP = I2-I1
        IF (I.EQ.1) NOP = NOP + 1
        WRITE (IWRITE,3050) I,RMESH(I1),RMESH(I2),XH,NOP
        I1 = I2
        XH = XH+XH
      ENDDO
C
C  Check that there are more than 4 points in each subinterval.
C  This is required by routine DDET5 which evaluates the derivative
C  of a function tabulated on the regular mesh.
C
      DO I = 1,NIX
        IF (IRX(I).LT.5) THEN
          WRITE (IWRITE,3080)
          STOP
        ENDIF
      ENDDO
C
C  Check that you can use Simpson's rule with this mesh.
C
      DO I = 1,NIX
        IF (MOD(IRX(I),2).NE.0) THEN
          WRITE (IWRITE,3000)
          STOP
        ENDIF
      ENDDO
C
C  Check that you can use Bode's rule with this mesh.
C  Reset to Simpson's rule if you cannot use Bode's rule.
C
      IF (ITC(44).EQ.1) THEN
        DO I = 1,NIX
          IF (MOD(IRX(I),4).NE.0) THEN
            ITC(44) = 0
          ENDIF
        ENDDO
        IF (ITC(44).EQ.0) WRITE (IWRITE,3010)
      ENDIF
C
C  Set the appropriate integration weights.
C
      DO K = 1,NPTS
        WEIGHT(K) = ZERO
      ENDDO
C
      IF (ITC(44).EQ.0) THEN
        IST = 2
        H = HINT
        DO I = 1,NIX
          IFI = IRX(I)
          DO J = IST,IFI,2
            WEIGHT(J-1) = WEIGHT(J-1)+SRW2*H
            WEIGHT(J) = WEIGHT(J)+SRW3*H
          ENDDO
          WEIGHT(IFI) = WEIGHT(IFI)+(SRW1-SRW3)*H
          H = H+H
          WEIGHT(IFI) = WEIGHT(IFI)+SRW1*H
          IST = IFI+2
        ENDDO
        WEIGHT(IFI) = WEIGHT(IFI)-SRW1*H
        WRITE (IWRITE,3060)
      ELSE
        IST = 4
        H = HINT
        DO I = 1,NIX
          IFI = IRX(I)
          DO J = IST,IFI,4
            WEIGHT(J-3) = WEIGHT(J-3)+BRW2*H
            WEIGHT(J-2) = WEIGHT(J-2)+BRW3*H
            WEIGHT(J-1) = WEIGHT(J-1)+BRW4*H
            WEIGHT(J) = WEIGHT(J)+BRW5*H
          ENDDO
          WEIGHT(IFI) = WEIGHT(IFI)+(BRW1-BRW5)*H
          H = H+H
          WEIGHT(IFI) = WEIGHT(IFI)+BRW1*H
          IST = IFI+4
        ENDDO
        WEIGHT(IFI) = WEIGHT(IFI)-BRW1*H
        WRITE (IWRITE,3070)
      ENDIF
C
 3000 FORMAT (/' ***************************************************'/  !
     +' ***       WARNING from routine SETW             ***'/           !
     +' *** Simpsons rule cannot be used with this mesh ***'/           !
     +' ***         Endpoints should be even            ***'/           !
     +' ***          Code is STOPPING                   ***'/           !
     +' ***************************************************'/)
 3010 FORMAT (/' ************************************************'/     !
     +' ***       WARNING from routine SETW          ***'/              !
     +' *** Bodes rule cannot be used with this mesh ***'/              !
     +' ***    Endpoints should be multiples of 4    ***'/              !
     +' ***        Simpsons rule will be used        ***'/              !
     +' ************************************************'/)
 3020 FORMAT (/' The regular radial mesh is defined by :'//             !
     +' HINT (basic step-size)       = ',1P,E16.9/                      !
     +' NIX (number of subintervals) = ',I5)
 3030 FORMAT (                                                          !
     +' IRX (array giving index of last point in each subinterval)'/(1X,!
     +10I5))
 3040 FORMAT (/                                                         !
     +' interval   first point  last point   step-size        points'/)
 3050 FORMAT (4X,I2,5X,F11.7,1X,F11.7,1X,F11.7,I12)
 3060 FORMAT (/' Using integration weights for Simpsons rule')
 3070 FORMAT (/' Using integration weights for Bodes rule')
 3080 FORMAT (/' ************************************************'/     !
     +' ***       WARNING from routine SETW          ***'/              !
     +' *** You must have more than 4 points in each ***'/              !
     +' ***            subinterval                   ***'/              !
     +' ***          Code is STOPPING                ***'/              !
     +' ************************************************'/)
      END
CEND--------------------------------------------------------------------
CEND    STP0.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE STP0 (IDISC3,IREC1,CP, CQ, DR1, HINT, IDMTST, IREAD, IR!
     +X, ITC, IWRITE, JREAD,KAG, KBMAX, LAG, MAXNQN, NDIMAX, NELC, NIX, !
     +NPTS, NUMORB,NZ, RA, RMESH, WEIGHT, ZCORE, ZNUCL, ZSTAT)
CRCS
CRCS $Source: /home/phn/DARC/RCS/STP0.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDISC3 ... stream number of file containing formatted output
C   IREC1  ... pointer to next available DA record in ORBITALS.DAT
C   CP     ... array containing orbital large components
C   CQ     ... array containing orbital small components
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IDMTST ... array containing maximum dimensions set in the code
C   IREAD  ... stream number of file containing input data
C   IRX    ... defines the radial index at the end of the subintervals
C   ITC    ... array containing option flags
C   IWRITE ... stream number for printed output
C   JREAD  ... stream number of file containing bound orbitals and
C              potentials (TARGET.INP)
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   LAG    ... array of orbital l quantum numbers
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NELC   ... number of target electrons
C   NIX    ... number of subintervals
C   NPTS   ... number of points in regular radial mesh (equals IRX(NIX))
C o NUMORB ... number of bound orbitals
C   NZ     ... atomic number
C   RA     ... R-matrix radius
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
      DOUBLE PRECISION EPS5
      PARAMETER (EPS5=1.D-5)
      DOUBLE PRECISION PMAX
      PARAMETER (PMAX=1.D-3)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
C
C  Argument variables
C
      INTEGER IDISC3
      INTEGER IREC1
C
      DOUBLE PRECISION CP(*)
      DOUBLE PRECISION CQ(*)
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION RMESH(*)
      DOUBLE PRECISION WEIGHT(*)
      DOUBLE PRECISION ZCORE(*)
      DOUBLE PRECISION ZNUCL(*)
      DOUBLE PRECISION ZSTAT(*)
      INTEGER IDMTST(*)
      INTEGER IREAD
      INTEGER IRX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JREAD
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER LAG(*)
      INTEGER MAXNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NELC
      INTEGER NIX
      INTEGER NPTS
      INTEGER NUMORB
      INTEGER NZ
C
C  Local variables
C
      CHARACTER*3 LABEL
      CHARACTER*2 NH(MXNW),ORBLAB(16)
      CHARACTER*6 ORBNAM
      DOUBLE PRECISION ASY,DR(MXNP),EXCESS,HLAST
      DOUBLE PRECISION HX,RNELC,STEP1
      DOUBLE PRECISION STEP2,UCF(MXNW),UCFX,VA0
      DOUBLE PRECISION VA1,VA2,VA3,VAL
      DOUBLE PRECISION X
      DOUBLE PRECISION XX(MXNP)
      DOUBLE PRECISION YY(MXNP)
      DOUBLE PRECISION ZZ(MXNP)
      DOUBLE PRECISION YA(MXNP)
      DOUBLE PRECISION YB(MXNP)
      DOUBLE PRECISION ZA(MXNP)
      DOUBLE PRECISION ZB(MXNP)
      INTEGER I,ILAST,IMCDF(MXNW),IMIN
      INTEGER IPOINT,IRA,IX,IY
      INTEGER J,JMCDF(MXNW),K
      INTEGER KMCDF(MXNW),N,NAK(MXNW),NAKX
      INTEGER NCASE,NCOUNT,NLAST,NN
      INTEGER NP(MXNW),NPOINT,NPX,NW
      INTEGER NWX,NX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA ORBLAB/'S ','P-','P ','D-','D ','F-','F ','G-','G ','H-','H '!
     +,'I-','I ','J-','J ','**'/
C-----------------------------------------------------------------------
C=======================================================================
      WRITE (IWRITE,3030)
C=======================================================================
C
C  *********** INPUT DATA ***********
C
C  Input record 7 (stream JREAD)
C
C  Read in the target orbitals.
C
C  Read the following input records using FORTRAN free-format.
C  The input records D to G are repeated for J=1,NW.
C
C  A. NW,N
C  B. (UCF(J),J=1,NW)
C  C. (DR(I),I=1,N)
C
C  D. NP(J),NAK(J)
C  E. (CP(ILAST+I),I=1,N)
C  F. NP(J),NAK(J)
C  G. (CQ(ILAST+I),I=1,N)
C
      READ (JREAD,*,END=40) NW,N
      CALL DMCHK1(11,N, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK1(12,NW, IWRITE, IDMTST, NDIMAX)
      READ (JREAD,*,END=40) (UCF(J),J=1,NW)
      READ (JREAD,*,END=40) (DR(I),I=1,N)
C
      STEP1 = DR(2)-DR(1)
      STEP2 = DR(N)-DR(N-1)
      WRITE (IWRITE,3160) NW,N,DR(1),DR(N),STEP1,STEP2
C
      ILAST = 0
      DO J = 1,NW
        IMCDF(J) = ILAST
        CALL DMCHK1(13,ILAST+N, IWRITE, IDMTST, NDIMAX)
        READ (JREAD,*,END=40) NP(J),NAK(J)
        READ (JREAD,*,END=40) (CP(ILAST+I),I=1,N)
        READ (JREAD,*,END=40) NP(J),NAK(J)
        READ (JREAD,*,END=40) (CQ(ILAST+I),I=1,N)
        ILAST = ILAST+N
C
        IF (NAK(J).LT.0) THEN
          K = -2*NAK(J)-1
        ELSE
          K = 2*NAK(J)
        ENDIF
        IF (K.LT.16) THEN
          NH(J) = ORBLAB(K)
        ELSE
          NH(J) = ORBLAB(16)
        ENDIF
C
      ENDDO
C=======================================================================
C
C  set the nuclear charge to point charge
C
      DO I = 1,N
        ZZ(I) = NZ
      ENDDO
      WRITE (IWRITE,3200)
C=======================================================================
C
C  *********** INPUT DATA ***********
C
C  Input record 8 (stream IREAD)
C
C  When option 43 is set read in the array UCF.
C
C  Read the following input records using FORTRAN free-format.
C
C  A. (UCF(J),J=1,NW)
C
      IF (ITC(43).EQ.1) THEN
        READ (IREAD,*,END=30) (UCF(J),J=1,NW)
      ENDIF
C=======================================================================
C
C  Write information about the orbitals.
C
      WRITE (IWRITE,3170)
      DO J = 1,NW
        WRITE (IWRITE,3180) J,NP(J),NH(J),NP(J),NAK(J),UCF(J)
      ENDDO
C=======================================================================
C
C  Check that the orbitals read in agree with the input data requests.
C  You can have too many orbitals read in .. care needed with UCF array
C  Too few orbitals read in is a failure.
C
      NWX = 0
      DO K = 1,KBMAX
        NWX = NWX+MAXNQN(K)-LAG(K)
      ENDDO
      IF (NW.GT.NWX) WRITE (IWRITE,3060)
      NCOUNT = 0
      DO J = 1,NW
        KMCDF(J) = 0
      ENDDO
C
      DO K = 1,KBMAX
        NCASE = MAXNQN(K)-LAG(K)
        NPX = LAG(K)
        NAKX = KAG(K)
        DO NX = 1,NCASE
          NPX = NPX+1
          DO J = 1,NW
            IF (NPX.EQ.NP(J) .AND. NAKX.EQ.NAK(J)) THEN
              NCOUNT = NCOUNT+1
              JMCDF(NCOUNT) = J
              KMCDF(J) = 1
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      IF (NCOUNT.LT.NWX) THEN
        PRINT 3050
        WRITE (IWRITE,3050)
        STOP
      ENDIF
C=======================================================================
C
C  Check that the input UCF is ok
C
      UCFX = ZERO
      DO NCOUNT = 1,NWX
        J = JMCDF(NCOUNT)
        UCFX = UCFX+UCF(J)
      ENDDO
C
      RNELC = DBLE(NELC)
C
      IF (ABS(UCFX-RNELC).GT.EPS5) THEN
        PRINT 3190,UCFX,RNELC
        WRITE (IWRITE,3190) UCFX,RNELC
        STOP
      ENDIF
C=======================================================================
C
C  Automatically generate the regular mesh
C
      IF (HINT.LE.ZERO) THEN
C
        IF (RA.LE.ZERO) THEN
C
C  If RA <= 0  then determine the R-matrix boundary.
C
          NN = N
C
   10     CONTINUE
          DO I = 1,NW
            IF (KMCDF(I).EQ.1) THEN
              IPOINT = IMCDF(I)+NN
              IF (ABS(CP(IPOINT)).GT.PMAX) GOTO 20
            ENDIF
          ENDDO
          NN = NN-2
          GOTO 10
C
   20     CONTINUE
          RA = DR(NN)
C
          IRA = INT(100.D0*RA+HALF)
          RA = DBLE(IRA)
          RA = RA/100.D0
C
          WRITE (IWRITE,3120) PMAX,RA
C
        ENDIF
C
C  Determine the regular mesh.
C
        HINT = 1.953125D-5
        DR1 = HINT
C
        IRX(1) = 32
        HX = HINT
        X = HX*32.D0
C
        IMIN = 5
C
        CALL DMCHK1(24,IMIN+1, IWRITE, IDMTST, NDIMAX)
C
        DO I = 2,IMIN
          HX = HX+HX
          IRX(I) = IRX(I-1)+16
          X = X+HX*16.D0
        ENDDO
C
        IMIN = IMIN+1
C
        HLAST = RA/400.D0
C
        IF (HLAST.LT.HX*TWO) THEN
          NLAST = 0
        ELSE
          NLAST = NINT(LOG(HLAST/(HX*TWO))/LOG(TWO))
        ENDIF
C
        NIX = IMIN+NLAST
        NIX = MIN(NIX,IDMTST(24))
C
        DO I = IMIN,NIX
          HX = HX+HX
C
          IF (I.EQ.NIX) THEN
            NPOINT = NINT((RA-X)/HX)
            IF (MOD(NPOINT,4).NE.0) THEN
              NPOINT = NPOINT+4-MOD(NPOINT,4)
            ENDIF
            IF (NPOINT.LT.16) THEN
              NIX = NIX-1
              IRX(NIX) = IRX(NIX)+2*NPOINT
            ELSE
              IRX(I) = IRX(I-1)+NPOINT
            ENDIF
          ELSE
            IRX(I) = IRX(I-1)+16
            X = X+HX*16.D0
          ENDIF
C
        ENDDO
C
C  Option 31 set --- double the step-size --- coarser mesh
C
        IF (ITC(31).EQ.1) THEN
          HINT = HINT*TWO
          DR1 = HINT
          DO I = 1,NIX
            IRX(I) = IRX(I)/2
          ENDDO
        ENDIF
C
C  Option 32 set --- half the step-size --- finer mesh
C
        IF (ITC(32).EQ.1) THEN
          HINT = HINT*HALF
          DR1 = HINT
          DO I = 1,NIX
            IRX(I) = IRX(I)*2
          ENDDO
        ENDIF
C
        NPTS = IRX(NIX)
C
        IF (NPTS.GT.IDMTST(11)) THEN
          WRITE (IWRITE,3130) RA
          WRITE (IWRITE,3140) HINT,NIX
          WRITE (IWRITE,3150) (IRX(I),I=1,NIX)
C
C  Attempt to fit the generated radial mesh into the available space.
C
          WRITE (IWRITE,3000)
          IF (MOD(IDMTST(11),2).NE.0) THEN
            IY = IDMTST(11)-1
          ELSE
            IY = IDMTST(11)
          ENDIF
C
          IX = 2*IY-IRX(NIX)
          IF (IX.GT.0) THEN
            NIX = NIX+1
            CALL DMCHK1(24,NIX, IWRITE, IDMTST, NDIMAX)
            IRX(NIX-1) = IX
            IRX(NIX) = IY
            NPTS = IRX(NIX)
          ENDIF
C
        ENDIF
C
        CALL DMCHK1(11,NPTS, IWRITE, IDMTST, NDIMAX)
C
      ENDIF
C=======================================================================
C
C  Set up the regular mesh and weights for integration
C
      CALL SETW(DR1,HINT,IRX,ITC,IWRITE,NIX,NPTS,RMESH,WEIGHT)
C
      RA = RMESH(NPTS)
C
      WRITE (IWRITE,3130) RA
C=======================================================================
C
C  Write the radial mesh and weights to ORBITALS.DAT
C
      CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,RMESH)
      CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,WEIGHT)
C=======================================================================
C
C  Interpolate the nuclear potential to the regular mesh
C
      CALL XFITPL(ZZ,ZNUCL,2,DR,RMESH,N,NPTS,IWRITE)
C
      IF (ITC(40).EQ.0) THEN
        DO I = 1,NPTS
          ZA(I) = ZERO
        ENDDO
      ENDIF
C=======================================================================
C
C  Interpolate the bound orbitals
C  Write them to ORBITALS.DAT
C
      WRITE (IWRITE,3010)
      WRITE (IWRITE,3020)
      NCOUNT = 0
      NUMORB = NWX
C-----------------------------------------------------------------------
      DO K = 1,KBMAX
        NCASE = MAXNQN(K)-LAG(K)
        NPX = LAG(K)
        NAKX = KAG(K)
        DO NX = 1,NCASE
          NPX = NPX+1
          NCOUNT = NCOUNT+1
          I = JMCDF(NCOUNT)
          IPOINT = IMCDF(I)
C
          DO J = 1,N
            ZB(J) = CQ(IPOINT+J)
          ENDDO
          CALL XFITPL(ZB,YB,1,DR,RMESH,N,NPTS,IWRITE)
C
          DO J = 1,N
            ZB(J) = CP(IPOINT+J)
          ENDDO
          CALL XFITPL(ZB,YA,1,DR,RMESH,N,NPTS,IWRITE)
C
          DO J = 1,NPTS
            XX(J) = WEIGHT(J)*(YA(J)*YA(J)+YB(J)*YB(J))
          ENDDO
          VA0 = ZERO
          DO J = 1,NPTS
            VA0 = VA0+XX(J)
          ENDDO
C
          DO J = 1,NPTS
            YY(J) = XX(J)/RMESH(J)
          ENDDO
          VA1 = ZERO
          DO J = 1,NPTS
            VA1 = VA1+YY(J)
          ENDDO
C
          DO J = 1,NPTS
            YY(J) = XX(J)*RMESH(J)
          ENDDO
          VA2 = ZERO
          DO J = 1,NPTS
            VA2 = VA2+YY(J)
          ENDDO
C
          DO J = 1,NPTS
            YY(J) = YY(J)*RMESH(J)
          ENDDO
          VA3 = ZERO
          DO J = 1,NPTS
            VA3 = VA3+YY(J)
          ENDDO
C
          VAL = ONE/SQRT(VA0)
          DO J = 1,NPTS
            YA(J) = YA(J)*VAL
            YB(J) = YB(J)*VAL
          ENDDO
          VA0 = VA0-ONE
C
          WRITE (IWRITE,3040) NCOUNT,NPX,NH(I),VA0,YA(NPTS),YB(NPTS),VA1!
     +,VA2,VA3
C
          CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,YA)
          CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,YB)
C
C  If option 36 is set, write the bound orbitals to stream IDISC3
C
          IF (ITC(36).EQ.1) THEN
            CALL IN2CH3 (NCOUNT,LABEL,IWRITE)
            ORBNAM = 'orb'//LABEL
            DO J = 1,NPTS
              ZB(J) = YA(J)*YA(J)+YB(J)*YB(J)
            ENDDO
            WRITE (IDISC3,3230) ORBNAM,NP(I),NH(I),NAK(I),NPTS
            WRITE (IDISC3,3240) ORBNAM
            WRITE (IDISC3,3250) (RMESH(J),YA(J),YB(J),ZB(J),J=1,NPTS)
            WRITE (IDISC3,3260)
          ENDIF
C
          IF (ITC(40).EQ.0) THEN
            DO J = 1,NPTS
              ZA(J) = ZA(J)+UCF(I)*(YA(J)*YA(J)+YB(J)*YB(J))
            ENDDO
          ENDIF
C
        ENDDO
      ENDDO
C=======================================================================
C
C  *********** INPUT DATA ***********
C
C  Input record 9 (stream JREAD)
C
C  When option 40 is set then read in the static potential.
C
C  Read the following input records using FORTRAN free-format.
C
C  A. N
C  B. (DR(I),I=1,N)
C  C. (ZZ(I),I=1,N)
C
      IF (ITC(40).EQ.1) THEN
C
        READ (JREAD,*,END=40) N
        CALL DMCHK1(11,N, IWRITE, IDMTST, NDIMAX)
        READ (JREAD,*,END=40) (DR(I),I=1,N)
        READ (JREAD,*,END=40) (ZZ(I),I=1,N)
C
C  Interpolate to the regular mesh.
C
        CALL XFITPL(ZZ,ZSTAT,2,DR,RMESH,N,NPTS,IWRITE)
C
      ELSE
C
C  Calculate the static potential to be used if option 40 is not set.
C
C  Note when R is small ZSTAT=Z (the atomic number)
C       when R is large ZSTAT=z (the residual charge)
C
        ASY = NELC
        CALL DQSF(0,ZA,YA,ASY,RMESH,DR1,HINT,IRX,NIX,NPTS)
C
        DO I = 1,NPTS
          ZSTAT(I) = ZNUCL(I)-YA(I)*RMESH(I)
        ENDDO
C
      ENDIF
C=======================================================================
C
C  *********** INPUT DATA ***********
C
C  Input record 10 (stream JREAD)
C
C  When option 42 is set then read in the core potential.
C
C  Read the following input records using FORTRAN free-format.
C
C  A. N
C  B. (DR(I),I=1,N)
C  C. (ZZ(I),I=1,N)
C
      IF (ITC(42).EQ.1) THEN
C
        READ (JREAD,*,END=40) N
        CALL DMCHK1(11,N, IWRITE, IDMTST, NDIMAX)
        READ (JREAD,*,END=40) (DR(I),I=1,N)
        READ (JREAD,*,END=40) (ZZ(I),I=1,N)
C
C  Interpolate to the regular mesh.
C
        CALL XFITPL(ZZ,ZCORE,2,DR,RMESH,N,NPTS,IWRITE)
C
      ELSE
C
C  Set the core potential to be zero if option 42 is not set.
C
        DO I = 1,NPTS
          ZCORE(I) = ZERO
        ENDDO
C
      ENDIF
C=======================================================================
C
C  Write the potentials to ORBITALS.DAT
C
      CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,ZSTAT)
      CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,ZNUCL)
      CALL DA1('ORBITALS.DAT',2,IREC1,11,NPTS,ZCORE)
C=======================================================================
C
C  If option 37 is set, write the regular mesh, the static potential,
C  the nuclear potential and the core potential to stream IDISC3
C
      IF (ITC(37).EQ.1) THEN
        ORBNAM = 'stapot'
        WRITE (IDISC3,3270) ORBNAM,NPTS
        WRITE (IDISC3,3240) ORBNAM
        WRITE (IDISC3,3300) (RMESH(I),ZSTAT(I),I=1,NPTS)
        WRITE (IDISC3,3260)
        ORBNAM = 'nucpot'
        WRITE (IDISC3,3280) ORBNAM,NPTS
        WRITE (IDISC3,3240) ORBNAM
        WRITE (IDISC3,3300) (RMESH(I),ZNUCL(I),I=1,NPTS)
        WRITE (IDISC3,3260)
        ORBNAM = 'corpot'
        WRITE (IDISC3,3290) ORBNAM,NPTS
        WRITE (IDISC3,3240) ORBNAM
        WRITE (IDISC3,3300) (RMESH(I),ZCORE(I),I=1,NPTS)
        WRITE (IDISC3,3260)
      ENDIF
C=======================================================================
      EXCESS = NZ-NELC-ZSTAT(NPTS)
C
      WRITE (IWRITE,3110) RMESH(NPTS)
      WRITE (IWRITE,3090) ZSTAT(NPTS)
      WRITE (IWRITE,3100) ZNUCL(NPTS)
      WRITE (IWRITE,3070) EXCESS
C
      IF (ABS(EXCESS).GT.1.D-2) THEN
        PRINT 3080
        WRITE (IWRITE,3080)
        STOP
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
   30 CONTINUE
      WRITE (IWRITE,3210)
      STOP
   40 CONTINUE
      WRITE (IWRITE,3220)
      STOP
C-----------------------------------------------------------------------
 3000 FORMAT (/                                                         !
     +' Attempt to fit the generated radial mesh into the available sp',!
     +'ace.')
 3010 FORMAT (/' Bound orbitals'/1X,14('-'))
 3020 FORMAT (/8X,                                                      !
     +'  NORM-1   P(RA)    Q(RA)     <1/R>        <R>          <R*R>  ',!
     +'   '/)
 3030 FORMAT (/31X,'Routine STP0'/31X,12('-'))
 3040 FORMAT (1X,I2,1X,I2,A2,1P,1X,E8.1,1X,E8.1,1X,E8.1,1X,E12.5,1X,E12.!
     +5,1X,E12.5)
 3050 FORMAT (/' *****************************************************'/!
     +' ***          WARNING from routine STP0            ***'/         !
     +' *** Not all of the orbitals are on the grasp dump ***'/         !
     +' ***          The code is STOPPING here            ***'/         !
     +' *****************************************************')
 3060 FORMAT (/' GRASP dump contains extra orbitals'/                   !
     +' Be careful with the UCF array used for the static potential !'/)
 3070 FORMAT (' Excess (residual charge-static potential) = ',1P,E14.7)
 3080 FORMAT (/' **************************************'/               !
     +' ***   WARNING from routine STP0    ***'/                        !
     +' *** The excess charge is too large ***'/                        !
     +' ***   The code is STOPPING here    ***'/                        !
     +' **************************************')
 3090 FORMAT (/' Last point in  static potential  V=-Z(R)  = ',1P,E14.7)
 3100 FORMAT (' Last point in nuclear potential           = ',1P,E14.7)
 3110 FORMAT (/' The following information relates to the last point'/  !
     +' in the regular mesh which is ',1P,E14.7)
 3120 FORMAT (/                                                         !
     +' The R-matrix boundary has been determined automatically.'/      !
     +' It is chosen so that all large components have a magnitude'/    !
     +' less than ',1P,E12.5/' The boundary is at ',E14.7)
 3130 FORMAT (/' R-matrix boundary at ',F10.7/)
 3140 FORMAT (/' Regular mesh is defined by :'//' HINT = ',1P,E16.9/    !
     +' NIX  = ',I5)
 3150 FORMAT (' IRX = ',10I5)
 3160 FORMAT (1P/'  Target data'//'  number of orbitals (NW) = ',I4/    !
     +'  number of points   (N)  = ',I4/'  first mesh point        = ',E!
     +12.5/'  last  mesh point        = ',E12.5/                        !
     +'  initial stepsize        = ',E12.5/                             !
     +'  final   stepsize        = ',E12.5/)
 3170 FORMAT (/11X,'N',3X,'K   occupation number'/)
 3180 FORMAT (1X,I2,1X,I2,A2,2I4,3X,1P,E13.5)
 3190 FORMAT (/' ****************************************************'/ !
     +' ***          WARNING from routine STP0           ***'/          !
     +' *** array UCF is inconsistent with earlier input ***'/          !
     +' ***  read  has : ',F7.3,'                         ***'/         !
     +' ***  input has : ',F7.3,'                         ***'/         !
     +' ***          The code is STOPPING here.          ***'/          !
     +' ****************************************************')
 3200 FORMAT (/' Nuclear potential is set to point charge')
 3210 FORMAT (/' *********************************************'/        !
     +' ***      WARNING from routine STP0        ***'/                 !
     +' *** The read statement expects more input ***'/                 !
     +' ***        from IREAD (ORBS.INP)          ***'/                 !
     +' ***     Please check your input data.     ***'/                 !
     +' ***      The code is STOPPING here.       ***'/                 !
     +' *********************************************')
 3220 FORMAT (/' *********************************************'/        !
     +' ***      WARNING from routine STP0        ***'/                 !
     +' *** The read statement expects more input ***'/                 !
     +' ***        from JREAD (TARGET.INP)        ***'/                 !
     +' ***     Please check your input data.     ***'/                 !
     +' ***      The code is STOPPING here.       ***'/                 !
     +' *********************************************')
 3230 FORMAT ('cat << eof >> INDEX1'/A6,' : ',I2,A2,I3,' : r (a.u.) : ',!
     +I4/'eof')
 3240 FORMAT ('cat << eof >> ',A6)
 3250 FORMAT (1X,1P,4E12.4)
 3260 FORMAT ('eof')
 3270 FORMAT ('cat << eof >> INDEX2'/A6,                                !
     +' : static potential : r (a.u.) : ',I4/'eof')
 3280 FORMAT ('cat << eof >> INDEX2'/A6,                                !
     +' : nuclear potential : r (a.u.) : ',I4/'eof')
 3290 FORMAT ('cat << eof >> INDEX2'/A6,                                !
     +' : core potential : r (a.u.) : ',I4/'eof')
 3300 FORMAT (1X,1P,2E12.4)
      END
CEND--------------------------------------------------------------------
CEND    WDISC4.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE WDISC4 (
     + IDISC4, BSTO, CL, DR1, HINT, IHED, IRX, IWRITE, JAG, KAG,
     + KBMAX, KCMAX, KCMIN, KX, LAG, MAXFUL, MAXNLG, MAXNQN, MINNQN,
     + NELC, NIX, NRANG2, NUMORB, NZ, RA, RECORD
     +)
CRCS
CRCS $Source: /home/phn/DARC/RCS/WDISC4.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:12:04 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IDISC4 ... stream number of ORBS.DAT (DSTG1-ORBS dump file)
C   BSTO   ... constant that arises in the boundary condition for
C              the continuum orbitals
C   CL     ... speed of light in au
C   DR1    ... initial radial point (equals HINT)
C   HINT   ... basic step-size
C   IHED   ... 80-character title for the calculation
C   IRX    ... defines the radial index at the end of the subintervals
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   KAG    ... array of orbital kappa quantum numbers
C   KBMAX  ... maximum bound K-value
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   KX     ... maximum of KBMAX and KCMAX
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
C   NELC   ... number of target electrons
C   NIX    ... number of subintervals
C   NRANG2 ... number of continuum orbitals per K-value
C   NUMORB ... number of bound orbitals
C   NZ     ... atomic number
C   RA     ... R-matrix radius
C   RECORD ... 20-character record of time/date at execution
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IDISC4
C
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION BSTO
      DOUBLE PRECISION CL
      DOUBLE PRECISION DR1
      DOUBLE PRECISION HINT
      DOUBLE PRECISION RA
      INTEGER IRX(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER KAG(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KX
      INTEGER LAG(*)
      INTEGER MAXFUL(*)
      INTEGER MAXNLG(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NELC
      INTEGER NIX
      INTEGER NRANG2
      INTEGER NUMORB
      INTEGER NZ
C
C  Local variables
C
      INTEGER L
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      LAMBB = -1
      LAMBC = -1
      LAMCC = -1
C
      REWIND IDISC4
C
      WRITE (IDISC4) IHED,RECORD
      WRITE (IDISC4) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      WRITE (IDISC4) LAMBB,LAMBC,LAMCC
      WRITE (IDISC4) (MINNQN(L),L=1,KX), (MAXNQN(L),L=1,KX),
     +               (MAXNLG(L),L=1,KX), (MAXFUL(L),L=1,KBMAX),
     +               (LAG(L),L=1,KX), (KAG(L),L=1,KX), (JAG(L),L=1,KX)
      WRITE (IDISC4) RA,BSTO,HINT,CL,DR1
      WRITE (IDISC4) NIX
      WRITE (IDISC4) (IRX(L),L=1,NIX)
      WRITE (IDISC4) NUMORB
C
      WRITE (IWRITE,3000)
C=======================================================================
 3000 FORMAT (/
     +' **** basic data written to stream IDISC4 by routine WDISC4')
      END
CEND--------------------------------------------------------------------
CEND    XFITPL.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE XFITPL(FA,FB,ICON,RA,RB,MAXA,MAXB,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/XFITPL.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 11:57:32 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   FA     ... initial function
C o FB     ... final function
C   ICON   ...  =1 indicates that the function is a bound orbital which
C               can be completed with zeros for R > RA(MAXA)
C   RA     ... initial radial mesh
C   RB     ... final radial mesh
C   MAXA   ... number of points in RA and FA
C   MAXB   ... number of points in RB and FB
C   IWRITE ... stream number for printed output
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Statement functions
C
      DOUBLE PRECISION FX
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION FA(*),FB(*),RA(*),RB(*)
      INTEGER ICON,MAXA,MAXB
      INTEGER IWRITE
C
C  Local variables
C
      DOUBLE PRECISION X,X0,X1,X2
      DOUBLE PRECISION X3,Y0,Y1,Y2
      DOUBLE PRECISION Y3
      INTEGER I,IA,IB,ITMP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      FX(X) = ((X-X1)/(X0-X1))*((X-X2)/(X0-X2))*((X-X3)/(X0-X3))*Y0+((X-!
     +X0)/(X1-X0))*((X-X2)/(X1-X2))*((X-X3)/(X1-X3))*Y1+((X-X0)/(X2-X0))!
     +*((X-X1)/(X2-X1))*((X-X3)/(X2-X3))*Y2+((X-X0)/(X3-X0))*((X-X1)/(X3!
     +-X1))*((X-X2)/(X3-X2))*Y3
C-----------------------------------------------------------------------
      IF (MAXA.LT.4) THEN
        WRITE (IWRITE,3000)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      IB = 1
      IA = 0
      X = RB(1)
C-----------------------------------------------------------------------
   10 CONTINUE
      IF (X.LT.RA(IA+1)) GOTO 20
      IA = IA+1
      GOTO 10
C
   20 CONTINUE
      IF (IA.LT.2) THEN
        ITMP = 2
        GOTO 30
      ENDIF
C
      IF (IA.GT.MAXA-2) THEN
        ITMP = MAXA-2
        GOTO 30
      ENDIF
C
      ITMP = IA
C-----------------------------------------------------------------------
C
C  Interpolation
C
C-----------------------------------------------------------------------
   30 CONTINUE
      X0 = RA(ITMP-1)
      X1 = RA(ITMP)
      X2 = RA(ITMP+1)
      X3 = RA(ITMP+2)
      Y0 = FA(ITMP-1)
      Y1 = FA(ITMP)
      Y2 = FA(ITMP+1)
      Y3 = FA(ITMP+2)
      FB(IB) = FX(X)
C-----------------------------------------------------------------------
      IF (IB.EQ.MAXB) RETURN
C-----------------------------------------------------------------------
      IB = IB+1
      X = RB(IB)
C-----------------------------------------------------------------------
      IF (IA.GT.MAXA) THEN
C-----------------------------------------------------------------------
C
C   Special section for the interpolating at the end of the arrays
C   Complete the wave-function interpolation with zeros.
C
C-----------------------------------------------------------------------
        IF (ICON.EQ.1) THEN
          DO I = IB,MAXB
            FB(I) = ZERO
          ENDDO
          RETURN
        ELSE
          ITMP = MAXA-2
          GOTO 30
        ENDIF
C
      ELSE
        GOTO 10
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/' STOPPING in routine XFITPL'/                           !
     +' There are not enough points to do cubic interpolation'/         !
     +' You need at least 4 points')
      END
CEND--------------------------------------------------------------------
CEND    DA1.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    IN2CH3.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE IN2CH3 (i,name,iwrite)
CRCS
CRCS $Source: /home/phn/DARC/RCS/IN2CH3.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:57:16 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   I      ... integer
C o NAME   ... 3-character string
C   IWRITE ... stream number for printed output
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      CHARACTER CHAR
      INTEGER ICHAR
C
C  Argument variables
C
      CHARACTER*3 NAME
      INTEGER I,IWRITE
C
C  Local variables
C
      CHARACTER NAME1
      CHARACTER*2 NAME2
      INTEGER I1,I2,I3,IA
      INTEGER IB,N1,N2,N3
      INTEGER NUM(0:9)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (i.lt.0 .or. i.gt.999) THEN
        WRITE (iwrite,3000) i
        stop
      ENDIF
C
      num(0) = ichar('0')
      num(1) = ichar('1')
      num(2) = ichar('2')
      num(3) = ichar('3')
      num(4) = ichar('4')
      num(5) = ichar('5')
      num(6) = ichar('6')
      num(7) = ichar('7')
      num(8) = ichar('8')
      num(9) = ichar('9')
C
      ia = i/10
      i3 = i-ia*10
      ib = ia/10
      i2 = ia-ib*10
      i1 = ib
C
      n1 = num(i1)
      n2 = num(i2)
      n3 = num(i3)
C
      name1 = char(n1)
      name2 = name1//char(n2)
      name = name2//char(n3)
C
 3000 FORMAT (/' STOPPING in routine IN2CH3.'/' The number ',I6,        !
     +' is out of range.')
      END
CEND--------------------------------------------------------------------
CEND    MATOUT.f    Wed Jan 21 10:17:32 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MATOUT(IWRITE,HAMIL,NR,NC,NRX,NCX,MODE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MATOUT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:57:16 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... stream number for printed output
C   HAMIL  ... array containing matrix
C   NR     ... number of rows
C   NC     ... number of columns
C   NRX    ... dimension of first index
C   NCX    ... dimension of second index
C   MODE   ... parameter controlling output
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IWRITE,MODE,NC,NCX
      INTEGER NR,NRX
      DOUBLE PRECISION HAMIL(NRX,NCX)
C
C  Local variables
C
      INTEGER J1,J2,J3,JM
      INTEGER JP,L,L1,L2
      INTEGER M
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (MODE.EQ.1 .OR. MODE.EQ.5) GOTO 10
      IF (MODE.GT.1 .AND. MODE.LT.5) GOTO 80
      RETURN
C-----------------------------------------------------------------------
C
C   Symmetric matrix.
C
C-----------------------------------------------------------------------
   10 CONTINUE
      IF (MODE.EQ.1) WRITE (IWRITE,3070)
      JP = 5
      J1 = 1
      J2 = JP
C
   20 CONTINUE
      IF (J2.GT.NR) J2 = NR
      DO L1 = 1,J1
        WRITE (IWRITE,3010) (HAMIL(L1,L2),L2=J1,J2)
      ENDDO
C
      JM = J2-J1
      J3 = J1+1
C
      IF (JM.EQ.0) RETURN
      IF (JM.EQ.1) GOTO 30
      IF (JM.EQ.2) GOTO 40
      IF (JM.EQ.3) GOTO 50
      IF (JM.EQ.4) GOTO 60
C
   30 CONTINUE
      WRITE (IWRITE,3030) ((HAMIL(L1,L2),L2=L1,J2),L1=J3,J2)
      GOTO 70
C
   40 CONTINUE
      WRITE (IWRITE,3040) ((HAMIL(L1,L2),L2=L1,J2),L1=J3,J2)
      GOTO 70
C
   50 CONTINUE
      WRITE (IWRITE,3050) ((HAMIL(L1,L2),L2=L1,J2),L1=J3,J2)
      GOTO 70
C
   60 CONTINUE
      WRITE (IWRITE,3060) ((HAMIL(L1,L2),L2=L1,J2),L1=J3,J2)
C
   70 CONTINUE
      IF (J2.EQ.NR) RETURN
      J1 = J1+JP
      J2 = J2+JP
      GOTO 20
C-----------------------------------------------------------------------
C
C   Unsymmetric matrix.
C
C-----------------------------------------------------------------------
   80 CONTINUE
      IF (MODE.EQ.2) WRITE (IWRITE,3070)
      IF (MODE.EQ.3) WRITE (IWRITE,3080)
      JP = 5
      J1 = 1
      J2 = JP
   90 CONTINUE
      IF (J2.GT.NC) J2 = NC
      IF (MODE.EQ.3) THEN
        WRITE (IWRITE,3000) (L,L=J1,J2)
        WRITE (IWRITE,3020)
      ENDIF
C
      DO M = 1,NR
        WRITE (IWRITE,3010) (HAMIL(M,L),L=J1,J2)
      ENDDO
      WRITE (IWRITE,3020)
      IF (J2.EQ.NC) RETURN
      J1 = J1+JP
      J2 = J2+JP
      GOTO 90
C
 3000 FORMAT (1X,5(I8,7X))
 3010 FORMAT (1X,1P,5E15.7)
 3020 FORMAT (1X)
 3030 FORMAT (16X,1P,E15.7/)
 3040 FORMAT (16X,1P,2E15.7/31X,E15.7/)
 3050 FORMAT (16X,1P,3E15.7/31X,2E15.7/46X,E15.7/)
 3060 FORMAT (16X,1P,4E15.7/31X,3E15.7/46X,2E15.7/61X,E15.7/)
 3070 FORMAT (/' matrix elements'/)
 3080 FORMAT (/' eigenvectors'/)
      END
CEND--------------------------------------------------------------------
CEND    CALEN.f    Wed Jan 21 10:17:32 GMT 2004
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
CEND    QUARTZ.f    Wed Jan 21 10:17:32 GMT 2004
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
