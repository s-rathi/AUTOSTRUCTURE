C    DSTG2.f                 v11.3                       phn 12 May 2001

C                                                        nrb 27 Aug 2009
c    Restrict NR CSF used to form N+1 to first  -ncfgp
c    Fix NJGRAF bug in SR.CUTNL
c    Fix GENSUM INTEGER*4 bug
c
CEND--------------------------------------------------------------------
CEND    DSTG2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      PROGRAM DSTG2
CRCS
CRCS $Source: /home/phn/DARC/RCS/dstg2.mak,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:36:27 $
CRCS $Revision: 11.3 $
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
      CHARACTER*60 AUT
      CHARACTER*12 CODE
      CHARACTER*5 FILE
      CHARACTER*60 DAT
      CHARACTER EMPTY
      CHARACTER*9 FILE1,FILE2
      CHARACTER INPREC(80)
      CHARACTER*60 REV,SOU
      INTEGER I,INPUNI,LENREC
      INTEGER IDMTST(30)
      INTEGER NDIMAX(30)
      LOGICAL EX
C
      INTEGER IREAD,IWRITE
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA EMPTY/' '/
      DATA CODE/'DSTG2-NJGRAF'/
      DATA FILE/'DSTG2'/
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
C-----------------------------------------------------------------------
      WRITE (IWRITE,4000)
 4000 FORMAT(/' DSTG2-NJGRAF (dstg2-njgraf) module')
C-----------------------------------------------------------------------
C
C  Set version information
C
      SOU = '$Source: /home/phn/DARC/RCS/dstg2.mak,v $'
      AUT = '$Author: phn $'
      DAT = '$Date: 2001/12/05 19:36:27 $'
      REV = '$Revision: 11.3 $'
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
        CALL DMSET2(IWRITE, IDMTST, NDIMAX)
        WRITE (IWRITE,4010)
        WRITE (IWRITE,4020)
        WRITE (IWRITE,4030)
        WRITE (IWRITE,4040)
        WRITE (IWRITE,4050)
        WRITE (IWRITE,4060)
        WRITE (IWRITE,4070)
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
        WRITE (IWRITE,4230)
        WRITE (IWRITE,4240)
        WRITE (IWRITE,4250)
        WRITE (IWRITE,4260)
        WRITE (IWRITE,4270)
        WRITE (IWRITE,4280)
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
      CALL AAMN2 (IREAD,IWRITE)
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/1X,A60/1X,A60/1X,A60/1X,A60/1X,71('*')//
     +' DARC, the Dirac Atomic R-matrix Codes.'/
     +' homepage: www.am.qub.ac.uk/DARC/'/1X,A12,' module'/
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
     +/' ==============================================',
     + '==================='
     +/' ==============================================',
     + '==================='
     +/' '
     +/' DSTG2 defines the target states and channels. ',
     + 'Then it evaluates'
     +/' the angular integrals, asymptotic coefficients',
     + ' and continuum'
     +/' Hamiltonian. The photo-ionisation code evaluat',
     + 'es dipole matrix'
     +/' elements.'
     +/' '
     +)
 4020 FORMAT (
     + ' Routine BMAT uses the input target CSFs to cal',
     + 'culate a target'
     +/' Hamiltonian that is diagonalised. This gives t',
     + 'he target energies'
     +/' and CSF mixing coefficients that are to be use',
     + 'd in the scattering'
     +/' calculation. All angular integrals are calcula',
     + 'ted in DSTG2 and'
     +/' these are combined with the appropriate radial',
     + ' integrals that'
     +/' have been calculated in DSTG1INT.'
     +/' '
     +/' For each (N_e+1)-electron symmetry, specified ',
     + 'by a total'
     +)
 4030 FORMAT (
     + ' angular momentum J and parity Pi, the followin',
     + 'g steps are'
     +/' carried out.'
     +/' '
     +/' Routine CONT is called to generate the channel',
     + 's. A channel is a'
     +/' target state coupled to a continuum electron. ',
     + 'Each target state'
     +/' is built from a linear combination of target C',
     + 'SFs. In a similar'
     +/' way we can define a set of continuum CSFs whic',
     + 'h consist of a'
     +/' target CSF coupled to the continuum electron. ',
     + 'The channels are'
     +)
 4040 FORMAT (
     + ' then built from the continuum CSFs. These cont',
     + 'inuum CSFs are the'
     +/' basic quantities that are used to evaluate the',
     + ' angular integrals.'
     +/' '
     +/' The angular integrals are evaluated by calls to:'
     +/' --- routine ANGULA for the asymptotic coefficients'
     +/' --- routine ANGULH for the continuum Hamiltonian matrix'
     +/' --- routine ANGULD for the dipole matrix elements'
     +/' '
     +/' All of the angular integrals are evaluated at ',
     + 'this stage and'
     +/' stored in a file. Later they are read back and',
     + ' combined with the'
     +)
 4050 FORMAT (
     + ' appropriate radial integrals. This procedure i',
     + 's adopted so that'
     +/' the code for the angular calculation requires little'
     +/' modification when taken from GRASP.'
     +/' '
     +/' A call to routine AIJZ controls the calculation of the'
     +/' asymptotic coefficients.'
     +/' '
     +/' A call to routine CMAT controls the calculatio',
     + 'n of the continuum'
     +/' Hamiltonian matrix.'
     +/' '
     +/' A call to routine DMEL controls the calculatio',
     + 'n of the dipole'
     +)
 4060 FORMAT (
     + ' matrix elements.'
     +/' '
     +/' I/O (of both radial and angular integrals) can',
     + ' be a significant'
     +/' part of the computation.'
     +/' '
     +/' ==============================================',
     + '==================='
     +/' ==============================================',
     + '==================='
     +/' '
     +/' Code versions'
     +/' -------------'
     +/' '
     +)
 4070 FORMAT (
     + ' Two programs to calculate general recoupling c',
     + 'oefficients are'
     +/' available. NJGRAF is much faster than NJSYM in',
     + ' most cases and is'
     +/' recommended. With the warning that it has been buggy.'
     +/' '
     +/' ==============================================',
     + '==================='
     +/' ==============================================',
     + '==================='
     +/' '
     +/' File usage'
     +/' ----------'
     +/' '
     +/' The files are defined internally. Messages are',
     + ' also written to'
     +/' output stream 6 using the PRINT command.'
     +)
 4100 FORMAT (
     + ' '
     +/' The following files are used:'
     +/' '
     +/' |------------|-----------------------------|',
     + '------------------|'
     +/' |file        |description                  |',
     + 'stream stream FORM|'
     +/' |name        |                             |',
     + 'name   number     |'
     +/' |------------|-----------------------------|',
     + '------------------|'
     +/' |DSTG2.INP   |Input.                       |',
     + 'IREAD    5    F   |'
     +/' |DSTG2.OUT   |Output.                      |',
     + 'IWRITE   7    F   |'
     +)
 4110 FORMAT (
     + ' |------------|-----------------------------|',
     + '------------------|'
     +/' |DSTG1.DAT   |DSTG1 dump.                  |',
     + 'ITAPE1  20    U   |'
     +/' |DSTG2.DAT   |DSTG2 dump.                  |',
     + 'ITAPE2  21    U   |'
     +/' |INTEGRAL.DAT|DA file for radial integrals.|',
     + '---     22    U   |'
     +/' |------------|-----------------------------|',
     + '------------------|'
     +/' |---         |Scratch file for input data. |',
     + '---      9    F   |'
     +/' |---         |Scratch  angular  file.      |',
     + 'IDISC1  11    U   |'
     +)
 4120 FORMAT (
     + ' |---         |Scratch  radial  file.       |',
     + 'IDISC2  12    U   |'
     +/' |---         |Scratch AIJ angular          |',
     + 'IDISC3  13    U   |'
     +/' |            |  coefficients (bound parts) |',
     + '                  |'
     +/' |---         |Scratch direct slater        |',
     + 'IDISC4  14    U   |'
     +/' |            |  coefficients               |',
     + '                  |'
     +/' |---         |Scratch storage of asymptotic|',
     + 'IDISC5  15    U   |'
     +/' |            |  coefficients               |',
     + '                  |'
     +)
 4130 FORMAT (
     + ' |------------|-----------------------------|',
     + '------------------|'
     +/' '
     +/' DA == direct access'
     +/' F  == formatted'
     +/' U  == unformatted'
     +/' '
     +/'                        INTEGRAL.DAT (created b',
     + 'y dstg1-ints)'
     +/'                        DSTG1.DAT    (created b',
     + 'y dstg1-ints)'
     +/'                           |'
     +/'                           |'
     +)
 4140 FORMAT (
     + '                           |'
     +/'          DSTG2.INP ---> dstg2    ---> DSTG2.OUT'
     +/'          input            |          output'
     +/'                           |'
     +/'                           |'
     +/'                        DSTG2.DAT    (created b',
     + 'y dstg2,'
     +/'                                      used by dstgh)'
     +/' '
     +/' ==============================================',
     + '==================='
     +/' ==============================================',
     + '==================='
     +/' '
     +)
 4150 FORMAT (
     + ' Input data on stream IREAD (file: DSTG2.INP)'
     +/' --------------------------'
     +/' '
     +/' Namelist default values are given in square brackets.'
     +/' '
     +/' Input record 1'
     +/' --------------'
     +/' '
     +/' IHED ... 80-character title.'
     +/' '
     +/' Input record 2'
     +/' --------------'
     +/' '
     +/' NAMELIST / DSTG2 / IPOLPH, NAST, NMAN, NWM, OPT'
     +)
 4160 FORMAT (
     + ' '
     +/' IPOLPH [1]  ... type of calculation:'
     +/'                  = 1 for electron scattering;'
     +/'                  = 2 for photo-ionisation or p',
     + 'olarisation'
     +/' NAST   [-1] ... target levels'
     +/' NMAN   [-1] ... non-relativistic target CSFs (',
     + 'compulsory).'
     +/' NWM    [-1] ... non-relativistic bound orbital',
     + 's (compulsory).'
     +/' '
     +/' OPT    [-1] ... option array.'
     +/' '
     +/' Operational options.'
     +)
 4170 FORMAT (
     + ' '
     +/'  1 ... only direct angular coefficients are ca',
     + 'lculated for the'
     +/'        continuum Hamiltonian.'
     +/'  2 ... maximum value for lambda-value is set to 2.'
     +/'  3 ... skip the computation of asymptotic scattering'
     +/'        coefficients.'
     +/' 47 ... define all possible continuum CSFs.'
     +/' 48 ... calculate all asymptotic scattering coe',
     + 'fficients.'
     +/' 49 ... calculate all Hamiltonian angular coefficients.'
     +/' 50 ... calculate all angular asymptotic scattering'
     +/'        coefficients.'
     +/' '
     +)
 4180 FORMAT (
     + ' Printing options.'
     +/' '
     +/'  7 ... print CSFs for target case.'
     +/'  8 ... print CSFs for continuum case.'
     +/'  9 ... print CSFs for photo-ionisation case.'
     +/' 10 ... print the target energies in various units.'
     +/' 11 ... print the target Hamiltonian.'
     +/' 12 ... print the target eigenvectors.'
     +/' 13 ... print the target Hamiltonian angular co',
     + 'efficients.'
     +/' 14 ... printout from routine MCPINB.'
     +/' 15 ... printout from routine BMATX.'
     +/' 20 ... print the dipole and quadrupole coupling.'
     +/' 21 ... print the asymptotic scattering coefficients.'
     +)
 4190 FORMAT (
     + ' 22 ... printout from routine FINM.'
     +/' 23 ... examine the build-up of the asymptotic ',
     + 'scattering'
     +/'        coefficients with target CI.'
     +/' 25 ... printout from routine DMEL.'
     +/' 26 ... printout from routine DELIN.'
     +/' 27 ... printout from routines FINMBB, FINMBC a',
     + 'nd FINMCC.'
     +/' 30 ... print the continuum Hamiltonians.'
     +/' 31 ... examine the build-up of the continuum-continuum'
     +/'        contributions to the continuum Hamiltonian with'
     +/'        target CI.'
     +/' 32 ... examine the build-up of the bound-conti',
     + 'nuum contributions'
     +)
 4200 FORMAT (
     + '        to the continuum Hamiltonian with target CI.'
     +/' 33 ... print the bound-bound contributions to ',
     + 'the continuum'
     +/'        Hamiltonian.'
     +/' 34 ... print the continuum Hamiltonian angular',
     + ' coefficients.'
     +/' 35 ... printout from routine MCPINC.'
     +/' 36 ... one-line printout from the LOC... routines.'
     +/' 37 ... additional output from the LOC... routines.'
     +/' 38 ... one-line printout from routine FINBB.'
     +/' 39 ... one-line printout from routine FINBC.'
     +/' 40 ... one-line printout from routine FINCC.'
     +/' 41 ... printout of radial integrals from FINCC',
     + ' and FINBC.'
     +)
 4210 FORMAT (
     + ' '
     +/' Input record 3 - orbital data'
     +/' --------------'
     +/' The input data is for non-relativistic CSFs.'
     +/' '
     +/' NAMELIST / ORB / PRINC, KAPPA, CSF'
     +/' '
     +/' Read NWM input records.'
     +/' '
     +/' PRINC [-1] ... principal quantum number n.'
     +/'                n=1,2,3,....'
     +/' KAPPA [0]  ... kappa quantum number.'
     +/'                kappa = -1 s+'
     +/'                kappa = -2 p+'
     +/'                kappa = -3 d+'
     +/' CSF   [-1] ... (array)'
     +/'                orbital occupation numbers for each CSF:'
     +)
 4220 FORMAT (
     + '                0 numbers    --- subshell is fu',
     + 'll in each CSF'
     +/'                1 number     --- each CSF has t',
     + 'his occupation'
     +/'                NMAN numbers --- occupation for each CSF'
     +/' '
     +/' Input record 4 - options for the angular momen',
     + 'tum programs'
     +/' --------------'
     +/' '
     +/' NAMELIST / ANGOPT / OPT'
     +/' '
     +/' OPT    [-1] ... option array.'
     +/' '
     +/' 1 ... write out the angular coefficients calculated'
     +/' 2 ... output from RKCO, COR, MUMDAD, VIJOUT (2',
     + '-electron case)'
     +)
 4230 FORMAT (
     + ' 3 ... output from NJGRAF/NJSYM'
     +/' 4 ... output from VIJOUT (1-electron case)'
     +/' 5 ... output from TMSOUT'
     +/' 6 ... output from TNSRJJ (1-electron case)'
     +/' '
     +/' Input record 5 - non-relativistic CSF total J-values'
     +/' --------------'
     +/' '
     +/' NAMELIST / JVALUE / JOPT, JLOW, JHIGH'
     +/' '
     +/' This input record specifies the total J-values of the'
     +/' non-relativistic CSFs as defined above.'
     +/' '
     +/' JOPT [-1] ... = -1'
     +)
 4240 FORMAT (
     + ' If JLOW(1) is not specified then all possible ',
     + 'CSFs are to'
     +/' be generated i.e. all possible J-values. Optio',
     + 'nally input'
     +/' JLOW(1) and only CSFs with this J-value are included.'
     +/' '
     +/' JOPT [-1] ... = -2'
     +/' You must specify NMAN values for JLOW and NMAN',
     + ' values for JHIGH.'
     +/' These specify limits for the allowed J-values ',
     + 'for the particular'
     +/' CSF i.e. only J-values satisfying (JMIN leq J ',
     + 'leq JMAX) are'
     +/' included.'
     +)
 4250 FORMAT (
     + ' '
     +/' JOPT [-1] ... = -3'
     +/' You must specify JLOW(1) and JHIGH(1) values. ',
     + 'These specify'
     +/' limits for the allowed J-values for all the CS',
     + 'Fs i.e. only'
     +/' J-values satisfying (JMIN leq J leq JMAX) are included.'
     +/' '
     +/' JOPT [-1] ... = -4'
     +/' You must specify NMAN values for JLOW. These a',
     + 're the J-values'
     +/' of the CSFs in the order described above.'
     +/' '
     +/' Input record 6'
     +)
 4260 FORMAT (
     + ' --------------'
     +/' '
     +/' NAMELIST / SYM / CORINC, JTOT, NCFGP, NPTY'
     +/' '
     +/' Repeat for each (N_e+1)-electron symmetry required.'
     +/' Terminate with JTOT=-1.'
     +/' Also the code will stop if this input record i',
     + 's not present.'
     +/' '
     +/' JTOT [-1]'
     +/' is the total J-value for the (N_e+1)-electron sy',
     + 'mmetry. JTOT is a'
     +/' real variable such as 0.5 or 1.5 or 2.0. Set n',
     + 'egative to end'
     +)
 4270 FORMAT (
     + ' input.'
     +/' '
     +/' NPTY [0]'
     +/' is the parity of the (N_e+1)-electron symmetry (',
     + '-1 for odd or 1'
     +/' for even)'
     +/' '
     +/' NCFGP [-1]'
     +/' is the number of correlation functions to be included.'
     +/' Default gives all correlation functions. At pr',
     + 'esent you can have'
     +/' either 0 or all possible correlation functions.'
     +/' '
     +/' CORINC [1]'
     +)
 4280 FORMAT (
     + ' is an array used to mark the non-relativistic ',
     + 'CSFs that are'
     +/' to be included in generating correlation funct',
     + 'ions. By default'
     +/' CORINC is set to 1, all CSFs are to be include',
     + 'd. By setting'
     +/' CORINC(I) to 0 then CSF I will be excluded.'
     +/' '
     +/' ==============================================',
     + '==================='
     +/' ==============================================',
     + '==================='
     +/' '
     +)
      END
CEND--------------------------------------------------------------------
CEND    AAMN2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AAMN2 (IREAD,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AAMN2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
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
      DOUBLE PRECISION CLOCAL
      PARAMETER (CLOCAL=137.0359895D0)
      DOUBLE PRECISION EPS8
      PARAMETER (EPS8=1.D-8)
      DOUBLE PRECISION B1
      PARAMETER (B1=5.48579903D-4)
      DOUBLE PRECISION B2
      PARAMETER (B2=2.D0*1.0973731534D5)
      DOUBLE PRECISION B3
      PARAMETER (B3=510999.06D0/(CLOCAL*CLOCAL))
      DOUBLE PRECISION B4
      PARAMETER (B4=1.D8)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      INTEGER ND14
      PARAMETER (ND14=MXNC*(MXNC-1))
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NDZ
      PARAMETER (NDZ=5)
C
C  Argument variables
C
      INTEGER IREAD,IWRITE
C
C  Local variables
C
      DOUBLE PRECISION BB,TIM1,TIM2,TIM3
      DOUBLE PRECISION TIM4,TIMTO1,TIMTO2,TIMTO3
      DOUBLE PRECISION TIMTO4
      DOUBLE PRECISION TIME2
      DOUBLE PRECISION DUMMY(1)
      DOUBLE PRECISION RAC
      INTEGER IREC
      INTEGER IDISC1,IDISC2,IDISC3,IDISC4
      INTEGER IDISC5,ITAPE1,ITAPE2,MORE
      INTEGER MX
      INTEGER M,N
      LOGICAL EX
C
      CHARACTER*2 INH(MXNW)
      CHARACTER*2 NH(MXNW)
      CHARACTER*2 NHC(MXCH)
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      DOUBLE PRECISION ASY(MXCH,MXCH)
      DOUBLE PRECISION ATW
      DOUBLE PRECISION EMTB(MXNL,MXNL)
      DOUBLE PRECISION EMTR(MXNL,MXNL)
      DOUBLE PRECISION ENAT(MXNL)
      DOUBLE PRECISION FACTAN
      DOUBLE PRECISION FACTCM
      DOUBLE PRECISION FACTEV
      DOUBLE PRECISION RKSTO(MXI1)
      DOUBLE PRECISION XSLDR(MXNM)
      INTEGER IBBPOL(NDX,NDX,NDZ)
      INTEGER IBCPOL(NDX,MXNK,NDZ)
      INTEGER ICCPOL(MXNK,MXNK,NDZ)
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER ICHOP(MXNW,MXNC)
      INTEGER ICTXB(NDX,NDX,NDX)
      INTEGER IDMTST(30)
      INTEGER IEXC
      INTEGER IICHOP(MXNW,MXNC)
      INTEGER IIQ(MXNW,MXNC)
      INTEGER IISPAR(MXNC)
      INTEGER IITJPO(MXNC)
      INTEGER IJ2P1
      INTEGER IJCUP(10,MXNC)
      INTEGER IJQS(3,MXNW,MXNC)
      INTEGER INAK(MXNW)
      INTEGER INAKK(MXCH)
      INTEGER INCCP(MXNL,MXCH)
      INTEGER INCF
      INTEGER INCHAN
      INTEGER INTARG(MXCH)
      INTEGER IPOLPH
      INTEGER IPOS(MXNW)
      INTEGER IQ(MXNW,MXNC)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISLDR(MXNM)
      INTEGER ISPAR(MXNC)
      INTEGER ISTX1B(MXI2)
      INTEGER ISTX2B(MXI2)
      INTEGER ISTXB(NDX)
      INTEGER ITC(50)
      INTEGER ITJPO(MXNC)
      INTEGER J2P1
      INTEGER JAG(MXNK)
      INTEGER JCHOP(MXNW,MXNC)
      INTEGER JCUP(10,MXNC)
      INTEGER JLAST
      INTEGER JLASTX
      INTEGER JQS(3,MXNW,MXNC)
      INTEGER JSLDR(MXNM)
      INTEGER K1S
      INTEGER K2P(MXCH)
      INTEGER K2S
      INTEGER KAG(MXNK)
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBCN1(MXNK)
      INTEGER KBCN2(MXNK)
      INTEGER KBCR(MXNK)
      INTEGER KBMAX
      INTEGER KCCN(MXNK,MXNK)
      INTEGER KCCR(MXNK,MXNK)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KMULTN
      INTEGER KMULTR
      INTEGER KPOS
      INTEGER KX
      INTEGER LAG(MXNK)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MAXP
      INTEGER MINNQN(MXNK)
      INTEGER MNLDR(MXNC,MXNC)
      INTEGER NAK(MXNW)
      INTEGER NAKK(MXCH)
      INTEGER NAST
      INTEGER NCASES
      INTEGER NCCA(MXNC,MXCH)
      INTEGER NCCK(MXNC)
      INTEGER NCCN(MXNC)
      INTEGER NCCP(MXNL,MXCH)
      INTEGER NCCT(MXNC)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(30)
      INTEGER NLX(MXNW)
      INTEGER NMAN
      INTEGER NMCP
      INTEGER NNLDR(ND14)
      INTEGER NP(MXNW)
      INTEGER NPTY
      INTEGER NQX(MXNW,MXNC)
      INTEGER NRANG2
      INTEGER NSLDF(ND14)
      INTEGER NTARG(MXCH)
      INTEGER NW
      INTEGER NW1
      INTEGER NWM
C
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
C
      INTEGER NUMC
      INTEGER NUMR
      INTEGER NUMSTO
C
C  Common variables
C
      INTEGER ITAB(16),JTAB(16),NROWS
      INTEGER NTAB(255)
      COMMON / TERMS  / NROWS,ITAB,JTAB,NTAB
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA TIMTO1,TIMTO2,TIMTO3,TIMTO4/0.D0,0.D0,0.D0,0.D0/
      DATA TIM1,TIM2,TIM3,TIM4/0.D0,0.D0,0.D0,0.D0/
C-----------------------------------------------------------------------
C
C  Now make calls to:
C
C  QUARTZ ... initialise timing
C  CALEN  ... get RECORD of current time/date
C  DMSETx ... set dimensions
C
      CALL QUARTZ(-1,IWRITE,TIME2)
      CALL CALEN(IWRITE,RECORD)
      CALL DMSET2(IWRITE, IDMTST, NDIMAX)
C
C  Work out conversion factors taking account of atomic weight
C
      ATW = ZERO
C
      IF (ATW.GT.EPS8) THEN
        BB = ONE/(ONE+B1/ATW)
      ELSE
        BB = ONE
      ENDIF
C
      FACTCM = B2*BB
      FACTEV = B3*BB
      FACTAN = B4
C
      WRITE (IWRITE,3000) ATW,FACTCM,FACTEV
C-----------------------------------------------------------------------
      NUMC = 0
      NUMR = 0
      NUMSTO = 0
C
      MORE = 1
      NCASES = 0
C
C  Initialise JLAST and JLASTX.
C  These are set to -1.
C  This indicates that previous results cannot be reused.
C  JLAST  - reuse asymptotic coefficients
C  JLASTX - reuse continuum-continuum direct Slater angular integrals
C
      JLAST = -1
      JLASTX = -1
C
C  Read the input data
C
      PRINT 3080
      CALL READ2(IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6, ICHOP, IDMTST!
     +, IHED,IPOLPH, IPOS, IQ, IREAD, ISPAR, ITAB, ITC, ITJPO, IWRITE, J!
     +CHOP,JCUP, JQS, JTAB, NAK, NAST, NCF, NDIMAX, NH, NLX, NMAN,NP, NQ!
     +X, NROWS, NTAB, NW, NWM)
C
C   Set the dataset numbers for program
C
C
      IDISC1 = 11
      IDISC2 = 12
      IDISC3 = 13
      IDISC4 = 14
      IDISC5 = 15
C
      ITAPE1 = 20
      ITAPE2 = 21
C
      WRITE (IWRITE,3070) IDISC1,IDISC2,IDISC3,IDISC4,IDISC5,ITAPE1,ITAP!
     +E2
C
      OPEN (UNIT=IDISC1,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN (UNIT=IDISC2,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN (UNIT=IDISC3,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN (UNIT=IDISC4,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN (UNIT=IDISC5,STATUS='SCRATCH',FORM='UNFORMATTED')
C
      INQUIRE (FILE='DSTG1.DAT',EXIST=EX)
      IF (.NOT.EX) THEN
        PRINT 3170
        WRITE (IWRITE,3170)
        STOP
      ENDIF
      OPEN (UNIT=ITAPE1,STATUS='OLD',FORM='UNFORMATTED',FILE='DSTG1.DAT'!
     +)
C
      OPEN (UNIT=ITAPE2,STATUS='UNKNOWN',FORM='UNFORMATTED',FILE='DSTG2.!
     +DAT')
C
C  Open the random access file for reading
C
      IREC = 0
      CALL DA1('INTEGRAL.DAT',1,IREC,22,0,DUMMY)
C
C  Read the dstg1 dump
C
      PRINT 3100
      CALL WRITP2(ITAPE1,ITAPE2,IDISC2,RECORD, IHED, IBBPOL, IBCPOL, ICC!
     +POL, ICTXB, IDMTST, IPOLPH,ISTX1B, ISTX2B, ISTXB, IWRITE, JAG, K1S!
     +, K2S, KAG, KBBN1, KBBN2,KBBR, KBCN1, KBCN2, KBCR, KBMAX, KCCN, KC!
     +CR, KCMAX, KCMIN,KMULTN, KMULTR, KPOS, KX, LAG, LAMBB, LAMBC, LAMC!
     +C, MAXFUL,MAXNQN, MAXP, MINNQN, NDIMAX, NRANG2)
C
C  Evaluate the target Hamiltonian, level energies and eigenvectors
C
      PRINT 3110
      CALL BMAT(IDISC1,ITAPE2,EMTB, EMTR, ENAT,FACTAN, FACTCM, FACTEV, I!
     +BUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6,ICHOP, IDMTST, IEXC, IQ, I!
     +RK1, IRK2, ISPAR, ITC, ITJPO,IWRITE, JCUP, JQS, KBMAX, MAXFUL, MAX!
     +NQN, MINNQN, NAK, NAST,NCF, NDIMAX, NH, NP, NW, NW1,ICTXB, ISTX1B,!
     + ISTX2B, ISTXB, KBBN1, KBBN2, KBBR,K1S, K2S,ITAB, JTAB, NROWS, NTA!
     +B,RKSTO, ISLDR, JSLDR, NMCP, NNLDR, NSLDF, XSLDR)
C-----------------------------------------------------------------------
C
C  For each symmetry
C
C-----------------------------------------------------------------------
   10 CONTINUE
      NCASES = NCASES+1
      WRITE (IWRITE,3010)
      WRITE (IWRITE,3020) NCASES
C
C  Read the input data
C
      PRINT 3090
      CALL READ2C(MORE,ICHOP, IDMTST, IPOS, IQ, IREAD, ISPAR, ITAB, ITJP!
     +O,IWRITE, J2P1, JCHOP, JCUP, JQS, JTAB, NAK, NCF, NCFGP,NDIMAX, NL!
     +X, NMAN, NPTY, NQX, NROWS, NTAB, NW, NWM)
C
      IF (MORE.EQ.0) GOTO 20
C
C  Identify the channels and calculate the continuum angular integrals
C
      PRINT 3120,NCASES
      CALL QUARTZ(2,IWRITE,TIME2)
      CALL CONT(IDISC1,IDISC3,MX,EMTR, IBUG1, IBUG2, IBUG3, IBUG4, IBUG5!
     +, IBUG6, ICFCON, ICFCOR,ICHOP, IDMTST, IEXC, IICHOP, IIQ, IISPAR, !
     +IITJPO, IJ2P1,IJCUP, IJQS, INAK, INAKK, INCCP, INCF, INCHAN, INH, !
     +INTARG,IPOLPH, IQ, ISPAR, ITAB, ITC, ITJPO, IWRITE, J2P1, JCUP,JLA!
     +ST, JLASTX, JQS, JTAB, K2P, KCMAX, KCMIN, NAK, NAKK,NAST, NCASES, !
     +NCCA, NCCK, NCCN, NCCP, NCCT, NCF, NCFCON,NCFGP, NCHAN, NDIMAX, NH!
     +, NHC, NP, NPTY, NROWS, NTAB, NTARG, NW)
      CALL QUARTZ(2,IWRITE,TIME2)
      TIMTO1 = TIMTO1+TIME2
      TIM1 = TIME2
C
      IF (MX.EQ.0) THEN
C
C  Evaluate the asymptotic coefficients
C
        PRINT 3130
        CALL QUARTZ(2,IWRITE,TIME2)
        CALL AIJZ(ASY, EMTR, IBBPOL, IBCPOL, ICCPOL, IDISC1, IDISC5, IPO!
     +LPH,IRK1, ITAPE2, ITC, IWRITE, J2P1, JLAST, K1S, K2P,K2S, KBMAX, K!
     +CMAX, KMULTN, KMULTR, LAMBB, LAMBC,LAMCC, NAST, NCASES, NCCA, NCCN!
     +, NCCT, NCFGP, NCHAN,NPTY, NRANG2, NTARG, RKSTO, LAG, MAXNQN, NAK,!
     + NH, NP)
        CALL QUARTZ(2,IWRITE,TIME2)
        TIMTO2 = TIMTO2+TIME2
        TIM2 = TIME2
C
C  Set up the continuum Hamiltonian
C
        PRINT 3140
        CALL QUARTZ(2,IWRITE,TIME2)
        CALL CMAT(IDISC1,IDISC2,IDISC4,ITAPE2,EMTB, EMTR, ENAT, FACTEV, !
     +ICTXB, IDMTST,IQ, IRK1, IRK2, ISLDR, ISTX1B, ISTX2B, ISTXB, ITC, I!
     +WRITE,J2P1, JLASTX, JSLDR, K1S, K2P, K2S, KBBN1, KBBN2, KBBR,KBCN1!
     +, KBCN2, KBCR, KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS,MAXFUL, MAXNQ!
     +N, MAXP, MINNQN, NAK, NAKK, NCCK, NCCP, NCCT,NCF, NCFCON, NCFGP, N!
     +CHAN, NDIMAX, NH, NMCP, NNLDR, NP,NRANG2, NSLDF, NTARG, NW, NW1, R!
     +KSTO, XSLDR)
        CALL QUARTZ(2,IWRITE,TIME2)
        TIMTO3 = TIMTO3+TIME2
        TIM3 = TIME2
C
C  Calculate the photo-ionisation matrix elements
C
        IF (NCASES.GT.1 .AND. IPOLPH.EQ.2) THEN
          PRINT 3150
          CALL QUARTZ(2,IWRITE,TIME2)
          CALL DMEL(IDISC1,ITAPE2,EMTR, IBBPOL, IBCPOL, ICCPOL, ICFCON, !
     +ICFCOR, IDMTST,IJ2P1, INAKK, INCCP, INCHAN, INTARG, IRK1, ISLDR, I!
     +TC,IWRITE, J2P1, JSLDR, K1S, K2S, KBMAX, KCMAX, KMULTN, KMULTR,LAG!
     +, LAMBB, LAMBC, LAMCC, MAXNQN, MNLDR, NAK,NAKK, NCCP, NCF, NCFCON,!
     + NCFGP, NCHAN, NDIMAX, NH, NMCP,NP, NRANG2, NTARG, RKSTO, XSLDR   !
c kab
     +,K2P
c kab
     +)
          CALL QUARTZ(2,IWRITE,TIME2)
          TIMTO4 = TIMTO4+TIME2
          TIM4 = TIME2
        ENDIF
C
      ENDIF
C
      WRITE (IWRITE,3030) TIM1,TIMTO1,TIM2,TIMTO2,TIM3,TIMTO3
      IF (NCASES.GT.1 .AND. IPOLPH.EQ.2) THEN
        WRITE (IWRITE,3040) TIM4,TIMTO4
      ENDIF
C
      GOTO 10
C-----------------------------------------------------------------------
C
C  End of calculation
C
C-----------------------------------------------------------------------
   20 CONTINUE
      CALL DRACAH(-1,NUMC,NUMR,NUMSTO,M,N,RAC)
      IF (NUMC+NUMR.GT.0) THEN
        WRITE (IWRITE,3050) NUMC,NUMR,NUMC + NUMR,NUMSTO
      ENDIF
C
      NCASES = NCASES-1
      WRITE (IWRITE,3060) NCASES
      CALL DMPRT2(IWRITE, IDMTST, NDIMAX)
      PRINT 3160
C-----------------------------------------------------------------------
 3000 FORMAT (/' Conversion of units --- using atomic weight = ',F8.4// !
     +'       1 a.u. = ',F10.3,' cm-1'/'       1 a.u. = ',F10.7,' e.V.'/!
     +)
 3010 FORMAT (//1X,70('=')//)
 3020 FORMAT (31X,'Symmetry ',I2/31X,'-----------')
 3030 FORMAT (/' Calculation time for this symmetry and total'/         !
     +' --------------------------------------------'//                 !
     +' Set up channels and angular integrals     : ',F8.2,2X,F8.2/     !
     +' Evaluate asymptotic coefficients          : ',F8.2,2X,F8.2/     !
     +' Evaluate continuum Hamiltonian            : ',F8.2,2X,F8.2)
 3040 FORMAT (' Evaluate photo-ionisation matrix elements : ',F8.2,2X,F8!
     +.2)
 3050 FORMAT (/' Racah coefficients calculated = ',I9/                  !
     +' Racah coefficients from table = ',I9/                           !
     +' Racah coefficients total      = ',I9/                           !
     +' Racah coefficients stored     = ',I9)
 3060 FORMAT (/1X,I3,' symmetries (J,parity combinations) handled'/)
 3070 FORMAT (/1X,71('*')/' I/O files'/1X,71('*')/'  IDISC1 = ',I3,     !
     +' - scratch - angular file'/'  IDISC2 = ',I3,                     !
     +' - scratch -  radial file'/'  IDISC3 = ',I3,                     !
     +' - scratch - AIJ angular coefficients (bound parts)'/            !
     +'  IDISC4 = ',I3,' - scratch - for direct slater coefficients'/   !
     +'  IDISC5 = ',I3,                                                 !
     +' - scratch - temporary storage of asymptotic coefficients'/      !
     +'  ITAPE1 = ',I3,' - READ    - DSTG1 file'/'  ITAPE2 = ',I3,      !
     +' - WRITE   - DSTG2 file'/1X,71('*'))
 3080 FORMAT (/' about to call routine READ2')
 3090 FORMAT (/' about to call routine READ2C')
 3100 FORMAT (/' about to call routine WRITP2')
 3110 FORMAT (/' about to call routine BMAT')
 3120 FORMAT (/' about to call routine CONT : symmetry ',I3)
 3130 FORMAT (' about to call routine AIJZ')
 3140 FORMAT (' about to call routine CMAT')
 3150 FORMAT (' about to call routine DMEL')
 3160 FORMAT (' STOPPING normally')
 3170 FORMAT (/' DSTG1.DAT does not exist....STOPPING.')
      END
CEND--------------------------------------------------------------------
CEND    AIJ1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AIJ1(IDISC3,IBUG1, IBUG2, IBUG3, IBUG4, IBUG6,ICHOP, IQ!
     +, ISPAR, ITC, ITJPO, IWRITE, JCUP, JQS,NAK, NCF, NH, NP, NW,ITAB, !
     +JTAB, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AIJ1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG6
C   ICHOP
C   IQ
C   ISPAR
C   ITC
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   NAK
C   NCF
C   NH
C   NP
C   NW
C   ITAB
C   JTAB
C   NTAB
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL CRE
      DOUBLE PRECISION CRE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      INTEGER IZ
      PARAMETER (IZ=0)
C
C  Argument variables
C
      INTEGER IDISC3
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NP(*)
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      DOUBLE PRECISION CLEB,COEF,COX,FACT
      DOUBLE PRECISION VSHELL(MXNW)
      INTEGER IA,IC,IMT,IOPAR
      INTEGER ITEST,JA,JB,JLA
      INTEGER JLB,KA,KAMAX,KAMIN
      INTEGER KAPA,KAPB
      LOGICAL HEAD
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      HEAD = .FALSE.
      REWIND IDISC3
C
C  Option 3 set - skip the calculation of asymptotic coefficients.
C
      IF (ITC(3).EQ.1) THEN
        WRITE (IWRITE,3030)
        WRITE (IDISC3) IZ,IZ,IZ,IZ,IZ,EPS10
        RETURN
      ENDIF
C
C  JA and JB refer to the initial and final states in the list of
C  NCF CSFs, respectively.
C  Loop over all pairs of CSFs.
C
      IMT = 0
C
      DO JA = 1,NCF
        IA = ITJPO(JA)-1
        FACT = SQRT(DBLE(IA+1))
        DO JB = 1,NCF
          IC = ITJPO(JB)-1
C
C  Determine the lambda value
C
          KAMIN = ABS(IA-IC)/2
          IF (KAMIN.EQ.0) KAMIN = 1
          KAMAX = (IA+IC)/2
C
C  Option 2 set --- only use a maximum of 2
C  This is all that is required for using the Seaton asymptotic package.
C
          IF (KAMAX.GT.2 .AND. ITC(2).EQ.1) KAMAX = 2
C
          IF (KAMIN.LE.KAMAX) THEN
C
C  Set initial value for the parity
C
            IOPAR = (-1)**(KAMIN-1)
C
C  Loop over the lambda values
C
            DO KA = KAMIN,KAMAX
C
C  Set the parity
C
              IOPAR = -IOPAR
C
C  Call MCT package
C
              CALL TNSRJJ(KA,IOPAR,JA,JB,JLA,JLB,VSHELL,IBUG2, IBUG3, IB!
     +UG4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, N!
     +W,ITAB, JTAB, NTAB)
C
C  Output any coefficients that are calculated
C
              IF (JLA.GT.0) THEN
C
                IF (JLA.NE.JLB) THEN
C
                  COEF = VSHELL(1)
                  IF (ABS(COEF).GE.EPS10) THEN
                    KAPA = NAK(JLA)
                    KAPB = NAK(JLB)
                    ITEST = ABS(KAPA)+ABS(KAPB)+KA
                    IF (KAPA*KAPB.LT.0) ITEST = ITEST + 1
                    IF (MOD(ITEST,2).EQ.0) THEN
                      CLEB = CRE(KAPA,KA,KAPB)
                      COX = FACT*COEF*CLEB/SQRT(DBLE(2*ABS(KAPA)))
                      IF (ABS(COX).GE.EPS10) THEN
                        WRITE (IDISC3) JA,JB,JLA,JLB,KA,COX
                        IF (IBUG1.EQ.1) THEN
                          IF (.NOT.HEAD) THEN
                            WRITE (IWRITE,3000)
                            HEAD = .TRUE.
                          ENDIF
                          WRITE (IWRITE,3010) JA,JB,NP(JLA),NH(JLA),NP(J!
     +LB),NH(JLB),KA,COEF,COX
                        ENDIF
                        IMT = IMT+1
                      ENDIF
                    ENDIF
                  ENDIF
C
                ELSE
C
                  DO JLA = 1,NW
                    COEF = VSHELL(JLA)
                    IF (ABS(COEF).GE.EPS10) THEN
                      KAPA = NAK(JLA)
                      ITEST = 2*ABS(KAPA)+KA
                      IF (MOD(ITEST,2).EQ.0) THEN
                        CLEB = CRE(KAPA,KA,KAPA)
                        COX = FACT*COEF*CLEB/SQRT(DBLE(2*ABS(KAPA)))
                        IF (ABS(COX).GE.EPS10) THEN
                          WRITE (IDISC3) JA,JB,JLA,JLA,KA,COX
                          IF (IBUG1.EQ.1) THEN
                            IF (.NOT.HEAD) THEN
                              WRITE (IWRITE,3000)
                              HEAD = .TRUE.
                            ENDIF
                            WRITE (IWRITE,3010) JA,JB,NP(JLA),NH(JLA),NP!
     +(JLB),NH(JLB),KA,COEF,COX
                          ENDIF
                          IMT = IMT+1
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDDO
C
                ENDIF
C
              ENDIF
            ENDDO
C
          ENDIF
        ENDDO
C
      ENDDO
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020) IMT
C
      WRITE (IDISC3) IZ,IZ,IZ,IZ,IZ,EPS10
      REWIND IDISC3
C-----------------------------------------------------------------------
 3000 FORMAT (/5X,'r',3X,'s',4X,'a',4X,'b',4X,'k',4X,'coefficient'/)
 3010 FORMAT (2X,2I4,2X,2(I2,A2,1X),I3,2X,1P,2E16.8)
 3020 FORMAT (/' ===== AIJ1 called. ',I9,                               !
     +' asymptotic coefficients written to IDISC3')
 3030 FORMAT (/' ****************************************************'/ !
     +' ** Routine AIJ1 : option 3 has been set.          **'/          !
     +' ** No asymptotic coefficients will be calculated. **'/          !
     +' ** This corresponds to using Coulomb solutions in **'/          !
     +' ** the asymptotic region.                         **'/          !
     +' ****************************************************')
      END
CEND--------------------------------------------------------------------
CEND    AIJ2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AIJ2(IDISC1,IDISC3,IBUG1, ITC, ITJPO, IWRITE, J2P1, K2P!
     +, NCCK, NCCT,NCFCON, NH, NP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AIJ2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDISC1
C   IDISC3
C   IBUG1
C   ITC
C   ITJPO
C   IWRITE
C   J2P1
C   K2P
C   NCCK
C   NCCT
C   NCFCON
C   NH
C   NP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  External functions
C
      EXTERNAL CRE
      DOUBLE PRECISION CRE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      INTEGER IZ
      PARAMETER (IZ=0)
C
C  Argument variables
C
      INTEGER IDISC1,IDISC3
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER K2P(*)
      INTEGER NCCK(*)
      INTEGER NCCT(*)
      INTEGER NCFCON
      INTEGER NP(*)
C
C  Local variables
C
      DOUBLE PRECISION CLEB,COEF,COX,RAC
      INTEGER I,IA,IB,IC
      INTEGER ID,IE,IMT,ITEST
      INTEGER J,JA,JB,JLA
      INTEGER JLB,JMIN,KA,KAPI
      INTEGER KAPJ,LA
      LOGICAL HEAD
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      HEAD = .FALSE.
      REWIND IDISC3
C
      IMT = 0
      IE = J2P1-1
C-----------------------------------------------------------------------
C
C  Loop over all the coefficients stored on IDISC3
C
   10 CONTINUE
      READ (IDISC3) JA,JB,JLA,JLB,KA,COX
      IF (JA.EQ.0) THEN
        WRITE (IWRITE,3020) IMT
        WRITE (IDISC1) IZ,IZ,IZ,IZ,IZ,EPS10
        RETURN
      ENDIF
C-----------------------------------------------------------------------
      IA = ITJPO(JA)-1
      IC = ITJPO(JB)-1
C
      LA = KA+KA
      IF (MOD(KA,2).EQ.1) COX = -COX
C-----------------------------------------------------------------------
      DO I = 1,NCFCON
        IF (JA.EQ.NCCT(I)) THEN
C
          KAPI = NCCK(I)
          KAPI = K2P(KAPI)
          IB = 2*ABS(KAPI)-1
C-----------------------------------------------------------------------
C
C  Option 50 set --- angular asymptotic coefficients are calculated
C                    for all pairs of continuum CSFs.
C                    There should be symmetry between the pairs
C                    of continuum CSFs and this option can be
C                    used to check this occurs correctly.
C
          IF (ITC(50).EQ.1) THEN
            JMIN = 1
          ELSE
            JMIN = I
          ENDIF
C-----------------------------------------------------------------------
          DO J = JMIN,NCFCON
            IF (JB.EQ.NCCT(J)) THEN
C
              KAPJ = NCCK(J)
              KAPJ = K2P(KAPJ)
              ID = 2*ABS(KAPJ)-1
C
              ITEST = (IB+ID)/2+KA+1
              IF (KAPI*KAPJ.LT.0) ITEST = ITEST + 1
C
              IF (MOD(ITEST,2).EQ.0) THEN
                CLEB = CRE(KAPI,KA,KAPJ)
                IF (ABS(CLEB).GE.EPS10) THEN
                  CALL DRACAH(IC,LA,IE,IB,IA,ID,RAC)
                  COEF = COX*CLEB*RAC
                  IF (ABS(COEF).GE.EPS10) THEN
                    IF (IBUG1.EQ.1) THEN
                      IF (.NOT.HEAD) THEN
                        WRITE (IWRITE,3000)
                        HEAD = .TRUE.
                      ENDIF
                      WRITE (IWRITE,3010) I,J,NP(JLA),NH(JLA),NP(JLB),NH!
     +(JLB),KA,COX,COEF
                    ENDIF
                    WRITE (IDISC1) I,J,JLA,JLB,KA,COEF
                    IMT = IMT+1
                  ENDIF
                ENDIF
              ENDIF
C
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
      GOTO 10
C-----------------------------------------------------------------------
 3000 FORMAT (/5X,'r',3X,'s',4X,'a',4X,'b',4X,'k',4X,'coefficient'/)
 3010 FORMAT (2X,2I4,2X,2(I2,A2,1X),I3,2X,1P,2E16.8)
 3020 FORMAT (/' ===== AIJ2 called. ',I9,' A-coefficients calculated')
      END
CEND--------------------------------------------------------------------
CEND    AIJZ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AIJZ(ASY, EMTR, IBBPOL, IBCPOL, ICCPOL, IDISC1, IDISC5,!
     + IPOLPH,IRK1, ITAPE2, ITC, IWRITE, J2P1, JLAST, K1S, K2P,K2S, KBMA!
     +X, KCMAX, KMULTN, KMULTR, LAMBB, LAMBC,LAMCC, NAST, NCASES, NCCA, !
     +NCCN, NCCT, NCFGP, NCHAN,NPTY, NRANG2, NTARG, RKSTO, LAG, MAXNQN, !
     +NAK, NH, NP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AIJZ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ASY
C   EMTR
C   IBBPOL
C   IBCPOL
C   ICCPOL
C   IDISC1 ... file containing angular coefficients
C   IDISC5 ... temporary storage of the asymptotic coefficients
C   IPOLPH
C   IRK1
C   ITAPE2 ... DSTG2 dump file
C   ITC
C   IWRITE
C   J2P1
C   JLAST
C   K1S
C   K2P
C   K2S
C   KBMAX
C   KCMAX
C   KMULTN
C   KMULTR
C   LAMBB
C   LAMBC
C   LAMCC
C   NAST
C   NCASES
C   NCCA
C   NCCN
C   NCCT
C   NCFGP
C   NCHAN
C   NPTY
C   NRANG2
C   NTARG
C   RKSTO
C   LAG
C   MAXNQN
C   NAK
C   NH
C   NP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL FINM
      DOUBLE PRECISION FINM
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER MAXLAM
      PARAMETER (MAXLAM=10)
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION ASY(MXCH,*)
      DOUBLE PRECISION EMTR(MXNL,*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IBCPOL(NDX,MXNK,*)
      INTEGER ICCPOL(MXNK,MXNK,*)
      INTEGER IDISC1
      INTEGER IDISC5
      INTEGER IPOLPH
      INTEGER IRK1
      INTEGER ITAPE2
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JLAST
      INTEGER K1S
      INTEGER K2P(*)
      INTEGER K2S
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KMULTN
      INTEGER KMULTR
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXNQN(*)
      INTEGER NAK(*)
      INTEGER NAST
      INTEGER NCASES
      INTEGER NCCA(MXNC,*)
      INTEGER NCCN(*)
      INTEGER NCCT(*)
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NP(*)
      INTEGER NPTY
      INTEGER NRANG2
      INTEGER NTARG(*)
C
C  Local variables
C
      DOUBLE PRECISION CONTR,WX,X,Y
      INTEGER I,IA,IB,II
      INTEGER IKMAX,ILMAX,IMARK(MAXLAM)
      INTEGER IR,IS,IT,J
      INTEGER J2P1X,JJ,JT,LAM
      INTEGER LAMAX,LAMIN,MNP1
      INTEGER NCONAT(MXNL),NIR,NIS
      INTEGER NMCP,NR,NS,NUSED
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
C
C   Write continuum information to stream ITAPE2.
C
C-----------------------------------------------------------------------
      MNP1 = NRANG2*NCHAN+NCFGP
C
      DO I = 1,NAST
        IS = 0
        DO J = 1,NCHAN
          JT = NTARG(J)
          IF (JT.EQ.I) IS = IS + 1
        ENDDO
        NCONAT(I) = IS
      ENDDO
C
      IF (IPOLPH.EQ.2) THEN
        J2P1X = -J2P1
      ELSE
        J2P1X = J2P1
      ENDIF
C
      WRITE (ITAPE2) J2P1X,NPTY,NCFGP
      WRITE (ITAPE2) MNP1,NCHAN
      WRITE (ITAPE2) (NCONAT(I),I=1,NAST)
      WRITE (ITAPE2) (K2P(I),I=1,NCHAN)
C-----------------------------------------------------------------------
C
C   Locate integral block.
C
C-----------------------------------------------------------------------
      CALL LOCM(999,999, K1S, K2S, RKSTO,IBBPOL, IBCPOL, ICCPOL, IRK1, I!
     +TC, IWRITE,KMULTN, KMULTR, KBMAX, KCMAX, LAMBB, LAMBC, LAMCC)
C-----------------------------------------------------------------------
C
C   Reuse the last set of asymptotic coefficients
C   if J value was unchanged.
C
C   Option 48 set --- do not reuse the last set of asymptotic
C                     coefficients.
C
C-----------------------------------------------------------------------
      IF (ITC(48).EQ.0) THEN
C
        IF (J2P1.EQ.JLAST) THEN
          WRITE (IWRITE,3050)
          REWIND IDISC5
          READ (IDISC5) IKMAX
          WRITE (ITAPE2) IKMAX
          IF (IKMAX.GT.0) THEN
            DO LAM = 1,IKMAX
              READ (IDISC5) ((ASY(I,J),J=1,NCHAN),I=1,NCHAN)
              WRITE (ITAPE2) ((ASY(I,J),J=1,NCHAN),I=1,NCHAN)
            ENDDO
          ENDIF
          RETURN
        ENDIF
C
        IF (JLAST.EQ.-2) THEN
          JLAST = -1
        ELSE
          JLAST = J2P1
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
C
C   Zeroise the array IMARK.
C
C-----------------------------------------------------------------------
      DO LAM = 1,MAXLAM
        IMARK(LAM) = 0
      ENDDO
C-----------------------------------------------------------------------
      NMCP = 0
      NUSED = 0
      IKMAX = 0
      ILMAX = 0
      LAMAX = 0
C-----------------------------------------------------------------------
C
C  Loop over the LAM values.
C
C-----------------------------------------------------------------------
      DO LAM = 1,MAXLAM
        IF (LAM.GT.1 .AND. LAM.GT.IKMAX) GOTO 30
C-----------------------------------------------------------------------
C
C  Zeroise the array ASY.
C
C-----------------------------------------------------------------------
        DO I = 1,NCHAN
          DO J = 1,NCHAN
            ASY(I,J) = ZERO
          ENDDO
        ENDDO
C-----------------------------------------------------------------------
        IF (LAM.GT.1 .AND. IMARK(LAM).EQ.0) GOTO 20
C-----------------------------------------------------------------------
C
C   Read the A angular coefficients.
C
C   Note that these angular coefficients
C   should be symmetric in IR and IS.
C
C-----------------------------------------------------------------------
        REWIND IDISC1
C-----------------------------------------------------------------------
   10   CONTINUE
        READ (IDISC1) IR,IS,IA,IB,LAMIN,X
        IF (IR.EQ.0) GOTO 20
C-----------------------------------------------------------------------
C
C  The first time through
C  ..   count the angular coefficients in NMCP
C  ..   determine LAMAX which is the maximum value of LAM
C  ..   determine ILMAX
C  ..   determine IKMAX which is the maximum value of LAM to be used
C  ..   set up the array IMARK.
C
C-----------------------------------------------------------------------
        IF (LAM.EQ.1) THEN
          IF (LAMIN.GT.LAMAX) LAMAX = LAMIN
          NMCP = NMCP+1
          IF (LAMIN.LE.MAXLAM) THEN
            IMARK(LAMIN) = IMARK(LAMIN)+1
          ENDIF
          ILMAX = MIN(LAMAX,MAXLAM)
          IKMAX = MIN(ILMAX,LAMBB)
        ENDIF
C-----------------------------------------------------------------------
        IF (LAM.EQ.LAMIN) THEN
          IF (LAM.LE.IKMAX) THEN
            NUSED = NUSED+1
            Y = X*FINM(IR,IS,IA,IB,LAM,X,IBBPOL,IRK1,ITC,IWRITE,LAG,MAXN!
     +QN,NAK,NH,NP,RKSTO)
            NIR = NCCN(IR)
            NIS = NCCN(IS)
            II = NCCT(IR)
            JJ = NCCT(IS)
C
            DO NR = 1,NIR
              I = NCCA(IR,NR)
              IT = NTARG(I)
C
              DO NS = 1,NIS
                J = NCCA(IS,NS)
                JT = NTARG(J)
C
                WX = EMTR(II,IT)*EMTR(JJ,JT)
                CONTR = WX*Y
C
                ASY(I,J) = ASY(I,J)+CONTR
                IF (ITC(23).EQ.1) WRITE (IWRITE,3000) IR,IS,WX,I,J
C
C  Do not add contribution to the symmetric coefficient
C     when IR = IS          ( i.e. counting it twice)
C  or when option 50 is set ( i.e. all pairs of continuum CSFs
C                             are included)
C
                IF (IR.NE.IS .AND. ITC(50).EQ.0) THEN
                  ASY(J,I) = ASY(J,I)+CONTR
                  IF (ITC(23).EQ.1) WRITE (IWRITE,3000) IR,IS,WX,J,I
                ENDIF
C
              ENDDO
            ENDDO
C
          ENDIF
        ENDIF
C
        GOTO 10
C-----------------------------------------------------------------------
   20   CONTINUE
        IF (NCASES.EQ.1) THEN
          IF (ITC(20).EQ.1) THEN
            IF (LAM.EQ.1) CALL AIJZ1(ASY, IMARK, IWRITE, NAST, NCHAN, NT!
     +ARG)
            IF (LAM.EQ.2) CALL AIJZ2(ASY, IMARK, IWRITE, NAST, NCHAN, NT!
     +ARG)
          ENDIF
        ENDIF
C
C   Write out the coefficients.
C
C    stream ITAPE2 ... permanent output
C    stream IDISC5 ... temporary storage
C
        IF (LAM.EQ.1) THEN
          WRITE (ITAPE2) IKMAX
          REWIND IDISC5
          WRITE (IDISC5) IKMAX
        ENDIF
C-----------------------------------------------------------------------
        IF (IKMAX.GT.0) THEN
          IF (IMARK(LAM).GT.0) THEN
C
C  Check the symmetry of the coefficients.
C
            DO I = 1,NCHAN
              DO J = I,NCHAN
                IF (ABS(ASY(I,J)-ASY(J,I)).GT.EPS10) THEN
                  WRITE (IWRITE,3060) I,J,LAM
                ENDIF
              ENDDO
            ENDDO
C
C  Write out the coefficients if option 21 is set.
C
            IF (ITC(21).EQ.1) THEN
              WRITE (IWRITE,3020) LAM
              CALL MATOUT(IWRITE,ASY,NCHAN,NCHAN,MXCH,MXCH,1)
            ENDIF
C
          ENDIF
          WRITE (ITAPE2) ((ASY(I,J),J=1,NCHAN),I=1,NCHAN)
          WRITE (IDISC5) ((ASY(I,J),J=1,NCHAN),I=1,NCHAN)
        ENDIF
C-----------------------------------------------------------------------
   30   CONTINUE
      ENDDO
C-----------------------------------------------------------------------
C
C  Write information on the asymptotic coefficients.
C
C-----------------------------------------------------------------------
      IF (NMCP.GT.0) THEN
        WRITE (IWRITE,3030) NMCP,NUSED,LAMAX,IKMAX
        DO LAM = 1,ILMAX
          IF (IMARK(LAM).GT.0) THEN
            WRITE (IWRITE,3040) IMARK(LAM),LAM
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (' Contr. from continuum CSFS :',2I5,8X,'weight = ',1P,E12.!
     +5,8X,'for channels :',2I5)
 3010 FORMAT (/31X,'Routine AIJZ'/31X,'------------'/                   !
     +' ===== Evaluate the asymptotic coefficients')
 3020 FORMAT (/' LAM = ',I4)
 3030 FORMAT (/1X,I9,' asymptotic angular coefficients read'/1X,I9,     !
     +' asymptotic angular coefficients used'/1X,I4,                    !
     +' is the maximum value of LAM calculated'/1X,I4,                  !
     +' is the maximum value of LAM to be used'/)
 3040 FORMAT (1X,I9,' asymptotic angular coefficients for LAM =',I4)
 3050 FORMAT (/                                                         !
     +' **** using the asymptotic coefficients from the last symmetry ',!
     +'****'/)
 3060 FORMAT (/' *****************************************************'/!
     +' ***            WARNING from AIJZ                  ***'/         !
     +' *** Asymptotic coefficients are not symmetric for ***'/         !
     +' *** I,J,LAM : ',3I5,'                     ***'/                 !
     +' *****************************************************')
      END
CEND--------------------------------------------------------------------
CEND    AIJZ1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AIJZ1(ASY, IMARK, IWRITE, NAST, NCHAN, NTARG)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AIJZ1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ASY
C   IMARK
C   IWRITE
C   NAST
C   NCHAN
C   NTARG
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
C
C  Argument variables
C
      DOUBLE PRECISION ASY(MXCH,*)
      INTEGER IMARK(*)
      INTEGER IWRITE
      INTEGER NAST
      INTEGER NCHAN
      INTEGER NTARG(*)
C
C  Local variables
C
      INTEGER I,IT,ITEMP(MXNL,MXNL)
      INTEGER J,JT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Which states are coupled by the dipole interaction ?
C
C-----------------------------------------------------------------------
      IF (IMARK(1).GT.0) THEN
C
        WRITE (IWRITE,3000)
C
        DO I = 1,NAST
          DO J = 1,NAST
            ITEMP(I,J) = 0
          ENDDO
        ENDDO
C
        DO I = 1,NCHAN
          DO J = I,NCHAN
            IF (ABS(ASY(I,J)).GT.EPS10) THEN
              IT = NTARG(I)
              JT = NTARG(J)
              ITEMP(IT,JT) = 1
              ITEMP(JT,IT) = 1
            ENDIF
          ENDDO
        ENDDO
C
        DO I = 1,NAST
          DO J = I,NAST
            IF (ITEMP(I,J).EQ.1) WRITE (IWRITE,3010) I,J
          ENDDO
        ENDDO
C
      ELSE
        WRITE (IWRITE,3020)
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/                                                         !
     +' There is dipole coupling between the following target states : '!
     +/)
 3010 FORMAT (10X,2I6)
 3020 FORMAT (/' There is no dipole coupling between the target states')
      END
CEND--------------------------------------------------------------------
CEND    AIJZ2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE AIJZ2(ASY, IMARK, IWRITE, NAST, NCHAN, NTARG)
CRCS
CRCS $Source: /home/phn/DARC/RCS/AIJZ2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ASY
C   IMARK
C   IWRITE
C   NAST
C   NCHAN
C   NTARG
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
C
C  Argument variables
C
      DOUBLE PRECISION ASY(MXCH,*)
      INTEGER IMARK(*)
      INTEGER IWRITE
      INTEGER NAST
      INTEGER NCHAN
      INTEGER NTARG(*)
C
C  Local variables
C
      INTEGER I,IT,ITEMP(MXNL,MXNL)
      INTEGER J,JT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Which states are coupled by the quadrupole interaction ?
C
C-----------------------------------------------------------------------
      IF (IMARK(2).GT.0) THEN
C
        WRITE (IWRITE,3000)
C
        DO I = 1,NAST
          DO J = 1,NAST
            ITEMP(I,J) = 0
          ENDDO
        ENDDO
C
        DO I = 1,NCHAN
          DO J = I,NCHAN
            IF (ABS(ASY(I,J)).GT.EPS10) THEN
              IT = NTARG(I)
              JT = NTARG(J)
              ITEMP(IT,JT) = 1
              ITEMP(JT,IT) = 1
            ENDIF
          ENDDO
        ENDDO
C
        DO I = 1,NAST
          DO J = I,NAST
            IF (ITEMP(I,J).EQ.1) WRITE (IWRITE,3010) I,J
          ENDDO
        ENDDO
C
      ELSE
        WRITE (IWRITE,3020)
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/                                                         !
     +' There is quadrupole coupling between the following target stat',!
     +'es : '/)
 3010 FORMAT (10X,2I6)
 3020 FORMAT (/                                                         !
     +' There is no quadrupole coupling between the target states')
      END
CEND--------------------------------------------------------------------
CEND    ANGLB1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGLB1(IWRITE,ICHOP, IQ, ISPAR, ITJPO, JCUP, JQS, NCF, !
     +NW,NICHOP, IQTAR, NISPAR, NITJPO, NJCUP, NJQS, NCFTAR, NWTAR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGLB1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE
C   ICHOP
C   IQ
C   ISPAR
C   ITJPO
C   JCUP
C   JQS
C   NCF
C   NW
C   NICHOP
C   IQTAR
C   NISPAR
C   NITJPO
C   NJCUP
C   NJQS
C   NCFTAR
C   NWTAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Local variables
C
      INTEGER I,J,K
C
C  Argument variables
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER IQTAR(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER NCF
      INTEGER NCFTAR
      INTEGER NICHOP(MXNW,*)
      INTEGER NISPAR(*)
      INTEGER NITJPO(*)
      INTEGER NJCUP(10,*)
      INTEGER NJQS(3,MXNW,*)
      INTEGER NW
      INTEGER NWTAR
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NWTAR = NW
      NCFTAR = NCF
C
      IF (NW.GT.0) THEN
        DO I = 1,NCF
          DO J = 1,NW
            NICHOP(J,I) = ICHOP(J,I)
            IQTAR(J,I) = IQ(J,I)
            DO K = 1,3
              NJQS(K,J,I) = JQS(K,J,I)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
      DO I = 1,NCF
        NITJPO(I) = ITJPO(I)
        NISPAR(I) = ISPAR(I)
        DO K = 1,10
          NJCUP(K,I) = JCUP(K,I)
        ENDDO
      ENDDO
C
      WRITE (IWRITE,3000)
C
 3000 FORMAT (/' ===== ANGLB1 called. Target data stored.')
      END
CEND--------------------------------------------------------------------
CEND    ANGLB2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGLB2(IWRITE,ICHOP, IQ, ISPAR, ITJPO, JCUP, JQS, NCF, !
     +NW,NCFCON, NCFGP,NICHOP, IQTAR, NISPAR, NITJPO, NJCUP, NJQS, NCFTA!
     +R, NWTAR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGLB2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE
C   ICHOP
C   IQ
C   ISPAR
C   ITJPO
C   JCUP
C   JQS
C   NCF
C   NW
C   NCFCON
C   NCFGP
C   NICHOP
C   IQTAR
C   NISPAR
C   NITJPO
C   NJCUP
C   NJQS
C   NCFTAR
C   NWTAR
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Local variables
C
      INTEGER I,I1,I2,II
      INTEGER IQTMP(MXNW,MXNC),J,K
C
C  Argument variables
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER IQTAR(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCFTAR
      INTEGER NICHOP(MXNW,*)
      INTEGER NISPAR(*)
      INTEGER NITJPO(*)
      INTEGER NJCUP(10,*)
      INTEGER NJQS(3,MXNW,*)
      INTEGER NW
      INTEGER NWTAR
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NW = NWTAR
      NCF = NCFTAR
C
C  reposition correlation values of IQ
C
      IF (NCFGP.GT.0) THEN
        I1 = NCF+1
        I2 = NCF+NCFGP
        II = NCFCON+1
        DO I = I1,I2
          DO J = 1,NW
            IQTMP(J,I) = IQ(J,II)
          ENDDO
          II = II+1
        ENDDO
        DO I = I1,I2
          DO J = 1,NW
            IQ(J,I) = IQTMP(J,I)
          ENDDO
        ENDDO
      ENDIF
C
      IF (NW.GT.0) THEN
        DO I = 1,NCF
          DO J = 1,NW
            ICHOP(J,I) = NICHOP(J,I)
            IQ(J,I) = IQTAR(J,I)
            DO K = 1,3
              JQS(K,J,I) = NJQS(K,J,I)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
      DO I = 1,NCF
        ITJPO(I) = NITJPO(I)
        ISPAR(I) = NISPAR(I)
        DO K = 1,10
          JCUP(K,I) = NJCUP(K,I)
        ENDDO
      ENDDO
C
      WRITE (IWRITE,3000)
C
 3000 FORMAT (/' ===== ANGLB2 called. Target data restored.')
      END
CEND--------------------------------------------------------------------
CEND    ANGLD1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGLD1(ICFCON, ICFCOR, ICHOP, IICHOP, IIQ, IISPAR, IITJ!
     +PO, IJ2P1,IJCUP, IJQS, INAK, INAKK, INCCP, INCF, INCHAN, INH, INTA!
     +RG,IQ, ISPAR, ITJPO, IWRITE, J2P1, JCUP, JQS, NAK, NAKK,NCCP, NCF,!
     + NCFCON, NCFGP, NCFTAR, NCHAN, NH, NTARG, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGLD1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ICFCON
C   ICFCOR
C   ICHOP
C   IICHOP
C   IIQ
C   IISPAR
C   IITJPO
C   IJ2P1
C   IJCUP
C   IJQS
C   INAK
C   INAKK
C   INCCP
C   INCF
C   INCHAN
C   INH
C   INTARG
C   IQ
C   ISPAR
C   ITJPO
C   IWRITE
C   J2P1
C   JCUP
C   JQS
C   NAK
C   NAKK
C   NCCP
C   NCF
C   NCFCON
C   NCFGP
C   NCFTAR
C   NCHAN
C   NH
C   NTARG
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Local variables
C
      INTEGER I,J,K,L
      INTEGER N
C
C  Argument variables
C
      CHARACTER*2 INH(*)
      CHARACTER*2 NH(*)
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER ICHOP(MXNW,*)
      INTEGER IICHOP(MXNW,*)
      INTEGER IIQ(MXNW,*)
      INTEGER IISPAR(*)
      INTEGER IITJPO(*)
      INTEGER IJ2P1
      INTEGER IJCUP(10,*)
      INTEGER IJQS(3,MXNW,*)
      INTEGER INAK(*)
      INTEGER INAKK(*)
      INTEGER INCCP(MXNL,*)
      INTEGER INCF
      INTEGER INCHAN
      INTEGER INTARG(*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NCCP(MXNL,*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCFTAR
      INTEGER NCHAN
      INTEGER NTARG(*)
      INTEGER NW
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      INCHAN = NCHAN
      IJ2P1 = J2P1
      ICFCOR = NCFGP
      ICFCON = NCFCON
      INCF = NCF
C
      DO J = 1,NW
        INH(J) = NH(J)
        INAK(J) = NAK(J)
      ENDDO
C
      DO I = 1,NCF
        DO J = 1,NW
          IICHOP(J,I) = ICHOP(J,I)
          IIQ(J,I) = IQ(J,I)
          DO K = 1,3
            IJQS(K,J,I) = JQS(K,J,I)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,NCF
        IITJPO(I) = ITJPO(I)
        IISPAR(I) = ISPAR(I)
        DO K = 1,10
          IJCUP(K,I) = JCUP(K,I)
        ENDDO
      ENDDO
C
      DO N = 1,NCHAN
        INTARG(N) = NTARG(N)
        INAKK(N) = NAKK(N)
      ENDDO
C
      DO N = 1,NCHAN
        DO L = 1,NCFTAR
          INCCP(L,N) = NCCP(L,N)
        ENDDO
      ENDDO
C
      WRITE (IWRITE,3000)
C
 3000 FORMAT (/                                                         !
     +' ===== ANGLD1 called. Initial (N+1)-electron data stored.')
      END
CEND--------------------------------------------------------------------
CEND    ANGULA.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGULA(IDISC1,IDISC3,IBUG1, IBUG2, IBUG3, IBUG4, IBUG6,!
     + ICHOP, IQ, ISPAR, ITAB, ITC,ITJPO, IWRITE, J2P1, JCUP, JLAST, JQS!
     +, JTAB, K2P, NAK,NCASES, NCCK, NCCT, NCF, NCFCON, NH, NP, NTAB, NW!
     +)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGULA.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG6
C   ICHOP
C   IQ
C   ISPAR
C   ITAB
C   ITC
C   ITJPO
C   IWRITE
C   J2P1
C   JCUP
C   JLAST
C   JQS
C   JTAB
C   K2P
C   NAK
C   NCASES
C   NCCK
C   NCCT
C   NCF
C   NCFCON
C   NH
C   NP
C   NTAB
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IDISC1,IDISC3
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JCUP(10,*)
      INTEGER JLAST
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER K2P(*)
      INTEGER NAK(*)
      INTEGER NCASES
      INTEGER NCCK(*)
      INTEGER NCCT(*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NP(*)
      INTEGER NTAB(*)
      INTEGER NW
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (NCASES.EQ.1) CALL AIJ1(IDISC3,IBUG1, IBUG2, IBUG3, IBUG4, IBUG!
     +6,ICHOP, IQ, ISPAR, ITC, ITJPO, IWRITE, JCUP, JQS,NAK, NCF, NH, NP!
     +, NW,ITAB, JTAB, NTAB)
      IF (J2P1.NE.JLAST) CALL AIJ2(IDISC1,IDISC3,IBUG1, ITC, ITJPO, IWRI!
     +TE, J2P1, K2P, NCCK, NCCT,NCFCON, NH, NP)
      END
CEND--------------------------------------------------------------------
CEND    ANGULD.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGULD(IDISC1,IBUG1, IBUG2, IBUG3, IBUG4, IBUG6,ICFCON,!
     + ICFCOR, ICHOP, IDMTST, IICHOP, IIQ, IISPAR, IITJPO,IJCUP, IJQS, I!
     +NAK, INCF, INCHAN, INH, IQ, ISPAR, ITAB, ITC,ITJPO, IWRITE, JCUP, !
     +JQS, JTAB, NAK, NCF, NCFCON, NCFGP,NCHAN, NDIMAX, NH, NP, NTAB, NW!
     +, NWTAR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGULD.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   WARNING
C   The value of COEFX may be wrong when K1=K2 (diagonal case)
C   but this case should not arise in the calculation anyway.
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG6
C   ICFCON
C   ICFCOR
C   ICHOP
C   IDMTST
C   IICHOP
C   IIQ
C   IISPAR
C   IITJPO
C   IJCUP
C   IJQS
C   INAK
C   INCF
C   INCHAN
C   INH
C   IQ
C   ISPAR
C   ITAB
C   ITC
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   JTAB
C   NAK
C   NCF
C   NCFCON
C   NCFGP
C   NCHAN
C   NDIMAX
C   NH
C   NP
C   NTAB
C   NW
C   NWTAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL CRE
      DOUBLE PRECISION CRE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      INTEGER IZ
      PARAMETER (IZ=0)
C
C  Argument variables
C
      INTEGER IDISC1
C
      CHARACTER*2 INH(*)
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG6
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IICHOP(MXNW,*)
      INTEGER IIQ(MXNW,*)
      INTEGER IISPAR(*)
      INTEGER IITJPO(*)
      INTEGER IJCUP(10,*)
      INTEGER IJQS(3,MXNW,*)
      INTEGER INAK(*)
      INTEGER INCF
      INTEGER INCHAN
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(*)
      INTEGER NP(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWTAR
C
C  Local variables
C
      DOUBLE PRECISION COEF,COEFX,COEFY
      DOUBLE PRECISION VSHELL(MXNW)
      INTEGER I,IA1,IA2,IA2S
      INTEGER IMF,IMT,IN,IOPAR
      INTEGER IX,J,JA,JB
      INTEGER JBS,JX,K,K1
      INTEGER K2,KA
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020)
C
C  set up the arrays for evaluating the angular coefficients
C
      CALL DMCHK2(12, NW + INCHAN, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK2(07, NCF + INCF, IWRITE, IDMTST, NDIMAX)
C
      IX = NWTAR
      IN = NP(NW)
      DO I = NW+1,NW+INCHAN
        IX = IX+1
        IN = IN+1
        NAK(I) = INAK(IX)
        NH(I) = INH(IX)
        NP(I) = IN
      ENDDO
C
      DO I = 1,NCF
        DO J = NW+1,NW+INCHAN
          IQ(J,I) = 0
          JQS(1,J,I) = 0
          JQS(2,J,I) = 0
          JQS(3,J,I) = 1
          ICHOP(J,I) = -1
        ENDDO
      ENDDO
C
      IX = 0
      DO I = NCF+1,NCF+INCF
        IX = IX+1
        ITJPO(I) = IITJPO(IX)
        ISPAR(I) = IISPAR(IX)
        DO K = 1,10
          JCUP(K,I) = IJCUP(K,IX)
        ENDDO
C
        DO J = 1,NWTAR
          IQ(J,I) = IIQ(J,IX)
          DO K = 1,3
            JQS(K,J,I) = IJQS(K,J,IX)
          ENDDO
          ICHOP(J,I) = IICHOP(J,IX)
        ENDDO
C
        DO J = NWTAR+1,NW
          IQ(J,I) = 0
          JQS(1,J,I) = 0
          JQS(2,J,I) = 0
          JQS(3,J,I) = 1
          ICHOP(J,I) = -1
        ENDDO
C
        JX = NWTAR
        DO J = NW+1,NW+INCHAN
          JX = JX+1
          IQ(J,I) = IIQ(JX,IX)
          DO K = 1,3
            JQS(K,J,I) = IJQS(K,JX,IX)
          ENDDO
          ICHOP(J,I) = IICHOP(JX,IX)
        ENDDO
C
      ENDDO
C
C   reset NW and NCF
C
      NW = NW+INCHAN
      NCF = NCF+INCF
C
      IF (ITC(9).EQ.1) THEN
        CALL CFOUT(IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS, NAK, NCF, NH, NP!
     +, NW)
      ENDIF
C
C   calculate the angular coefficients
C
      KA = 1
      IOPAR = -1
      IF (IBUG1.EQ.1) WRITE (IWRITE,3010)
      IMT = 0
C
C   continuum-continuum terms
C
      DO JA = 1,NCFCON
        DO JB = NCFCON+NCFGP+1,NCFCON+NCFGP+ICFCON
          IMF = 0
          CALL TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG4,!
     + IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,IT!
     +AB, JTAB, NTAB)
          IF (IA1.NE.0) THEN
            IF (IA1.NE.IA2) THEN
              COEF = VSHELL(1)
              IF (ABS(COEF).GT.EPS10) THEN
                K1 = NAK(IA1)
                K2 = NAK(IA2)
                COEFX = CRE(K1,KA,K2)/SQRT(ABS(DBLE(2*K1)))
                COEFY = COEFX*COEF
                IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH(IA1!
     +),NP(IA2),NH(IA2),COEF,COEFX,COEFY
                JBS = JB-NCFCON-NCFGP
                IA2S = IA2-NCHAN
                WRITE (IDISC1) JA,JBS,IA1,IA2S,COEFY
                IMF = IMF+1
              ENDIF
            ELSE
              DO IA1 = 1,NW
                COEF = VSHELL(IA1)
                IF (ABS(COEF).GT.EPS10) THEN
                  K1 = NAK(IA1)
                  COEFX = CRE(K1,KA,K1)
                  COEFY = COEFX*COEF
                  IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH(I!
     +A1),NP(IA1),NH(IA1),COEF,COEFX,COEFY
                  JBS = JB-NCFCON-NCFGP
                  WRITE (IDISC1) JA,JBS,IA1,IA1,COEFY
                  IMF = IMF+1
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          IMT = IMT+IMF
        ENDDO
      ENDDO
C
C   bound-continuum terms
C
      IF (NCFGP.GT.0) THEN
        DO JA = NCFCON+1,NCFCON+NCFGP
          DO JB = NCFCON+NCFGP+1,NCFCON+NCFGP+ICFCON
            IMF = 0
            CALL TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG!
     +4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,!
     +ITAB, JTAB, NTAB)
            IF (IA1.NE.0) THEN
              IF (IA1.NE.IA2) THEN
                COEF = VSHELL(1)
                IF (ABS(COEF).GT.EPS10) THEN
                  K1 = NAK(IA1)
                  K2 = NAK(IA2)
                  COEFX = CRE(K1,KA,K2)/SQRT(ABS(DBLE(2*K1)))
                  COEFY = COEFX*COEF
                  IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH(I!
     +A1),NP(IA2),NH(IA2),COEF,COEFX,COEFY
                  JBS = JB-NCFCON-NCFGP
                  IA2S = IA2-NCHAN
                  WRITE (IDISC1) JA,JBS,IA1,IA2S,COEFY
                  IMF = IMF+1
                ENDIF
              ELSE
                DO IA1 = 1,NW
                  COEF = VSHELL(IA1)
                  IF (ABS(COEF).GT.EPS10) THEN
                    K1 = NAK(IA1)
                    COEFX = CRE(K1,KA,K1)
                    COEFY = COEFX*COEF
                    IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH!
     +(IA1),NP(IA1),NH(IA1),COEF,COEFX,COEFY
                    JBS = JB-NCFCON-NCFGP
                    WRITE (IDISC1) JA,JBS,IA1,IA1,COEFY
                    IMF = IMF+1
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            IMT = IMT+IMF
          ENDDO
        ENDDO
      ENDIF
C
C   continuum-bound terms
C
      IF (ICFCOR.GT.0) THEN
        DO JA = 1,NCFCON
          DO JB = NCFCON+NCFGP+ICFCON+1,NCFCON+NCFGP+ICFCON+ICFCOR
            IMF = 0
            CALL TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG!
     +4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,!
     +ITAB, JTAB, NTAB)
            IF (IA1.NE.0) THEN
              IF (IA1.NE.IA2) THEN
                COEF = VSHELL(1)
                IF (ABS(COEF).GT.EPS10) THEN
                  K1 = NAK(IA1)
                  K2 = NAK(IA2)
                  COEFX = CRE(K1,KA,K2)/SQRT(ABS(DBLE(2*K1)))
                  COEFY = COEFX*COEF
                  IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH(I!
     +A1),NP(IA2),NH(IA2),COEF,COEFX,COEFY
                  JBS = JB-NCFCON-NCFGP
                  WRITE (IDISC1) JA,JBS,IA1,IA2,COEFY
                  IMF = IMF+1
                ENDIF
              ELSE
                DO IA1 = 1,NW
                  COEF = VSHELL(IA1)
                  IF (ABS(COEF).GT.EPS10) THEN
                    K1 = NAK(IA1)
                    COEFX = CRE(K1,KA,K1)
                    COEFY = COEFX*COEF
                    IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH!
     +(IA1),NP(IA1),NH(IA1),COEF,COEFX,COEFY
                    JBS = JB-NCFCON-NCFGP
                    WRITE (IDISC1) JA,JBS,IA1,IA1,COEFY
                    IMF = IMF+1
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            IMT = IMT+IMF
          ENDDO
        ENDDO
      ENDIF
C
C   bound-bound terms
C
      IF (NCFGP.GT.0 .AND. ICFCOR.GT.0) THEN
        DO JA = NCFCON+1,NCFCON+NCFGP
          DO JB = NCFCON+NCFGP+ICFCON+1,NCFCON+NCFGP+ICFCON+ICFCOR
            IMF = 0
            CALL TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG!
     +4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,!
     +ITAB, JTAB, NTAB)
            IF (IA1.NE.0) THEN
              IF (IA1.NE.IA2) THEN
                COEF = VSHELL(1)
                IF (ABS(COEF).GT.EPS10) THEN
                  K1 = NAK(IA1)
                  K2 = NAK(IA2)
                  COEFX = CRE(K1,KA,K2)/SQRT(ABS(DBLE(2*K1)))
                  COEFY = COEFX*COEF
                  IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH(I!
     +A1),NP(IA2),NH(IA2),COEF,COEFX,COEFY
                  JBS = JB-NCFCON-NCFGP
                  WRITE (IDISC1) JA,JBS,IA1,IA2,COEFY
                  IMF = IMF+1
                ENDIF
              ELSE
                DO IA1 = 1,NW
                  COEF = VSHELL(IA1)
                  IF (ABS(COEF).GT.EPS10) THEN
                    K1 = NAK(IA1)
                    COEFX = CRE(K1,KA,K1)
                    COEFY = COEFX*COEF
                    IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA1),NH!
     +(IA1),NP(IA1),NH(IA1),COEF,COEFX,COEFY
                    JBS = JB-NCFCON-NCFGP
                    WRITE (IDISC1) JA,JBS,IA1,IA1,COEFY
                    IMF = IMF+1
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            IMT = IMT+IMF
          ENDDO
        ENDDO
      ENDIF
C
      WRITE (IWRITE,3030) IMT
C
      WRITE (IDISC1) IZ,IZ,IZ,IZ,EPS10
C
C   reset NCF and NW to the values when this routine was called
C
      NW = NW-INCHAN
      NCF = NCF-INCF
C
 3000 FORMAT (2X,2I6,2X,2(I2,A2,1X),5X,1P,3E12.4)
 3010 FORMAT (/7X,'r',5X,'s',4X,'a',4X,'b',6X,'coefficients'/)
 3020 FORMAT (/                                                         !
     +' ===== ANGULD called. Evaluate photo-ionisation angular coeffic',!
     +'ients.')
 3030 FORMAT (/1X,I8,' P-coefficients calculated')
      END
CEND--------------------------------------------------------------------
CEND    ANGULH.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ANGULH(IDISC1,IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6,!
     + ICHOP, IDMTST, IEXC,IQ, ISPAR, ITAB, ITC, ITJPO, IWRITE, J2P1, JC!
     +UP, JLASTX,JQS, JTAB, K2P, NAK, NCCK, NCCT, NCF, NCFCON, NCFGP, NC!
     +HAN,NDIMAX, NH, NHC, NP, NPTY, NROWS, NTAB, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ANGULH.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICHOP
C   IDMTST
C   IEXC
C   IQ
C   ISPAR
C   ITAB
C   ITC
C   ITJPO
C   IWRITE
C   J2P1
C   JCUP
C   JLASTX
C   JQS
C   JTAB
C   K2P
C   NAK
C   NCCK
C   NCCT
C   NCF
C   NCFCON
C   NCFGP
C   NCHAN
C   NDIMAX
C   NH
C   NHC
C   NP
C   NPTY
C   NROWS
C   NTAB
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IDISC1
C
      CHARACTER*2 NH(*)
      CHARACTER*2 NHC(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IEXC
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JCUP(10,*)
      INTEGER JLASTX
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER K2P(*)
      INTEGER NAK(*)
      INTEGER NCCK(*)
      INTEGER NCCT(*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(*)
      INTEGER NP(*)
      INTEGER NPTY
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      INTEGER I,I1,I2,I3
      INTEGER I4,I5,I6,IN
      INTEGER ISMCP,ITEMP(3,MXNW,MXNC),IX
      INTEGER J,JT,K,NOPEN
      INTEGER NRECUP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   set up arrays for evaluating continuum MCP coefficients
C
C-----------------------------------------------------------------------
      I1 = NW+1
      I2 = NW+NCHAN
      CALL DMCHK2(12,I2, IWRITE, IDMTST, NDIMAX)
      I3 = NCFCON+1
      I4 = NCFCON+NCFGP
      CALL DMCHK2(07,I4, IWRITE, IDMTST, NDIMAX)
      I5 = NCF+1
      I6 = NCF+NCFGP
C
      IX = I1
      IN = 20
      DO J = 1,NCHAN
        IN = IN+1
        NAK(IX) = K2P(J)
        NH(IX) = NHC(J)
        NP(IX) = IN
        IX = IX+1
      ENDDO
C-----------------------------------------------------------------------
C
C   ICHOP array
C
C-----------------------------------------------------------------------
      DO J = 1,NCF
        DO I = 1,NW
          ITEMP(1,I,J) = ICHOP(I,J)
        ENDDO
      ENDDO
C
      IF (NCFGP.GT.0) THEN
        DO J = I5,I6
          DO I = 1,NW
            ITEMP(1,I,J) = ICHOP(I,J)
          ENDDO
        ENDDO
        IX = I3
        DO J = I5,I6
          DO I = 1,NW
            ICHOP(I,IX) = ITEMP(1,I,J)
          ENDDO
          IX = IX+1
        ENDDO
        DO J = I3,I4
          DO I = I1,I2
            ICHOP(I,J) = -1
          ENDDO
        ENDDO
      ENDIF
C
      DO J = 1,NCFCON
        JT = NCCT(J)
        DO I = 1,NW
          ICHOP(I,J) = ITEMP(1,I,JT)
        ENDDO
        DO I = I1,I2
          ICHOP(I,J) = -1
        ENDDO
        IX = I1-1+NCCK(J)
        ICHOP(IX,J) = 0
      ENDDO
C-----------------------------------------------------------------------
C
C   JQS array
C
C-----------------------------------------------------------------------
      DO K = 1,3
        DO J = 1,NCF
          DO I = 1,NW
            ITEMP(K,I,J) = JQS(K,I,J)
          ENDDO
        ENDDO
      ENDDO
C
      IF (NCFGP.GT.0) THEN
C
        DO K = 1,3
          DO J = I5,I6
            DO I = 1,NW
              ITEMP(K,I,J) = JQS(K,I,J)
            ENDDO
          ENDDO
        ENDDO
C
        DO K = 1,3
          IX = I3
          DO J = I5,I6
            DO I = 1,NW
              JQS(K,I,IX) = ITEMP(K,I,J)
            ENDDO
            IX = IX+1
          ENDDO
        ENDDO
C
        DO J = I3,I4
          DO I = I1,I2
            JQS(1,I,J) = 0
            JQS(2,I,J) = 0
            JQS(3,I,J) = 1
          ENDDO
        ENDDO
C
      ENDIF
C
      DO K = 1,3
        DO J = 1,NCFCON
          JT = NCCT(J)
          DO I = 1,NW
            JQS(K,I,J) = ITEMP(K,I,JT)
          ENDDO
        ENDDO
      ENDDO
C
      DO J = 1,NCFCON
        DO I = I1,I2
          JQS(1,I,J) = 0
          JQS(2,I,J) = 0
          JQS(3,I,J) = 1
        ENDDO
        IX = I1-1+NCCK(J)
        JQS(1,IX,J) = 1
        JQS(3,IX,J) = 2*ABS(NAK(IX))
      ENDDO
C-----------------------------------------------------------------------
C
C   JCUP array
C
C-----------------------------------------------------------------------
      DO J = 1,NCF
        DO I = 1,10
          ITEMP(1,I,J) = JCUP(I,J)
        ENDDO
      ENDDO
C
      IF (NCFGP.GT.0) THEN
        DO J = I5,I6
          DO I = 1,10
            ITEMP(1,I,J) = JCUP(I,J)
          ENDDO
        ENDDO
        IX = I3
        DO J = I5,I6
          DO I = 1,10
            JCUP(I,IX) = ITEMP(1,I,J)
          ENDDO
          IX = IX+1
        ENDDO
      ENDIF
C
      DO J = 1,NCFCON
C
        JT = NCCT(J)
C
        NOPEN = 0
        DO I = 1,NW
          IF (ICHOP(I,J).EQ.0) NOPEN = NOPEN + 1
        ENDDO
C
        IF (NOPEN.GT.0) THEN
          NRECUP = NOPEN-1
          IF (NRECUP.GT.0) THEN
            DO I = 1,NRECUP
              JCUP(I,J) = ITEMP(1,I,JT)
            ENDDO
          ENDIF
          JCUP(NRECUP+1,J) = J2P1
        ENDIF
C
      ENDDO
C-----------------------------------------------------------------------
C
C   IQ , ITJPO and ISPAR arrays
C
C-----------------------------------------------------------------------
      DO J = 1,NCF
        DO I = 1,NW
          ITEMP(1,I,J) = IQ(I,J)
        ENDDO
      ENDDO
C
      IF (NCFGP.GT.0) THEN
        DO J = I5,I6
          DO I = 1,NW
            ITEMP(1,I,J) = IQ(I,J)
          ENDDO
        ENDDO
        IX = I3
        DO J = I5,I6
          DO I = 1,NW
            IQ(I,IX) = ITEMP(1,I,J)
          ENDDO
          IX = IX+1
        ENDDO
        DO J = I3,I4
          ITJPO(J) = J2P1
          ISPAR(J) = NPTY
        ENDDO
        DO J = I3,I4
          DO I = I1,I2
            IQ(I,J) = 0
          ENDDO
        ENDDO
      ENDIF
C
      DO J = 1,NCFCON
        ITJPO(J) = J2P1
        ISPAR(J) = NPTY
        JT = NCCT(J)
        DO I = 1,NW
          IQ(I,J) = ITEMP(1,I,JT)
        ENDDO
        DO I = I1,I2
          IQ(I,J) = 0
        ENDDO
        IX = I1-1+NCCK(J)
        IQ(IX,J) = 1
      ENDDO
C
      NW = I2
      NCF = I4
      WRITE (IWRITE,3000)
C-----------------------------------------------------------------------
C
C  evaluate the continuum MCP coefficients
C
C-----------------------------------------------------------------------
      IF (ITC(8).EQ.1) THEN
        WRITE (IWRITE,3010)
        CALL CFOUT(IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS, NAK, NCF, NH, NP!
     +, NW)
      ENDIF
C-----------------------------------------------------------------------
C
C   IEXC =  0  all angular integrals are calculated in SMCP
C   IEXC =  1  no exchange Slater integrals are calculated
C   IEXC = -1  no direct Slater integrals are calculated
C
C   New code which allows reuse of angular coefficients.
C
C   When option 49 is not set (default) then no direct angular
C     integrals are calculated when J-values are the same
C     and there are no correlation functions (IEXC=-1).
C
C   When option 1 is set and there are no correlation functions
C     then only the direct angular integrals are calculated.
C     In this case if no direct integrals are to be calculated
C     (IEXC=-1) then SMCP is entered with ISMCP=0 and only 1-electron
C     integrals are calculated.
C
C-----------------------------------------------------------------------
      IEXC = 0
      ISMCP = 1
C
      IF (ITC(49).EQ.0 .AND. NCFGP.EQ.0 .AND. J2P1.EQ.JLASTX) IEXC = -1
C
      IF (ITC(1).EQ.1 .AND. NCFGP.EQ.0) THEN
        IF (IEXC.EQ.-1) THEN
          ISMCP = 0
        ELSE
          IEXC = 1
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      CALL SMCP(IDISC1,ISMCP,IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6,IC!
     +HOP, IEXC, IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS,NAK, NCF, NH, NP, N!
     +W,ITAB, JTAB, NROWS, NTAB)
C-----------------------------------------------------------------------
 3000 FORMAT (/' ===== ANGULH called.'/                                 !
     +' ===== Evaluate continuum Hamiltonian angular coefficients.'/)
 3010 FORMAT (/' continuum CSFs'/' --------------')
      END
CEND--------------------------------------------------------------------
CEND    BLDST2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      BLOCK DATA BLDST2
CRCS
CRCS $Source: /home/phn/DARC/RCS/BLDST2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Common variables
C
      INTEGER ITAB(16),JTAB(16),NROWS
      INTEGER NTAB(255)
      COMMON / TERMS  / NROWS,ITAB,JTAB,NTAB
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NROWS/16/
      DATA ITAB/1,1,1,2,1,3,3,1,4,6,8,1,5,10,18,20/
      DATA JTAB/0,3,6,9,15,18,27,36,39,51,69,93,96,111,141,195/
      DATA NTAB/0,0,1,1,0,2,1,0,4,0,0,1,2,0,5,1,0,6,0,0,1,2,0,5,2,0,9,1,!
     +0,6,3,0,4,3,0,10,1,0,8,0,0,1,2,0,5,2,0,9,2,0,13,1,0,8,3,0,4,3,0,6,!
     +3,0,10,3,0,12,3,0,16,0,0,1,2,0,5,2,0,9,2,0,13,4,0,5,4,0,9,4,0,11,4!
     +,0,17,1,0,10,0,0,1,2,0,5,2,0,9,2,0,13,2,0,17,1,0,10,3,0,4,3,0,6,3,!
     +0,8,3,0,10,3,0,12,3,0,14,3,0,16,3,0,18,3,0,22,0,0,1,2,0,5,2,0,9,2,!
     +0,13,2,0,17,4,0,1,4,0,5,4,0,7,4,0,9,4,1,9,4,0,11,4,0,13,4,1,13,4,0!
     +,15,4,0,17,4,0,19,4,0,21,4,0,25,1,0,10,3,0,4,3,0,6,3,0,8,3,0,10,3,!
     +0,12,3,0,14,3,0,16,3,0,18,3,0,22,5,0,2,5,0,6,5,0,8,5,0,10,5,0,12,5!
     +,0,14,5,0,16,5,0,18,5,0,20,5,0,26/
C
      END
CEND--------------------------------------------------------------------
CEND    BMAT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE BMAT(
     + IDISC1, ITAPE2, EMTB, EMTR, ENAT, FACTAN, FACTCM, FACTEV, IBUG1,
     + IBUG2, IBUG3, IBUG4, IBUG5, IBUG6, ICHOP, IDMTST, IEXC,
     + IQ, IRK1, IRK2, ISPAR, ITC, ITJPO, IWRITE, JCUP, JQS, KBMAX,
     + MAXFUL, MAXNQN, MINNQN, NAK, NAST, NCF, NDIMAX, NH, NP, NW, NW1,
     + ICTXB, ISTX1B, ISTX2B, ISTXB, KBBN1, KBBN2, KBBR, K1S, K2S, ITAB,
     + JTAB, NROWS, NTAB, RKSTO, ISLDR, JSLDR, NMCP, NNLDR, NSLDF,
     + XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/BMAT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IDISC1
C   ITAPE2
C   EMTB
C   EMTR
C   ENAT
C   FACTAN
C   FACTCM
C   FACTEV
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICHOP
C   IDMTST
C   IEXC
C   IQ
C   IRK1
C   IRK2
C   ISPAR
C   ITC
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   KBMAX
C   MAXFUL
C   MAXNQN
C   MINNQN
C   NAK
C   NAST
C   NCF
C   NDIMAX
C   NH
C   NP
C   NW
C   NW1
C   ICTXB
C   ISTX1B
C   ISTX2B
C   ISTXB
C   KBBN1
C   KBBN2
C   KBBR
C   K1S
C   K2S
C   ITAB
C   JTAB
C   NROWS
C   NTAB
C   RKSTO
C   ISLDR
C   JSLDR
C   NMCP
C   NNLDR
C   NSLDF
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER LWORK
      PARAMETER (LWORK=MXNL*3-1)
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      INTEGER IDISC1,ITAPE2
      CHARACTER*2 NH(*)
      DOUBLE PRECISION EMTB(MXNL,*)
      DOUBLE PRECISION EMTR(MXNL,*)
      DOUBLE PRECISION ENAT(*)
      DOUBLE PRECISION FACTAN
      DOUBLE PRECISION FACTCM
      DOUBLE PRECISION FACTEV
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IEXC
      INTEGER IQ(MXNW,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISLDR(*)
      INTEGER ISPAR(*)
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JSLDR(*)
      INTEGER JTAB(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBMAX
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAST
      INTEGER NCF
      INTEGER NDIMAX(*)
      INTEGER NMCP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NROWS
      INTEGER NSLDF(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NW1
C
C  Local variables
C
      DOUBLE PRECISION AMAX,EIG(MXNL),WORK(LWORK)
      DOUBLE PRECISION XMT(MXNL,MXNL)
      INTEGER I,IA,INFO,ISMCP
      INTEGER J,JAT(MXNL)
      INTEGER ARG1,ARG2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020)
C
C   print the target CSFs if option 7 is set
C
      IF (ITC(7).EQ.1) THEN
        WRITE (IWRITE,3070)
        CALL CFOUT(
     +  IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS, NAK, NCF, NH, NP, NW)
      ENDIF
C
C   evaluate angular coefficients for the target Hamiltonian
C
      REWIND IDISC1
      WRITE (IWRITE,3060)
      IEXC = 0
      ISMCP = 1
      CALL SMCP(
     + IDISC1, ISMCP, IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6, ICHOP,
     + IEXC, IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS, NAK, NCF, NH, NP,
     + NW, ITAB, JTAB, NROWS, NTAB)
      REWIND IDISC1
C
C   locate the b-b integral block
C
      CALL LOCBB(
     + 0, 0, K1S, K2S, RKSTO, ICTXB, IRK1, IRK2, ISTX1B, ISTX2B,
     + ISTXB, ITC, IWRITE, KBBN1, KBBN2, KBBR, KBMAX)
C
C   read in the angular coefficients
C
      NW1 = NW+1
      CALL MCPINB(
     + IDISC1, NCF, IDMTST, ISLDR, ITC, IWRITE, JSLDR, NDIMAX,
     + NH, NMCP, NNLDR, NP, NSLDF, NW1, XSLDR)
C
C   evaluate the Hamiltonian matrix
C
      CALL BMATX(
     + XMT, 1, NCF, 0, ICTXB, IQ, IRK1, ISLDR, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, JSLDR, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH,
     + NMCP, NNLDR, NP, NSLDF, NW, NW1, RKSTO, XSLDR)
C
C   print Hamiltonian matrix
C
      WRITE (IWRITE,3030)
      IF (ITC(11).EQ.1) CALL MATOUT(IWRITE,XMT,NCF,NCF,MXNL,MXNL,1)
C
C   EMTB contains the target Hamiltonian
C
      DO I = 1,NCF
        DO J = I,NCF
          EMTB(I,J) = XMT(I,J)
        ENDDO
      ENDDO
C
C   diagonalise the Hamiltonian
C
C   use a LAPACK routine
C
      WRITE (IWRITE,3000)
      CALL DSYEV('V','U',NCF,XMT,MXNL,EIG,WORK,LWORK,INFO)
      IF (INFO.NE.0) THEN
        WRITE (IWRITE,3010)
        STOP
      ENDIF
C
      DO I = 1,NCF
        DO J = 1,NCF
          IF (ABS(XMT(I,J)).LT.EPS10) XMT(I,J) = ZERO
        ENDDO
      ENDDO
C
C   write out the eigenvalues and eigenvectors
C
      WRITE (IWRITE,3050)
      ARG1 = NCF
      ARG2 = NCF
      CALL PROP(
     + EIG, XMT, FACTAN, FACTCM, FACTEV, ISPAR, ITC, ITJPO,
     + IWRITE, ARG1, ARG2)
C
C   ENAT contains the target energies
C
      DO I = 1,NAST
        ENAT(I) = EIG(I)
      ENDDO
C
C   EMTR contains the target eigenvectors
C
      DO J = 1,NAST
        DO I = 1,NCF
          EMTR(I,J) = XMT(I,J)
        ENDDO
      ENDDO
C
C   set up the array JAT containing 2J+1 values for the levels
C
      DO J = 1,NAST
C
        IA = 0
        AMAX = ZERO
C
        DO I = 1,NCF
          IF (ABS(EMTR(I,J)).GT.AMAX) THEN
            AMAX = ABS(EMTR(I,J))
            IA = I
          ENDIF
        ENDDO
C
        JAT(J) = ITJPO(IA)*ISPAR(IA)
C
        DO I = 1,NCF
          IF (ITJPO(IA).NE.ITJPO(I)) EMTR(I,J) = ZERO
        ENDDO
C
        IF (EMTR(IA,J).LT.ZERO) THEN
          DO I = 1,NCF
            EMTR(I,J) = -EMTR(I,J)
          ENDDO
        ENDIF
C
      ENDDO
C-----------------------------------------------------------------------
      WRITE (ITAPE2) NAST
      WRITE (ITAPE2) (JAT(I),I=1,NAST)
      WRITE (ITAPE2) (ENAT(I),I=1,NAST)
C
      WRITE (IWRITE,3040) NAST
C=======================================================================
 3000 FORMAT (/' diagonalisation using the DSYEV routine')
 3010 FORMAT (//' ERROR using the DSYEV routine'/' STOPPING')
 3020 FORMAT (/31X,'Routine BMAT'/31X,'------------')
 3030 FORMAT (/' ===== evaluate the bound Hamiltonian matrix'/)
 3040 FORMAT (/' ===== The first ',I5,
     +' levels are included in the problem'/)
 3050 FORMAT (/' ===== diagonalise the bound Hamiltonian'/
     +' ===== giving the following eigen-energies and eigenvectors'/)
 3060 FORMAT (/' ===== evaluate bound MCP coefficients'/)
 3070 FORMAT (/' ===== target CSFs'/)
      END
CEND--------------------------------------------------------------------
CEND    BMATX.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE BMATX(
     + XMT, IFST, ILST, JST, ICTXB, IQ, IRK1, ISLDR, ISTX1B, ISTX2B,
     + ISTXB, ITC, IWRITE, JSLDR, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK,
     + NH, NMCP, NNLDR, NP, NSLDF, NW, NW1, RKSTO, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/BMATX.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   XMT
C   IFST
C   ILST
C   JST
C   ICTXB
C   IQ
C   IRK1
C   ISLDR
C   ISTX1B
C   ISTX2B
C   ISTXB
C   ITC
C   IWRITE
C   JSLDR
C   KBMAX
C   MAXFUL
C   MAXNQN
C   MINNQN
C   NAK
C   NH
C   NMCP
C   NNLDR
C   NP
C   NSLDF
C   NW
C   NW1
C   RKSTO
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL CLRX
      DOUBLE PRECISION CLRX
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      INTEGER ND14
      PARAMETER (ND14=MXNC*(MXNC-1))
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      CHARACTER*2 NH(MXNW)
      DOUBLE PRECISION XMT(MXNL,MXNL)
      DOUBLE PRECISION RKSTO(MXI1)
      DOUBLE PRECISION XSLDR(MXNM)
      INTEGER ICTXB(NDX,NDX,NDX)
      INTEGER IFST
      INTEGER ILST
      INTEGER IQ(MXNW,MXNC)
      INTEGER IRK1
      INTEGER ISLDR(MXNM)
      INTEGER ISTX1B(MXI2)
      INTEGER ISTX2B(MXI2)
      INTEGER ISTXB(NDX)
      INTEGER ITC(50)
      INTEGER IWRITE
      INTEGER JSLDR(MXNM)
      INTEGER JST
      INTEGER KBMAX
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MINNQN(MXNK)
      INTEGER NAK(MXNW)
      INTEGER NMCP
      INTEGER NNLDR(ND14)
      INTEGER NP(MXNW)
      INTEGER NSLDF(ND14)
      INTEGER NW
      INTEGER NW1
C
C  Local variables
C
      DOUBLE PRECISION COEF,CONTR,ELMNT
      DOUBLE PRECISION SSS(MXNL),WA,WB
      INTEGER I,ICOEF,IFA,IFB
      INTEGER II,IL,ILCX,ILCY
      INTEGER ILDA,ILDB,ILDN,ILSTM
      INTEGER IQA,IQB,IRS,ITR
      INTEGER ITRP,J,JA,JB
      INTEGER JJ,JM,JTR,JX
      INTEGER K,KA,KB,KC
      INTEGER KD,KK,KMX,KX
      INTEGER L,LM,LMN,LMX
      INTEGER NTOT,NWM
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NTOT = ILST-IFST+1
C
      DO I = 1,NTOT
        DO J = I,NTOT
          XMT(I,J) = ZERO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C   I integrals
C
C-----------------------------------------------------------------------
      DO JA = 1,NW
        CALL FINBB(
     + WA, 1, JA, JA, 0, 0, 0, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB, ITC,
     + IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
        JJ = JST
        DO I = 1,NTOT
          JJ = JJ+1
          SSS(I) = DBLE(IQ(JA,JJ))
        ENDDO
        DO I = 1,NTOT
          XMT(I,I) = XMT(I,I)+SSS(I)*WA
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C   F integrals
C
C-----------------------------------------------------------------------
      DO JA = 1,NW
        KA = NAK(JA)
        IFA = 2*ABS(KA)
        DO JB = JA,NW
          IFB = 2*ABS(NAK(JB))
C
C   determine bounds in sum over K
C
          IF (IFA.GT.IFB) THEN
            KMX = IFB
          ELSE
            KMX = IFA
          ENDIF
C
          DO L = 1,KMX,2
            K = L-1
C
C   evaluate Slater integral
C
            CALL FINBB(
     + WA, 2, JA, JB, 0, 0, K, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
C
C   determine the angular coefficient
C
            ICOEF = 0
            II = 0
            JJ = JST
            DO ITR = IFST,ILST
              II = II+1
              JJ = JJ+1
              WB = ZERO
              IQA = IQ(JA,JJ)
              IQB = IQ(JB,JJ)
C
              IF (IQA.EQ.0 .OR. IQB.EQ.0) GOTO 10
C
C   if either orbital is in closed shell
C
              IF (IQA.EQ.IFA .OR. IQB.EQ.IFB) THEN
C
                IF (JA.EQ.JB) THEN
                  IF (K.EQ.0) THEN
                    WB = HALF*IQA*(IQA-1)
                  ELSE
                    IF (ICOEF.EQ.0) THEN
                      COEF = CLRX(KA,K,KA)**2
                      ICOEF = 1
                    ENDIF
                    WB = -HALF*IQA*IQA*COEF
                  ENDIF
                ELSE
                  IF (K.EQ.0) THEN
                    WB = IQA*IQB
                  ENDIF
                ENDIF
C
C   if both are in open shells
C
              ELSE
C
                IF (NMCP.EQ.0) GOTO 10
                IRS = (ITR-1)*(ILST+ILST-ITR)/2+ITR
                ILDN = NNLDR(IRS)
                IF (ILDN.LE.0) GOTO 10
                ILDA = NSLDF(IRS)
                ILDB = ILDA+ILDN-1
                ILCX = (K*NW1+JA)*NW1+JB
                ILCY = JA*NW1+JB
                DO IL = ILDA,ILDB
                  IF (ILCX.EQ.ISLDR(IL) .AND. ILCY.EQ.JSLDR(IL)) THEN
                    WB = XSLDR(IL)
                    GOTO 10
                  ENDIF
                ENDDO
C
              ENDIF
C
   10         CONTINUE
              SSS(II) = WB
            ENDDO
C
C   add to diagonal elements
C
            DO I = 1,NTOT
              XMT(I,I) = XMT(I,I)+SSS(I)*WA
            ENDDO
C
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C   G integrals
C
C-----------------------------------------------------------------------
      IF (NW.EQ.1) GOTO 30
C-----------------------------------------------------------------------
      NWM = NW-1
      DO JA = 1,NWM
        JM = JA+1
        KA = NAK(JA)
        IFA = 2*ABS(KA)
        DO JB = JM,NW
          KB = NAK(JB)
          IFB = 2*ABS(KB)
C
C   determine bounds in sum over K
C
          LMN = ABS(IFA-IFB)/2+1
          LMX = (IFA+IFB)/2
          IF (KA*KB.LT.0) LMN = LMN + 1
          DO LM = LMN,LMX,2
            K = LM-1
C
C   evaluate Slater integral
C
            CALL FINBB(
     + WA, 3, JA, JB, 0, 0, K, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
C
C   determine the angular coefficient
C
            ICOEF = 0
            II = 0
            JJ = JST
            DO ITR = IFST,ILST
              II = II+1
              JJ = JJ+1
              WB = ZERO
              IQA = IQ(JA,JJ)
              IQB = IQ(JB,JJ)
              IF (IQA.EQ.0 .OR. IQB.EQ.0) GOTO 20
C
C   if either orbital is in closed shell
C
              IF (IQA.EQ.IFA .OR. IQB.EQ.IFB) THEN
C
                IF (ICOEF.EQ.0) THEN
                  COEF = CLRX(KA,K,KB)**2
                  ICOEF = 1
                ENDIF
                WB = -IQA*IQB*COEF
C
C   if both orbitals are in open shells
C
              ELSE
C
                IF (NMCP.EQ.0) GOTO 20
                IRS = (ITR-1)*(ILST+ILST-ITR)/2+ITR
                ILDN = NNLDR(IRS)
                IF (ILDN.LE.0) GOTO 20
                ILDA = NSLDF(IRS)
                ILDB = ILDA+ILDN-1
                ILCX = (K*NW1+JA)*NW1+JB
                ILCY = JB*NW1+JA
                DO IL = ILDA,ILDB
                  IF (ILCX.EQ.ISLDR(IL) .AND. ILCY.EQ.JSLDR(IL)) THEN
                    WB = XSLDR(IL)
                    GOTO 20
                  ENDIF
                ENDDO
C
              ENDIF
C
   20         CONTINUE
              SSS(II) = WB
            ENDDO
C
C   add contribution to diagonal element
C
            DO I = 1,NTOT
              XMT(I,I) = XMT(I,I)+SSS(I)*WA
            ENDDO
C
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C   R integrals
C
C-----------------------------------------------------------------------
   30 CONTINUE
      IF (NMCP.EQ.0) RETURN
      IF (IFST.EQ.ILST) RETURN
C
      II = 0
      ILSTM = ILST-1
C
      DO ITR = IFST,ILSTM
        II = II+1
        ITRP = ITR+1
        JJ = II
C
C  cycle over the off-diagonal elements
C
        DO JTR = ITRP,ILST
          JJ = JJ+1
          IRS = (ITR-1)*(ILST+ILST-ITR)/2+JTR
          ILDN = NNLDR(IRS)
          IF (ILDN.LE.0) GOTO 40
C
          IF (ITC(15).EQ.1) WRITE (IWRITE,3000) II,JJ
C
          ELMNT = ZERO
C
          JA = NSLDF(IRS)
          JB = JA+ILDN-1
C
C  extract corresponding entries from table
C
          DO JX = JA,JB
C
            WB = XSLDR(JX)
            KX = JSLDR(JX)
            KD = MOD(KX,NW1)
            KC = KX/NW1
            KX = ISLDR(JX)
            KB = MOD(KX,NW1)
C
            IF (KB.EQ.0) THEN
              CALL FINBB(
     + WA, 1, KC, KD, 0, 0, 0, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
            ELSE
              KX = KX/NW1
              KA = MOD(KX,NW1)
              KK = KX/NW1
              CALL FINBB(
     + WA, 4, KA, KB, KC, KD, KK, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
            ENDIF
C
            CONTR = WB*WA
C
C   write out contributions if option 15 is set
C
            IF (ITC(15).EQ.1) THEN
              IF (KB.GT.0) THEN
                WRITE (IWRITE,3010)
     + NP(KA),NH(KA),NP(KB),NH(KB),NP(KC),
     + NH(KC),NP(KD),NH(KD),KK,WB,WA,CONTR
              ELSE
                WRITE (IWRITE,3020)
     + NP(KC),NH(KC),NP(KD),NH(KD),WB,WA,CONTR
              ENDIF
            ENDIF
C
            ELMNT = ELMNT+CONTR
C
          ENDDO
C
          IF (ITC(15).EQ.1) THEN
            WRITE (IWRITE,3030) ELMNT
          ENDIF
C
          XMT(II,JJ) = XMT(II,JJ)+ELMNT
C
   40     CONTINUE
        ENDDO
      ENDDO
C=======================================================================
 3000 FORMAT (/' Contributions to matrix element (',I2,',',I2,')'//
     +'   a    b    c    d    k          coeff.             integral  ',
     +'        contribution'/)
 3010 FORMAT (1X,4(I2,A2,1X),I3,9X,1P,E15.8,3X,E15.8,3X,E15.8)
 3020 FORMAT (11X,2(I2,A2,1X),12X,1P,E15.8,3X,E15.8,3X,E15.8)
 3030 FORMAT (1X,68X,'---------------'/1X,68X,1P,E15.8)
      END
CEND--------------------------------------------------------------------
CEND    CFOUT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFOUT(IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS, NAK, NCF, NH!
     +, NP, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFOUT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IQ
C   ISPAR
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   NAK
C   NCF
C   NH
C   NP
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Local variables
C
      CHARACTER*4 ILIT(MXNW),ITOT(MXNW),JLAB(20)
      CHARACTER*2 NNH(MXNW),NOCCP(MXNW),NSEN(MXNW),NUM(20)
      INTEGER I,IBIGJ,IFIRST
      INTEGER ILITJ(MXNW),ITOTJ(MXNW),J,K
      INTEGER K1,K2,MM,MX
      INTEGER NCOR,NCORE,NFULL
      INTEGER NN(MXNW),NOC,NOCC,NWP
      INTEGER NX
C
C  Argument variables
C
      CHARACTER*2 NH(*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NP(*)
      INTEGER NW
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NUM/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','10','1!
     +1','12','13','14','15','16','17','18','19'/
      DATA JLAB/'0   ','1/2 ','1   ','3/2 ','2   ','5/2 ','3   ','7/2 ',!
     +'4   ','9/2 ','5   ','11/2','6   ','13/2','7   ','15/2','8   ','17!
     +/2','9   ','19/2'/
C-----------------------------------------------------------------------
      NCORE = 0
      IF (NW.GT.0) THEN
        DO J = 1,NW
          NFULL = 2*ABS(NAK(J))
          DO I = 1,NCF
            IF (IQ(J,I).NE.NFULL) GOTO 10
          ENDDO
          NCORE = NCORE+1
        ENDDO
      ENDIF
C
   10 CONTINUE
      NWP = NW-NCORE
      NCOR = NCORE+1
C
      WRITE (IWRITE,3000)
C
      IF (NCORE.EQ.0) THEN
        WRITE (IWRITE,3050)
      ELSE
C
        DO I = 1,NCORE
          NX = IQ(I,1)
          NOCCP(I) = NUM(NX+1)
        ENDDO
C
        WRITE (IWRITE,3020)
C
        K1 = 1
        K2 = NCORE
        IF (K2.GT.10) K2 = 10
   20   CONTINUE
        WRITE (IWRITE,3030) (NOCCP(I),I=K1,K2)
        WRITE (IWRITE,3040) (NP(I),NH(I),I=K1,K2)
        IF (K2.EQ.NCORE) GOTO 30
        K1 = K1+10
        K2 = NCORE
        IF (K2.GT.K1+9) K2 = K1 + 9
        GOTO 20
C
   30   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
      IF (NWP.EQ.0) THEN
        WRITE (IWRITE,3060)
      ELSE
        WRITE (IWRITE,3070)
        DO I = 1,NCF
          MM = 1
          IBIGJ = 1
          IFIRST = 0
          NOC = 0
C
          DO J = NCOR,NW
            IF (IQ(J,I).GT.0) THEN
              NOC = NOC+1
              MX = IQ(J,I)
              NOCCP(NOC) = NUM(MX+1)
              NN(NOC) = NP(J)
              NNH(NOC) = NH(J)
              MX = JQS(1,J,I)
              NSEN(NOC) = NUM(MX+1)
              ILITJ(NOC) = JQS(3,J,I)
              NOCC = MIN(IQ(J,I),2*ABS(NAK(J))-IQ(J,I))
              IF (NOCC.GT.0) THEN
                IF (IFIRST.EQ.0) THEN
                  IFIRST = 1
                  IBIGJ = ILITJ(NOC)
                ELSE
                  IBIGJ = JCUP(MM,I)
                  MM = MM+1
                ENDIF
              ENDIF
              ITOTJ(NOC) = IBIGJ
            ENDIF
          ENDDO
C
          DO J = 1,NOC
            MX = ILITJ(J)
            ILIT(J) = JLAB(MX)
            MX = ITOTJ(J)
            ITOT(J) = JLAB(MX)
          ENDDO
C
          MX = ITJPO(I)
          IF (ISPAR(I).GT.0) THEN
            WRITE (IWRITE,3120) I,JLAB(MX)
          ELSE
            WRITE (IWRITE,3110) I,JLAB(MX)
          ENDIF
C
          K1 = 1
          K2 = NOC
          IF (K2.GT.4) K2 = 4
   40     CONTINUE
          WRITE (IWRITE,3080) (NOCCP(K),K=K1,K2)
          WRITE (IWRITE,3090) (NN(K),NNH(K),K=K1,K2)
          WRITE (IWRITE,3100) (NSEN(K),ILIT(K),ITOT(K),K=K1,K2)
          IF (K2.EQ.NOC) GOTO 50
          K1 = K1+4
          K2 = NOC
          IF (K2.GT.K1+3) K2 = K1 + 3
          GOTO 40
C
   50     CONTINUE
        ENDDO
C
      ENDIF
C
      WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/' routine CFOUT: write out jj-coupled CSFs'/1X!
     +,71('*'))
 3010 FORMAT (/1X,71('*'))
 3020 FORMAT (/' The core common to all CSFs is : ')
 3030 FORMAT (/2X,10(3X,A2,1X))
 3040 FORMAT (2X,10(I2,A2,2X))
 3050 FORMAT (/' No core has been defined')
 3060 FORMAT (/' No valence orbitals have been defined')
 3070 FORMAT (/' CSFs are defined using format :         Q'/            !
     +'                                    NL(-)'/                      !
     +'                                         V;J ) X')
 3080 FORMAT (/1X,4(6X,A2,10X))
 3090 FORMAT (1X,4(2X,I2,A2,12X))
 3100 FORMAT (1X,4(6X,A2,';',A4,')',A4))
 3110 FORMAT (/' CSF ',I5,'  J = ',A4,' odd')
 3120 FORMAT (/' CSF ',I5,'  J = ',A4,' even')
      END
CEND--------------------------------------------------------------------
CEND    CFP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFP(IWRITE,LOCK,NEL,IJD,IVD,IWD,IJP,IVP,IWP,COEFP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C=======================================================================
C
C
C  Input variables -
C
C   IWRITE   ... output stream number
C   LOCK     ... + or - (2*j + 1)
C   NEL      ... number of equivalent electrons in shell.
C   IJD      ... total J of daughter state.
C   IVD      ... seniority of daughter state
C   IWD      ... other quantum number (if needed).
C   IJP      ... total J of parent state.
C   IVP      ... seniority of parent state
C   IWP      ... other quantum number (if needed).
C
C  Output variable -
C
C   COEFP    - numerical result
C
C  This control routine does not check the input variables for
C  consistency, except the trivial case of j = 1/2.  All other
C  checks are performed at a lower level.
C
C  Routines called: CFP3,CFP5,CFP7,CFPD
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      DOUBLE PRECISION COEFP
      INTEGER IJD
      INTEGER IJP
      INTEGER IVD
      INTEGER IVP
      INTEGER IWD
      INTEGER IWP
      INTEGER LOCK
      INTEGER NEL
      INTEGER IWRITE
C
C  Local variables
C
      INTEGER K
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      K = ABS(LOCK)/2
C
      IF (K.EQ.1) THEN
        WRITE (IWRITE,3000)
        STOP
      ENDIF
C
      IF (K.EQ.2) THEN
        CALL CFP3(IWRITE,NEL,IJD,IJP,COEFP)
        RETURN
      ENDIF
C
      IF (K.EQ.3) THEN
        CALL CFP5(IWRITE,NEL,IJD,IVD,IJP,IVP,COEFP)
        RETURN
      ENDIF
C
      IF (K.EQ.4) THEN
        CALL CFP7(IWRITE,NEL,IJD,IVD,IJP,IVP,COEFP)
        RETURN
      ENDIF
C
      CALL CFPD(IWRITE,LOCK,NEL,COEFP)
C
 3000 FORMAT (/' STOPPING in routine CFP.'/                             !
     +' Unnecessary attempt to form CFP for an electron with j = 1/2.'/ !
     +' There is an ERROR.')
      END
CEND--------------------------------------------------------------------
CEND    CFP3.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFP3(IWRITE,NEL,IJD,IJP,COEFP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFP3.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IWRITE
C     NEL
C     IJD
C     IJP
C     COEFP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION SIX
      PARAMETER (SIX=6.D0)
      DOUBLE PRECISION FIVE
      PARAMETER (FIVE=5.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION COEFP
      INTEGER IJD
      INTEGER IJP
      INTEGER NEL
      INTEGER IWRITE
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF ((NEL.LE.0) .OR. (NEL.GT.4)) GOTO 70
C
      GOTO (10,20,30,50),NEL
C
   10 CONTINUE
      IF (IJD.NE.3 .OR. IJP.NE.0) GOTO 70
      GOTO 60
C
   20 CONTINUE
      IF (IJP.NE.3) GOTO 70
      IF (IJD.EQ.0 .OR. IJD.EQ.4) GOTO 60
      GOTO 70
C
   30 CONTINUE
      IF (IJD.NE.3) GOTO 70
      IF (IJP.NE.0) GOTO 40
      COEFP = SQRT(ONE/SIX)
      RETURN
C
   40 CONTINUE
      IF (IJP.NE.4) GOTO 70
      COEFP = -SQRT(FIVE/SIX)
      RETURN
C
   50 CONTINUE
      IF (IJD.NE.0 .OR. IJP.NE.3) GOTO 70
   60 CONTINUE
      COEFP = ONE
      RETURN
C
C  Fault mode section.
C
   70 CONTINUE
      COEFP = ZERO
      WRITE (IWRITE,3000) NEL,IJD,IJP
      STOP
C
 3000 FORMAT (/' STOPPING in routine CFP3.'/                            !
     +' ERROR in trying to compute CFP for a state with ',I4,           !
     +' electrons with j = 3/2'/' Parameters : ',2I6)
      END
CEND--------------------------------------------------------------------
CEND    CFP5.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFP5(IWRITE,NEL,IJD,IVD,IJP,IVP,COEFP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFP5.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IWRITE
C     NEL
C     IJD
C     IVD
C     IJP
C     IVP
C     COEFP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION SEVEN
      PARAMETER (SEVEN=7.D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION COEFP
      INTEGER IJD
      INTEGER IJP
      INTEGER IVD
      INTEGER IVP
      INTEGER NEL
      INTEGER IWRITE
C
C  Local variables
C
      DOUBLE PRECISION DENOM,DNEL,FACT
      INTEGER IJ(3,3),IJ1,IJ2,IS
      INTEGER IV(3,3),IV1,IV2,K
      INTEGER KD,KP,N,NORM(3)
      INTEGER NUM3(3,3)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C  1.0 tables of data
C
      DATA IJ/5,0,5,0,4,3,0,8,9/,IV/1,0,1,8,2,3,8,2,3/,NUM3/-4,0,0,5,-5,!
     +3,9,2,-11/,NORM/18,7,14/
C-----------------------------------------------------------------------
C
C  2.0 locate entry in CFP table
C
      IF (NEL.LE.0) GOTO 120
      IF (NEL.GE.4) GOTO 10
C
      N = NEL
      IJ1 = IJD
      IV1 = IVD
      IJ2 = IJP
      IV2 = IVP
      GOTO 20
C
   10 CONTINUE
      IF (NEL.GT.6) GOTO 120
      N = 7-NEL
      IJ1 = IJP
      IV1 = IVP
      IJ2 = IJD
      IV2 = IVD
C
C  2.1 find 'daughter' index
C
   20 CONTINUE
      K = 0
   30 CONTINUE
      K = K+1
      IF (K.GT.3) GOTO 120
      IF (IJ(N,K).NE.IJ1) GOTO 30
      IF (IV(N,K).NE.IV1) GOTO 30
      KD = K
C
C  2.2 find 'parent' index
C
      IF (N.NE.1) GOTO 40
      IF (IV2.NE.0) GOTO 120
      IF (IJ2.EQ.0) GOTO 60
      GOTO 120
C
   40 CONTINUE
      K = 0
   50 CONTINUE
      K = K+1
      IF (K.GT.3) GOTO 120
      IF (IJ(N-1,K).NE.IJ2) GOTO 50
      IF (IV(N-1,K).NE.IV2) GOTO 50
      KP = K
C-----------------------------------------------------------------------
C
C  3.0 compute coefficients
C
C  3.1 table look-up
C
      GOTO (60,60,70),N
C
   60 CONTINUE
      COEFP = ONE
      GOTO 100
C
   70 CONTINUE
      COEFP = DBLE(NUM3(KD,KP))
      DENOM = DBLE(NORM(KD))
      IF (COEFP) 90,110,80
   80 CONTINUE
      COEFP = SQRT(COEFP/DENOM)
      GOTO 100
C
   90 CONTINUE
      COEFP = -SQRT(-COEFP/DENOM)
C
C  3.2 insert additional factors for hole states
C
  100 CONTINUE
      IF (NEL.LE.3) GOTO 110
      DNEL = DBLE(NEL)
      FACT = ((SEVEN-DNEL)/DNEL)*(ONE+IJP)/(ONE+IJD)
      COEFP = COEFP*SQRT(FACT)
      IS = ABS((IJD-IJP-IVD+IVP)/2)
      IF (MOD(IS,2).EQ.0) GOTO 110
      COEFP = -COEFP
  110 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C
C  4.0 fault mode section
C
  120 CONTINUE
      COEFP = ZERO
      WRITE (IWRITE,3000) NEL,IJD,IVD,IJP,IVP
      STOP
C
 3000 FORMAT (/' STOPPING in routine CFP5.'/                            !
     +' ERROR in trying to compute CFP for a state with ',I4,           !
     +' electrons with j = 5/2'/' Parameters : ',4I6)
      END
CEND--------------------------------------------------------------------
CEND    CFP7.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFP7(IWRITE,NEL,IJD,IVD,IJP,IVP,COEFP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFP7.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IWRITE
C     NEL
C     IJD
C     IVD
C     IJP
C     IVP
C     COEFP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION NINE
      PARAMETER (NINE=9.D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION COEFP
      INTEGER IJD
      INTEGER IJP
      INTEGER IVD
      INTEGER IVP
      INTEGER NEL
      INTEGER IWRITE
C
C  Local variables
C
      DOUBLE PRECISION DENOM,DNEL,FACT
      INTEGER IJ(4,8),IJ1,IJ2,IS
      INTEGER IV(4,8),IV1,IV2,K
      INTEGER KD,KP,N
      INTEGER NORM3(6),NORM4(8),NUM3(6,4)
      INTEGER NUM4(8,6)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C  1.0 tables of data
C
      DATA IJ/7,0,7,0,0,4,3,4,0,8,5,8,0,12,9,12,0,0,11,4,0,0,15,8,3*0,10!
     +,3*0,16/
      DATA IV/1,0,1,0,10,2,3,2,10,2,3,2,10,2,3,2,10,10,3,4,10,10,3,4,3*1!
     +0,4,3*10,4/
      DATA NUM3/9,5*0,-5,3,121,143,-55,0,-9,-11,12,-900,39,5,-13,0,-65,3!
     +43,104,-17/
      DATA NORM3/36,14,198,1386,198,22/
      DATA NUM4/1,280,308,1144,4*0,0,54,-121,0,-968,169,462,0,0,-231,-14!
     +,195,-77,2366,-343,0,0,-65,250,-245,-1755,90,-945,140,0,-210,91,62!
     +4,280,2275,650,234,0,0,140,-1224,0,-560,680,627/
      DATA NORM4/1,840,924,3432,3080,5460,3080,1001/
C-----------------------------------------------------------------------
C
C  2.0 locate entry in CFP table
C
      IF (NEL.LE.0) GOTO 140
      IF (NEL.GE.5) GOTO 10
C
      N = NEL
      IJ1 = IJD
      IV1 = IVD
      IJ2 = IJP
      IV2 = IVP
      GOTO 20
C
   10 CONTINUE
      IF (NEL.GT.8) GOTO 140
      N = 9-NEL
      IJ1 = IJP
      IV1 = IVP
      IJ2 = IJD
      IV2 = IVD
C
C  2.1 find 'daughter' index
C
   20 CONTINUE
      K = 0
   30 CONTINUE
      K = K+1
      IF (K.GT.8) GOTO 140
      IF (IJ(N,K).NE.IJ1) GOTO 30
      IF (IV(N,K).NE.IV1) GOTO 30
      KD = K
C
C  2.2 find 'parent' index
C
      IF (N.NE.1) GOTO 40
      IF (IV2.NE.0) GOTO 140
      IF (IJ2.EQ.0) GOTO 60
      GOTO 140
C
   40 CONTINUE
      K = 0
   50 CONTINUE
      K = K+1
      IF (K.GT.8) GOTO 140
      IF (IJ(N-1,K).NE.IJ2) GOTO 50
      IF (IV(N-1,K).NE.IV2) GOTO 50
      KP = K
C-----------------------------------------------------------------------
C
C  3.0 compute coefficients
C
C  3.1 table look-up
C
      GOTO (60,60,70,110),N
C
   60 CONTINUE
      COEFP = ONE
      GOTO 120
C
   70 CONTINUE
      COEFP = NUM3(KD,KP)
      DENOM = NORM3(KD)
   80 CONTINUE
      IF (COEFP) 100,130,90
   90 CONTINUE
      COEFP = SQRT(COEFP/DENOM)
      GOTO 120
C
  100 CONTINUE
      COEFP = -SQRT(-COEFP/DENOM)
      GOTO 120
C
  110 CONTINUE
      COEFP = NUM4(KD,KP)
      DENOM = NORM4(KD)
      GOTO 80
C
C  3.2 insert additional factors for hole states
C
  120 CONTINUE
      IF (NEL.LE.4) GOTO 130
      DNEL = DBLE(NEL)
      FACT = ((NINE-DNEL)/DNEL)*(ONE+IJP)/(ONE+IJD)
      COEFP = COEFP*SQRT(FACT)
      IS = ABS((IJD-IJP-IVD+IVP)/2-3)
      IF (MOD(IS,2).EQ.0) GOTO 130
      COEFP = -COEFP
  130 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C
C  4.0 fault mode section
C
  140 CONTINUE
      COEFP = ZERO
      WRITE (IWRITE,3000) NEL,IJD,IVD,IJP,IVP
      STOP
C
 3000 FORMAT (/' STOPPING in routine CFP7.'/                            !
     +' ERROR in trying to compute CFP for a state with ',I4,           !
     +' electrons with j = 7/2'/' Parameters : ',4I6)
      END
CEND--------------------------------------------------------------------
CEND    CFPD.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CFPD(IWRITE,LOCK,NEL,COEFP)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CFPD.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IWRITE
C     LOCK
C     NEL
C     COEFP
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      DOUBLE PRECISION COEFP
      INTEGER IWRITE
      INTEGER LOCK
      INTEGER NEL
C
C  Local variables
C
      INTEGER LOCJ
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (NEL.EQ.1) GOTO 10
      IF (NEL.EQ.2) GOTO 10
      IF (NEL.EQ.ABS(LOCK)) GOTO 10
C
      COEFP = ZERO
      LOCJ = ABS(LOCK)-1
      WRITE (IWRITE,3000) LOCJ
      STOP
C
   10 CONTINUE
      COEFP = ONE
C-----------------------------------------------------------------------
 3000 FORMAT (/' STOPPING in routine CFPD.'/                            !
     +' Inadmissable attempt to obtain CFP for a state of the shell wi',!
     +'th J = ',I4,'/2'/' New subprogram required')
      END
CEND--------------------------------------------------------------------
CEND    CLEGOR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CLEGOR(XJ1,XJ2,XM1,XM2,XJ3,XM3,CG,IER)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CLEGOR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
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
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      INTEGER IFAK
      PARAMETER (IFAK=64)
C
C  Argument variables
C
      DOUBLE PRECISION CG,XJ1,XJ2,XJ3
      DOUBLE PRECISION XM1,XM2,XM3
      INTEGER IER
C
C  Local variables
C
      DOUBLE PRECISION AI,BIP1,CONST,SUM
      DOUBLE PRECISION FAK(IFAK)
      DOUBLE PRECISION X
      INTEGER I,IA,IB,IC
      INTEGER ID,IE,IG,J
      INTEGER J1,J2,J3,JX
      INTEGER JY,JZ,KMAX,KMIN
      INTEGER L,L1,L2,L3
      INTEGER M,M1,M2,M3
      INTEGER N,N1,N10,N11
      INTEGER N2,N3,N4,N5
      INTEGER N6,N7,N8
      INTEGER K
      LOGICAL FIRST
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE FAK
      SAVE FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C
C   Calculate log of factorials (0..........IFAK) to the base e
C
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        FAK(1) = ZERO
        FAK(2) = ZERO
        DO K = 3,IFAK
          N = K-1
          X = N
          FAK(K) = FAK(N)+LOG(X)
        ENDDO
        FIRST = .FALSE.
      ENDIF
C-----------------------------------------------------------------------
      CG = ZERO
      IER = 0
C
      J1 = NINT(TWO*XJ1)
      J2 = NINT(TWO*XJ2)
      J3 = NINT(TWO*XJ3)
      M1 = NINT(TWO*XM1)
      M2 = NINT(TWO*XM2)
      M3 = NINT(TWO*XM3)
      J = (J1+J2+J3)/2
      M = (M1+M2-M3)/2
      JX = (-J1+J2+J3)/2
      JY = (J1-J2+J3)/2
      JZ = (J1+J2-J3)/2
C
C   **********    check of the coupling rules    **********
C
      IF ((J1.LT.0) .OR. (J2.LT.0) .OR. (J3.LT.0) .OR. (JX.LT.0) .OR.(JY!
     +.LT.0) .OR. (JZ.LT.0) .OR. ((J1-ABS(M1)).LT.0) .OR.((J2-ABS(M2)).L!
     +T.0) .OR. ((J3-ABS(M3)).LT.0) .OR.(MOD(J1,2).NE.MOD(ABS(M1),2)) .O!
     +R.(MOD(J2,2).NE.MOD(ABS(M2),2)) .OR.(MOD(J3,2).NE.MOD(ABS(M3),2)) !
     +.OR.(MOD(J1+J2,2).NE.MOD(J3,2)) .OR. (M.NE.0)) THEN
        IER = 1
        RETURN
      ENDIF
C
C   **********   calculation of special values   **********
C
      IF ((J1.EQ.0) .OR. (J2.EQ.0)) THEN
        CG = ONE
        RETURN
      ENDIF
C
      IF (J3.EQ.0) THEN
        CG = ONE/SQRT(TWO*XJ1+ONE)
        N = (J1-M1)/2
        IF (MOD(N,2).EQ.1) CG = -CG
        RETURN
      ENDIF
C
      IF ((M1.EQ.0) .AND. (M2.EQ.0)) THEN
        IF (MOD(J,2).EQ.1) RETURN
        N10 = J/2
        L1 = J1/2
        L2 = J2/2
        L3 = J3/2
        N11 = (L1+L2-L3)/2
        CG = (TWO*XJ3+ONE)*EXP(FAK(JX+1)+FAK(JY+1)+FAK(JZ+1)-FAK(J+2)+TW!
     +O*(FAK(N10+1)-FAK(N10-L1+1)-FAK(N10-L2+1)-FAK(N10-L3+1)))
        CG = SQRT(CG)
        IF (MOD(N11,2).EQ.1) CG = -CG
        RETURN
      ENDIF
C
C   **********   calculation of the general case   **********
C
      N1 = (J1+M1)/2
      N2 = (J1-M1)/2
      N3 = (J2+M2)/2
      N4 = (J2-M2)/2
      N5 = (J3+M3)/2
      N6 = (J3-M3)/2
      N7 = (J2-J3-M1)/2
      N8 = (J1-J3+M2)/2
      KMIN = MAX(0,N7,N8)
      KMAX = MIN(JZ,N2,N3)
      L = KMIN
      CONST = FAK(N1+1)+FAK(N2+1)+FAK(N3+1)+FAK(N4+1)+FAK(N5+1)+FAK(N6+1!
     +)+FAK(JX+1)+FAK(JY+1)+FAK(JZ+1)-FAK(J+2)-TWO*(FAK(JZ-L+1)+FAK(N2-L!
     ++1)+FAK(N3-L+1)+FAK(L-N7+1)+FAK(L-N8+1)+FAK(L+1))
      N = KMAX-KMIN+1
      I = N-1
      SUM = ONE
      IA = JZ-L
      IB = N2-L
      IC = N3-L
      ID = L+1-N7
      IE = L+1-N8
      IG = L+1
C
   10 CONTINUE
      AI = (IA-I)*(IB-I)*(IC-I)
      BIP1 = (ID+I)*(IE+I)*(IG+I)
      SUM = ONE-AI*SUM/BIP1
      I = I-1
      IF (I.GE.0) GOTO 10
C
      CG = SQRT((TWO*XJ3+ONE)*EXP(CONST))*SUM
      IF (MOD(L,2).EQ.1) CG = -CG
C
      END
CEND--------------------------------------------------------------------
CEND    CLRX.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      FUNCTION CLRX(KAP1,K,KAP2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CLRX.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   KAP1   ... kappa-value for j1
C   K      ...
C   KAP2   ... kappa-value for j2
C
C  No routines called.
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      DOUBLE PRECISION CLRX
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
C
C  Argument variables
C
      INTEGER K,KAP1,KAP2
C
C  Local variables
C
      DOUBLE PRECISION X,Y
      INTEGER I,IP,IPHASE,IX
      INTEGER J,JM,JP,JQ
      INTEGER KMA,KMB
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      KMA = ABS(KAP1)
      KMB = ABS(KAP2)
      JP = KMA+KMB-1
      JM = KMA-KMB
      X = ONE/(KMA*KMB)
C
      IX = JP-K
      J = 1
      GOTO 90
C
   10 CONTINUE
      IX = JP+K+1
      J = 1
      GOTO 100
C
   20 CONTINUE
      IX = JM+K
      J = 2
      GOTO 90
C
   30 CONTINUE
      IX = K-JM
      J = 3
      GOTO 90
C
   40 CONTINUE
      Y = SQRT(X)
      IP = K
      JQ = JP+K
      IF (MOD(JQ,2).NE.0) THEN
        Y = -Y
        IP = K+1
      ENDIF
C
      X = ONE
      IX = (IP+JP)/2
      J = 4
      GOTO 90
C
   50 CONTINUE
      IX = (JP-IP)/2
      J = 2
      GOTO 100
C
   60 CONTINUE
      IX = (JM+IP-1)/2
      J = 3
      GOTO 100
C
   70 CONTINUE
      IX = (IP-1-JM)/2
      J = 4
      GOTO 100
C
   80 CONTINUE
      CLRX = Y*X
      IPHASE = (JP+IP-2)/2
      IF (MOD(IPHASE,2).EQ.1) CLRX = -CLRX
      RETURN
C-----------------------------------------------------------------------
   90 CONTINUE
      IF (IX.LT.0) GOTO 110
      IF (IX.GT.0) THEN
        DO I = 1,IX
          X = X*DBLE(I)
        ENDDO
      ENDIF
C
      IF (J.EQ.1) GOTO 10
      IF (J.EQ.2) GOTO 30
      IF (J.EQ.3) GOTO 40
      GOTO 50
C-----------------------------------------------------------------------
  100 CONTINUE
      IF (IX.LT.0) GOTO 110
      IF (IX.GT.0) THEN
        DO I = 1,IX
          X = X/DBLE(I)
        ENDDO
      ENDIF
C
      IF (J.EQ.1) GOTO 20
      IF (J.EQ.2) GOTO 60
      IF (J.EQ.3) GOTO 70
      GOTO 80
C-----------------------------------------------------------------------
  110 CONTINUE
      CLRX = ZERO
C
      END
CEND--------------------------------------------------------------------
CEND    CMAT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CMAT(IDISC1,IDISC2,IDISC4,ITAPE2,EMTB, EMTR, ENAT, FACT!
     +EV, ICTXB, IDMTST,IQ, IRK1, IRK2, ISLDR, ISTX1B, ISTX2B, ISTXB, IT!
     +C, IWRITE,J2P1, JLASTX, JSLDR, K1S, K2P, K2S, KBBN1, KBBN2, KBBR,K!
     +BCN1, KBCN2, KBCR, KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS,MAXFUL, M!
     +AXNQN, MAXP, MINNQN, NAK, NAKK, NCCK, NCCP, NCCT,NCF, NCFCON, NCFG!
     +P, NCHAN, NDIMAX, NH, NMCP, NNLDR, NP,NRANG2, NSLDF, NTARG, NW, NW!
     +1, RKSTO, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CMAT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  IDISC1 - file for the angular coefficients
C  IDISC2 - file for the radial integrals
C  IDISC4 - file for the angular coefficients
C  ITAPE2 - DSTG2 dump file
C
C  CMAT - MATOUT - utility to print matrices
C         MCPINC - read in the angular coefficients
C   (c-c) CMATX
C   (b-c) CMATY
C   (b-b) LOCBB
C         BMATX
C         DSYEV  - diagonalise a matrix
C
C   EMTB
C   EMTR
C   ENAT
C   FACTEV
C   ICTXB
C   IDMTST
C   IQ
C   IRK1
C   IRK2
C   ISLDR
C   ISTX1B
C   ISTX2B
C   ISTXB
C   ITC
C   IWRITE
C   J2P1
C   JLASTX
C   JSLDR
C   K1S
C   K2P
C   K2S
C   KBBN1
C   KBBN2
C   KBBR
C   KBCN1
C   KBCN2
C   KBCR
C   KBMAX
C   KCCN
C   KCCR
C   KCMAX
C   KCMIN
C   KPOS
C   MAXFUL
C   MAXNQN
C   MAXP
C   MINNQN
C   NAK
C   NAKK
C   NCCK
C   NCCP
C   NCCT
C   NCF
C   NCFCON
C   NCFGP
C   NCHAN
C   NDIMAX
C   NH
C   NMCP
C   NNLDR
C   NP
C   NRANG2
C   NSLDF
C   NTARG
C   NW
C   NW1
C   RKSTO
C   XSLDR
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
      INTEGER N1NML
      PARAMETER (N1NML=MXNB)
      INTEGER N2NML
      PARAMETER (N2NML=MXNL)
      INTEGER INML
      PARAMETER (INML=N1NML/N2NML)
      INTEGER JNML
      PARAMETER (JNML=N2NML/N1NML)
      INTEGER KNML
      PARAMETER (KNML=INML+JNML)
      INTEGER ND24
      PARAMETER (ND24=N1NML*INML/KNML+N2NML*JNML/KNML)
      INTEGER LWORK
      PARAMETER (LWORK=MXNL*3-1)
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      INTEGER IDISC1,IDISC2,IDISC4,ITAPE2
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION EMTB(MXNL,*)
      DOUBLE PRECISION EMTR(MXNL,*)
      DOUBLE PRECISION ENAT(MXNL)
      DOUBLE PRECISION FACTEV
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IQ(MXNW,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISLDR(*)
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JLASTX
      INTEGER JSLDR(*)
      INTEGER K1S
      INTEGER K2P(*)
      INTEGER K2S
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBCN1(*)
      INTEGER KBCN2(*)
      INTEGER KBCR(*)
      INTEGER KBMAX
      INTEGER KCCN(MXNK,*)
      INTEGER KCCR(MXNK,*)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KPOS
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NCCK(*)
      INTEGER NCCP(MXNL,*)
      INTEGER NCCT(*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(*)
      INTEGER NMCP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NSLDF(*)
      INTEGER NTARG(*)
      INTEGER NW
      INTEGER NW1
C
C  Local variables
C
      DOUBLE PRECISION EBOT,EBOT3,EIG(MXNL),EIGEV
      DOUBLE PRECISION WORK(LWORK),WX
      DOUBLE PRECISION XMT(MXNL,MXNL)
      DOUBLE PRECISION HAMIL(MXNB,ND24)
      DOUBLE PRECISION TOTAL(MXNB,ND24)
      INTEGER I,II,INFO,IP
      INTEGER IT,ITOT,J,J1
      INTEGER J2,JJ,JP,JT
      INTEGER K,K1,K2,KI
      INTEGER KJ,L1,L2,MODE
      INTEGER MPOSX(MXNK),MX,NSIZE,NXX
      LOGICAL TST1,TST2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NSIZE = NCHAN*NRANG2+NCFGP
      WRITE (IWRITE,3020) NCHAN,NRANG2,NCFGP,NSIZE
C-----------------------------------------------------------------------
C
C  Set up the array MPOSX.
C  This identifies the K-values that are used in the calculation.
C
C-----------------------------------------------------------------------
      DO K = KCMIN,KCMAX
        MPOSX(K) = 0
      ENDDO
C
      DO I = 1,NCHAN
        K = NAKK(I)
        MPOSX(K) = 1
      ENDDO
C-----------------------------------------------------------------------
C
C NW1 is the number of orbitals used in MCP section + 1
C NXX is the number of CSFs used in MCP section
C
C-----------------------------------------------------------------------
      NW1 = NW+NCHAN+1
      NXX = NCFCON+NCFGP
C-----------------------------------------------------------------------
C
C  Read in the MCP coefficients
C
C-----------------------------------------------------------------------
      CALL MCPINC(IDISC1,IDISC4,NXX,IDMTST, ISLDR, ITC, IWRITE, J2P1, JL!
     +ASTX, JSLDR, NCCK, NCFCON,NCFGP, NDIMAX, NH, NMCP, NNLDR, NP, NSLD!
     +F, NW, NW1, XSLDR)
C-----------------------------------------------------------------------
      ITOT = 0
      DO K1 = KCMIN,KCMAX
        IF (MPOSX(K1).EQ.1) THEN
          DO K2 = K1,KCMAX
            IF (MPOSX(K2).EQ.1) THEN
C
              DO I = 1,NCHAN
                KI = NAKK(I)
                DO J = I,NCHAN
                  KJ = NAKK(J)
                  TST1 = (KI.EQ.K1.AND.KJ.EQ.K2)
                  TST2 = (KI.EQ.K2.AND.KJ.EQ.K1)
                  IF (TST1 .OR. TST2) THEN
C
                    ITOT = ITOT+1
                    IT = NTARG(I)
                    JT = NTARG(J)
C
                    IF (ITC(30).EQ.1 .OR. ITC(31).EQ.1) WRITE (IWRITE,30!
     +30) I,J
                    MODE = 2
                    IF (I.EQ.J) MODE = 1
                    DO L2 = 1,NRANG2
                      DO L1 = 1,NRANG2
                        HAMIL(L1,L2) = ZERO
                      ENDDO
                    ENDDO
C
                    DO II = 1,NCF
                      IP = NCCP(II,I)
                      IF (IP.GT.0) THEN
                        DO JJ = 1,NCF
                          JP = NCCP(JJ,J)
                          IF (JP.GT.0) THEN
                            WX = EMTR(II,IT)*EMTR(JJ,JT)
                            CALL CMATX(IDISC2,I,J,NXX,IP,JP,MX,TOTAL,ITC!
     +,IWRITE,EMTB, IQ, K2P, KBMAX, KCMAX, KCMIN, MAXFUL, MAXNQN, MINNQN!
     +,NAK, NAKK, NCCK, NCCT, NH, NP, NRANG2, NW, NW1,KBBN1, KBBN2, KBBR!
     +, KCCN, KCCR,K1S, K2S, KPOS, MAXP,IRK1, IRK2, ICTXB, ISTX1B, ISTX2!
     +B, ISTXB, RKSTO,ISLDR, JSLDR, NMCP, NNLDR, NSLDF, XSLDR)
                            DO L2 = 1,NRANG2
                              DO L1 = 1,NRANG2
                                HAMIL(L1,L2) = HAMIL(L1,L2)+WX*TOTAL(L1,!
     +L2)
                              ENDDO
                            ENDDO
                            IF (ITC(31).EQ.1 .AND. MX.NE.0) THEN
                              WRITE (IWRITE,3070) IP,JP,WX
                              CALL MATOUT(IWRITE,TOTAL,NRANG2,NRANG2,MXN!
     +B,ND24,MODE)
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDDO
C
                    WRITE (ITAPE2) I,J
                    IF (I.EQ.J) THEN
                      WRITE (ITAPE2)((HAMIL(L1,L2),L2=L1,NRANG2),L1=1,NR!
     +ANG2)
                    ELSE
                      WRITE (ITAPE2)((HAMIL(L1,L2),L2=1,NRANG2),L1=1,NRA!
     +NG2)
                    ENDIF
C
                    IF (ITC(30).EQ.1) THEN
                      CALL MATOUT(IWRITE,HAMIL,NRANG2,NRANG2,MXNB,ND24,M!
     +ODE)
                    ENDIF
C
                  ENDIF
                ENDDO
              ENDDO
C
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      IF (ITC(30).EQ.1 .OR. ITC(31).EQ.1) WRITE (IWRITE,3090) ITOT
C-----------------------------------------------------------------------
C
C  ******** correlation contributions ************
C
C-----------------------------------------------------------------------
      IF (NCFGP.EQ.0) RETURN
C-----------------------------------------------------------------------
C
C *** bound-continuum contributions
C
C-----------------------------------------------------------------------
      IF (ITC(30).EQ.1 .OR. ITC(32).EQ.1) WRITE (IWRITE,3040)
C
C   loop over the channels
C
      ITOT = 0
      DO K1 = KCMIN,KCMAX
        IF (MPOSX(K1).EQ.1) THEN
C
          DO I = 1,NCHAN
            KI = NAKK(I)
            IF (KI.EQ.K1) THEN
C
              ITOT = ITOT+1
              IT = NTARG(I)
C
              IF (ITC(30).EQ.1 .OR. ITC(32).EQ.1) WRITE (IWRITE,3060) I
              DO L2 = 1,NCFGP
                DO L1 = 1,NRANG2
                  HAMIL(L1,L2) = ZERO
                ENDDO
              ENDDO
C
              DO II = 1,NCF
                IP = NCCP(II,I)
                IF (IP.GT.0) THEN
                  WX = EMTR(II,IT)
                  CALL CMATY(IDISC2,NXX,IP,TOTAL,ITC, IWRITE, KBMAX, KCM!
     +AX, KCMIN, MAXFUL, MAXNQN, MINNQN,NAK, NAKK, NCFCON, NCFGP, NH, NP!
     +, NRANG2, NW, NW1,K1S, K2S, KPOS, MAXP,KBCN1, KBCN2, KBCR, RKSTO,I!
     +SLDR, JSLDR, NNLDR, NSLDF, XSLDR)
                  DO L2 = 1,NCFGP
                    DO L1 = 1,NRANG2
                      HAMIL(L1,L2) = HAMIL(L1,L2)+WX*TOTAL(L1,L2)
                    ENDDO
                  ENDDO
                  IF (ITC(32).EQ.1) THEN
                    WRITE (IWRITE,3080) IP,WX
                    CALL MATOUT(IWRITE,TOTAL,NRANG2,NCFGP,MXNB,ND24,2)
                  ENDIF
                ENDIF
              ENDDO
C
              WRITE (ITAPE2) I,((HAMIL(L1,L2),L2=1,NCFGP),L1=1,NRANG2)
C
              IF (ITC(30).EQ.1) THEN
                CALL MATOUT(IWRITE,HAMIL,NRANG2,NCFGP,MXNB,ND24,2)
              ENDIF
C
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
      IF (ITC(30).EQ.1 .OR. ITC(32).EQ.1) WRITE (IWRITE,3100) ITOT
C-----------------------------------------------------------------------
C
C *** bound-bound contributions
C
C-----------------------------------------------------------------------
      CALL LOCBB(0,0, K1S, K2S, RKSTO,ICTXB, IRK1, IRK2, ISTX1B, ISTX2B,!
     + ISTXB, ITC,IWRITE, KBBN1, KBBN2, KBBR, KBMAX)
      IF (ITC(30).EQ.1 .OR. ITC(33).EQ.1) WRITE (IWRITE,3050)
      J1 = NCFCON+1
      J2 = NCFCON+NCFGP
      CALL BMATX(XMT,J1,J2,NCF,ICTXB, IQ, IRK1, ISLDR, ISTX1B, ISTX2B, I!
     +STXB, ITC, IWRITE,JSLDR, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, N!
     +MCP, NNLDR,NP, NSLDF, NW, NW1, RKSTO, XSLDR)
      IF (ITC(30).EQ.1 .OR. ITC(33).EQ.1) CALL MATOUT(IWRITE,XMT,NCFGP,N!
     +CFGP,MXNL,MXNL,1)
      DO I = 1,NCFGP
        WRITE (ITAPE2) (XMT(I,J),J=I,NCFGP)
      ENDDO
C-----------------------------------------------------------------------
C
C   diagonalise the bound-bound Hamiltonian
C
C   use a LAPACK routine
C
      WRITE (IWRITE,3000)
      CALL DSYEV('V','U',NCFGP,XMT,MXNL,EIG,WORK,LWORK,INFO)
      IF (INFO.NE.0) THEN
        WRITE (IWRITE,3010)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      EBOT = ENAT(1)
      EBOT3 = EBOT*FACTEV
      WRITE (IWRITE,3110) EBOT,EBOT3
      DO I = 1,NCFGP
        WORK(I) = EIG(I)-ENAT(1)
        EIGEV = WORK(I)*FACTEV
        WRITE (IWRITE,3120) I,WORK(I),EIGEV
      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (/' diagonalisation using the DSYEV routine')
 3010 FORMAT (//' ERROR using the DSYEV routine'/' STOPPING')
 3020 FORMAT (/31X,'Routine CMAT'/31X,'------------'//                  !
     +' ===== evaluate the continuum Hamiltonian matrix'/' ===== ',I6,  !
     +' channels'/' ===== ',I6,' continuum orbitals'/' ===== ',I6,      !
     +' correlation functions'/' ===== ',I6,                            !
     +' is the Hamiltonian dimension')
 3030 FORMAT (/' channel ',I5,4X,'channel ',I5,4X,'contribution'/)
 3040 FORMAT (/' bound-continuum contributions'/)
 3050 FORMAT (/' bound-bound contributions'/)
 3060 FORMAT (/' channel ',I5/)
 3070 FORMAT (' contr. from CSFs: ',2I5,8X,'weight = ',1P,E12.5)
 3080 FORMAT (' contr. from CSF: ',I5,8X,'weight = ',1P,E12.5)
 3090 FORMAT (/1X,I6,' continuum-continuum Hamiltonians evaluated')
 3100 FORMAT (/1X,I6,' bound-continuum Hamiltonians evaluated')
 3110 FORMAT (/' bound-bound eigen-energies relative to groundstate'/   !
     +' --------------------------------------------------'//           !
     +' groundstate energy : ',1P,E17.9,' a.u.',2X,E17.9,' e.V.'//      !
     +' level',7X,'a.u.',16X,'e.V.'/)
 3120 FORMAT (1X,I3,4X,1P,2E20.11)
      END
CEND--------------------------------------------------------------------
CEND    CMATX.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CMATX(
     + IDISC2, ICH, JCH, NXX, IR, IS, MX, TOTAL, ITC, IWRITE, EMTB, IQ,
     + K2P, KBMAX, KCMAX, KCMIN, MAXFUL, MAXNQN, MINNQN, NAK, NAKK,
     + NCCK, NCCT, NH, NP, NRANG2, NW, NW1, KBBN1, KBBN2, KBBR, KCCN,
     + KCCR, K1S, K2S, KPOS, MAXP, IRK1, IRK2, ICTXB, ISTX1B, ISTX2B,
     + ISTXB, RKSTO, ISLDR, JSLDR, NMCP, NNLDR, NSLDF, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CMATX.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   MX=0 ... TOTAL is zero and is not printed in CMAT
C
C   EMTB
C   IQ
C   K2P
C   KBMAX
C   KCMAX
C   KCMIN
C   MAXFUL
C   MAXNQN
C   MINNQN
C   NAK
C   NAKK
C   NCCK
C   NCCT
C   NH
C   NP
C   NRANG2
C   NW
C   NW1
C   KBBN1
C   KBBN2
C   KBBR
C   KCCN
C   KCCR
C   K1S
C   K2S
C   KPOS
C   MAXP
C   IRK1
C   IRK2
C   ICTXB
C   ISTX1B
C   ISTX2B
C   ISTXB
C   RKSTO
C   ISLDR
C   JSLDR
C   NMCP
C   NNLDR
C   NSLDF
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL CLRX
      DOUBLE PRECISION CLRX
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      DOUBLE PRECISION TOTAL(MXNB,*)
      INTEGER ICH,IDISC2,IR,IS
      INTEGER JCH,MX,NXX
      INTEGER ITC(*)
      INTEGER IWRITE
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION EMTB(MXNL,*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IQ(MXNW,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISLDR(*)
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER JSLDR(*)
      INTEGER K1S
      INTEGER K2P(*)
      INTEGER K2S
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBMAX
      INTEGER KCCN(MXNK,*)
      INTEGER KCCR(MXNK,*)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KPOS
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NCCK(*)
      INTEGER NCCT(*)
      INTEGER NMCP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NSLDF(*)
      INTEGER NW
      INTEGER NW1
C
C  Local variables
C
      DOUBLE PRECISION WA,WB,WX
      INTEGER I,IFA,IFB,II
      INTEGER IL,ILCX,ILCY,ILDA
      INTEGER ILDB,ILDN,IQA,IRS
      INTEGER J,JA,JB,JC
      INTEGER JD,JJ,JK,JX
      INTEGER K,KA,KB,KMN
      INTEGER KMX,L1,L2,LL
      INTEGER NS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      MX = 1
      IF (IR.NE.IS) GOTO 70
C-----------------------------------------------------------------------
C
C *** Form diagonal blocks of Hamiltonian ***
C
C-----------------------------------------------------------------------
      I = IR
      IRS = (I-1)*(NXX+NXX-I)/2+I
C
      II = NCCT(I)
C
C   Set the matrix elements to zero
C
      DO L2 = 1,NRANG2
        DO L1 = 1,NRANG2
          TOTAL(L1,L2) = ZERO
        ENDDO
      ENDDO
C
C   Add in the contribution from the atomic Hamiltonian
C
      IF (ICH.EQ.JCH) THEN
        DO L1 = 1,NRANG2
          TOTAL(L1,L1) = EMTB(II,II)
        ENDDO
      ENDIF
C
      JB = NW+NCCK(I)
C
C   Add in contribution from I integrals
C
      WA = ONE
      CALL FINCC(
     + IDISC2, WA, 1, JB, JB, 0, 0, 0, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
C
      KB = NCCK(I)
      KB = K2P(KB)
      IFB = ABS(KB)
      IFB = IFB+IFB
C
C   Add in the contribution from the F integrals
C
      DO JA = 1,NW
C
        KA = NAK(JA)
        IFA = ABS(KA)
        IFA = IFA+IFA
        IQA = IQ(JA,II)
        IF (IQA.EQ.0) GOTO 30
C
        KMX = IFA
        IF (IFA.GT.IFB) KMX = IFB
C
        DO LL = 1,KMX,2
C
          K = LL-1
          IF (IQA.LT.IFA) GOTO 10
C
          IF (K.GT.0) GOTO 20
C
          WA = IQA
          CALL FINCC(
     + IDISC2, WA, 2, JA, JB, 0, 0, K, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
          GOTO 20
C
   10     CONTINUE
          IF (NMCP.EQ.0) GOTO 20
          ILDN = NNLDR(IRS)
          IF (ILDN.EQ.0) GOTO 20
C
          ILDA = NSLDF(IRS)
          ILDB = ILDA+ILDN-1
          ILCX = (K*NW1+JA)*NW1+JB
          ILCY = JA*NW1+JB
C
          DO IL = ILDA,ILDB
            IF (ILCX.EQ.ISLDR(IL) .AND. ILCY.EQ.JSLDR(IL)) THEN
              WA = XSLDR(IL)
              CALL FINCC(
     + IDISC2, WA, 2, JA, JB, 0, 0, K, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
              GOTO 20
            ENDIF
          ENDDO
C
   20     CONTINUE
        ENDDO
   30   CONTINUE
      ENDDO
C
C   Add in the contribution from the G integrals
C
      DO JA = 1,NW
C
        KA = NAK(JA)
        IFA = ABS(KA)
        IFA = IFA+IFA
        IQA = IQ(JA,II)
        IF (IQA.EQ.0) GOTO 60
C
        KMX = (IFA+IFB)/2
        KMN = ABS(IFA-IFB)/2+1
        IF (KA*KB.LT.0) KMN = KMN + 1
C
        DO LL = KMN,KMX,2
C
          K = LL-1
          IF (IQA.LT.IFA) GOTO 40
C
          WA = -IQA*CLRX(KA,K,KB)**2
          CALL FINCC(
     + IDISC2, WA, 3, JA, JB, 0, 0, K, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
          GOTO 50
C
   40     CONTINUE
          IF (NMCP.EQ.0) GOTO 50
          ILDN = NNLDR(IRS)
          IF (ILDN.EQ.0) GOTO 50
C
          ILDA = NSLDF(IRS)
          ILDB = ILDA+ILDN-1
          ILCX = (K*NW1+JA)*NW1+JB
          ILCY = JB*NW1+JA
C
          DO IL = ILDA,ILDB
            IF (ILCX.EQ.ISLDR(IL) .AND. ILCY.EQ.JSLDR(IL)) THEN
              WA = XSLDR(IL)
              CALL FINCC(
     + IDISC2, WA, 3, JA, JB, 0, 0, K, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
              GOTO 50
            ENDIF
          ENDDO
C
   50     CONTINUE
        ENDDO
   60   CONTINUE
      ENDDO
C
      RETURN
C-----------------------------------------------------------------------
C
C *** Form off-diagonal blocks of Hamiltonian ***
C
C-----------------------------------------------------------------------
   70 CONTINUE
C
      IF (IR.GT.IS) THEN
        I = IS
        J = IR
      ELSE
        I = IR
        J = IS
      ENDIF
C
      IRS = (I-1)*(NXX+NXX-I)/2+J
C
C   Set the matrix elements to zero
C
      DO L1 = 1,NRANG2
        DO L2 = 1,NRANG2
          TOTAL(L1,L2) = ZERO
        ENDDO
      ENDDO
C
C   Add in the contribution from the atomic Hamiltonian
C
      IF (ICH.EQ.JCH) THEN
        II = NCCT(I)
        JJ = NCCT(J)
        DO L1 = 1,NRANG2
          TOTAL(L1,L1) = EMTB(II,JJ)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IF (NMCP.EQ.0) GOTO 80
      ILDN = NNLDR(IRS)
      IF (ILDN.EQ.0) GOTO 80
C
      ILDA = NSLDF(IRS)
      ILDB = ILDA+ILDN-1
C
      DO IL = ILDA,ILDB
C
        WA = XSLDR(IL)
        JX = JSLDR(IL)
        JD = MOD(JX,NW1)
        JC = JX/NW1
        JX = ISLDR(IL)
        JB = MOD(JX,NW1)
C
        IF (JB.EQ.0) THEN
C
C   One-electron integrals
C
          IF (JC.LE.NW .AND. JD.LE.NW) THEN
C
C       Bound-bound
C
            CALL LOCBB(
     + 0, 0, K1S, K2S, RKSTO, ICTXB, IRK1, IRK2, ISTX1B, ISTX2B,
     + ISTXB, ITC, IWRITE, KBBN1, KBBN2, KBBR, KBMAX)
            CALL FINBB(
     + WB, 1, JC, JD, 0, 0, 0, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
            WX = WA*WB
            DO L1 = 1,NRANG2
              TOTAL(L1,L1) = TOTAL(L1,L1)+WX
            ENDDO
C
          ELSE
C
C       Continuum-continuum
C
            CALL FINCC(
     + IDISC2, WA, 1, JC, JD, 0, 0, 0, TOTAL, ITC, IWRITE, K1S, K2S,
     + KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL, MAXNQN, MAXP,
     + MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
          ENDIF
C
        ELSE
C
C   Slater integrals
C
          JX = JX/NW1
          JA = MOD(JX,NW1)
          JK = JX/NW1
C
          IF (JB.LE.NW) THEN
C
C       Bound-bound
C
            CALL LOCBB(
     + 0, 0, K1S, K2S, RKSTO, ICTXB, IRK1, IRK2, ISTX1B, ISTX2B,
     + ISTXB, ITC, IWRITE, KBBN1, KBBN2, KBBR, KBMAX)
            CALL FINBB(
     + WB, 4, JA, JB, JC, JD, JK, ICTXB, IRK1, ISTX1B, ISTX2B, ISTXB,
     + ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP, RKSTO)
            WX = WA*WB
            DO L1 = 1,NRANG2
              TOTAL(L1,L1) = TOTAL(L1,L1)+WX
            ENDDO
C
          ELSE
C
C       Continuum-continuum
C
            IF (IR.GT.IS) THEN
              IF (JC.LE.NW) THEN
                NS = JA
                JA = JC
                JC = NS
                NS = JB
                JB = JD
                JD = NS
              ELSE
                NS = JA
                JA = JD
                JD = NS
                NS = JB
                JB = JC
                JC = NS
              ENDIF
            ENDIF
C
            IF (JC.GT.NW) THEN
              NS = JB
              JB = JD
              JD = NS
              NS = JA
              JA = JB
              JB = NS
              NS = JC
              JC = JD
              JD = NS
            ENDIF
C
            CALL FINCC(
     + IDISC2, WA, 4, JA, JB, JC, JD, JK, TOTAL, ITC, IWRITE,
     + K1S, K2S, KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS, MAXFUL,
     + MAXNQN, MAXP, MINNQN, NAK, NAKK, NH, NP, NRANG2, NW, RKSTO)
C
          ENDIF
C
        ENDIF
      ENDDO
C
      RETURN
C-----------------------------------------------------------------------
   80 CONTINUE
      IF (ICH.NE.JCH) MX = 0
C-----------------------------------------------------------------------
      END
CEND--------------------------------------------------------------------
CEND    CMATY.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CMATY(IDISC2,NXX,I,TOTAL,ITC, IWRITE, KBMAX, KCMAX, KCM!
     +IN, MAXFUL, MAXNQN, MINNQN,NAK, NAKK, NCFCON, NCFGP, NH, NP, NRANG!
     +2, NW, NW1,K1S, K2S, KPOS, MAXP,KBCN1, KBCN2, KBCR, RKSTO,ISLDR, J!
     +SLDR, NNLDR, NSLDF, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CMATY.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ITC
C   IWRITE
C   KBMAX
C   KCMAX
C   KCMIN
C   MAXFUL
C   MAXNQN
C   MINNQN
C   NAK
C   NAKK
C   NCFCON
C   NCFGP
C   NH
C   NP
C   NRANG2
C   NW
C   NW1
C   K1S
C   K2S
C   KPOS
C   MAXP
C   KBCN1
C   KBCN2
C   KBCR
C   RKSTO
C   ISLDR
C   JSLDR
C   NNLDR
C   NSLDF
C   XSLDR
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
C
C  Argument variables
C
      DOUBLE PRECISION TOTAL(MXNB,*)
      INTEGER I,IDISC2,NXX
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER ISLDR(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JSLDR(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER KBCN1(*)
      INTEGER KBCN2(*)
      INTEGER KBCR(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KPOS
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NSLDF(*)
      INTEGER NW
      INTEGER NW1
C
C  Local variables
C
      DOUBLE PRECISION WA
      INTEGER IL,ILDA,ILDB,ILDN
      INTEGER IRS,J,JA,JB
      INTEGER JC,JD,JJ,JK
      INTEGER JX,L1,L2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO L1 = 1,NRANG2
        DO L2 = 1,NCFGP
          TOTAL(L1,L2) = ZERO
        ENDDO
      ENDDO
C
      DO JJ = 1,NCFGP
        J = NCFCON+JJ
        IRS = (I-1)*(NXX+NXX-I)/2+J
C
        ILDN = NNLDR(IRS)
        IF (ILDN.GT.0) THEN
          ILDA = NSLDF(IRS)
          ILDB = ILDA+ILDN-1
C
          DO IL = ILDA,ILDB
            WA = XSLDR(IL)
            JX = JSLDR(IL)
            JD = MOD(JX,NW1)
            JC = JX/NW1
            JX = ISLDR(IL)
            JB = MOD(JX,NW1)
C
            IF (JB.EQ.0) THEN
              CALL FINBC(IDISC2,JJ,WA,1,JD,JC,0,0,0,TOTAL,ITC, IWRITE, K!
     +1S, K2S, KBCN1, KBCN2, KBCR,KBMAX, KCMAX, KCMIN, KPOS, MAXFUL, MAX!
     +NQN,MAXP, MINNQN, NAK, NAKK, NH, NP, NRANG2,NW, RKSTO)
            ELSE
              JX = JX/NW1
              JA = MOD(JX,NW1)
              JK = JX/NW1
              CALL FINBC(IDISC2,JJ,WA,2,JA,JD,JC,JB,JK,TOTAL,ITC, IWRITE!
     +, K1S, K2S, KBCN1, KBCN2, KBCR,KBMAX, KCMAX, KCMIN, KPOS, MAXFUL, !
     +MAXNQN,MAXP, MINNQN, NAK, NAKK, NH, NP, NRANG2,NW, RKSTO)
            ENDIF
C
          ENDDO
        ENDIF
C
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    CONT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CONT(IDISC1,IDISC3,MX,EMTR, IBUG1, IBUG2, IBUG3, IBUG4,!
     + IBUG5, IBUG6, ICFCON, ICFCOR,ICHOP, IDMTST, IEXC, IICHOP, IIQ, II!
     +SPAR, IITJPO, IJ2P1,IJCUP, IJQS, INAK, INAKK, INCCP, INCF, INCHAN,!
     + INH, INTARG,IPOLPH, IQ, ISPAR, ITAB, ITC, ITJPO, IWRITE, J2P1, JC!
     +UP,JLAST, JLASTX, JQS, JTAB, K2P, KCMAX, KCMIN, NAK, NAKK,NAST, NC!
     +ASES, NCCA, NCCK, NCCN, NCCP, NCCT, NCF, NCFCON,NCFGP, NCHAN, NDIM!
     +AX, NH, NHC, NP, NPTY, NROWS, NTAB, NTARG, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CONT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   EMTR
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICFCON
C   ICFCOR
C   ICHOP
C   IDMTST
C   IEXC
C   IICHOP
C   IIQ
C   IISPAR
C   IITJPO
C   IJ2P1
C   IJCUP
C   IJQS
C   INAK
C   INAKK
C   INCCP
C   INCF
C   INCHAN
C   INH
C   INTARG
C   IPOLPH
C   IQ
C   ISPAR
C   ITAB
C   ITC
C   ITJPO
C   IWRITE
C   J2P1
C   JCUP
C   JLAST
C   JLASTX
C   JQS
C   JTAB
C   K2P
C   KCMAX
C   KCMIN
C   NAK
C   NAKK
C   NAST
C   NCASES
C   NCCA
C   NCCK
C   NCCN
C   NCCP
C   NCCT
C   NCF
C   NCFCON
C   NCFGP
C   NCHAN
C   NDIMAX
C   NH
C   NHC
C   NP
C   NPTY
C   NROWS
C   NTAB
C   NTARG
C   NW
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Statement functions
C
      LOGICAL ITRG
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C  Argument variables
C
      INTEGER IDISC1,IDISC3,MX
C
      CHARACTER*2 INH(*)
      CHARACTER*2 NH(*)
      CHARACTER*2 NHC(*)
      DOUBLE PRECISION EMTR(MXNL,*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IEXC
      INTEGER IICHOP(MXNW,*)
      INTEGER IIQ(MXNW,*)
      INTEGER IISPAR(*)
      INTEGER IITJPO(*)
      INTEGER IJ2P1
      INTEGER IJCUP(10,*)
      INTEGER IJQS(3,MXNW,*)
      INTEGER INAK(*)
      INTEGER INAKK(*)
      INTEGER INCCP(MXNL,*)
      INTEGER INCF
      INTEGER INCHAN
      INTEGER INTARG(*)
      INTEGER IPOLPH
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JCUP(10,*)
      INTEGER JLAST
      INTEGER JLASTX
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER K2P(*)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NAST
      INTEGER NCASES
      INTEGER NCCA(MXNC,*)
      INTEGER NCCK(*)
      INTEGER NCCN(*)
      INTEGER NCCP(MXNL,*)
      INTEGER NCCT(*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(*)
      INTEGER NP(*)
      INTEGER NPTY
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NTARG(*)
      INTEGER NW
C
C  Local variables
C
      CHARACTER*2 LAB(22)
      DOUBLE PRECISION AMAX
      INTEGER I,IA,IPAR,ITEST
      INTEGER ITESTX,J,J1,J2
      INTEGER J3,JAG,JCHANX,JLEV
      INTEGER JT,K,K1,KAPA
      INTEGER KAPAJ,KJ,KXMAX,KXMIN
      INTEGER LAG,MK,NCCNK,NCHAN1
      INTEGER NJ,NL
C
      INTEGER IQTAR(MXNW,MXNC)
      INTEGER NCFTAR
      INTEGER NICHOP(MXNW,MXNC)
      INTEGER NISPAR(MXNC)
      INTEGER NITJPO(MXNC)
      INTEGER NJCUP(10,MXNC)
      INTEGER NJQS(3,MXNW,MXNC)
      INTEGER NWTAR
C
C  Save the TARGET data
C
      SAVE IQTAR
      SAVE NCFTAR
      SAVE NICHOP
      SAVE NISPAR
      SAVE NITJPO
      SAVE NJCUP
      SAVE NJQS
      SAVE NWTAR
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C   This function tests the triangular condition.
C   The arguments are 2J.
C
      ITRG(J1,J2,J3) = ABS(J1-J2).LE.J3.AND.J3.LE.J1+J2.AND.MOD(J1+J2+J3!
     +,2).EQ.0
C
      DATA LAB/'s ','p-','p ','d-','d ','f-','f ','g-','g ','h-','h ',  !
     +   'i-','i ','j-','j ','k-','k ','l-','l ','m-','m ','  '/
C-----------------------------------------------------------------------
      MX = 0
      WRITE (IWRITE,3000)
C-----------------------------------------------------------------------
C
C   Determine the channels allowed in this symmetry.
C
C   NCHAN    number of channels
C   NTARG(I) target level for channel I
C   K2P(I)   kappa-value for channel I
C   NAKK(I)  K-value for channel I
C
C   K2P  : -1, 1,-2, 2,-3, 3, ...
C   NAKK :  1, 2, 3, 4, 5, 6, ...
C
      NCHAN = 0
      NCHAN1 = 0
      KXMIN = -1
      KXMAX = -1
C
      DO J = 1,NAST
C
C   For each target level J determine the 2J value and parity.
C
        IA = 0
        AMAX = ZERO
        DO I = 1,NCF
          IF (ABS(EMTR(I,J)).GT.AMAX) THEN
            AMAX = ABS(EMTR(I,J))
            IA = I
          ENDIF
        ENDDO
C
        JLEV = ITJPO(IA)-1
C
        IF (ISPAR(IA).LT.0) THEN
          ITEST = 1
        ELSE
          ITEST = 0
        ENDIF
C
C   Examine the kappa values.
C
C   J2P1 is 2J+1 for the continnum state
C   JLEV is 2J for the target state
C   JAG  is 2J for the continuum orbital
C
        K1 = J2P1+JLEV
        DO K = 1,K1
C
          IF (MOD(K,2).EQ.1) THEN
            KAPA = -(K+1)/2
          ELSE
            KAPA = K/2
          ENDIF
C
          MK = ABS(KAPA)
          JAG = MK+MK-1
C
C  Test the triangular condition.
C
          IF (ITRG(JLEV,JAG,J2P1-1)) THEN
C
            LAG = MK+(SIGN(1,KAPA)-1)/2
            ITESTX = ITEST+LAG
C
            IF (MOD(ITESTX,2).NE.0) THEN
              IPAR = -1
            ELSE
              IPAR = 1
            ENDIF
C
C  Test the parity condition.
C
            IF (IPAR.EQ.NPTY) THEN
C
              IF (KXMIN.LT.0) THEN
                KXMIN = K
                KXMAX = K
              ELSE
                IF (K.GT.KXMAX) THEN
                  KXMAX = K
                ELSE
                  IF (K.LT.KXMIN) THEN
                    KXMIN = K
                  ENDIF
                ENDIF
              ENDIF
C
              IF (K.GT.KCMAX .OR. K.LT.KCMIN) THEN
C
C  This case cannot be done due to DSTG1 dump having insufficient
C  continuum angular momenta. Mark JLAST and JLASTX.
C  These are set to -2. This indicates that previous results cannot be
C  reused and also the present results.
C
C  JLAST  - reuse asymptotic coefficients
C  JLASTX - reuse continuum-continuum direct Slater angular integrals
C
                NCHAN1 = NCHAN1+1
                WRITE (IWRITE,3080) J,KAPA
                JLAST = -2
                JLASTX = -2
C
              ELSE
C
                NCHAN = NCHAN+1
C
                IF (NCHAN.LE.IDMTST(1)) THEN
                  NTARG(NCHAN) = J
                  K2P(NCHAN) = KAPA
                  NAKK(NCHAN) = K
                  IF (K.GT.21) THEN
                    NHC(NCHAN) = LAB(22)
                  ELSE
                    NHC(NCHAN) = LAB(K)
                  ENDIF
                ENDIF
C
              ENDIF
C
            ENDIF
C
          ENDIF
C
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      IF (NCHAN1.GT.0) WRITE (IWRITE,3010) NCHAN1
      WRITE (IWRITE,3020) KXMIN,KXMAX
C
      IF (NCHAN.EQ.0) THEN
        MX = 1
        WRITE (IWRITE,3070)
        RETURN
      ENDIF
C
      CALL DMCHK2(01,NCHAN, IWRITE, IDMTST, NDIMAX)
C
C   Write out the data defining the channels.
C
      JCHANX = J2P1-1
C
      IF (MOD(JCHANX,2).EQ.0) THEN
        JCHANX = JCHANX/2
        IF (NPTY.EQ.1) THEN
          WRITE (IWRITE,3030) NCHAN,JCHANX
        ELSE
          WRITE (IWRITE,3040) NCHAN,JCHANX
        ENDIF
      ELSE
        IF (NPTY.EQ.1) THEN
          WRITE (IWRITE,3090) NCHAN,JCHANX
        ELSE
          WRITE (IWRITE,3100) NCHAN,JCHANX
        ENDIF
      ENDIF
C
      WRITE (IWRITE,3110)
      DO I = 1,NCHAN
        K = K2P(I)
        MK = ABS(K)
        NJ = MK+MK-1
        NL = MK+(SIGN(1,K)-1)/2
        WRITE (IWRITE,3050) I,NTARG(I),NHC(I),K,NJ,NL
      ENDDO
C-----------------------------------------------------------------------
C
C Determine the continuum CSFs.
C
C NCFCON                  number of continuum CSFs
C NCCT(I)                 target CSF for continuum CSF I
C NCCK(I)                 channel for continuum CSF I
C NCCP(I,J)               continuum CSF for channel J and target CSF I
C NCCN(I)                 number of channels connected to cont. CSF I
C (NCCA(I,J),J=1,NCCN(I)) list of the channels connected to cont. CSF I
C
C-----------------------------------------------------------------------
      DO I = 1,NCF
        DO J = 1,NCHAN
          NCCP(I,J) = 0
        ENDDO
      ENDDO
C
      NCFCON = 0
      DO J = 1,NCHAN
        JT = NTARG(J)
        KAPAJ = K2P(J)
C
        DO I = 1,NCF
          IF (ABS(EMTR(I,JT)).LT.EPS10) GOTO 20
C-----------------------------------------------------------------------
C
C  Option 47 set -
C    there are distinct continuum CSFs for each channel.
C    The default is to reuse the continuum CSFs for
C    different channels. Setting this option will
C    increase the number of CSFs.
C
C-----------------------------------------------------------------------
          IF (ITC(47).EQ.1) GOTO 10
          IF (NCFCON.EQ.0) GOTO 10
C
          DO K = 1,NCFCON
            KJ = NCCK(K)
            KAPA = K2P(KJ)
            IF (NCCT(K).EQ.I .AND. KAPAJ.EQ.KAPA) THEN
              NCCP(I,J) = K
              NCCNK = NCCN(K)+1
              NCCN(K) = NCCNK
              NCCA(K,NCCNK) = J
              GOTO 20
            ENDIF
          ENDDO
C
   10     CONTINUE
          NCFCON = NCFCON+1
          CALL DMCHK2(07,NCFCON, IWRITE, IDMTST, NDIMAX)
          NCCP(I,J) = NCFCON
          NCCN(NCFCON) = 1
          NCCA(NCFCON,1) = J
          NCCT(NCFCON) = I
          NCCK(NCFCON) = J
C
   20     CONTINUE
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      WRITE (IWRITE,3060) NCFCON
C
C   store the target information
C
      IF (NCASES.EQ.1) CALL ANGLB1(IWRITE,ICHOP, IQ, ISPAR, ITJPO, JCUP,!
     + JQS, NCF, NW,NICHOP, IQTAR, NISPAR, NITJPO, NJCUP, NJQS, NCFTAR, !
     +NWTAR)
C
C   evaluate asymptotic angular coefficients
C
      REWIND IDISC1
      CALL ANGULA(IDISC1,IDISC3,IBUG1, IBUG2, IBUG3, IBUG4, IBUG6, ICHOP!
     +, IQ, ISPAR, ITAB, ITC,ITJPO, IWRITE, J2P1, JCUP, JLAST, JQS, JTAB!
     +, K2P, NAK,NCASES, NCCK, NCCT, NCF, NCFCON, NH, NP, NTAB, NW)
C
C   set up arrays for evaluating continuum MCP coefficients
C   calculate the angular coefficients for the Hamiltonian
C
      CALL ANGULH(IDISC1,IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6, ICHOP!
     +, IDMTST, IEXC,IQ, ISPAR, ITAB, ITC, ITJPO, IWRITE, J2P1, JCUP, JL!
     +ASTX,JQS, JTAB, K2P, NAK, NCCK, NCCT, NCF, NCFCON, NCFGP, NCHAN,ND!
     +IMAX, NH, NHC, NP, NPTY, NROWS, NTAB, NW)
C
C   code for photo-ionisation
C
      IF (IPOLPH.EQ.2) THEN
        IF (NCASES.EQ.1) THEN
          CALL ANGLD1(ICFCON, ICFCOR, ICHOP, IICHOP, IIQ, IISPAR, IITJPO!
     +, IJ2P1,IJCUP, IJQS, INAK, INAKK, INCCP, INCF, INCHAN, INH, INTARG!
     +,IQ, ISPAR, ITJPO, IWRITE, J2P1, JCUP, JQS, NAK, NAKK,NCCP, NCF, N!
     +CFCON, NCFGP, NCFTAR, NCHAN, NH, NTARG, NW)
        ELSE
          CALL ANGULD(IDISC1,IBUG1, IBUG2, IBUG3, IBUG4, IBUG6,ICFCON, I!
     +CFCOR, ICHOP, IDMTST, IICHOP, IIQ, IISPAR, IITJPO,IJCUP, IJQS, INA!
     +K, INCF, INCHAN, INH, IQ, ISPAR, ITAB, ITC,ITJPO, IWRITE, JCUP, JQ!
     +S, JTAB, NAK, NCF, NCFCON, NCFGP,NCHAN, NDIMAX, NH, NP, NTAB, NW, !
     +NWTAR)
        ENDIF
      ENDIF
C
C   set up the target information again and position stream IDISC1
C
      CALL ANGLB2(IWRITE,ICHOP, IQ, ISPAR, ITJPO, JCUP, JQS, NCF, NW,NCF!
     +CON, NCFGP,NICHOP, IQTAR, NISPAR, NITJPO, NJCUP, NJQS, NCFTAR, NWT!
     +AR)
      REWIND IDISC1
C-----------------------------------------------------------------------
 3000 FORMAT (/31X,'Routine CONT'/31X,'------------')
 3010 FORMAT (/1X,' The number of channels excluded was ',I5)
 3020 FORMAT (/1X,' The K-value ranges from ',I4,' to ',I4)
 3030 FORMAT (/1X,I5,' channels generated with  J = ',I2,               !
     +' and even parity')
 3040 FORMAT (/1X,I5,' channels generated with  J = ',I2,               !
     +' and odd parity')
 3050 FORMAT (1X,I4,I8,8X,A2,2I4,'/2',I4)
 3060 FORMAT (/1X,I5,' continuum CSFs generated')
 3070 FORMAT (/' ***********************************************'/      !
     +' ***          WARNING from CONT              ***'/               !
     +' *** No channels generated for this symmetry ***'/               !
     +' ***********************************************')
 3080 FORMAT (/                                                         !
     +' ***********************************************************'/   !
     +' ***              WARNING from CONT                      ***'/   !
     +' *** Channel with level ',I4,' and kappa ',I4,' not included ***'!
     +/' ***********************************************************')
 3090 FORMAT (/1X,I5,' channels generated with  J = ',I2,               !
     +'/2 and even parity')
 3100 FORMAT (/1X,I5,' channels generated with  J = ',I2,               !
     +'/2 and odd parity')
 3110 FORMAT (/10X,'target'/10X,'state ',10X,'K',3X,'J',5X,'L'/)
      END
CEND--------------------------------------------------------------------
CEND    COR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE COR(JA1,JB1,JA2,JB2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWR!
     +ITE, JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC!
     +2, JJQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/COR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   JA1      ...
C   JB1      ...
C   JA2      ...
C   JB2      ...
C   IBUG1
C   IBUG2
C   IBUG3
C   IEXC
C   IME
C   IWRITE
C   JA
C   JB
C   NAK
C   NH
C   NOUTX
C   NP
C   NW
C   NWA
C   ITAB
C   JTAB
C   NTAB
C   JJC1
C   JJC2
C   JJQ1
C   JJQ2
C   JLIST
C   NPEEL
C   NQ1
C   NQ2
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL CRE,OCON
      DOUBLE PRECISION CRE,OCON
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M3MNGM
      PARAMETER (M3MNGM=3*MANGM)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER IDIM
      PARAMETER (IDIM=20)
      INTEGER MANGMP
      PARAMETER (MANGMP = 2*(MANGM/3))
      INTEGER MSUM
      PARAMETER (MSUM = 10)
C
C  Argument variables
C
      INTEGER JA1,JA2,JB1,JB2
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IEXC
      INTEGER IME
      INTEGER ITAB(*)
      INTEGER IWRITE
      INTEGER JA
      INTEGER JB
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NOUTX
      INTEGER NP(*)
      INTEGER NPEEL
      INTEGER NQ1(*)
      INTEGER NQ2(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWA
C
C  Local variables
C
      DOUBLE PRECISION BOND(12,IDIM),BONE(12,IDIM)
      DOUBLE PRECISION COND(IDIM),CONE(IDIM),CONST,CRED
      DOUBLE PRECISION CREE,PROD,PRODD,PRODE
      DOUBLE PRECISION RECUP,S(12),X,XCODE
      INTEGER IBREIT
      INTEGER ICOUL
      INTEGER I,IA1,IA2,IB1
      INTEGER IB2,IBRD,IBRE
      INTEGER II,III,ILS(4)
      INTEGER IMUD,IMUE
      INTEGER IROWS(4),IS(4),ISNJ
      INTEGER IT11,IT12,IT13,IT14
      INTEGER IT2,IT3,ITKMO,ITYPE
      INTEGER J
      INTEGER JD6(M3MNGM),JD6C
      INTEGER JD7(M3MNGM),JD7C
      INTEGER JD8(M3MNGM),JD8C
      INTEGER JDW(6,M6J),JDWC
      INTEGER JE6(M3MNGM),JE6C
      INTEGER JE7(M3MNGM),JE7C
      INTEGER JE8(M3MNGM),JE8C
      INTEGER JEW(6,M6J),JEWC
      INTEGER JJ,JS(4)
      INTEGER K,KAP1,KAP2,KAP3
      INTEGER KAP4,KAPS(4),KBD1,KBD2
      INTEGER KBE1,KBE2,KCD1,KCD2
      INTEGER KCE1,KCE2
      INTEGER KJ23,KK,KS(4)
      INTEGER LA1,LA2,LB1,LB2
      INTEGER LLS(4),LLS1,LLS2,LLS3
      INTEGER LLS4,LS1,LS2,LS3
      INTEGER LS4,MU
      INTEGER NBRJ,NCODE
      INTEGER NN,NQS(4)
      INTEGER NU,NUD,NUP1
      LOGICAL FAILD,FAILE
      INTEGER INVD6J(M6J)
      INTEGER INVE6J(M6J)
      INTEGER JD6P(MANGMP)
      INTEGER JD7P(MANGMP)
      INTEGER JD8P(MANGMP)
      INTEGER JD9(MANGMP)
      INTEGER JD9C
      INTEGER JD9P(MANGMP)
      INTEGER JDDEL
      INTEGER JDSUM4(MTRIAD,M6J)
      INTEGER JDSUM5(MTRIAD,M6J)
      INTEGER JDSUM6(MTRIAD)
      INTEGER JDWORD(6,M6J)
      INTEGER JE6P(MANGMP)
      INTEGER JE7P(MANGMP)
      INTEGER JE8P(MANGMP)
      INTEGER JE9(MANGMP)
      INTEGER JE9C
      INTEGER JE9P(MANGMP)
      INTEGER JEDEL
      INTEGER JESUM4(MTRIAD,M6J)
      INTEGER JESUM5(MTRIAD,M6J)
      INTEGER JESUM6(MTRIAD)
      INTEGER JEWORD(6,M6J)
      INTEGER KD6CP(MSUM)
      INTEGER KD7CP(MSUM)
      INTEGER KD8CP(MSUM)
      INTEGER KD9CP(MSUM)
      INTEGER KE6CP(MSUM)
      INTEGER KE7CP(MSUM)
      INTEGER KE8CP(MSUM)
      INTEGER KE9CP(MSUM)
      INTEGER LDDEL(M6J,2)
      INTEGER LEDEL(M6J,2)
      INTEGER MDP
      INTEGER MEP
      INTEGER NDB6J(MSUM)
      INTEGER NDBJ(MSUM)
      INTEGER NDLSUM
      INTEGER NEB6J(MSUM)
      INTEGER NEBJ(MSUM)
      INTEGER NELSUM
      INTEGER J2S(MTRIAD,3)
      INTEGER J3S(MTRIAD,3)
      INTEGER JBQ1(3,MXNW)
      INTEGER JBQ2(3,MXNW)
      INTEGER JTQ1(3)
      INTEGER JTQ2(3)
C
      INTEGER J1(MANGM)
      INTEGER J2(MTRIAD,3)
      INTEGER J3(MTRIAD,3)
      INTEGER MMOM
      INTEGER NMOM
      LOGICAL FREE(MANGM)
C
      INTEGER J6(M3MNGM)
      INTEGER J6C
      INTEGER J7(M3MNGM)
      INTEGER J7C
      INTEGER J8(M3MNGM)
      INTEGER J8C
      INTEGER J9(MANGMP)
      INTEGER J9C
      INTEGER JDEL
      INTEGER JW(6,M6J)
      INTEGER JWC
      INTEGER LDEL(M6J,2)
      INTEGER MP
      INTEGER INV6J(M6J)
      INTEGER J6P(MANGMP)
      INTEGER J7P(MANGMP)
      INTEGER J8P(MANGMP)
      INTEGER J9P(MANGMP)
      INTEGER JSUM4(MTRIAD,M6J)
      INTEGER JSUM5(MTRIAD,M6J)
      INTEGER JSUM6(MTRIAD)
      INTEGER JWORD(6,M6J)
      INTEGER K6CP(MSUM)
      INTEGER K7CP(MSUM)
      INTEGER K8CP(MSUM)
      INTEGER K9CP(MSUM)
      INTEGER NB6J(MSUM)
      INTEGER NBJ(MSUM)
      INTEGER NLSUM
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER I1,I2,I3
      LOGICAL ITRIG
      ITRIG(I1,I2,I3) = I1.GE.(ABS(I2-I3)+1).AND.I1.LE.(I2+I3-1)
C-----------------------------------------------------------------------
C
C  Initialise pointers and flags and set any tables required.
C
C  The array IS points to the full list of orbitals,
C  the array JS to the array, JLIST, of peel orbital pointers.
C
C  Initialisation.
C
C-----------------------------------------------------------------------
      ICOUL = 1
      IBREIT = 0
C
      JS(1) = JA1
      JS(2) = JB1
      JS(3) = JA2
      JS(4) = JB2
C
      DO I = 1,4
        IS(I) = JLIST(JS(I))
        KAPS(I) = 2*NAK(IS(I))
        KS(I) = ABS(KAPS(I))
      ENDDO
C
      IA1 = IS(1)
      IB1 = IS(2)
      IA2 = IS(3)
      IB2 = IS(4)
C
      KJ23 = 0
      ISNJ = 0
C
      FAILD = .FALSE.
      FAILE = .FALSE.
C-----------------------------------------------------------------------
C
C  Initialise arrays.
C
      DO J = 1,NW
        DO K = 1,3
          JBQ1(K,J) = 0
          JBQ2(K,J) = 0
        ENDDO
      ENDDO
C
      NBRJ = 3*NPEEL+7
      DO I=1,NBRJ-1
        FREE(I)=.FALSE.
      ENDDO
      FREE(NBRJ)=.TRUE.
C-----------------------------------------------------------------------
C
C  Set tables of quantum numbers of spectator shells.
C
      DO JJ = 1,NPEEL
C
        J = JLIST(JJ)
        IF ((J.EQ.IA1) .OR. (J.EQ.IB1)) GOTO 10
        DO K = 1,3
          JBQ1(K,J) = JJQ1(K,J)
        ENDDO
   10   CONTINUE
        IF ((J.EQ.IA2) .OR. (J.EQ.IB2)) GOTO 20
        DO K = 1,3
          JBQ2(K,J) = JJQ2(K,J)
        ENDDO
C
C  Examine quantum numbers of spectator shells for orthogonality
C  and exit if found.
C
        IF (J.EQ.IA1 .OR. J.EQ.IB1 .OR. J.EQ.IA2 .OR.J.EQ.IB2) GOTO 20
C
        DO K = 1,3
          IF (JBQ1(K,J).NE.JBQ2(K,J)) THEN
            IF (IBUG2.EQ.1) WRITE (IWRITE,3000)
            RETURN
          ENDIF
        ENDDO
C
   20   CONTINUE
      ENDDO
C
      IF (IBUG2.EQ.1) WRITE (IWRITE,3010) IA1,IB1,IA2,IB2
C-----------------------------------------------------------------------
C
C  Set range of the parameter K for Coulomb integrals.
C  Terminate run if buffer store dimension IDIM is too small.
C
      IF (ICOUL.EQ.1) THEN
        CALL SKRC(IS,KAPS,KS,KCD1,KCD2,KCE1,KCE2)
        IF (IEXC.EQ.-1) THEN
          KCD2 = 0
        ELSE
          IF (IEXC.EQ.1) THEN
            KCE2 = 0
          ENDIF
        ENDIF
      ELSE
        KCD1 = 0
        KCD2 = 0
        KCE1 = 0
        KCE2 = 0
      ENDIF
C
      IF (IBUG2.EQ.1) WRITE (IWRITE,3020) KCD1,KCD2,KCE1,KCE2
C
      IF (KCD2.GT.IDIM .OR. KCE2.GT.IDIM) THEN
        KK = MAX(KCD2,KCE2)
        WRITE (IWRITE,3030) KK
        STOP
      ENDIF
C
      IF (KCD2.GT.0) THEN
        DO K = 1,KCD2
          COND(K) = ZERO
        ENDDO
      ENDIF
C
      IF (KCE2.GT.0) THEN
        DO K = 1,KCE2
          CONE(K) = ZERO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C  Set range of the parameter K for Breit integrals.
C  Terminate run if buffer store dimension IDIM is too small.
C
      IF (IBREIT.EQ.1) THEN
        CALL SNRC(IS,KAPS,KS,KBD1,KBD2,KBE1,KBE2,IBRD,IBRE)
      ELSE
        KBD1 = 0
        KBD2 = 0
        KBE1 = 0
        KBE2 = 0
        IBRD = -1
        IBRE = -1
      ENDIF
C
      IF (IBUG2.EQ.1) WRITE (IWRITE,3040) KBD1,KBD2,KBE1,KBE2,IBRD,IBRE
C
      IF (KCD2.EQ.0 .AND. KCE2.EQ.0 .AND. IBRD.LT.0 .AND.IBRE.LT.0) RETU!
     +RN
C
      IF (KBD2.GT.IDIM .OR. KBE2.GT.IDIM) THEN
        KK = MAX(KBD2,KBE2)
        WRITE (IWRITE,3050) KK
        STOP
      ENDIF
C
      IF (IBRD.GE.0) THEN
        DO K = 1,KBD2
          DO MU = 1,12
            BOND(MU,K) = ZERO
          ENDDO
        ENDDO
      ENDIF
C
      IF (IBRE.GE.0) THEN
        DO K = 1,KBE2
          DO MU = 1,12
            BONE(MU,K) = ZERO
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      NQS(1) = NQ1(IA1)
      NQS(2) = NQ1(IB1)
      NQS(3) = NQ2(IA2)
      NQS(4) = NQ2(IB2)
C-----------------------------------------------------------------------
C
C  Set parameters of summation over parent (barred) terms in eq.(5).
C  The array IROWS is formed to point at the list of allowed
C  parents of active shells in the array NTAB.
C
      CALL LTAB(IS,NQS,KS,IROWS,IWRITE)
C
      DO I = 1,4
        II = IROWS(I)
        LLS(I) = ITAB(II)
        ILS(I) = JTAB(II)
      ENDDO
C
      IF (IBUG2.EQ.1) THEN
        WRITE (IWRITE,3060) (NQS(I),I=1,4)
        WRITE (IWRITE,3070) (LLS(I),I=1,4)
        WRITE (IWRITE,3080) (ILS(I),I=1,4)
      ENDIF
C-----------------------------------------------------------------------
C
C  Sum contributions over all parent terms permitted by angular
C  momentum and seniority selection rules.
C
      LLS1 = LLS(1)
      IF (LLS1.NE.1) FREE(JA1)=.TRUE.
      LLS2 = LLS(2)
      LLS3 = LLS(3)
      LLS4 = LLS(4)
C
      LS2 = ILS(2)
      DO LB1 = 1,LLS2
        LS2 = LS2+3
        IT12 = NTAB(LS2)
        IT2 = KS(2)
        IT3 = JJQ1(3,IB1)
        IF (.NOT.ITRIG(IT12,IT2,IT3)) GOTO 140
        IF (ABS(NTAB(LS2-2)-JJQ1(1,IB1)).NE.1) GOTO 140
C
        LS1 = ILS(1)
        DO LA1 = 1,LLS1
          LS1 = LS1+3
          IT11 = NTAB(LS1)
          IT2 = KS(1)
C
          IF (IA1.EQ.IB1) THEN
            IT3 = IT12
            IF (.NOT.ITRIG(IT11,IT2,IT3)) GOTO 130
            IF (ABS(NTAB(LS1-2)-NTAB(LS2-2)).NE.1) GOTO 130
      IF (LLS2.NE.1) FREE(NBRJ-8)=.TRUE.
          ELSE
            IT3 = JJQ1(3,IA1)
            IF (.NOT.ITRIG(IT11,IT2,IT3)) GOTO 130
            IF (ABS(NTAB(LS1-2)-JJQ1(1,IA1)).NE.1) GOTO 130
      IF (LLS2.NE.1) FREE(JB1)=.TRUE.
          ENDIF
C
          LS4 = ILS(4)
          DO LB2 = 1,LLS4
            LS4 = LS4+3
            IT14 = NTAB(LS4)
            IT2 = KS(4)
            IT3 = JJQ2(3,IB2)
            IF (.NOT.ITRIG(IT14,IT2,IT3)) GOTO 120
            IF (ABS(NTAB(LS4-2)-JJQ2(1,IB2)).NE.1) GOTO 120
C
            LS3 = ILS(3)
            DO LA2 = 1,LLS3
              LS3 = LS3+3
              IT13 = NTAB(LS3)
              IT2 = KS(3)
C
              IF (IA2.EQ.IB2) THEN
                IT3 = IT14
                IF (.NOT.ITRIG(IT13,IT2,IT3)) GOTO 110
                IF (ABS(NTAB(LS3-2)-NTAB(LS4-2)).NE.1) GOTO 110
      IF (LLS4.NE.1) FREE(NBRJ-6)=.TRUE.
              ELSE
                IT3 = JJQ2(3,IA2)
                IF (.NOT.ITRIG(IT13,IT2,IT3)) GOTO 110
                IF (ABS(NTAB(LS3-2)-JJQ2(1,IA2)).NE.1) GOTO 110
              ENDIF
C
              IF (IBUG2.EQ.1) WRITE (IWRITE,3090) LS1,LS2,LS3,LS4
C-----------------------------------------------------------------------
C
C  The current parent has been completely defined, and its quantum
C  numbers can now be set. The JTQ arrays must be set if IA1=IB1 or
C  IA2=IB2. The matrix element should be diagonal in barred quantum
C  numbers.
C
C-----------------------------------------------------------------------
              DO K = 1,3
C
                JBQ1(K,IA1) = NTAB(LS1+K-3)
                JBQ2(K,IA2) = NTAB(LS3+K-3)
C
                JTQ1(K) = 0
C
                IF (IB1.EQ.IA1) THEN
                  JTQ1(K) = NTAB(LS2+K-3)
                ELSE
                  JBQ1(K,IB1) = NTAB(LS2+K-3)
                ENDIF
C
                JTQ2(K) = 0
C
                IF (IB2.EQ.IA2) THEN
                  JTQ2(K) = NTAB(LS4+K-3)
                ELSE
                  JBQ2(K,IB2) = NTAB(LS4+K-3)
                ENDIF
C
              ENDDO
C
              IF (IBUG2.EQ.1) WRITE (IWRITE,3100) (IS(KK),(JBQ1(K,IS(KK)!
     +),K=1,3), (JBQ2(K,IS(KK)),K=1,3),KK=1,4)
C
              DO K = 1,3
                DO KK = 1,4
                  IF (JBQ1(K,IS(KK)).NE.JBQ2(K,IS(KK))) GOTO 110
                ENDDO
              ENDDO
C-----------------------------------------------------------------------
C
C  Evaluate product of 4 CFP'S, eq.(5).
C
              CALL MUMDAD(IBUG2, IS, IWRITE, JBQ1, JBQ2, JJQ1, JJQ2, JTQ!
     +1, JTQ2,KAPS, NQ1, NQ2, PROD)
              IF (ABS(PROD).LT.EPS10) GOTO 110
C-----------------------------------------------------------------------
C
C  Set arrays for defining the recoupling coefficient.
C
              CALL SETJ(IS, JBQ1, JJC1, JJC2, JJQ1, JJQ2, JLIST, JS, JTQ!
     +1, JTQ2,KS, NPEEL,J1, J2, J2S, J3, J3S, KJ23, MMOM, NMOM)
C
              IF (ISNJ.NE.0) GOTO 40
C-----------------------------------------------------------------------
C
C  Set up arrays and variables for direct case.
C
C  J1(NBRJ) ( = J1(M) ) is set to (2*KCD1+1) so that NJGRAF is
C  called correctly.
C
              IF (KCD2.EQ.0 .AND. IBRD.LT.0) GOTO 30
C
              IF (KCD2.GT.0) THEN
                J1(NBRJ) = KCD1+KCD1+1
              ELSE
                J1(NBRJ) = KBD1+KBD1+1
              ENDIF
C
      CALL NJGRAF (
     + RECUP,-1,FAILD,
     + J1,J2,J3,MMOM,NMOM,FREE,
     + IBUG3,IWRITE,
     + J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     + JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     + K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
C
              ISNJ = 1
              IF (FAILD) GOTO 30
C-----------------------------------------------------------------------
C
C  Store data for future calls in direct case.
C
      JD6C=J6C
      JD7C=J7C
      JD8C=J8C
      JD9C=J9C
      JDWC=JWC
      JDDEL=JDEL
      MDP=MP
      NDLSUM=NLSUM
      IF (J6C.NE.0) THEN
        DO I=1,J6C
          JD6(I)=J6(I)
        ENDDO
      ENDIF
      IF (J7C.NE.0) THEN
        DO I=1,J7C
          JD7(I)=J7(I)
        ENDDO
      ENDIF
      IF (J8C.NE.0) THEN
        DO I=1,J8C
          JD8(I)=J8(I)
        ENDDO
      ENDIF
      IF (J9C.NE.0) THEN
        DO I=1,J9C
          JD9(I)=J9(I)
        ENDDO
      ENDIF
      IF (JWC.NE.0) THEN
        DO I=1,6
          DO J=1,JWC
            JDW(I,J)=JW(I,J)
          ENDDO
        ENDDO
        DO I=1,JWC
          INVD6J(I)=INV6J(I)
        ENDDO
      ENDIF
      IF (JDEL.NE.0) THEN
        DO I=1,2
          DO J=1,JDEL
            LDDEL(J,I)=LDEL(J,I)
          ENDDO
        ENDDO
      ENDIF
      IF (NLSUM.NE.0) THEN
        DO I=1,NLSUM
          NDBJ(I)=NBJ(I)
          NDB6J(I)=NB6J(I)
          KD6CP(I)=K6CP(I)
          KD7CP(I)=K7CP(I)
          KD8CP(I)=K8CP(I)
          KD9CP(I)=K9CP(I)
        ENDDO
      ENDIF
      DO I=1,MANGMP
        JD6P(I)=J6P(I)
        JD7P(I)=J7P(I)
        JD8P(I)=J8P(I)
        JD9P(I)=J9P(I)
      ENDDO
      DO I=1,MTRIAD
        JDSUM6(I)=JSUM6(I)
        DO J=1,M6J
          JDSUM4(I,J)=JSUM4(I,J)
          JDSUM5(I,J)=JSUM5(I,J)
        ENDDO
      ENDDO
      DO I=1,6
        DO J=1,M6J
          JDWORD(I,J)=JWORD(I,J)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C  Set up arrays and variables for exchange case.
C
C  J1(NBRJ) ( = J1(M) ) is set to (2*KCE1+1) so that NJGRAF is
C  called correctly.
C
   30         CONTINUE
              IF (KCE2.EQ.0 .AND. IBRE.LT.0) GOTO 40
C
              CALL MODJ23( J2, J3, J2S, J3S, NMOM)
C
              IF (KCE2.GT.0) THEN
                J1(NBRJ) = KCE1+KCE1+1
              ELSE
                J1(NBRJ) = KBE1+KBE1+1
              ENDIF
C
      CALL NJGRAF (
     + RECUP,-1,FAILE,
     + J1,J2,J3,MMOM,NMOM,FREE,
     + IBUG3,IWRITE,
     + J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     + JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     + K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
C
              ISNJ = 2
C
              IF (FAILE) GOTO 40
C-----------------------------------------------------------------------
C
C  Store data for future calls in exchange case.
C
      JE6C=J6C
      JE7C=J7C
      JE8C=J8C
      JE9C=J9C
      JEWC=JWC
      JEDEL=JDEL
      MEP=MP
      NELSUM=NLSUM
      IF (J6C.NE.0) THEN
        DO I=1,J6C
          JE6(I)=J6(I)
        ENDDO
      ENDIF
      IF (J7C.NE.0) THEN
        DO I=1,J7C
          JE7(I)=J7(I)
        ENDDO
      ENDIF
      IF (J8C.NE.0) THEN
        DO I=1,J8C
          JE8(I)=J8(I)
        ENDDO
      ENDIF
      IF (J9C.NE.0) THEN
        DO I=1,J9C
          JE9(I)=J9(I)
        ENDDO
      ENDIF
      IF (JWC.NE.0) THEN
        DO I=1,6
          DO J=1,JWC
            JEW(I,J)=JW(I,J)
          ENDDO
        ENDDO
        DO I=1,JWC
          INVE6J(I)=INV6J(I)
        ENDDO
      ENDIF
      IF (JDEL.NE.0) THEN
        DO I=1,2
          DO J=1,JDEL
            LEDEL(J,I)=LDEL(J,I)
          ENDDO
        ENDDO
      ENDIF
      IF (NLSUM.NE.0) THEN
        DO I=1,NLSUM
          NEBJ(I)=NBJ(I)
          NEB6J(I)=NB6J(I)
          KE6CP(I)=K6CP(I)
          KE7CP(I)=K7CP(I)
          KE8CP(I)=K8CP(I)
          KE9CP(I)=K9CP(I)
        ENDDO
      ENDIF
      DO I=1,MANGMP
        JE6P(I)=J6P(I)
        JE7P(I)=J7P(I)
        JE8P(I)=J8P(I)
        JE9P(I)=J9P(I)
      ENDDO
      DO I=1,MTRIAD
        JESUM6(I)=JSUM6(I)
        DO J=1,M6J
          JESUM4(I,J)=JSUM4(I,J)
          JESUM5(I,J)=JSUM5(I,J)
        ENDDO
      ENDDO
      DO I=1,6
        DO J=1,M6J
          JEWORD(I,J)=JWORD(I,J)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C  COULOMB
C  Calculate recoupling coefficients for direct cases.
C  Calculate AD, eq.(6), without the phase factor
C
C-----------------------------------------------------------------------
   40         CONTINUE
              IF ((KCD2.NE.0) .AND. (.NOT.FAILD)) THEN
                KK = KCD1-2
                DO K = 1,KCD2
                  KK = KK+2
                  J1(MMOM) = KK+KK+1
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JD6C,JD7C,JD8C,JD9C,JDWC,
     + JD6,JD7,JD8,JD9,JDW,
     + JDDEL,LDDEL,MDP,JD6P,JD7P,JD8P,JD9P,
     + JDWORD,NDLSUM,NDBJ,NDB6J,
     + KD6CP,KD7CP,KD8CP,KD9CP,
     + JDSUM4,JDSUM5,JDSUM6,INVD6J
     + )
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3110) KK,X
                  COND(K) = COND(K)+X*PROD
                ENDDO
              ENDIF
C-----------------------------------------------------------------------
C
C  COULOMB
C  Calculate recoupling coefficients for exchange cases.
C  Calculate AE, eq.(6), without the phase factor
C
C-----------------------------------------------------------------------
              IF ((KCE2.NE.0) .AND. (.NOT.FAILE)) THEN
                KK = KCE1-2
                DO K = 1,KCE2
                  KK = KK+2
                  J1(MMOM) = KK+KK+1
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JE6C,JE7C,JE8C,JE9C,JEWC,
     + JE6,JE7,JE8,JE9,JEW,
     + JEDEL,LEDEL,MEP,JE6P,JE7P,JE8P,JE9P,
     + JEWORD,NELSUM,NEBJ,NEB6J,
     + KE6CP,KE7CP,KE8CP,KE9CP,
     + JESUM4,JESUM5,JESUM6,INVE6J
     + )
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3120) KK,X
                  CONE(K) = CONE(K)+X*PROD
                ENDDO
              ENDIF
C-----------------------------------------------------------------------
C
C  BREIT
C  Calculate recoupling coefficients for direct cases.
C
C-----------------------------------------------------------------------
              IF ((IBRD.GE.0) .AND. (.NOT.FAILD)) THEN
C
                IF (IBRD.GT.1) THEN
                  IMUD = 1
                ELSE
                  IMUD = 4
                ENDIF
C
                NCODE = 0
C======================================================
                DO NN = 1,KBD2
C
                  NU = KBD1+2*(NN-1)
                  NUD = NU+NU+1
C
                  IF (NU.EQ.0) GOTO 60
C
                  IF (.NOT.ITRIG(KS(1),KS(3),NUD)) GOTO 50
                  IF (.NOT.ITRIG(KS(2),KS(4),NUD)) GOTO 50
                  K = NU
                  J1(MMOM) = NUD
C
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JD6C,JD7C,JD8C,JD9C,JDWC,
     + JD6,JD7,JD8,JD9,JDW,
     + JDDEL,LDDEL,MDP,JD6P,JD7P,JD8P,JD9P,
     + JDWORD,NDLSUM,NDBJ,NDB6J,
     + KD6CP,KD7CP,KD8CP,KD9CP,
     + JDSUM4,JDSUM5,JDSUM6,INVD6J
     + )
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3130) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRD,1,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,IM!
     +UD)
                    DO MU = 1,IMUD
                      BOND(MU,NN) = BOND(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
   50             CONTINUE
                  IF (IBRD.GT.1) GOTO 70
                  K = NU-1
C
                  IF (NCODE.EQ.NN) THEN
                    X = XCODE
                  ELSE
                    ITKMO = NUD-2
                    IF (.NOT.ITRIG(KS(1),KS(3),ITKMO)) GOTO 60
                    IF (.NOT.ITRIG(KS(2),KS(4),ITKMO)) GOTO 60
                    J1(MMOM) = ITKMO
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JD6C,JD7C,JD8C,JD9C,JDWC,
     + JD6,JD7,JD8,JD9,JDW,
     + JDDEL,LDDEL,MDP,JD6P,JD7P,JD8P,JD9P,
     + JDWORD,NDLSUM,NDBJ,NDB6J,
     + KD6CP,KD7CP,KD8CP,KD9CP,
     + JDSUM4,JDSUM5,JDSUM6,INVD6J
     + )
                  ENDIF
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3130) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRD,1,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,4)
                    DO MU = 1,4
                      BOND(MU,NN) = BOND(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
   60             CONTINUE
                  IF (IBRD.GT.1 .OR. NN.EQ.KBD2) GOTO 70
                  NCODE = NN+1
                  XCODE = ZERO
C
                  ITKMO = NUD+2
                  IF (.NOT.ITRIG(KS(1),KS(3),ITKMO)) GOTO 70
                  IF (.NOT.ITRIG(KS(2),KS(4),ITKMO)) GOTO 70
                  K = NU+1
                  J1(MMOM) = ITKMO
C
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JD6C,JD7C,JD8C,JD9C,JDWC,
     + JD6,JD7,JD8,JD9,JDW,
     + JDDEL,LDDEL,MDP,JD6P,JD7P,JD8P,JD9P,
     + JDWORD,NDLSUM,NDBJ,NDB6J,
     + KD6CP,KD7CP,KD8CP,KD9CP,
     + JDSUM4,JDSUM5,JDSUM6,INVD6J
     + )
                  XCODE = X
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3130) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRD,1,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,12!
     +)
                    DO MU = 1,12
                      BOND(MU,NN) = BOND(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
   70             CONTINUE
                ENDDO
C
              ENDIF
C-----------------------------------------------------------------------
C
C  BREIT
C  Calculate recoupling coefficients for exchange cases.
C
C-----------------------------------------------------------------------
              IF ((IBRE.GE.0) .AND. (.NOT.FAILE)) THEN
                NCODE = 0
C
                DO NN = 1,KBE2
                  IMUE = 4
                  IF (IBRE.EQ.2) IMUE = 1
                  IF (IBRE.EQ.4) IMUE = 3
C
                  NU = KBE1+2*(NN-1)
                  NUD = NU+NU+1
C
                  IF (NU.EQ.0) GOTO 90
C
                  IF (.NOT.ITRIG(KS(1),KS(4),NUD)) GOTO 80
                  IF (.NOT.ITRIG(KS(2),KS(3),NUD)) GOTO 80
                  K = NU
                  J1(MMOM) = NUD
C
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JE6C,JE7C,JE8C,JE9C,JEWC,
     + JE6,JE7,JE8,JE9,JEW,
     + JEDEL,LEDEL,MEP,JE6P,JE7P,JE8P,JE9P,
     + JEWORD,NELSUM,NEBJ,NEB6J,
     + KE6CP,KE7CP,KE8CP,KE9CP,
     + JESUM4,JESUM5,JESUM6,INVE6J
     + )
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3150) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRE,2,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,IM!
     +UE)
                    DO MU = 1,IMUE
                      BONE(MU,NN) = BONE(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
   80             CONTINUE
                  IF (IBRE.EQ.2) GOTO 100
C
                  IMUE = 4
                  IF (IBRE.EQ.4) IMUE = 3
                  K = NU-1
C
                  IF (NCODE.EQ.NN) THEN
                    X = XCODE
                  ELSE
                    ITKMO = NUD-2
                    IF (.NOT.ITRIG(KS(1),KS(4),ITKMO)) GOTO 90
                    IF (.NOT.ITRIG(KS(2),KS(3),ITKMO)) GOTO 90
                    J1(MMOM) = ITKMO
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JE6C,JE7C,JE8C,JE9C,JEWC,
     + JE6,JE7,JE8,JE9,JEW,
     + JEDEL,LEDEL,MEP,JE6P,JE7P,JE8P,JE9P,
     + JEWORD,NELSUM,NEBJ,NEB6J,
     + KE6CP,KE7CP,KE8CP,KE9CP,
     + JESUM4,JESUM5,JESUM6,INVE6J
     + )
                  ENDIF
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3150) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRE,2,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,IM!
     +UE)
                    DO MU = 1,IMUE
                      BONE(MU,NN) = BONE(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
   90             CONTINUE
                  IF (IBRE.EQ.2 .OR. NN.EQ.KBE2) GOTO 100
C
                  NCODE = NN+1
                  XCODE = ZERO
C
                  IMUE = 12
                  IF (IBRE.EQ.4) IMUE = 7
C
                  ITKMO = NUD+2
                  IF (.NOT.ITRIG(KS(1),KS(4),ITKMO)) GOTO 100
                  IF (.NOT.ITRIG(KS(2),KS(3),ITKMO)) GOTO 100
                  K = NU+1
                  J1(MMOM) = ITKMO
C
      CALL GENSUM (
     + X,
     + J1,MMOM,
     + IBUG3,
     + JE6C,JE7C,JE8C,JE9C,JEWC,
     + JE6,JE7,JE8,JE9,JEW,
     + JEDEL,LEDEL,MEP,JE6P,JE7P,JE8P,JE9P,
     + JEWORD,NELSUM,NEBJ,NEB6J,
     + KE6CP,KE7CP,KE8CP,KE9CP,
     + JESUM4,JESUM5,JESUM6,INVE6J
     + )
C
                  XCODE = X
C
                  IF (IBUG2.EQ.1) WRITE (IWRITE,3150) NU,K,X
C
                  IF (ABS(X).GE.EPS10) THEN
                    X = X*PROD
                    CALL CXK(S,IS,KAPS,NU,K,IBRE,2,IWRITE)
                    IF (IBUG2.EQ.1) WRITE (IWRITE,3140) (S(III),III=1,IM!
     +UE)
                    DO MU = 1,IMUE
                      BONE(MU,NN) = BONE(MU,NN)+X*S(MU)
                    ENDDO
                  ENDIF
C++++++++++++++++++++++++++++++++++++++++++++++++++
  100             CONTINUE
                ENDDO
C
              ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
              IF (FAILD .AND. FAILE) GOTO 150
C-----------------------------------------------------------------------
  110         CONTINUE
            ENDDO
  120       CONTINUE
          ENDDO
  130     CONTINUE
        ENDDO
  140   CONTINUE
      ENDDO
C-----------------------------------------------------------------------
C
C  Insert factors independent of barred quantum numbers.
C  Output results.
C
C  Begin with common statistical factors, eq.(5).
C
C-----------------------------------------------------------------------
  150 CONTINUE
      CONST = OCON(IA1,IB1,IA2,IB2,NQ1,NQ2)
C
      IF (IBUG2.EQ.1) WRITE (IWRITE,3160) CONST
C
      KAP1 = NAK(IA1)
      KAP2 = NAK(IB1)
      KAP3 = NAK(IA2)
      KAP4 = NAK(IB2)
C-----------------------------------------------------------------------
C
C  Compute products of reduced matrix elements,eq.(7).
C
C  CRED for direct terms
C  CREE for exchange terms
C
C-----------------------------------------------------------------------
      IF (KCD2.GT.0) THEN
        PRODD = CONST/SQRT(DBLE(KS(1)*KS(4)))
        IF (MOD(KCD1,2).NE.0) PRODD = -PRODD
        IF (IA1.EQ.IB1 .AND. IA2.EQ.IB2) PRODD = PRODD*HALF
        KK = KCD1-2
        DO K = 1,KCD2
          KK = KK+2
          CRED = CRE(KAP1,KK,KAP3)*CRE(KAP2,KK,KAP4)
          X = PRODD*COND(K)*CRED
          IF (ABS(X).GT.EPS10) CALL SPEAK(8,IA1,IB1,IA2,IB2,KK,X,IBUG1, !
     +IME, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
        ENDDO
      ENDIF
C
      IF (KCE2.GT.0) THEN
        PRODE = CONST/SQRT(DBLE(KS(1)*KS(3)))
        IF (MOD(KCE1,2).NE.0) PRODE = -PRODE
        PRODE = -PRODE
        KK = KCE1-2
        DO K = 1,KCE2
          KK = KK+2
          CREE = CRE(KAP1,KK,KAP4)*CRE(KAP2,KK,KAP3)
          X = PRODE*CONE(K)*CREE
          IF (ABS(X).GT.EPS10) CALL SPEAK(9,IA1,IB1,IB2,IA2,KK,X,IBUG1, !
     +IME, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IF (IBRD.GE.0) THEN
C
        PRODD = CONST/SQRT(DBLE(KS(1)*KS(4)))
        IF (IA1.EQ.IB1 .AND. IA2.EQ.IB2) PRODD = PRODD*HALF
        DO NN = 1,KBD2
          DO MU = 1,12
            BOND(MU,NN) = BOND(MU,NN)*PRODD
          ENDDO
        ENDDO
C======================================================
        DO NN = 1,KBD2
C
          NU = KBD1+2*(NN-1)
C
          IF (IBRD.EQ.2) THEN
            ITYPE = 3
            CALL SPEAK(ITYPE,IA1,IA2,IB1,IB2,NU,BOND(1,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            GOTO 160
          ENDIF
C
          IF (IBRD.EQ.3) THEN
            ITYPE = 4
            CALL SPEAK(ITYPE,IA1,IA2,IB1,IB2,NU,BOND(1,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            GOTO 160
          ENDIF
C
          ITYPE = 1
          CALL SPEAK(ITYPE,IA1,IA2,IB1,IB2,NU,BOND(1,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IA2,IA1,IB2,IB1,NU,BOND(2,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IA1,IA2,IB2,IB1,NU,BOND(3,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IA2,IA1,IB1,IB2,NU,BOND(4,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
C
          IF (NN.NE.KBD2) THEN
            NUP1 = NU+1
            ITYPE = 2
            CALL SPEAK(ITYPE,IA1,IA2,IB1,IB2,NUP1,BOND(5,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB1,IB2,IA1,IA2,NUP1,BOND(6,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA2,IA1,IB2,IB1,NUP1,BOND(7,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB2,IB1,IA2,IA1,NUP1,BOND(8,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA1,IA2,IB2,IB1,NUP1,BOND(9,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB2,IB1,IA1,IA2,NUP1,BOND(10,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA2,IA1,IB1,IB2,NUP1,BOND(11,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB1,IB2,IA2,IA1,NUP1,BOND(12,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
          ENDIF
C
  160     CONTINUE
        ENDDO
C
      ENDIF
C-----------------------------------------------------------------------
      IF (IBRE.GE.0) THEN
C
        PRODE = CONST/SQRT(DBLE(KS(1)*KS(3)))
        PRODE = -PRODE
        DO NN = 1,KBE2
          DO MU = 1,12
            BONE(MU,NN) = BONE(MU,NN)*PRODE
          ENDDO
        ENDDO
C======================================================
        DO NN = 1,KBE2
C
          NU = KBE1+2*(NN-1)
C
          IF (IBRE.EQ.4) THEN
            ITYPE = 5
            CALL SPEAK(ITYPE,IB1,IA1,IB1,IA1,NU,BONE(1,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA1,IB1,IB1,IA1,NU,BONE(2,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA1,IB1,IA1,IB1,NU,BONE(3,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            IF (NN.NE.KBE2) THEN
              NUP1 = NU+1
              ITYPE = 6
              CALL SPEAK(ITYPE,IA1,IB1,IA1,IB1,NUP1,BONE(4,NN),IBUG1, IM!
     +E, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
              CALL SPEAK(ITYPE,IB1,IA1,IB1,IA1,NUP1,BONE(5,NN),IBUG1, IM!
     +E, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
              CALL SPEAK(ITYPE,IA1,IB1,IB1,IA1,NUP1,BONE(6,NN),IBUG1, IM!
     +E, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
              CALL SPEAK(ITYPE,IB1,IA1,IA1,IB1,NUP1,BONE(7,NN),IBUG1, IM!
     +E, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            ENDIF
            GOTO 170
          ENDIF
C
          IF (IBRE.EQ.2) THEN
            ITYPE = 3
            CALL SPEAK(ITYPE,IA1,IB2,IB1,IA2,NU,BONE(1,NN),IBUG1, IME, I!
     +WRITE, JA, JB, NOUTX, NH, NP, NWA)
            GOTO 170
          ENDIF
C
          ITYPE = 1
          CALL SPEAK(ITYPE,IA1,IB2,IB1,IA2,NU,BONE(1,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IB2,IA1,IA2,IB1,NU,BONE(2,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IA1,IB2,IA2,IB1,NU,BONE(3,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
          CALL SPEAK(ITYPE,IB2,IA1,IB1,IA2,NU,BONE(4,NN),IBUG1, IME, IWR!
     +ITE, JA, JB, NOUTX, NH, NP, NWA)
C
          IF (NN.NE.KBE2) THEN
            NUP1 = NU+1
            ITYPE = 2
            CALL SPEAK(ITYPE,IA1,IB2,IB1,IA2,NUP1,BONE(5,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB1,IA2,IA1,IB2,NUP1,BONE(6,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB2,IA1,IA2,IB1,NUP1,BONE(7,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA2,IB1,IB2,IA1,NUP1,BONE(8,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA1,IB2,IA2,IB1,NUP1,BONE(9,NN),IBUG1, IME,!
     + IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IA2,IB1,IA1,IB2,NUP1,BONE(10,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB2,IA1,IB1,IA2,NUP1,BONE(11,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
            CALL SPEAK(ITYPE,IB1,IA2,IB2,IA1,NUP1,BONE(12,NN),IBUG1, IME!
     +, IWRITE, JA, JB, NOUTX, NH, NP, NWA)
          ENDIF
C
  170     CONTINUE
        ENDDO
C
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (                                                          !
     +' spectator quantum numbers not diagonal for non-interacting she',!
     +'lls')
 3010 FORMAT (' COR called : orbitals                      = ',4I5)
 3020 FORMAT (' COR called : KCD1,KCD2,KCE1,KCE2           = ',4I5)
 3030 FORMAT (/' STOPPING in routine COR'/                              !
     +' recompile and increase dimension of arrays COND,CONE to at lea',!
     +'st ',I5/' i.e. increase parameter IDIM to this value')
 3040 FORMAT (' COR called : KBD1,KBD2,KBE1,KBE2,IBRD,IBRE = ',6I5)
 3050 FORMAT (/' STOPPING in routine COR'/                              !
     +' recompile and increase dimension of arrays BOND,BONE to at lea',!
     +'st ',I5/' i.e. increase parameter IDIM to this value')
 3060 FORMAT (' COR called : NQS                           = ',4I5)
 3070 FORMAT (' COR called : LLS                           = ',4I5)
 3080 FORMAT (' COR called : ILS                           = ',4I5)
 3090 FORMAT (' COR called : LS1,LS2,LS3,LS4               = ',4I5)
 3100 FORMAT (' COR called : IS                            = ',I5/      !
     +' COR called : JBQ1                          = ',3I5/             !
     +' COR called : JBQ2                          = ',3I5)
 3110 FORMAT ('   direct    K recoupling coef      ',I5,2X,1P,E20.9)
 3120 FORMAT (' exchange    K recoupling coef      ',I5,2X,1P,E20.9)
 3130 FORMAT ('   direct NU K recoupling coef ',2I5,2X,1P,E20.9)
 3140 FORMAT (' S = ',1P,4E12.4)
 3150 FORMAT (' exchange NU K recoupling coef ',2I5,2X,1P,E20.9)
 3160 FORMAT (' COR called : statistical factor = ',1P,E20.9/)
      END
CEND--------------------------------------------------------------------
CEND    COUP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE COUP(IWRITE,NL,NQ,JI,JF,JQS,IQ,JCUP,ICASE,MODE,ICASEX,I!
     +TAB, JTAB, NROWS, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/COUP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... output stream for printer
C   NL     ... L value
C   NQ     ... occupation number
C   JI     ... initial 2J
C   JF     ... final   2J
C o JQS    ...
C o IQ     ...
C o JCUP   ...
C   ICASE  ... number of relativistic subshells
C   MODE   ... =0 if JF is not fixed, =1 if JF is fixed
C   ICASEX ... dimension of arrays
C   ITAB   ...
C   JTAB   ...
C   NROWS  ...
C   NTAB   ...
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Statement functions
C
      LOGICAL ITRG
C
C  Argument variables
C
      INTEGER ICASEX
      INTEGER ICASE,IQ(2,*)
      INTEGER IWRITE,JCUP(*),JF
      INTEGER JI,JQS(3,2,*),MODE
      INTEGER NL,NQ
      INTEGER ITAB(*),JTAB(*),NROWS
      INTEGER NTAB(*)
C
C  Local variables
C
      INTEGER I1,I2,I3,ISA
      INTEGER ISB,ITA,ITB,ITBX
      INTEGER IXA,IXB,IXJA,IXJB
      INTEGER J1,J2,J3,J4
      INTEGER JMAX,JMIN,K,NC
      INTEGER NCA,NCB,NJA,NJB
      INTEGER NQA,NQB,NX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ITRG(J1,J2,J3) = ABS(J1-J2).LE.J3.AND.J3.LE.J1+J2.AND.MOD(J1+J2+J3!
     +,2).EQ.0
C-----------------------------------------------------------------------
      ICASE = 0
      IF (NQ.EQ.0) GOTO 10
      IF (NL.EQ.0) GOTO 20
C
      NJA = NL+NL
      NJB = NJA+2
      IXJA = 1+(NJA*(NJA-2))/8
      IXJB = 1+(NJB*(NJB-2))/8
      NX = MIN(NQ,NJA)+1
C
      DO I1 = 1,NX
        NQA = NX-I1
        NQB = NQ-NQA
        NCA = MIN(NQA,NJA-NQA)
        NCB = MIN(NQB,NJB-NQB)
C
        IF (NCB.GE.0) THEN
C
          IF (NCA.EQ.0) THEN
            IXA = 1
          ELSE
            IXA = NCA+IXJA
            IF (IXA.GT.NROWS) GOTO 50
          ENDIF
C
          IF (NCB.EQ.0) THEN
            IXB = 1
          ELSE
            IXB = NCB+IXJB
            IF (IXB.GT.NROWS) GOTO 50
          ENDIF
C
          ISA = ITAB(IXA)
          ITA = JTAB(IXA)
          ISB = ITAB(IXB)
          ITB = JTAB(IXB)
C
          DO I2 = 1,ISA
            J1 = NTAB(ITA+3)-1
            JMIN = ABS(J1-JI)
            JMAX = J1+JI
            DO J4 = JMIN,JMAX,2
              ITBX = ITB
              DO I3 = 1,ISB
                J2 = NTAB(ITBX+3)-1
                IF (MODE.EQ.0 .OR. ITRG(J4,J2,JF)) THEN
                  ICASE = ICASE+1
                  IF (ICASE.GT.ICASEX) GOTO 40
                  DO K = 1,3
                    JQS(K,1,ICASE) = NTAB(ITA+K)
                    JQS(K,2,ICASE) = NTAB(ITBX+K)
                  ENDDO
                  IQ(1,ICASE) = NQA
                  IQ(2,ICASE) = NQB
                  JCUP(ICASE) = J4+1
                ENDIF
                ITBX = ITBX+3
              ENDDO
            ENDDO
            ITA = ITA+3
          ENDDO
C
        ENDIF
C
      ENDDO
      RETURN
C
C   NQ=0   occupation number is zero
C
   10 CONTINUE
      IF (MODE.EQ.1 .AND. JI.NE.JF) RETURN
      ICASE = 1
      DO K = 1,3
        JQS(K,1,1) = NTAB(K)
        JQS(K,2,1) = NTAB(K)
      ENDDO
      IQ(1,1) = 0
      IQ(2,1) = 0
      JCUP(1) = JI+1
      RETURN
C
C   NL=0  L-value is zero
C
   20 CONTINUE
      J2 = 0
      IF (NQ.EQ.1) J2 = 1
      IF (MODE.EQ.0) GOTO 30
      IF (ITRG(JI,J2,JF)) GOTO 30
      RETURN
C
   30 CONTINUE
      NC = MIN(NQ,2-NQ)*3
      ICASE = 1
      DO K = 1,3
        JQS(K,1,1) = NTAB(K)
        JQS(K,2,1) = NTAB(K+NC)
      ENDDO
      IQ(1,1) = 0
      IQ(2,1) = NQ
      JCUP(1) = JI+1
      RETURN
C
C    Error messages
C
   40 CONTINUE
      WRITE (IWRITE,3000)
      STOP
C
   50 CONTINUE
      WRITE (IWRITE,3010)
      STOP
C
 3000 FORMAT (/' ERROR in COUP : dimension for ICASE ... STOPPING')
 3010 FORMAT (/' ERROR in COUP : dimension for NROWS ... STOPPING')
      END
CEND--------------------------------------------------------------------
CEND    CRE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      FUNCTION CRE(KAP1,K,KAP2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CRE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   KAP1   ... kappa-value for j1
C   K      ...
C   KAP2   ... kappa-value for j2
C
C   Routine called: CLRX
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      DOUBLE PRECISION CRE
C
C  External functions
C
      EXTERNAL CLRX
      DOUBLE PRECISION CLRX
C
C  Argument variables
C
      INTEGER K,KAP1,KAP2
C
C  Local variables
C
      INTEGER K1,K2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      K1 = ABS(KAP1)
      K2 = ABS(KAP2)
      CRE = SQRT(DBLE(K1*K2))*CLRX(KAP1,K,KAP2)
      IF (MOD(K1,2).EQ.1) CRE = -CRE
      CRE = CRE+CRE
      END
CEND--------------------------------------------------------------------
CEND    CXK.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CXK(S,IS,KAPS,NU,K,IBR,IEX,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CXK.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C    S
C    IS
C    KAPS
C    NU
C    K
C    IBR
C    IEX
C    IWRITE
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  External functions
C
      EXTERNAL CRE
      DOUBLE PRECISION CRE
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
C
C  Argument variables
C
      DOUBLE PRECISION S(12)
      INTEGER IBR,IEX,IS(4),K
      INTEGER KAPS(4),NU
      INTEGER IWRITE
C
C  Local variables
C
      DOUBLE PRECISION A,B,D,DK
      DOUBLE PRECISION DK1,DK2,F1,F2
      DOUBLE PRECISION F3,F4,FK,G1
      DOUBLE PRECISION G2,G3,G4,GK
      DOUBLE PRECISION H
      INTEGER IA,IB,IC,ID
      INTEGER IK,IP,KA,KB
      INTEGER KC,KD,KK,MU
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO MU = 1,12
        S(MU) = ZERO
      ENDDO
C
      IA = IS(1)
      IB = IS(2)
      IC = IS(3)
      ID = IS(4)
C
      KA = KAPS(1)/2
      KB = KAPS(2)/2
      KC = KAPS(3)/2
      KD = KAPS(4)/2
C
      IF (IEX.NE.2) GOTO 10
C
      KK = KD
      IK = ID
      KD = KC
      ID = IC
      KC = KK
      IC = IK
C
   10 CONTINUE
      IF (IBR.EQ.1) GOTO 20
      IF (IBR.EQ.2) GOTO 60
      IF (IBR.EQ.3) GOTO 90
      IF (IBR.EQ.4) GOTO 100
      GOTO 150
C-----------------------------------------------------------------------
   20 CONTINUE
      IF (NU-K) 50,30,40
C
   30 CONTINUE
      S(1) = -((KA+KC)*(KD+KB))
      IF (K.EQ.0) GOTO 140
      D = DBLE(K*(K+1))
      H = CRE(KA,K,KC)*CRE(KB,K,KD)
      IF (MOD(K,2).NE.0) H = -H
      S(1) = S(1)*H/D
      DO MU = 2,4
        S(MU) = S(1)
      ENDDO
      RETURN
C
   40 CONTINUE
      DK1 = DBLE(KC-KA)
      DK2 = DBLE(KD-KB)
      FK = DBLE(K)
      GK = DBLE(K+1)
      G1 = DK1-GK
      G2 = DK1+GK
      G3 = DK2-GK
      G4 = DK2+GK
      KK = K+K+1
      H = CRE(KA,K,KC)*CRE(KB,K,KD)
      IF (MOD(K,2).NE.0) H = -H
      A = H*FK/GK/(KK*(KK+2))
      S(1) = A*G1*G3
      S(2) = A*G2*G4
      S(3) = A*G1*G4
      S(4) = A*G2*G3
      RETURN
C
   50 CONTINUE
      DK1 = DBLE(KC-KA)
      DK2 = DBLE(KD-KB)
      FK = DBLE(K)
      GK = DBLE(K+1)
      F1 = DK1-FK
      F2 = DK1+FK
      F3 = DK2-FK
      F4 = DK2+FK
      G1 = DK1-GK
      G2 = DK1+GK
      G3 = DK2-GK
      G4 = DK2+GK
      KK = K+K+1
      H = CRE(KA,K,KC)*CRE(KB,K,KD)
      IF (MOD(K,2).NE.0) H = -H
      A = H*GK/FK/(KK*(KK-2))
      S(1) = A*F2*F4
      S(2) = A*F1*F3
      S(3) = A*F2*F3
      S(4) = A*F1*F4
      B = H/(KK*KK)
      S(5) = B*F2*G3
      S(6) = B*F4*G1
      S(7) = B*F1*G4
      S(8) = B*F3*G2
      S(9) = B*F2*G4
      S(10) = B*F3*G1
      S(11) = B*F1*G3
      S(12) = B*F4*G2
      RETURN
C-----------------------------------------------------------------------
   60 CONTINUE
      IF (IA.EQ.IC .AND. IB.NE.ID) GOTO 80
      IF (IA.NE.IC .AND. IB.EQ.ID) GOTO 70
      GOTO 150
C
   70 CONTINUE
      IK = IB
      IB = IA
      IA = IK
      IK = ID
      ID = IC
      IC = IK
      KK = KB
      KB = KA
      KA = KK
      KK = KD
      KD = KC
      KC = KK
C
   80 CONTINUE
      IF (MOD(K,2).NE.1) RETURN
C
      DK = DBLE(K*(K+1))
      H = CRE(KA,K,KC)*CRE(KB,K,KD)/DK
      S(1) = H*(4*KA*(KB+KD))
      RETURN
C-----------------------------------------------------------------------
   90 CONTINUE
      IF (IA.NE.IC .OR. IB.NE.ID) GOTO 150
      IF (MOD(K,2).NE.1) RETURN
      DK = DBLE(K*(K+1))
      H = CRE(KA,K,KA)*CRE(KB,K,KB)/DK
      S(1) = H*(16*KA*KB)
      RETURN
C-----------------------------------------------------------------------
  100 CONTINUE
      IF (IA.NE.ID .OR. IB.NE.IC) GOTO 150
C
      IF (NU-K) 130,110,120
C
  110 CONTINUE
      S(1) = DBLE(KA+KB)*CRE(KA,K,KB)
      IP = ABS(KA)-ABS(KB)+K+1
      S(1) = S(1)*S(1)/(K*(K+1))
      IF (MOD(IP,2).NE.0) S(1) = -S(1)
      S(3) = S(1)
      S(2) = S(1)+S(1)
      RETURN
C
  120 CONTINUE
      DK = DBLE(KB-KA)
      GK = DBLE(K+1)
      FK = DBLE(K)
      G1 = DK+GK
      G2 = DK-GK
      KK = K+K+1
      H = CRE(KA,K,KB)**2
      IF (KA*KB.LT.0) H = -H
      A = H*FK/GK/(KK*(KK+2))
      S(1) = -A*G1*G1
      S(2) = -TWO*A*G1*G2
      S(3) = -A*G2*G2
      RETURN
C
  130 CONTINUE
      DK = DBLE(KB-KA)
      FK = DBLE(K)
      GK = DBLE(K+1)
      F1 = DK+FK
      F2 = DK-FK
      G1 = DK+GK
      G2 = DK-GK
      KK = K+K+1
      H = CRE(KA,K,KB)**2
      IF (KA*KB.LT.0) H = -H
      A = H*GK/FK/(KK*(KK-2))
      S(1) = -A*F2*F2
      S(2) = -TWO*A*F1*F2
      S(3) = -A*F1*F1
      B = H/(KK*KK)
      B = B+B
      S(4) = -B*F1*G2
      S(5) = -B*F2*G1
      S(6) = -B*F1*G1
      S(7) = -B*F2*G2
      RETURN
C-----------------------------------------------------------------------
  140 CONTINUE
      WRITE (IWRITE,3000) IS(1),IS(2),IS(3),IS(4),NU,IBR,IEX
      STOP
C-----------------------------------------------------------------------
  150 CONTINUE
      WRITE (IWRITE,3010) IBR,IS(1),IS(2),IS(3),IS(4),NU,K,IEX
      STOP
C-----------------------------------------------------------------------
 3000 FORMAT (/' CXK halted on illegal value K = 0'/                    !
     +' IS(1),IS(2),IS(3),IS(4),NU,IBR,IEX : ',4I3,2X,I3,2X,2I2)
 3010 FORMAT (/' CXK halted on type ',I2/                               !
     +' IBR,IS(1),IS(2),IS(3),IS(4),NU,K,IEX : ',I2,3X,4I3,2X,2I3,2X,I2)
      END
CEND--------------------------------------------------------------------
CEND    DATNR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DATNR(NMAN,NWM,IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6!
     +,ICHOP, IDMTST, IPOS, IQ, IREAD, ISPAR, ITAB, ITJPO,IWRITE, JCHOP,!
     + JCUP, JQS, JTAB, NAK, NCF, NDIMAX,NH, NLX, NP, NQX, NROWS, NTAB, !
     +NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DATNR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C        NMAN  - number of NR config.  to be defined
C        NWM   - number of NR orbitals to be defined
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICHOP
C   IDMTST
C   IPOS
C   IQ
C   IREAD
C   ISPAR
C   ITAB
C   ITJPO
C   IWRITE
C   JCHOP
C   JCUP
C   JQS
C   JTAB
C   NAK
C   NCF
C   NDIMAX
C   NH
C   NLX
C   NP
C   NQX
C   NROWS
C   NTAB
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER JSTMAX
      PARAMETER (JSTMAX=40)
C
C  Argument variables
C
      INTEGER NMAN,NWM
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IPOS(*)
      INTEGER IQ(MXNW,*)
      INTEGER IREAD
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCHOP(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NDIMAX(*)
      INTEGER NLX(*)
      INTEGER NP(*)
      INTEGER NQX(MXNW,*)
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      CHARACTER*2 LABT,NG(5),NGX(6),NHX
      INTEGER I,II,IPAR
      INTEGER ITEST,J,JF
      INTEGER JFX(MXNC),JMAN,JMAX
      INTEGER JMAXX(MXNC),JMIN,JMINX(MXNC)
      INTEGER JST,JST1,JST2
      INTEGER JSTORE(0:JSTMAX),K
      INTEGER KCHOP(MXNW),M,NELEC,NELEC1
      INTEGER NFULL,NL,NOPEN1,NOPENX
      INTEGER NQ,NST
      INTEGER JTC(20)
      INTEGER JPOS(5)
      INTEGER MLX(5)
      INTEGER MQX(5)
      INTEGER NOPEN
      LOGICAL MFAIL
C
      INTEGER OPT(20)
      INTEGER JOPT
      DOUBLE PRECISION JLOW(MXNC)
      DOUBLE PRECISION JHIGH(MXNC)
      INTEGER PRINC
      INTEGER KAPPA
      INTEGER CSF(MXNC)
C
C  Namelists
C
      NAMELIST / ANGOPT / OPT
      NAMELIST / JVALUE / JOPT,JLOW,JHIGH
      NAMELIST / ORB / PRINC,KAPPA,CSF
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NGX/'s ','p ','d ','f ','g ','h '/
      DATA NG/'p-','d-','f-','g-','h-'/
C-----------------------------------------------------------------------
C
C   NOPENX - maximum number of open shells in any CSF
C            that can be handled by this routine.
C
C-----------------------------------------------------------------------
      NOPENX = IDMTST(26)
C-----------------------------------------------------------------------
      IF (NWM.GT.MXNW) THEN
        WRITE (IWRITE,3000)
      ENDIF
C
      CALL DMCHK2(12,NWM, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
      IF (NMAN.GT.MXNC) THEN
        WRITE (IWRITE,3010)
      ENDIF
C
      CALL DMCHK2(07,NMAN, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
C
C   Input record 3 --- orbital data
C
C   NAMELIST ... ORB
C
C-----------------------------------------------------------------------
      NW = 0
C
      DO I = 1,NWM
        NW = NW+2
C
        PRINC = -1
        KAPPA = 0
        DO K = 1,MXNC
          CSF(K) = -1
        ENDDO
C
        READ (IREAD,ORB)
C
        IF (PRINC.LT.1) THEN
          WRITE (IWRITE,3050)
          STOP
        ENDIF
C
        IF (KAPPA.GE.0) THEN
          WRITE (IWRITE,3060)
          STOP
        ENDIF
C
        NHX = NGX(-KAPPA)
C
        NL = -(KAPPA+1)
        NFULL = 4*NL+2
C
        IF (CSF(1).EQ.-1) THEN
          DO K = 1,NMAN
            NQX(I,K) = NFULL
          ENDDO
        ELSE
          IF (CSF(2).EQ.-1) THEN
            IF (CSF(1).LT.0 .OR. CSF(1).GT.NFULL) THEN
              WRITE (IWRITE,3070)
              STOP
            ENDIF
            DO K = 1,NMAN
              NQX(I,K) = CSF(1)
            ENDDO
          ELSE
            DO K = 1,NMAN
              IF (CSF(K).LT.0 .OR. CSF(K).GT.NFULL) THEN
                WRITE (IWRITE,3070)
                STOP
              ENDIF
              NQX(I,K) = CSF(K)
            ENDDO
          ENDIF
        ENDIF
C
        IF (NL.EQ.0) NW = NW - 1
        IF (NW.GT.MXNW) THEN
          WRITE (IWRITE,3020)
        ENDIF
C
        CALL DMCHK2(12,NW, IWRITE, IDMTST, NDIMAX)
C
        NLX(I) = NL
        IPOS(I) = NW
C
        DO K = 1,NMAN
          IF (NQX(I,K).EQ.0) THEN
            JCHOP(NW,K) = -1
          ELSE
            JCHOP(NW,K) = 0
          ENDIF
        ENDDO
        NP(NW) = PRINC
        NAK(NW) = KAPPA
        NH(NW) = NHX
C
        IF (NL.GT.0) THEN
          DO K = 1,NMAN
            IF (NQX(I,K).EQ.0) THEN
              JCHOP(NW-1,K) = -1
            ELSE
              JCHOP(NW-1,K) = 0
            ENDIF
          ENDDO
          NP(NW-1) = PRINC
          NAK(NW-1) = NL
          NH(NW-1) = NG(NL)
        ENDIF
C
      ENDDO
C-----------------------------------------------------------------------
C
C   Check number of electrons in each CSF
C
C-----------------------------------------------------------------------
      IF (NMAN.GT.1) THEN
        NELEC1 = 0
        DO J = 1,NWM
          NELEC1 = NELEC1+NQX(J,1)
        ENDDO
        DO I = 2,NMAN
          NELEC = 0
          DO J = 1,NWM
            NELEC = NELEC+NQX(J,I)
          ENDDO
          IF (NELEC.NE.NELEC1) THEN
            WRITE (IWRITE,3040)
            STOP
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C   Input record 4 - Options for the angular momentum programs
C
C   NAMELIST ... ANGOPT
C
C-----------------------------------------------------------------------
      DO I = 1,20
        OPT(I) = -1
        JTC(I) = 0
      ENDDO
C
      READ (IREAD,ANGOPT)
C
      DO I = 1,20
        IF (OPT(I).GE.1 .AND. OPT(I).LE.20) THEN
          JTC(OPT(I)) = 1
        ENDIF
      ENDDO
C
      IBUG1 = JTC(1)
      IBUG2 = JTC(2)
      IBUG3 = JTC(3)
      IBUG4 = JTC(4)
      IBUG5 = JTC(5)
      IBUG6 = JTC(6)
C-----------------------------------------------------------------------
C
C   Input record 5 --- non-relativistic CSF total J values
C
C   NAMELIST ... JVALUE
C
C-----------------------------------------------------------------------
      JOPT = -1
      DO JMAN = 1,MXNC
        JLOW(JMAN) = -1.D0
        JHIGH(JMAN) = -1.D0
      ENDDO
C
      READ (IREAD,JVALUE)
C
      IF (JOPT.EQ.-1) THEN
        JF = NINT(JLOW(1)+JLOW(1))
        DO JMAN = 1,NMAN
          JMINX(JMAN) = JF
          JMAXX(JMAN) = JF
          JFX(JMAN) = JF
        ENDDO
      ENDIF
C
      IF (JOPT.EQ.-2) THEN
        DO JMAN = 1,NMAN
          JMIN = NINT(JLOW(JMAN)+JLOW(JMAN))
          JMAX = NINT(JHIGH(JMAN)+JHIGH(JMAN))
          IF (JMIN.LT.0 .OR. JMAX.LT.0 .OR. JMAX.LT.JMIN) THEN
            WRITE (IWRITE,3080)
            STOP
          ENDIF
          JMINX(JMAN) = JMIN
          JMAXX(JMAN) = JMAX
          JFX(JMAN) = -2
        ENDDO
      ENDIF
C
      IF (JOPT.EQ.-3) THEN
        JMIN = NINT(JLOW(1)+JLOW(1))
        JMAX = NINT(JHIGH(1)+JHIGH(1))
        IF (JMIN.LT.0 .OR. JMAX.LT.0 .OR. JMAX.LT.JMIN) THEN
          WRITE (IWRITE,3080)
          STOP
        ENDIF
        DO JMAN = 1,NMAN
          JMINX(JMAN) = JMIN
          JMAXX(JMAN) = JMAX
          JFX(JMAN) = -2
        ENDDO
      ENDIF
C
      IF (JOPT.EQ.-4) THEN
        DO JMAN = 1,NMAN
          JF = NINT(JLOW(JMAN)+JLOW(JMAN))
          IF (JF.LT.0) THEN
            WRITE (IWRITE,3080)
            STOP
          ENDIF
          JMINX(JMAN) = JF
          JMAXX(JMAN) = JF
          JFX(JMAN) = JF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C   Loop over each non-relativistic CSF in turn.
C
C    (1) determine which shells are open
C
C    (2) set up arrays KCHOP,MLX,MQX,JPOS for call to MAN5
C        which generates relativistic CSFs
C
C-----------------------------------------------------------------------
      MFAIL = .FALSE.
      NCF = 0
      JST1 = 0
      JST2 = 0
      DO JMAN = 1,NMAN
        JMIN = JMINX(JMAN)
        JMAX = JMAXX(JMAN)
        JF = JFX(JMAN)
C
        DO I = 1,NW
          KCHOP(I) = JCHOP(I,JMAN)
        ENDDO
C
C  Select open shells
C
        NOPEN = 0
        DO I = 1,NWM
C
          NL = NLX(I)
          NQ = NQX(I,JMAN)
          NFULL = 4*NL+2
C
          IF (NQ.NE.NFULL .AND. NQ.NE.0) THEN
            NOPEN = NOPEN+1
            IF (NOPEN.GT.NOPENX) THEN
              WRITE (IWRITE,3030)
            ENDIF
            CALL DMCHK2(26,NOPEN, IWRITE, IDMTST, NDIMAX)
            MLX(NOPEN) = NL
            MQX(NOPEN) = NQ
            JPOS(NOPEN) = IPOS(I)
          ENDIF
C
        ENDDO
C
C   Set the parity
C
        IF (NOPEN.EQ.0) THEN
          IF (JF.GT.0) GOTO 10
          IPAR = 1
        ELSE
          ITEST = 0
          DO I = 1,NOPEN
            ITEST = ITEST+MLX(I)*MQX(I)
          ENDDO
          IF (MOD(ITEST,2).NE.0) THEN
            IPAR = -1
          ELSE
            IPAR = 1
          ENDIF
        ENDIF
C
C   Five open shells must always be defined in MAN5
C   and the following code includes extra dummy orbitals
C   to ensure this.
C
        IF (NOPEN.LT.NOPENX) THEN
          NOPEN1 = NOPEN+1
          II = 1
          DO I = NOPEN1,NOPENX
            MLX(I) = 0
            MQX(I) = 0
            JPOS(I) = NW+II
            II = II+1
          ENDDO
        ENDIF
C
        NST = NCF+1
        CALL MAN5(IWRITE,KCHOP,IPAR,JMIN,JMAX,MFAIL,JSTORE,ICHOP, IQ, IS!
     +PAR, ITAB, ITJPO, JCUP, JPOS, JQS, JTAB,MLX, MQX, NAK, NCF, NROWS,!
     + NTAB, NW)
C
        DO JST = JSTMAX,0,-1
          IF (JSTORE(JST).GT.0) THEN
            JST1 = JST2+1
            JST2 = JST1+JSTORE(JST)-1
            IF (MOD(JST,2).EQ.0) THEN
              IF (IPAR.EQ.1) THEN
                WRITE (IWRITE,3100) JMAN,JSTORE(JST),JST/2,JST1,JST2
              ELSE
                WRITE (IWRITE,3110) JMAN,JSTORE(JST),JST/2,JST1,JST2
              ENDIF
            ELSE
              IF (IPAR.EQ.1) THEN
                WRITE (IWRITE,3120) JMAN,JSTORE(JST),JST,JST1,JST2
              ELSE
                WRITE (IWRITE,3130) JMAN,JSTORE(JST),JST,JST1,JST2
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        IF (.NOT.MFAIL) THEN
          IF (NCF.GT.NST .AND. JF.LT.0) THEN
            CALL REORDR(NST,ICHOP, IQ, ITJPO, JCUP, JQS, NCF, NW)
          ENDIF
        ENDIF
C
   10   CONTINUE
      ENDDO
C
      IF (MFAIL) THEN
        WRITE (IWRITE,3090) NCF,MXNC
        CALL DMCHK2(07,NCF, IWRITE, IDMTST, NDIMAX)
        STOP
      ENDIF
C-----------------------------------------------------------------------
C
C   Check for orbitals with zero occupation in all CSF
C   and eliminate
C
C-----------------------------------------------------------------------
      M = 0
      DO J = 1,NW
        DO I = 1,NCF
          IF (IQ(J,I).NE.0) GOTO 20
        ENDDO
        M = M+1
        GOTO 30
C
   20   CONTINUE
        IF (M.EQ.0) GOTO 30
        NP(J-M) = NP(J)
        LABT = NH(J)
        NH(J-M) = LABT
        NAK(J-M) = NAK(J)
        DO I = 1,NCF
          DO K = 1,3
            JQS(K,J-M,I) = JQS(K,J,I)
          ENDDO
          IQ(J-M,I) = IQ(J,I)
          ICHOP(J-M,I) = ICHOP(J,I)
        ENDDO
   30   CONTINUE
      ENDDO
      NW = NW-M
C-----------------------------------------------------------------------
 3000 FORMAT (/' ERROR in DATNR : dimension ERROR for NWM ... STOPPING')
 3010 FORMAT (/' ERROR in DATNR : dimension ERROR for NMAN ... STOPPING'!
     +)
 3020 FORMAT (/' ERROR in DATNR : dimension ERROR for NW ... STOPPING')
 3030 FORMAT (/                                                         !
     +' ERROR in DATNR : dimension ERROR for NOPEN ... STOPPING')
 3040 FORMAT (/                                                         !
     +' ERROR in DATNR : inconsistency in the number of electrons ... ',!
     +'STOPPING'/' Check orbital data')
 3050 FORMAT (/                                                         !
     +' ERROR in DATNR : PRINC must be a positive integer ... STOPPING')
 3060 FORMAT (/                                                         !
     +' ERROR in DATNR : KAPPA must be a negative integer ... STOPPING')
 3070 FORMAT (/' ERROR in DATNR : A CSF value is incorrect ... STOPPING'!
     +)
 3080 FORMAT (/' ERROR in DATNR : J values are incorrect ... STOPPING')
 3090 FORMAT (/                                                         !
     +' ERROR in DATNR : the number of CSFs generated in MAN5 exceeds ',!
     +'the dimensions ... STOPPING'/'     NCF is ',I6,                  !
     +'    maximum allowed is ',I6)
 3100 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'   even (',I5,',',I5,')')
 3110 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'   odd  (',I5,',',I5,')')
 3120 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'/2 even (',I5,',',I5,')')
 3130 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'/2 odd  (',I5,',',I5,')')
      END
CEND--------------------------------------------------------------------
CEND    DELIN.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DELIN(IDISC1,IDMTST, NDIMAX, ITC, IWRITE, ICFCON, ICFCO!
     +R, NCFCON, NCFGP,ISLDR, JSLDR, MNLDR, NMCP, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DELIN.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDMTST
C   NDIMAX
C   ITC
C   IWRITE
C   ICFCON
C   ICFCOR
C   NCFCON
C   NCFGP
C   ISLDR
C   JSLDR
C   MNLDR
C   NMCP
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IDISC1
C
      DOUBLE PRECISION XSLDR(*)
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER ISLDR(*)
      INTEGER JSLDR(*)
      INTEGER MNLDR(MXNC,*)
      INTEGER NMCP
C
C  Local variables
C
      DOUBLE PRECISION X
      INTEGER I,IA,IB,IR
      INTEGER IS,J
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NMCP = 0
      IF (ITC(26).EQ.1) WRITE (IWRITE,3000)
C
      DO I = 1,NCFCON+NCFGP
        DO J = 1,ICFCON+ICFCOR
          MNLDR(I,J) = 0
        ENDDO
      ENDDO
C
C   read the coefficients
C
   10 CONTINUE
      READ (IDISC1) IR,IS,IA,IB,X
      IF (IR.EQ.0) GOTO 20
      IF (ITC(26).EQ.1) WRITE (IWRITE,3010) IR,IS,IA,IB,X
      NMCP = NMCP+1
      CALL DMCHK2(10,NMCP, IWRITE, IDMTST, NDIMAX)
      MNLDR(IR,IS) = NMCP
      ISLDR(NMCP) = IA
      JSLDR(NMCP) = IB
      XSLDR(NMCP) = X
      GOTO 10
C
   20 CONTINUE
      WRITE (IWRITE,3020) NMCP
C
 3000 FORMAT (/' Photo-ionisation angular coefficients'//4X,'r',3X,'s'/)
 3010 FORMAT (1X,2I4,3X,2I4,3X,1P,E14.7)
 3020 FORMAT (/1X,I5,' angular coefficients read by routine DELIN'/)
      END
CEND--------------------------------------------------------------------
CEND    DMCHK2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMCHK2(I,ID, IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMCHK2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
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
      IF (I.EQ.1) THEN
        PRINT 3010
        WRITE (IWRITE,3010)
        GOTO 10
      ENDIF
C
      IF (I.EQ.3) THEN
        PRINT 3020
        WRITE (IWRITE,3020)
        GOTO 10
      ENDIF
C
      IF (I.EQ.4) THEN
        PRINT 3030
        WRITE (IWRITE,3030)
        GOTO 10
      ENDIF
C
      IF (I.EQ.6) THEN
        PRINT 3040
        WRITE (IWRITE,3040)
        GOTO 10
      ENDIF
C
      IF (I.EQ.8) THEN
        PRINT 3060
        WRITE (IWRITE,3060)
        GOTO 10
      ENDIF
C
      IF (I.EQ.7) THEN
        PRINT 3050
        WRITE (IWRITE,3050)
        GOTO 10
      ENDIF
C
      IF (I.EQ.9) THEN
        PRINT 3070
        WRITE (IWRITE,3070)
        GOTO 10
      ENDIF
C
      IF (I.EQ.10) THEN
        PRINT 3080
        WRITE (IWRITE,3080)
        GOTO 10
      ENDIF
C
      IF (I.EQ.12) THEN
        PRINT 3090
        WRITE (IWRITE,3090)
        GOTO 10
      ENDIF
C
      IF (I.EQ.24) THEN
        PRINT 3100
        WRITE (IWRITE,3100)
        GOTO 10
      ENDIF
C
      IF (I.EQ.26) THEN
        PRINT 3110
        WRITE (IWRITE,3110)
        GOTO 10
      ENDIF
C
   10 CONTINUE
      CALL DMPRT2(IWRITE, IDMTST, NDIMAX)
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
     +/' IDMTST(1) contains the maximum number of channels'
     +/' It is set by variable MXCH in file darc.inc.')
 3020 FORMAT (
     +/' IDMTST(3) contains the dimension of the array'
     +/' RKSTO used to store radial integrals.'
     +/' It is set by variable MXI1 in file darc.inc.')
 3030 FORMAT (
     +/' IDMTST(4) contains the dimension of the arrays'
     +/' ISTX1 and ISTX2.'
     +/' It is set by variable MXI2 in file darc.inc.')
 3040 FORMAT (
     +/' IDMTST(6) contains the maximum number of continuum'
     +/' orbitals per angular momentum.'
     +/' It is set by variable MXNB in file darc.inc.')
 3050 FORMAT (
     +/' IDMTST(7) contains the number of relativistic'
     +/' CSFs. This is the dimension that'
     +/' is used throughout the MCP angular program.'
     +/' It is set by variable MXNC in file darc.inc.')
 3060 FORMAT (
     +/' IDMTST(8) contains the maximum value of KX.'
     +/' This is the number of bound/continuum KAPPA values.'
     +/' It is set by variable MXNK in file darc.inc.')
 3070 FORMAT (
     +/' IDMTST(9) contains the maximum number of'
     +/' target CSFs or correlation functions.'
     +/' It is set by variable MXNL in file darc.inc.')
 3080 FORMAT (
     +/' IDMTST(10) contains the maximum number of MCP'
     +/' angular coefficients.'
     +/' It is set by variable MXNM in file darc.inc.')
 3090 FORMAT (
     +/' IDMTST(12) contains the number of relativistic'
     +/' orbitals. This is the dimension that is set'
     +/' throughout the MCP angular program.'
     +/' It is set by variable MXNW in file darc.inc.')
 3100 FORMAT (
     +/' IDMTST(24) contains the maximum number of'
     +/' regular mesh intervals (variable NIX).'
     +/' This is the dimension of array IRX used in routine WRITP2.'
     +/' It is set by variable NIRX.')
 3110 FORMAT (
     +/' IDMTST(26) contains the maximum number of'
     +/' open shells in a non-relativistic CSF.'
     +/' At present this MUST be 5.')
 3120 FORMAT (
     +/'      *******************************************'
     +/'      *** Module terminates in routine DMCHK2 ***'
     +/'      *******************************************'
     +)
      END
CEND--------------------------------------------------------------------
CEND    DMEL.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMEL(IDISC1,ITAPE2,EMTR, IBBPOL, IBCPOL, ICCPOL, ICFCON!
     +, ICFCOR, IDMTST,IJ2P1, INAKK, INCCP, INCHAN, INTARG, IRK1, ISLDR,!
     + ITC,IWRITE, J2P1, JSLDR, K1S, K2S, KBMAX, KCMAX, KMULTN, KMULTR,L!
     +AG, LAMBB, LAMBC, LAMCC, MAXNQN, MNLDR, NAK,NAKK, NCCP, NCF, NCFCO!
     +N, NCFGP, NCHAN, NDIMAX, NH, NMCP,NP, NRANG2, NTARG, RKSTO, XSLDR !
c kab
     +,K2P
c kab
     +)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMEL.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   EMTR
C   IBBPOL
C   IBCPOL
C   ICCPOL
C   ICFCON
C   ICFCOR
C   IDMTST
C   IJ2P1
C   INAKK
C   INCCP
C   INCHAN
C   INTARG
C   IRK1
C   ISLDR
C   ITC
C   IWRITE
C   J2P1
C   JSLDR
C   K1S
C   K2S
C   KBMAX
C   KCMAX
C   KMULTN
C   KMULTR
C   LAG
C   LAMBB
C   LAMBC
C   LAMCC
C   MAXNQN
C   MNLDR
C   NAK
C   NAKK
C   NCCP
C   NCF
C   NCFCON
C   NCFGP
C   NCHAN
C   NDIMAX
C   NH
C   NMCP
C   NP
C   NRANG2
C   NTARG
C   RKSTO
C   XSLDR
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
      DOUBLE PRECISION HALF
      PARAMETER (HALF=.5D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      INTEGER N1NML
      PARAMETER (N1NML=MXNB)
      INTEGER N2NML
      PARAMETER (N2NML=MXNL)
      INTEGER INML
      PARAMETER (INML=N1NML/N2NML)
      INTEGER JNML
      PARAMETER (JNML=N2NML/N1NML)
      INTEGER KNML
      PARAMETER (KNML=INML+JNML)
      INTEGER ND24
      PARAMETER (ND24=N1NML*INML/KNML+N2NML*JNML/KNML)
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      INTEGER IDISC1,ITAPE2
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION EMTR(MXNL,*)
      DOUBLE PRECISION RKSTO(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IBCPOL(NDX,MXNK,*)
      INTEGER ICCPOL(MXNK,MXNK,*)
      INTEGER ICFCON
      INTEGER ICFCOR
      INTEGER IDMTST(*)
      INTEGER IJ2P1
      INTEGER INAKK(*)
      INTEGER INCCP(MXNL,*)
      INTEGER INCHAN
      INTEGER INTARG(*)
      INTEGER IRK1
      INTEGER ISLDR(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JSLDR(*)
      INTEGER K1S
      INTEGER K2S
c kab
      INTEGER K2P(*)
c kab
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KMULTN
      INTEGER KMULTR
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXNQN(*)
      INTEGER MNLDR(MXNC,*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NCCP(MXNL,*)
      INTEGER NCF
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NCHAN
      INTEGER NDIMAX(*)
      INTEGER NMCP
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NTARG(*)
C
C  Local variables
C
      DOUBLE PRECISION CAMP,CGC(20),CLEB,COEF
      DOUBLE PRECISION FACTOR,WX,WY,XINTL
      DOUBLE PRECISION XINTV,XJP,XM,XMJP
      DOUBLE PRECISION FINTL(MXNB,MXNB),FINTV(MXNB,MXNB)
      DOUBLE PRECISION DEL(ND24,ND24)
      DOUBLE PRECISION DEV(ND24,ND24)
      INTEGER I,IA,IB,IER
      INTEGER II,IL,IMARK,IP
      INTEGER IT,ITEST,J,JJ
      INTEGER JP,JT,K
      INTEGER KI,KJ,L
      INTEGER L1,L2,LAM,M
      INTEGER MAXM1,MJP,MX
C
ckab for outer region dipole
      DOUBLE PRECISION AC(MXCH,MXCH), BLC(MXCH,MXCH), BVC(MXCH,MXCH)
      INTEGER ICHANL,JCHANL
ckab
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020)
C
C  read the angular coefficients
C
      CALL DELIN(IDISC1,IDMTST, NDIMAX, ITC, IWRITE, ICFCON, ICFCOR, NCF!
     +CON, NCFGP,ISLDR, JSLDR, MNLDR, NMCP, XSLDR)
C
C  locate integral block
C
      CALL LOCM(999,999, K1S, K2S, RKSTO,IBBPOL, IBCPOL, ICCPOL, IRK1, I!
     +TC, IWRITE,KMULTN, KMULTR, KBMAX, KCMAX, LAMBB, LAMBC, LAMCC)
C
C  electric dipole transitions
C
      LAM = 1
      CAMP = SQRT(DBLE(J2P1))
C
C  loop over the channels
C
      DO I = 1,NCHAN
C
C  continuum-continuum terms
C  -------------------------
C
        KI = NAKK(I)
        IT = NTARG(I)
        DO J = 1,INCHAN
C
ckab for outer region dipole
          AC(J,I)  = ZERO
          BLC(J,I) = ZERO
          BVC(J,I) = ZERO
          ICHANL   = K2P(I)
          IF( K2P(I).lt.0 ) ICHANL = -K2P(I)-1
          JCHANL   = K2P(J)
          IF( K2P(J).lt.0 ) JCHANL = -K2P(J)-1
ckab
          KJ = INAKK(J)
          JT = INTARG(J)
C
          IMARK = 0
          DO L1 = 1,NRANG2
            DO L2 = 1,NRANG2
              DEL(L1,L2) = ZERO
              DEV(L1,L2) = ZERO
            ENDDO
          ENDDO
C
C   NCF here is the number of target CSFs (=NCFTAR)
C
          DO II = 1,NCF
            IP = NCCP(II,I)
            IF (IP.GT.0) THEN
C
              DO JJ = 1,NCF
                JP = INCCP(JJ,J)
                IF (JP.GT.0) THEN
                  IL = MNLDR(IP,JP)
                  IF (IL.GT.0) THEN
C
                    WX = EMTR(II,IT)*EMTR(JJ,JT)
                    WY = XSLDR(IL)
                    COEF = WX*WY*CAMP
                    CALL FINMCC(WX,WY,KI,KJ,LAM,FINTL,FINTV,ICCPOL, IRK1!
     +, ITC, IWRITE, NRANG2, RKSTO)
                    IMARK = 1
                    DO L1 = 1,NRANG2
                      DO L2 = 1,NRANG2
                        DEL(L1,L2) = DEL(L1,L2)+COEF*FINTL(L1,L2)
                        DEV(L1,L2) = DEV(L1,L2)+COEF*FINTV(L1,L2)
                      ENDDO
                    ENDDO
                    IF (ITC(25).EQ.1) THEN
                      WRITE (IWRITE,3030) I,J
                      WRITE (IWRITE,3070) IP,JP,WX,WY
                      CALL MATOUT(IWRITE,FINTL,NRANG2,NRANG2,MXNB,MXNB,2!
     +)
                      CALL MATOUT(IWRITE,FINTV,NRANG2,NRANG2,MXNB,MXNB,2!
     +)
                    ENDIF
C
ckab for outer region dipole
                    IF( ICHANL.EQ.JCHANL ) THEN
                      BLC(J,I) = DEL(1,1)
                      BVC(J,I) = DEV(1,1)
                    ELSE
                      AC(J,I)  = AC(J,I) + COEF
                    ENDIF
ckab
C
                  ENDIF
                ENDIF
              ENDDO
C
            ENDIF
          ENDDO
C
          IF (IMARK.GT.0) THEN
            WRITE (ITAPE2) I,J,NRANG2,NRANG2
            WRITE (ITAPE2) ((DEL(L1,L2),L1=1,NRANG2),L2=1,NRANG2)
            WRITE (ITAPE2) ((DEV(L1,L2),L1=1,NRANG2),L2=1,NRANG2)
            IF (ITC(25).EQ.1) THEN
              WRITE (IWRITE,3030) I,J
              CALL MATOUT(IWRITE,DEL,NRANG2,NRANG2,ND24,ND24,2)
              CALL MATOUT(IWRITE,DEV,NRANG2,NRANG2,ND24,ND24,2)
            ENDIF
          ENDIF
C
        ENDDO
C
C  continuum-bound terms
C  ---------------------
C
        IF (ICFCOR.GT.0) THEN
C
          IMARK = 0
          DO L2 = 1,ICFCOR
            JP = ICFCON+L2
C
            DO L1 = 1,NRANG2
              DEL(L1,L2) = ZERO
              DEV(L1,L2) = ZERO
            ENDDO
C
            DO II = 1,NCF
              IP = NCCP(II,I)
              IF (IP.GT.0) THEN
                IL = MNLDR(IP,JP)
                IF (IL.GT.0) THEN
                  WX = EMTR(II,IT)
                  WY = XSLDR(IL)
                  COEF = WX*WY*CAMP
                  IB = JSLDR(IL)
                  CALL FINMBC(WX,WY,KI,IB,LAM,FINTL,FINTV,IBCPOL, IRK1, !
     +ITC, IWRITE, LAG, NAK, NH, NP, NRANG2, RKSTO)
                  IMARK = 1
                  DO L1 = 1,NRANG2
                    DEL(L1,L2) = DEL(L1,L2)+COEF*FINTL(L1,1)
                    DEV(L1,L2) = DEV(L1,L2)-COEF*FINTV(L1,1)
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
C
          IF (IMARK.GT.0) THEN
            J = 0
            WRITE (ITAPE2) I,J,NRANG2,ICFCOR
            WRITE (ITAPE2) ((DEL(L1,L2),L1=1,NRANG2),L2=1,ICFCOR)
            WRITE (ITAPE2) ((DEV(L1,L2),L1=1,NRANG2),L2=1,ICFCOR)
            IF (ITC(25).EQ.1) THEN
              WRITE (IWRITE,3040) I
              CALL MATOUT(IWRITE,DEL,NRANG2,ICFCOR,ND24,ND24,2)
              CALL MATOUT(IWRITE,DEV,NRANG2,ICFCOR,ND24,ND24,2)
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDDO
C
C  bound-continuum terms
C  ---------------------
C
      IF (NCFGP.GT.0) THEN
C
C  loop over the channels
C
        DO J = 1,INCHAN
          KJ = INAKK(J)
          JT = INTARG(J)
C
          IMARK = 0
          DO L1 = 1,NCFGP
            IP = NCFCON+L1
C
            DO L2 = 1,NRANG2
              DEL(L1,L2) = ZERO
              DEV(L1,L2) = ZERO
            ENDDO
C
            DO JJ = 1,NCF
              JP = INCCP(JJ,J)
              IF (JP.GT.0) THEN
                IL = MNLDR(IP,JP)
                IF (IL.GT.0) THEN
                  WX = EMTR(JJ,JT)
                  WY = XSLDR(IL)
                  COEF = WX*WY*CAMP
                  IB = ISLDR(IL)
                  CALL FINMBC(WX,WY,KJ,IB,LAM,FINTL,FINTV,IBCPOL, IRK1, !
     +ITC, IWRITE, LAG, NAK, NH, NP, NRANG2, RKSTO)
                  IMARK = 1
                  DO L2 = 1,NRANG2
                    DEL(L1,L2) = DEL(L1,L2)+COEF*FINTL(L2,1)
                    DEV(L1,L2) = DEV(L1,L2)+COEF*FINTV(L2,1)
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
C
          ENDDO
C
          IF (IMARK.GT.0) THEN
            I = 0
            WRITE (ITAPE2) I,J,NCFGP,NRANG2
            WRITE (ITAPE2) ((DEL(L1,L2),L1=1,NCFGP),L2=1,NRANG2)
            WRITE (ITAPE2) ((DEV(L1,L2),L1=1,NCFGP),L2=1,NRANG2)
            IF (ITC(25).EQ.1) THEN
              WRITE (IWRITE,3050) J
              CALL MATOUT(IWRITE,DEL,NCFGP,NRANG2,ND24,ND24,2)
              CALL MATOUT(IWRITE,DEV,NCFGP,NRANG2,ND24,ND24,2)
            ENDIF
          ENDIF
C
        ENDDO
C
C  bound-bound terms
C  -----------------
C
        IF (ICFCOR.GT.0) THEN
C
          IMARK = 0
          DO L1 = 1,NCFGP
            IP = NCFCON+L1
            DO L2 = 1,ICFCOR
              JP = ICFCON+L2
              DEL(L1,L2) = ZERO
              DEV(L1,L2) = ZERO
              IL = MNLDR(IP,JP)
              IF (IL.GT.0) THEN
                COEF = XSLDR(IL)*CAMP
                IA = ISLDR(IL)
                IB = JSLDR(IL)
                CALL FINMBB(COEF,IA,IB,LAM,XINTL,XINTV,IBBPOL, IRK1, IWR!
     +ITE, ITC, LAG, MAXNQN, NAK, NH, NP, RKSTO)
                IMARK = 1
                DEL(L1,L2) = DEL(L1,L2)+COEF*XINTL
                DEV(L1,L2) = DEV(L1,L2)+COEF*XINTV
              ENDIF
            ENDDO
          ENDDO
C
          IF (IMARK.GT.0) THEN
            I = 0
            J = 0
            WRITE (ITAPE2) I,J,NCFGP,ICFCOR
            WRITE (ITAPE2) ((DEL(L1,L2),L1=1,NCFGP),L2=1,ICFCOR)
            WRITE (ITAPE2) ((DEV(L1,L2),L1=1,NCFGP),L2=1,ICFCOR)
            IF (ITC(25).EQ.1) THEN
              WRITE (IWRITE,3060)
              CALL MATOUT(IWRITE,DEL,NCFGP,ICFCOR,ND24,ND24,2)
              CALL MATOUT(IWRITE,DEV,NCFGP,ICFCOR,ND24,ND24,2)
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDIF
C
C  mark end of this data on file
C  -----------------------------
C
      I = -1
      J = -1
      K = -1
      L = -1
      WRITE (ITAPE2) I,J,K,L
C
C  evaluate 3J symbol for polarisability
C  -------------------------------------
C
      JP = J2P1-1
      MJP = IJ2P1-1
      XJP = DBLE(JP)*HALF
      XMJP = DBLE(MJP)*HALF
      FACTOR = ONE/SQRT(TWO*XMJP+ONE)
      MAXM1 = MIN(JP,MJP)+1
      MX = 1
      IF (MOD(MAXM1,2).EQ.0) MX = MX + 1
      IF (MAXM1.GT.20) THEN
        WRITE (IWRITE,3080) MAXM1
        STOP
      ENDIF
C
      DO M = 1,MAXM1
        CGC(M) = ZERO
      ENDDO
      ITEST = (JP-2+MX-1)/2
      IF (MOD(ITEST,2).EQ.1) FACTOR = -FACTOR
      IF (ITC(25).EQ.1) WRITE (IWRITE,3010)
      DO M = MX,MAXM1,2
        XM = (M-1)*HALF
        CALL CLEGOR(XJP,ONE,XM,ZERO,XMJP,XM,CLEB,IER)
        CGC(M) = FACTOR*CLEB
        IF (ITC(25).EQ.1) WRITE (IWRITE,3000) M,XJP,ONE,XM,ZERO,XMJP,XM,!
     +CLEB,FACTOR,CGC(M)
        FACTOR = -FACTOR
      ENDDO
C
C  write on tape the 3J symbol for dipole matrix
C  ---------------------------------------------
C
      WRITE (ITAPE2) MAXM1
      WRITE (ITAPE2) (CGC(M),M=1,MAXM1)
C
ckab for outer region dipole
      WRITE (ITAPE2) ((AC(J,I),J=1,INCHAN),I=1,NCHAN)
      WRITE (ITAPE2) ((BLC(J,I),J=1,INCHAN),I=1,NCHAN)
      WRITE (ITAPE2) ((BVC(J,I),J=1,INCHAN),I=1,NCHAN)
ckab
C
 3000 FORMAT (1X,I3,'  (',3(F5.1,',',F5.1,';'),E13.5,')',2(2X,E13.5))
 3010 FORMAT (/                                                         !
     +'   M  (   J1,   J2;   M1,   M2;   J3,   M3;         CLEB)      ',!
     +'    FACTOR            CGC')
 3020 FORMAT (/                                                         !
     +' ===== DMEL called. Evaluate photo-ionisation matrix elements.')
 3030 FORMAT (/' continuum-continuum contributions: channels ',2I6/)
 3040 FORMAT (/' continuum-bound contributions: channel ',I6/)
 3050 FORMAT (/' bound-continuum contributions: channel ',I6/)
 3060 FORMAT (/' bound-bound contributions'/)
 3070 FORMAT (' contribution from CSFs:',2I6,4X,'weight = ',1P,E12.5,4X,!
     +'angular coefficient = ',E12.5)
 3080 FORMAT (/                                                         !
     +' STOPPING in DMEL because the dimension of array CGC has been e',!
     +'xceeded.'/' Maximum = ',I4,' is greater than 20.')
      END
CEND--------------------------------------------------------------------
CEND    DMPRT2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMPRT2(IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMPRT2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
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
      WRITE (IWRITE,3010) IDMTST(1),NDIMAX(1)
      WRITE (IWRITE,3020) IDMTST(3),NDIMAX(3)
      WRITE (IWRITE,3030) IDMTST(4),NDIMAX(4)
      WRITE (IWRITE,3040) IDMTST(6),NDIMAX(6)
      WRITE (IWRITE,3050) IDMTST(7),NDIMAX(7)
      WRITE (IWRITE,3060) IDMTST(8),NDIMAX(8)
      WRITE (IWRITE,3070) IDMTST(9),NDIMAX(9)
      WRITE (IWRITE,3080) IDMTST(10),NDIMAX(10)
      WRITE (IWRITE,3090) IDMTST(12),NDIMAX(12)
      WRITE (IWRITE,3100) IDMTST(24),NDIMAX(24)
      WRITE (IWRITE,3110) IDMTST(26),NDIMAX(26)
C-----------------------------------------------------------------------
 3000 FORMAT (/31X,'Routine DMPRT2'/31X,'--------------'//              !
     +' The following information relates to the dimensions set in the',!
     +' code.'/)
 3010 FORMAT (' IDMTST(01)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXCH')
 3020 FORMAT (' IDMTST(03)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXI1')
 3030 FORMAT (' IDMTST(04)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXI2')
 3040 FORMAT (' IDMTST(06)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNB')
 3050 FORMAT (' IDMTST(07)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNC')
 3060 FORMAT (' IDMTST(08)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNK')
 3070 FORMAT (' IDMTST(09)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNL')
 3080 FORMAT (' IDMTST(10)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNM')
 3090 FORMAT (' IDMTST(12)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNW')
 3100 FORMAT (' IDMTST(24)   set to : ',I9,'   maximum used : ',I9)
 3110 FORMAT (' IDMTST(26)   set to : ',I9,'   maximum used : ',I9)
      END
CEND--------------------------------------------------------------------
CEND    DMSET2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DMSET2(IWRITE, IDMTST, NDIMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DMSET2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
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
      WRITE (IWRITE,3020) MXCH
      WRITE (IWRITE,3030) MXI1
      WRITE (IWRITE,3040) MXI2
      WRITE (IWRITE,3050) MXNB
      WRITE (IWRITE,3060) MXNC
      WRITE (IWRITE,3070) MXNK
      WRITE (IWRITE,3080) MXNL
      WRITE (IWRITE,3090) MXNM
      WRITE (IWRITE,3100) MXNW
      WRITE (IWRITE,3110)
C
      IF (MXNW.LT.10) THEN
        PRINT 3120
        WRITE (IWRITE,3120)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      IDMTST(01) = MXCH
      IDMTST(03) = MXI1
      IDMTST(04) = MXI2
      IDMTST(06) = MXNB
      IDMTST(07) = MXNC
      IDMTST(08) = MXNK
      IDMTST(09) = MXNL
      IDMTST(10) = MXNM
      IDMTST(12) = MXNW
      IDMTST(24) = NIRX
      IDMTST(26) = 5
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/                                              !
     +' routine DMSET2 : the code has been dimensioned as follows'/1X,71!
     +('*'))
 3010 FORMAT (/' The following dimensions have been set :'/)
 3020 FORMAT (1X,I9,' = CH  channels')
 3030 FORMAT (1X,I9,' = I1  radial integrals')
 3040 FORMAT (1X,I9,' = I2  marks position of radial integrals')
 3050 FORMAT (1X,I9,' = NB  continuum orbitals per KAPPA value')
 3060 FORMAT (1X,I9,' = NC  relativistic CSFs')
 3070 FORMAT (1X,I9,' = NK  continuum angular momenta (KAPPA values)')
 3080 FORMAT (1X,I9,' = NL  target levels or correlation functions')
 3090 FORMAT (1X,I9,' = NM  MCP angular coefficients')
 3100 FORMAT (1X,I9,' = NW  relativistic orbitals')
 3110 FORMAT (/1X,71('*'))
 3120 FORMAT (/' STOPPING in DMSET2'/' NW must be at least 10')
      END
CEND--------------------------------------------------------------------
CEND    FINBB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINBB(
     + RESULT, ICON, JA, JB, JC, JD, LAM, ICTXB, IRK1, ISTX1B, ISTX2B,
     + ISTXB, ITC, IWRITE, KBMAX, MAXFUL, MAXNQN, MINNQN, NAK, NH, NP,
     + RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINBB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C The input arguments JA, JB, JC, JD are not reordered in the routine.
C
C   RESULT
C   ICON
C   JA
C   JB
C   JC
C   JD
C   LAM
C   ICTXB
C   IRK1
C   ISTX1B
C   ISTX2B
C   ISTXB
C   ITC
C   IWRITE
C   KBMAX
C   MAXFUL
C   MAXNQN
C   MINNQN
C   NAK
C   NH
C   NP
C   RKSTO
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      DOUBLE PRECISION RESULT
      INTEGER ICON,JA,JB,JC
      INTEGER JD,LAM
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IRK1
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KBMAX
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NP(*)
C
C  Local variables
C
      INTEGER IL,ILX,IRK,IS
      INTEGER JJA,JJB,JJC,JJD
      INTEGER JJS,K1,K2,K3
      INTEGER K4,KAP1,KAP2,KAP3
      INTEGER KAP4,KS,N1,N2
      INTEGER N3,N4,NS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3060)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      IF (ICON.EQ.1) GOTO 10
      IF (ICON.EQ.2) GOTO 20
      IF (ICON.EQ.3) GOTO 30
      IF (ICON.EQ.4) GOTO 40
C-----------------------------------------------------------------------
      WRITE (IWRITE,3040) ICON
      STOP
C-----------------------------------------------------------------------
C
C   I integrals
C
C-----------------------------------------------------------------------
   10 CONTINUE
      JJA = JA
      JJB = JB
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N2 = NP(JB)
C
      IF (N1.LT.N2) THEN
        JJS = JJA
        JJA = JJB
        JJB = JJS
        NS = N1
        N1 = N2
        N2 = NS
      ENDIF
C
      CALL PLACE2(N1,K1,N2,IS,MAXFUL,MINNQN)
      IS = IS-1
      IRK = ISTXB(K1)+IS
C
      IF (IRK.LT.1 .OR. IRK.GT.IRK1) THEN
        WRITE (IWRITE,3050) IRK,IRK1
        STOP
      ENDIF
C
      RESULT = RKSTO(IRK)
      IF (ITC(38).EQ.1) THEN
        WRITE (IWRITE,3000) NP(JJA),NH(JJA),NP(JJB),NH(JJB),RESULT,IRK
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
C
C   F integrals
C
C-----------------------------------------------------------------------
   20 CONTINUE
      JJA = JA
      JJB = JB
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
C
      IF (K1.GT.K2) THEN
        JJS = JJA
        JJA = JJB
        JJB = JJS
        KS = K1
        K1 = K2
        K2 = KS
        NS = N1
        N1 = N2
        N2 = NS
      ELSE
        IF (K1.EQ.K2) THEN
          IF (N1.LT.N2) THEN
            JJS = JJA
            JJA = JJB
            JJB = JJS
            NS = N1
            N1 = N2
            N2 = NS
          ENDIF
        ENDIF
      ENDIF
C
      N3 = N1
      K3 = K1
      N4 = N2
      K4 = K2
      GOTO 50
C-----------------------------------------------------------------------
C
C   G integrals
C
C-----------------------------------------------------------------------
   30 CONTINUE
      JJA = JA
      JJC = JB
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N3 = NP(JB)
      KAP3 = NAK(JB)
      K3 = KAP3+KAP3
      IF (KAP3.LT.0) K3 = - (K3+1)
C
      IF (K1.GT.K3) THEN
        JJS = JJA
        JJA = JJC
        JJC = JJS
        KS = K1
        K1 = K3
        K3 = KS
        NS = N1
        N1 = N3
        N3 = NS
      ELSE
        IF (K1.EQ.K3) THEN
          IF (N1.LT.N3) THEN
            JJS = JJA
            JJA = JJC
            JJC = JJS
            NS = N1
            N1 = N3
            N3 = NS
          ENDIF
        ENDIF
      ENDIF
C
      N2 = N1
      K2 = K1
      N4 = N3
      K4 = K3
      GOTO 50
C-----------------------------------------------------------------------
C
C   R integrals
C
C-----------------------------------------------------------------------
   40 CONTINUE
      JJA = JA
      JJC = JC
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N3 = NP(JC)
      KAP3 = NAK(JC)
      K3 = KAP3+KAP3
      IF (KAP3.LT.0) K3 = - (K3+1)
C
      IF (K1.GT.K3) THEN
        JJS = JJA
        JJA = JJC
        JJC = JJS
        KS = K1
        K1 = K3
        K3 = KS
        NS = N1
        N1 = N3
        N3 = NS
      ELSE
        IF (K1.EQ.K3) THEN
          IF (N1.LT.N3) THEN
            JJS = JJA
            JJA = JJC
            JJC = JJS
            NS = N1
            N1 = N3
            N3 = NS
          ENDIF
        ENDIF
      ENDIF
C
      JJB = JB
      JJD = JD
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
      N4 = NP(JD)
      KAP4 = NAK(JD)
      K4 = KAP4+KAP4
      IF (KAP4.LT.0) K4 = - (K4+1)
C
      IF (K2.GT.K4) THEN
        JJS = JJB
        JJB = JJD
        JJD = JJS
        KS = K2
        K2 = K4
        K4 = KS
        NS = N2
        N2 = N4
        N4 = NS
      ELSE
        IF (K2.EQ.K4) THEN
          IF (N2.LT.N4) THEN
            JJS = JJB
            JJB = JJD
            JJD = JJS
            NS = N2
            N2 = N4
            N4 = NS
          ENDIF
        ENDIF
      ENDIF
C
      IF (K1.GT.K2) THEN
        JJS = JJA
        JJA = JJB
        JJB = JJS
        KS = K1
        K1 = K2
        K2 = KS
        NS = N1
        N1 = N2
        N2 = NS
        JJS = JJC
        JJC = JJD
        JJD = JJS
        KS = K3
        K3 = K4
        K4 = KS
        NS = N3
        N3 = N4
        N4 = NS
      ELSE
        IF (K1.EQ.K2) THEN
          IF (N1.LT.N2) THEN
            JJS = JJA
            JJA = JJB
            JJB = JJS
            NS = N1
            N1 = N2
            N2 = NS
            JJS = JJC
            JJC = JJD
            JJD = JJS
            KS = K3
            K3 = K4
            K4 = KS
            NS = N3
            N3 = N4
            N4 = NS
          ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
   50 CONTINUE
      CALL PLACE3(N1,K1,N2,K2,N3,K3,N4,K4,IS,MAXFUL,MAXNQN,MINNQN)
      IS = IS-1
C
      IRK = ICTXB(K1,K2,K3)
      IL = LAM*(KBMAX+1)+K4
   60 CONTINUE
      ILX = ISTX1B(IRK)
      IF (IL.EQ.ILX) GOTO 70
      IRK = IRK+1
      GOTO 60
C
   70 CONTINUE
      IRK = ISTX2B(IRK)+IS
C
      IF (IRK.LT.1 .OR. IRK.GT.IRK1) THEN
        WRITE (IWRITE,3050) IRK,IRK1
        STOP
      ENDIF
C
      RESULT = RKSTO(IRK)
      IF (ITC(38).EQ.1) THEN
C
        IF (ICON.EQ.2) THEN
          WRITE (IWRITE,3010)
     + NP(JJA),NH(JJA),NP(JJB),NH(JJB),LAM,RESULT,IRK
          RETURN
        ENDIF
C
        IF (ICON.EQ.3) THEN
          WRITE (IWRITE,3020)
     + NP(JJA),NH(JJA),NP(JJC),NH(JJC),LAM,RESULT,IRK
          RETURN
        ENDIF
C
        IF (ICON.EQ.4) THEN
          WRITE (IWRITE,3030)
     + NP(JJA),NH(JJA),NP(JJB),NH(JJB),NP(JJC),
     + NH(JJC),NP(JJD),NH(JJD),LAM,RESULT,IRK
          RETURN
        ENDIF
C
      ENDIF
C=======================================================================
 3000 FORMAT (' BB I',2(I3,A2,3X),25X,1P,E14.7,2X,I5)
 3010 FORMAT (' BB F',2(I3,A2,3X),18X,I3,4X,1P,E14.7,2X,I5)
 3020 FORMAT (' BB G',2(I3,A2,3X),18X,I3,4X,1P,E14.7,2X,I5)
 3030 FORMAT (' BB R',4(I3,A2,3X),2X,I3,4X,1P,E14.7,2X,I5)
 3040 FORMAT (' ERROR in FINBB : incorrect value for ICON = ',I5)
 3050 FORMAT (/' STOPPING in routine FINBB.'/
     +' There is a problem with locating the integrals.'/
     +' The integral is at position          : ',I7/
     +' The number of integrals available is : ',I7/
     +' You may have incorrect input data.'/
     +' Check that the full shells (core) is used correctly.')
 3060 FORMAT (/' STOPPING in routine FINBB.'/
     +' You have requested an integral but'/
     +' there are no integrals on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FINBC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINBC(IDISC2,JJ,WA,ICON,JJA,JJB,JJC,JJD,LAM,TOTAL,ITC, !
     +IWRITE, K1S, K2S, KBCN1, KBCN2, KBCR,KBMAX, KCMAX, KCMIN, KPOS, MA!
     +XFUL, MAXNQN,MAXP, MINNQN, NAK, NAKK, NH, NP, NRANG2,NW, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINBC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C The input arguments JJA, JJB, JJC, JJD are not reordered in the
C routine.
C
C   IDISC2
C   JJ
C   WA
C   ICON
C   JJA
C   JJB
C   JJC
C   JJD
C   LAM
C   TOTAL
C   ITC
C   IWRITE
C   K1S
C   K2S
C   KBCN1
C   KBCN2
C   KBCR
C   KBMAX
C   KCMAX
C   KCMIN
C   KPOS
C   MAXFUL
C   MAXNQN
C   MAXP
C   MINNQN
C   NAK
C   NAKK
C   NH
C   NP
C   NRANG2
C   NW
C   RKSTO
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
C
C  Argument variables
C
      DOUBLE PRECISION TOTAL(MXNB,*),WA
      INTEGER ICON,IDISC2,JJ,JJA
      INTEGER JJB,JJC,JJD,LAM
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER K1S
      INTEGER K2S
      INTEGER KBCN1(*)
      INTEGER KBCN2(*)
      INTEGER KBCR(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KPOS
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NW
C
C  Local variables
C
      DOUBLE PRECISION FA
      INTEGER I,I1,IL,ILX
      INTEGER IRK,IS,IST,JA
      INTEGER JB,JC,JD,JS
      INTEGER K1,K2,K3,K4
      INTEGER KAP1,KAP2,KAP3,KAP4
      INTEGER KS,LST,N1,N2
      INTEGER N3,NS
      INTEGER ICTX(NDX,NDX,NDX)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISTX1(MXI2)
      INTEGER ISTX2(MXI2)
C
      SAVE ICTX
      SAVE ISTX1
      SAVE ISTX2
      SAVE IRK1
      SAVE IRK2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      JA = JJA
      JB = JJB
      JC = JJC
      JD = JJD
      IF (ICON.EQ.1) GOTO 10
      IF (ICON.EQ.2) GOTO 20
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020) ICON
      STOP
C-----------------------------------------------------------------------
C
C   I INTEGRALS
C
C-----------------------------------------------------------------------
   10 CONTINUE
      IF (JA.GT.NW) THEN
        JS = JA
        JA = JB
        JB = JS
      ENDIF
C
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
C
      CALL LOCBC(IDISC2,K1,0, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ISTX1, I!
     +STX2, IRK1, IRK2, ITC, IWRITE,KBCN1, KBCN2, KBCR, KBMAX, KCMAX, KC!
     +MIN)
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3050)
        STOP
      ENDIF
C
      I1 = N1-MINNQN(K1)
      IS = I1*NRANG2
      IRK = 1+IS
      IST = IRK
C
      LST = IST+NRANG2-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3040) IST,LST,IRK1
        STOP
      ENDIF
C
      DO I = 1,NRANG2
        TOTAL(I,JJ) = TOTAL(I,JJ)+WA*RKSTO(IRK)
        IRK = IRK+1
      ENDDO
C
      IF (ITC(39).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3000) NP(JA),NH(JA),NP(JB),NH(JB),WA,FA,IST
        IF (ITC(41).EQ.1) THEN
          WRITE (IWRITE,3030) (RKSTO(I),I=IST,LST)
        ENDIF
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
C
C   S INTEGRALS
C
C-----------------------------------------------------------------------
   20 CONTINUE
      IF (JD-NW) 30,30,60
   30 CONTINUE
      IF (JB-NW) 50,50,40
   40 CONTINUE
      JS = JB
      JB = JD
      JD = JS
      GOTO 60
C
   50 CONTINUE
      JS = JA
      JA = JB
      JB = JS
      JS = JC
      JC = JD
      JD = JS
      GOTO 20
C
   60 CONTINUE
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N3 = NP(JC)
      KAP3 = NAK(JC)
      K3 = KAP3+KAP3
      IF (KAP3.LT.0) K3 = - (K3+1)
C
      IF (K1.GT.K3) THEN
        JS = JA
        JA = JC
        JC = JS
        KS = K1
        K1 = K3
        K3 = KS
        NS = N1
        N1 = N3
        N3 = NS
      ELSE
        IF (K1.EQ.K3) THEN
          IF (N1.LT.N3) THEN
            JS = JA
            JA = JC
            JC = JS
            NS = N1
            N1 = N3
            N3 = NS
          ENDIF
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
C
      KAP4 = JD-NW
      K4 = NAKK(KAP4)
C
      CALL PLACE1(N1,K1,N2,K2,N3,K3,IS, MAXFUL,MAXNQN,MINNQN )
      IS = (IS-1)*NRANG2
C
      CALL LOCBC(IDISC2,K4,0, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ISTX1, I!
     +STX2, IRK1, IRK2, ITC, IWRITE,KBCN1, KBCN2, KBCR, KBMAX, KCMAX, KC!
     +MIN)
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3050)
        STOP
      ENDIF
      IRK = ICTX(K1,K2,K3)
      IL = LAM
C-----------------------------------------------------------------------
   70 CONTINUE
      ILX = ISTX1(IRK)
      IF (IL.EQ.ILX) GOTO 80
      IRK = IRK+1
      GOTO 70
C-----------------------------------------------------------------------
   80 CONTINUE
      IRK = ISTX2(IRK)+IS
      IST = IRK
C
      LST = IST+NRANG2-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3040) IST,LST,IRK1
        STOP
      ENDIF
C
      DO I = 1,NRANG2
        TOTAL(I,JJ) = TOTAL(I,JJ)+WA*RKSTO(IRK)
        IRK = IRK+1
      ENDDO
C
      IF (ITC(39).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3010) NP(JA),NH(JA),NP(JB),NH(JB),NP(JC),NH(JC),NP!
     +(JD),NH(JD),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
          WRITE (IWRITE,3030) (RKSTO(I),I=IST,LST)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (' BC I',2X,2(I3,A2),19X,1P,2E12.4,I6)
 3010 FORMAT (' BC S',2X,4(I3,A2),2X,I3,4X,1P,2E12.4,I6)
 3020 FORMAT (' ERROR in FINBC : incorrect value for ICON = ',I5)
 3030 FORMAT (1X,1P,5E12.4)
 3040 FORMAT (/' STOPPING in routine FINBC.'/                           !
     +' There is a problem with locating the integrals.'/               !
     +' The first integral is at position    : ',I7/                    !
     +' The last  integral is at position    : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3050 FORMAT (/' STOPPING in routine FINBC.'/                           !
     +' You have requested an integral but'/                            !
     +' there are no integrals on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FINCC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINCC(IDISC2,WA,ICON,JA,JB,JC,JD,LAM,TOTAL,ITC, IWRITE,!
     + K1S, K2S, KBMAX, KCCN, KCCR, KCMAX, KCMIN, KPOS,MAXFUL, MAXNQN, M!
     +AXP, MINNQN, NAK, NAKK, NH, NP, NRANG2, NW,RKSTO)    
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINCC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C The input arguments JA, JB, JC, JD are not reordered in the routine.
C
C   IDISC2
C   WA
C   ICON
C   JA
C   JB
C   JC
C   JD
C   LAM
C   TOTAL
C   ITC
C   IWRITE
C   K1S
C   K2S
C   KBMAX
C   KCCN
C   KCCR
C   KCMAX
C   KCMIN
C   KPOS
C   MAXFUL
C   MAXNQN
C   MAXP
C   MINNQN
C   NAK
C   NAKK
C   NH
C   NP
C   NRANG2
C   NW
C   RKSTO
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
      INTEGER NDY
      PARAMETER (NDY=(NDX+MXNK)/4+1)
C
C  Argument variables
C
      DOUBLE PRECISION TOTAL(MXNB,*)
      DOUBLE PRECISION WA
      INTEGER ICON,IDISC2,JA,JB
      INTEGER JC,JD,LAM
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER K1S
      INTEGER K2S
      INTEGER KBMAX
      INTEGER KCCN(MXNK,*)
      INTEGER KCCR(MXNK,*)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KPOS
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NAK(*)
      INTEGER NAKK(*)
      INTEGER NP(*)
      INTEGER NRANG2
      INTEGER NW
C
C  Local variables
C
      DOUBLE PRECISION FA
C      DOUBLE PRECISION INTCC(MXNB,MXNB)
CPB      REAL*8,ALLOCATABLE :: INTCC(:,:)
      INTEGER I,IRK,IS,IST
      INTEGER ISW,IX,J,K1
      INTEGER K2,K3,K4,KAP1
      INTEGER KAP2,KAP3,KAP4,KS
      INTEGER LST,N1,N2,N3
      INTEGER NS
      INTEGER ICTX(NDX,NDX,NDX)
      INTEGER ICTY(NDX,NDX,NDY)
      INTEGER IRK1
C
      SAVE IRK1
      SAVE ICTX
      SAVE ICTY
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CPB      IF(IFLAGSS.eq.0)then
CPB      ALLOCATE(INTCC(NRANG2,NRANG2))
CPB      IFLAGSS=1
CPB      ENDIF
CPB      IFLAGS=0
C
      IF (ITC(40).EQ.1.AND.ITC(41).EQ.1) THEN
        WRITE (IWRITE,*) 'INTCC NO LONGER IN USE, OUTPUT SUPRESSED'
      ENDIF
C-----------------------------------------------------------------------
      IF (ICON.EQ.1) GOTO 10
      IF (ICON.EQ.2) GOTO 20
      IF (ICON.EQ.3) GOTO 30
      IF (ICON.EQ.4) GOTO 40
C-----------------------------------------------------------------------
      WRITE (IWRITE,3040) ICON
      STOP
C-----------------------------------------------------------------------
C
C   I integrals
C
C-----------------------------------------------------------------------
   10 CONTINUE
      KAP1 = JA-NW
      K1 = NAKK(KAP1)
      CALL LOCCC(IDISC2,K1,K1, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ICTY, I!
     +RK1, ITC, IWRITE,KCCN, KCCR, KBMAX, KCMAX, KCMIN)
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3060)
        STOP
      ENDIF
      IRK = 1
      IST = IRK
C
      IX = NRANG2*(NRANG2+1)/2
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
C      DO I = 1,NRANG2
C        DO J = 1,I
C          INTCC(I,J) = RKSTO(IRK)
C          INTCC(J,I) = RKSTO(IRK)
C          IRK = IRK+1
C        ENDDO
C      ENDDO
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3000) NP(JA),NH(JA),NP(JB),NH(JB),WA,FA,IST
        IF (ITC(41).EQ.1) THEN
C          CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
        ENDIF
      ENDIF
C
      GOTO 90
C-----------------------------------------------------------------------
C
C   F integrals
C
C-----------------------------------------------------------------------
   20 CONTINUE
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
C
      IX = NRANG2*(NRANG2+1)/2
      CALL PLACE(1,N1,K1,N1,K1,IS, MAXFUL,MAXNQN,MINNQN )
      IRK = ICTX(K1,K1,LAM/2+1)+IS*IX
      IST = IRK
C
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
C      DO I = 1,NRANG2
C        DO J = 1,I
C          INTCC(I,J) = RKSTO(IRK)
C          INTCC(J,I) = RKSTO(IRK)
C          IRK = IRK+1
C        ENDDO
C      ENDDO
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3010) NP(JA),NH(JA),NP(JB),NH(JB),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
C          CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
        ENDIF
      ENDIF
C
      GOTO 90
C-----------------------------------------------------------------------
C
C   G integrals
C
C-----------------------------------------------------------------------
   30 CONTINUE
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
C
      IX = NRANG2*(NRANG2+1)/2
      CALL PLACE(2,N1,K1,N1,K1,IS, MAXFUL,MAXNQN,MINNQN )
      IRK = ICTY(K1,K1,LAM/2+1)+IS*IX
      IST = IRK
C
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
C      DO I = 1,NRANG2
C        DO J = 1,I
C          INTCC(I,J) = RKSTO(IRK)
C          INTCC(J,I) = RKSTO(IRK)
C          IRK = IRK+1
C        ENDDO
C      ENDDO
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3020) NP(JA),NH(JA),NP(JB),NH(JB),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
C          CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
        ENDIF
      ENDIF
C
      GOTO 90
C-----------------------------------------------------------------------
C
C   S integrals
C
C-----------------------------------------------------------------------
   40 CONTINUE
      ISW = 0
      IF (JC.GT.NW) GOTO 50
C-----------------------------------------------------------------------
C
C   Direct integrals in the general case
C
C-----------------------------------------------------------------------
      KAP2 = JB-NW
      K2 = NAKK(KAP2)
      KAP4 = JD-NW
      K4 = NAKK(KAP4)
      IX = NRANG2*NRANG2
C
      IF (K2.GT.K4) THEN
        KS = K2
        K2 = K4
        K4 = KS
        ISW = 1
      ELSE
        IF (K2.EQ.K4) THEN
          IX = (IX+NRANG2)/2
        ENDIF
      ENDIF
C
      CALL LOCCC(IDISC2,K2,K4, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ICTY, I!
     +RK1, ITC, IWRITE,KCCN, KCCR, KBMAX, KCMAX, KCMIN)
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3060)
        STOP
      ENDIF
C
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N3 = NP(JC)
      KAP3 = NAK(JC)
      K3 = KAP3+KAP3
      IF (KAP3.LT.0) K3 = - (K3+1)
C
      IF (K1.GT.K3) THEN
        KS = K1
        K1 = K3
        K3 = KS
        NS = N1
        N1 = N3
        N3 = NS
      ELSE
        IF (K1.EQ.K3) THEN
          IF (N1.LT.N3) THEN
            NS = N1
            N1 = N3
            N3 = NS
          ENDIF
        ENDIF
      ENDIF
C
      CALL PLACE(1,N1,K1,N3,K3,IS, MAXFUL,MAXNQN,MINNQN )
      IRK = ICTX(K1,K3,LAM/2+1)+IS*IX
C
      IF (K2.EQ.K4) GOTO 60
      GOTO 80
C-----------------------------------------------------------------------
C
C   Exchange integrals in the general case
C
C-----------------------------------------------------------------------
   50 CONTINUE
      KAP3 = JC-NW
      K3 = NAKK(KAP3)
      KAP4 = JD-NW
      K4 = NAKK(KAP4)
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
      IX = NRANG2*NRANG2
C
      IF (K3.GT.K4) THEN
        KS = K3
        K3 = K4
        K4 = KS
        ISW = 1
        NS = N1
        N1 = N2
        N2 = NS
        KS = K1
        K1 = K2
        K2 = KS
      ELSE
        IF (K3.EQ.K4) THEN
          IX = (IX+NRANG2)/2
        ENDIF
      ENDIF
C
      CALL LOCCC(IDISC2,K3,K4, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ICTY, I!
     +RK1, ITC, IWRITE,KCCN, KCCR, KBMAX, KCMAX, KCMIN)
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3060)
        STOP
      ENDIF
C
      CALL PLACE(2,N1,K1,N2,K2,IS, MAXFUL,MAXNQN,MINNQN )
      IRK = ICTY(K1,K2,LAM/2+1)+IS*IX
C
      IF (K3.EQ.K4) GOTO 60
      GOTO 80
C-----------------------------------------------------------------------
C
C  K2 = K4    direct
C  K3 = K4    exchange
C
C-----------------------------------------------------------------------
   60 CONTINUE
      IST = IRK
C
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
      IF (JA.NE.JB .AND. JC.GT.NW)THEN
       DO I = 1,NRANG2
          DO J = 1,I
C          INTCC(I,J) = RKSTO(IRK)
          IF(I.ne.J) TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
          IRK = IRK+1
        ENDDO
      ENDDO
      ELSE 
      DO I = 1,NRANG2
        DO J = 1,I
          TOTAL(J,I) = TOTAL(J,I)+WA*RKSTO(IRK)
          IF(I.ne.J) TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
          IRK=IRK+1
        ENDDO
      ENDDO
      RETURN
      ENDIF
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3030) NP(JA),NH(JA),NP(JB),NH(JB),NP(JC),NH(JC),NP!
     +(JD),NH(JD),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
          IF (JA.EQ.JB .OR. JC.LE.NW) THEN
C            CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
          ENDIF
        ENDIF
      ENDIF
C
      IF (JA.EQ.JB .OR. JC.LE.NW) GOTO 90
C
      CALL PLACE(2,N2,K2,N1,K1,IS, MAXFUL,MAXNQN,MINNQN )
      IRK = ICTY(K2,K1,LAM/2+1)+IS*IX
      IST = IRK
C
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
      DO J = 1,NRANG2
        DO I = 1,J
C          INTCC(I,J) = RKSTO(IRK)
           TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
          IRK = IRK+1
        ENDDO
      ENDDO
      RETURN
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3030) NP(JB),NH(JB),NP(JA),NH(JA),NP(JC),NH(JC),NP!
     +(JD),NH(JD),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
C          CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
        ENDIF
      ENDIF
C
C      GOTO 90
C-----------------------------------------------------------------------
C
C  K2 <> K4   direct
C  K3 <> K4   exchange
C
C-----------------------------------------------------------------------
   80 CONTINUE
      IST = IRK
C
      LST = IST+IX-1
      IF (IST.LT.1 .OR. LST.GT.IRK1) THEN
        WRITE (IWRITE,3050) IST,LST,IRK1
        STOP
      ENDIF
C
      IF (ISW.EQ.0) THEN
        DO I = 1,NRANG2
          DO J = 1,NRANG2
C            INTCC(I,J) = RKSTO(IRK)
            TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
            IRK = IRK+1
          ENDDO
        ENDDO
      ELSE
        DO J = 1,NRANG2
          DO I = 1,NRANG2
C            INTCC(I,J) = RKSTO(IRK)
            TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
            IRK = IRK+1
          ENDDO
        ENDDO
      ENDIF
       RETURN
C
      IF (ITC(40).EQ.1) THEN
        FA = RKSTO(IST)
        WRITE (IWRITE,3030) NP(JA),NH(JA),NP(JB),NH(JB),NP(JC),NH(JC),NP!
     +(JD),NH(JD),LAM,WA,FA,IST
        IF (ITC(41).EQ.1) THEN
C          CALL MATOUT(IWRITE,INTCC,NRANG2,NRANG2,MXNB,MXNB,4)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C   add in the contribution to array TOTAL
C
C-----------------------------------------------------------------------
   90 CONTINUE
       DO J = 1,NRANG2
        DO I = 1,J
          TOTAL(I,J) = TOTAL(I,J)+WA*RKSTO(IRK)
          IF(I.ne.J) TOTAL(J,I) = TOTAL(J,I)+WA*RKSTO(IRK)
          IRK=IRK+1
        ENDDO
      ENDDO
C
C     The following lines should now be redundant        CPB oct2004
C     as should INTCC
C
C      DO J = 1,NRANG2
C        DO I = 1,NRANG2
C          TOTAL(I,J) = TOTAL(I,J)+WA*INTCC(I,J)
C        ENDDO
C      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (' CC I',2X,2(I3,A2),19X,1P,2E12.4,I6)
 3010 FORMAT (' CC F',2X,2(I3,A2),12X,I3,4X,1P,2E12.4,I6)
 3020 FORMAT (' CC G',2X,2(I3,A2),12X,I3,4X,1P,2E12.4,I6)
 3030 FORMAT (' CC S',2X,4(I3,A2),2X,I3,4X,1P,2E12.4,I6)
 3040 FORMAT (' ERROR in FINCC : incorrect value for ICON = ',I5)
 3050 FORMAT (/' STOPPING in routine FINCC.'/                           !
     +' There is a problem with locating the integrals.'/               !
     +' The first integral is at position    : ',I7/                    !
     +' The last  integral is at position    : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3060 FORMAT (/' STOPPING in routine FINCC.'/                           !
     +' You have requested an integral but'/                            !
     +' there are no integrals on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FINM.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      FUNCTION FINM(NCFR,NCFS,JA,JB,LAM,WA,IBBPOL, IRK1, ITC, IWRITE, LA!
     +G, MAXNQN, NAK, NH, NP, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINM.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C The arguments JA, JB are not reordered in the routine.
C
C   IBBPOL
C   IRK1
C   ITC
C   IWRITE
C   LAG
C   MAXNQN
C   NAK
C   NH
C   NP
C   RKSTO
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      DOUBLE PRECISION FINM
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      DOUBLE PRECISION WA
      INTEGER JA,JB,LAM,NCFR
      INTEGER NCFS
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER LAG(*)
      INTEGER MAXNQN(*)
      INTEGER NAK(*)
      INTEGER NP(*)
C
C  Local variables
C
      INTEGER I1,I2,IRK,IS
      INTEGER K1,K2,KAP1,KAP2
      INTEGER KS,LAMST,N1,N2
      INTEGER NS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3020)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
C-----------------------------------------------------------------------
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
C-----------------------------------------------------------------------
      IF (K1.GT.K2) THEN
        KS = K1
        K1 = K2
        K2 = KS
        NS = N1
        N1 = N2
        N2 = NS
C
      ELSE
        IF (K1.EQ.K2) THEN
          IF (N1.LT.N2) THEN
            NS = N1
            N1 = N2
            N2 = NS
          ENDIF
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
      I1 = N1-LAG(K1)-1
      I2 = N2-LAG(K2)-1
      IF (K1.EQ.K2) GOTO 10
      IS = (MAXNQN(K2)-LAG(K2))*I1+I2
      GOTO 20
C
   10 CONTINUE
      IS = I1*(I1+1)/2+I2
C-----------------------------------------------------------------------
   20 CONTINUE
      IF (LAM.EQ.1) IS = IS + IS
      LAMST = (LAM+1)/2
      IRK = IBBPOL(K1,K2,LAMST)+IS
C
      IF (IRK.LT.1 .OR. IRK.GT.IRK1) THEN
        WRITE (IWRITE,3010) IRK,IRK1
        STOP
C
      ENDIF
C
      FINM = RKSTO(IRK)
C-----------------------------------------------------------------------
      IF (ITC(22).EQ.1) THEN
        WRITE (IWRITE,3000) NCFR,NCFS,NP(JA),NH(JA),NP(JB),NH(JB),LAM,WA!
     +,FINM,IRK
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (1X,'M',2X,2I4,2X,2(I3,A2),2X,I3,2X,1P,2E12.4,I6)
 3010 FORMAT (/' STOPPING in routine FINM.'/                            !
     +' There is a problem with locating the integrals.'/               !
     +' The integral is at position          : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3020 FORMAT (/' STOPPING in routine FINM.'/                            !
     +' You have requested an integral but'/                            !
     +' there are no integrals on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FINMBB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINMBB(WA,JA,JB,LAM,FINTL,FINTV,IBBPOL, IRK1, IWRITE, I!
     +TC, LAG, MAXNQN, NAK, NH, NP, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINMBB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBBPOL
C   IRK1
C   IWRITE
C   ITC
C   LAG
C   MAXNQN
C   NAK
C   NH
C   NP
C   RKSTO
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
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      DOUBLE PRECISION FINTL,FINTV,WA
      INTEGER JA,JB,LAM
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IRK1
      INTEGER IWRITE
      INTEGER ITC(*)
      INTEGER LAG(*)
      INTEGER MAXNQN(*)
      INTEGER NAK(*)
      INTEGER NP(*)
C
C  Local variables
C
      DOUBLE PRECISION AAA
      INTEGER I1,I2,IRK,IRKT
      INTEGER IS,JAS,JASS,JBS
      INTEGER K1,K2,KAP1,KAP2
      INTEGER KS,LAMST,N1,N2
      INTEGER NS
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3020)
        STOP
      ENDIF
C
      JAS = JA
      JBS = JB
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
      N2 = NP(JB)
      KAP2 = NAK(JB)
      K2 = KAP2+KAP2
      IF (KAP2.LT.0) K2 = - (K2+1)
C
      AAA = ONE
      IF (K1.GT.K2) THEN
        JASS = JAS
        JAS = JBS
        JBS = JASS
        KS = K1
        K1 = K2
        K2 = KS
        NS = N1
        N1 = N2
        N2 = NS
        AAA = -ONE
      ELSE
        IF (K1.EQ.K2) THEN
          IF (N1.LT.N2) THEN
            NS = N1
            N1 = N2
            N2 = NS
            AAA = -ONE
          ENDIF
        ENDIF
      ENDIF
C
      I1 = N1-LAG(K1)-1
      I2 = N2-LAG(K2)-1
C
      IF (K1.EQ.K2) THEN
        IS = I1*(I1+1)/2+I2
      ELSE
        IS = (MAXNQN(K2)-LAG(K2))*I1+I2
      ENDIF
C
      IF (LAM.EQ.1) IS = IS + IS
      LAMST = (LAM+1)/2
      IRK = IBBPOL(K1,K2,LAMST)+IS
C
C  Calculate the number of integrals stored in RKSTO.
C  If LAM = 1 then length and velocity integrals are stored.
C
      IRKT = 1
      IF (LAM.EQ.1) IRKT = 2
      IF (IRK.LT.1 .OR. IRK+IRKT-1.GT.IRK1) THEN
        WRITE (IWRITE,3010) IRK,IRKT,IRK1
        STOP
      ENDIF
C
      FINTL = RKSTO(IRK)
C
      IF (LAM.EQ.1) THEN
        FINTV = RKSTO(IRK+1)*AAA
      ELSE
        FINTV = ZERO
      ENDIF
C
      IF (ITC(27).EQ.1) THEN
        WRITE (IWRITE,3000) N1,NH(JAS),N2,NH(JBS),LAM,WA,FINTL,FINTV,AAA!
     +,IRK
      ENDIF
C
 3000 FORMAT (' FINMBB',2(I3,A2,2X),2X,I3,4X,1P,4E12.4,2X,I5)
 3010 FORMAT (/' STOPPING in routine FINMBB.'/                          !
     +' There is a problem with locating the integrals.'/               !
     +' The integrals start at position      : ',I7/                    !
     +' The number of integrals required is  : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3020 FORMAT (/' STOPPING in routine FINMBB.'/                          !
     +' You have requested an integral but there are no integrals'/     !
     +' on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FINMBC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINMBC(WA,WB,K2,JA,LAM,FINTL,FINTV,IBCPOL, IRK1, ITC, I!
     +WRITE, LAG, NAK, NH, NP, NRANG2, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINMBC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBCPOL
C   IRK1
C   ITC
C   IWRITE
C   LAG
C   NAK
C   NH
C   NP
C   NRANG2
C   RKSTO
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
C
C  Argument variables
C
      DOUBLE PRECISION FINTL(MXNB,*),FINTV(MXNB,*)
      DOUBLE PRECISION WA,WB
      INTEGER JA,K2,LAM
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION RKSTO(*)
      INTEGER IBCPOL(NDX,MXNK,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER LAG(*)
      INTEGER NAK(*)
      INTEGER NP(*)
      INTEGER NRANG2
C
C  Local variables
C
      INTEGER I,I1,IRK,IRKS
      INTEGER IRKT,IS,K1,KAP1
      INTEGER LAMST,N1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3010)
        STOP
      ENDIF
C
      N1 = NP(JA)
      KAP1 = NAK(JA)
      K1 = KAP1+KAP1
      IF (KAP1.LT.0) K1 = - (K1+1)
C
      I1 = N1-LAG(K1)-1
      IS = I1*NRANG2
C
      IF (LAM.EQ.1) IS = IS + IS
      LAMST = (LAM+1)/2
      IRKS = IBCPOL(K1,K2,LAMST)+IS
C
      IRK = IRKS
C
C  Calculate the number of integrals stored in RKSTO.
C  If LAM = 1 then length and velocity integrals are stored.
C
      IRKT = NRANG2
      IF (LAM.EQ.1) IRKT = IRKT*2
      IF (IRK.LT.1 .OR. IRK+IRKT-1.GT.IRK1) THEN
        WRITE (IWRITE,3000) IRK,IRKT,IRK1
        STOP
      ENDIF
C
      DO I = 1,NRANG2
        FINTL(I,1) = RKSTO(IRK)
        IF (LAM.EQ.1) THEN
          IRK = IRK+1
          FINTV(I,1) = RKSTO(IRK)
        ELSE
          FINTV(I,1) = ZERO
        ENDIF
        IRK = IRK+1
      ENDDO
C
      IF (ITC(27).EQ.1) THEN
        WRITE (IWRITE,3020) N1,NH(JA),K2,LAM,WA,WB,IRKS
        WRITE (IWRITE,3030) (FINTL(I,1),I=1,NRANG2)
        WRITE (IWRITE,3040) (FINTV(I,1),I=1,NRANG2)
      ENDIF
C
 3000 FORMAT (/' STOPPING in routine FINMBC.'/                          !
     +' There is a problem with locating the integrals.'/               !
     +' The integrals start at position      : ',I7/                    !
     +' The number of integrals required is  : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3010 FORMAT (/' STOPPING in routine FINMBC.'/                          !
     +' You have requested an integral but there are no integrals'/     !
     +' on the DSTG1 dump.')
 3020 FORMAT (' FINMBC ',I3,A2,2X,I4,2X,I2,1P,2E12.4,4X,I5)
 3030 FORMAT (' L      ',1P,5E12.4)
 3040 FORMAT (' V      ',1P,5E12.4)
      END
CEND--------------------------------------------------------------------
CEND    FINMCC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FINMCC(WA,WB,KI,KJ,LAM,FINTL,FINTV,ICCPOL, IRK1, ITC, I!
     +WRITE, NRANG2, RKSTO)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FINMCC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ICCPOL
C   IRK1
C   ITC
C   IWRITE
C   NRANG2
C   RKSTO
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
C
C  Argument variables
C
      DOUBLE PRECISION FINTL(MXNB,*),FINTV(MXNB,*)
      DOUBLE PRECISION WA,WB
      INTEGER KI,KJ,LAM
C
      DOUBLE PRECISION RKSTO(*)
      INTEGER ICCPOL(MXNK,MXNK,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER NRANG2
C
C  Local variables
C
      INTEGER I,IRK,IRKS,IRKT
      INTEGER J,K1,K2,KS
      INTEGER LAMST
      LOGICAL SWAP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IRK1.EQ.0) THEN
        WRITE (IWRITE,3020)
        STOP
      ENDIF
C
      K1 = KI
      K2 = KJ
      SWAP = .FALSE.
C
C  Swap kappas if first is greater than second because DSTG1 only
C  calculates integrals for K1 = 1 to K2.
C
      IF (K1.GT.K2) THEN
        KS = K1
        K1 = K2
        K2 = KS
        SWAP = .TRUE.
      ENDIF
C
C  Locate start of integrals in RKSTO using indexing array ICCPOL
C
      LAMST = (LAM+1)/2
      IRKS = ICCPOL(K1,K2,LAMST)
C
      IRK = IRKS
C
C  Calculate the number of integrals stored in RKSTO.
C  If K1 = K2 then about half the integrals were calculated by DSTG1.
C  If LAM = 1 then length and velocity integrals are stored.
C
      IF (K1.NE.K2) THEN
        IRKT = NRANG2*NRANG2
      ELSE
        IRKT = NRANG2*(NRANG2+1)/2
      ENDIF
      IF (LAM.EQ.1) IRKT = IRKT*2
      IF (IRK.LT.1 .OR. IRK+IRKT-1.GT.IRK1) THEN
        WRITE (IWRITE,3010) IRK,IRKT,IRK1
        STOP
      ENDIF
C
C  Velocity integral has opposite sign when kappas are swapped.
C
      IF (K1.NE.K2) THEN
        IF (SWAP) THEN
          DO I = 1,NRANG2
            DO J = 1,NRANG2
              FINTL(J,I) = RKSTO(IRK)
              IF (LAM.EQ.1) THEN
                IRK = IRK+1
                FINTV(J,I) = -RKSTO(IRK)
              ELSE
                FINTV(J,I) = ZERO
              ENDIF
              IRK = IRK+1
            ENDDO
          ENDDO
        ELSE
          DO I = 1,NRANG2
            DO J = 1,NRANG2
              FINTL(I,J) = RKSTO(IRK)
              IF (LAM.EQ.1) THEN
                IRK = IRK+1
                FINTV(I,J) = RKSTO(IRK)
              ELSE
                FINTV(I,J) = ZERO
              ENDIF
              IRK = IRK+1
            ENDDO
          ENDDO
        ENDIF
      ELSE
        DO I = 1,NRANG2
          DO J = 1,I
            FINTL(I,J) = RKSTO(IRK)
            IF (LAM.EQ.1) THEN
              IRK = IRK+1
              FINTV(I,J) = RKSTO(IRK)
            ELSE
              FINTV(I,J) = ZERO
            ENDIF
            FINTL(J,I) = FINTL(I,J)
            FINTV(J,I) = -FINTV(I,J)
            IRK = IRK+1
          ENDDO
        ENDDO
      ENDIF
C
      IF (ITC(27).EQ.1) THEN
        WRITE (IWRITE,3000) K1,K2,LAM,WA,WB,IRKS
      ENDIF
C
 3000 FORMAT (' FINMCC',I4,2X,I4,2X,I2,1P,2E12.4,4X,I5)
 3010 FORMAT (/' STOPPING in routine FINMCC.'/                          !
     +' There is a problem with locating the integrals.'/               !
     +' The integrals start at position      : ',I7/                    !
     +' The number of integrals required is  : ',I7/                    !
     +' The number of integrals available is : ',I7/                    !
     +' You may have incorrect input data.'/                            !
     +' Check that the full shells (core) is used correctly.')
 3020 FORMAT (/' STOPPING in routine FINMCC.'/                          !
     +' You have requested an integral but there are no integrals'/     !
     +' on the DSTG1 dump.')
      END
CEND--------------------------------------------------------------------
CEND    FIXJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE FIXJ(JA1,JA2,KA,IS,KS,NS,KJ23,J1, J2, J3, MMOM, NMOM,JB!
     +Q2, JJC1, JJC2, JJQ1, JJQ2, JLIST)
CRCS
CRCS $Source: /home/phn/DARC/RCS/FIXJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   JA1
C   JA2
C   KA
C   IS
C   KS
C   NS
C   KJ23
C   J1
C   J2
C   J3
C   MMOM
C   NMOM
C   JBQ2
C   JJC1
C   JJC2
C   JJQ1
C   JJQ2
C   JLIST
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
C
C  Argument variables
C
      INTEGER IS(*),JA1,JA2,KA
      INTEGER KJ23,KS(*),NS
C
      INTEGER J1(*)
      INTEGER J2(MTRIAD,*)
      INTEGER J3(MTRIAD,*)
      INTEGER MMOM
      INTEGER NMOM
      INTEGER JBQ2(3,*)
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
C
C  Local variables
C
      INTEGER II,IJ,JAF1,JJ
      INTEGER JK,JWW,N2,N3
      INTEGER NM1,NS1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   SET UP THE J2 AND J3 ARRAYS
C
C-----------------------------------------------------------------------
      NM1 = NS-1
      IF (KJ23.EQ.1) GOTO 30
      NS1 = NS+1
      N2 = NS+NS
      N3 = N2+NS
C
      J2(1,1) = N3+2
      J2(1,2) = N3+3
      J2(1,3) = N3+1
      J2(2,1) = JA1
      J2(2,2) = N3+1
      J2(2,3) = N3-1
C
      J3(1,1) = JA2
      J3(1,2) = N3+2
      J3(1,3) = N3
C
      IF (NS.EQ.1) GOTO 20
C
      DO JWW = 1,NM1
        JJ = JWW+2
        J2(JJ,1) = NS+JWW-1
        J2(JJ,2) = JWW+1
        J2(JJ,3) = NS+JWW
C
        JK = JWW+1
        J3(JK,1) = N2+JWW-2
        J3(JK,2) = JWW+1
        J3(JK,3) = N2+JWW-1
      ENDDO
C
      J2(3,1) = 1
      IF (JA1.EQ.1) J2(3,1) = N3 - 1
C
      J3(2,1) = 1
      IF (JA2.EQ.1) J3(2,1) = N3
C
      J2(NS1,3) = N2-1
C
      J3(NS1,1) = N3-2
      J3(NS1,2) = N3+3
      J3(NS1,3) = N2-1
C
      IF (JA1.EQ.1) GOTO 10
      JAF1 = JA1+1
      J2(JAF1,2) = N3-1
C
   10 CONTINUE
      IF (JA2.EQ.1) GOTO 30
      J3(JA2,2) = N3
C
      IF (NS.GT.1) GOTO 30
   20 CONTINUE
      J3(2,1) = N3
      J3(2,2) = N3+3
      J3(2,3) = N3-1
C-----------------------------------------------------------------------
C
C   SET THE J1 ARRAY
C
C-----------------------------------------------------------------------
   30 CONTINUE
      II = 0
C
      DO JWW = 1,NS
        IJ = JLIST(JWW)
        II = II+1
        J1(II) = JBQ2(3,IJ)
      ENDDO
C
      IF (NS.EQ.1) GOTO 40
C
      DO JWW = 1,NM1
        II = II+1
        J1(II) = JJC1(JWW)
      ENDDO
C
      DO JWW = 1,NM1
        II = II+1
        J1(II) = JJC2(JWW)
      ENDDO
C
   40 CONTINUE
      II = II+1
      IJ = IS(1)
      J1(II) = JJQ1(3,IJ)
      J1(II+2) = KS(1)
      II = II+1
      IJ = IS(2)
      J1(II) = JJQ2(3,IJ)
      J1(II+2) = KS(2)
C
      II = II+3
      J1(II) = KA+KA+1
      MMOM = II
      NMOM = NS+2
C
      END
CEND--------------------------------------------------------------------
CEND    GENCOR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENCOR(NMAN,NWM,NCFST,JF,NPTY,CORINC,ICHOP, IDMTST, IPO!
     +S, IQ, ISPAR, ITAB, ITJPO, IWRITE, JCHOP,JCUP, JQS, JTAB, NAK, NCF!
     +, NDIMAX, NLX,NQX, NROWS, NTAB, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENCOR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NMAN  ... number of NR config. to be defined
C   NWM   ... number of nr orbitals to be defined
C   NCFST ... position of last CSF to be defined
C   JF    ... 2J value
C   NPTY  ... parity
C   ICHOP
C   IDMTST
C   IPOS
C   IQ
C   ISPAR
C   ITAB
C   ITJPO
C   IWRITE
C   JCHOP
C   JCUP
C   JQS
C   JTAB
C   NAK
C   NCF
C   NDIMAX
C   NLX
C   NQX
C   NROWS
C   NTAB
C   NW
C
C   Routines called : MAN5, DMCHK2
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER JSTMAX
      PARAMETER (JSTMAX=40)
C
C  Argument variables
C
      INTEGER CORINC(*),JF,NCFST
      INTEGER NMAN,NPTY,NWM
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IPOS(*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCHOP(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NDIMAX(*)
      INTEGER NLX(*)
      INTEGER NQX(MXNW,*)
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      INTEGER I,II,III,IMAN
      INTEGER IPAR,ITEST,J,JMAN
      INTEGER JST,JST1,JST2
      INTEGER JSTORE(0:JSTMAX),KCHOP(MXNW),NFULL
      INTEGER NL,NOPENX,NQ,NQS
      INTEGER NQY(MXNW,MXNC)
      INTEGER JPOS(5)
      INTEGER MLX(5)
      INTEGER MQX(5)
      INTEGER NOPEN
      LOGICAL MFAIL
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NOPENX = IDMTST(26)
C
C  Set NQY to be the target occupation numbers (stored in NQX)
C
      DO I = 1,NWM
        DO J = 1,NMAN
          NQY(I,J) = NQX(I,J)
        ENDDO
      ENDDO
C
C  Loop over each non-relativistic CSF in turn
C  (1) determine which shells are open
C  (2) set up arrays KCHOP,MLX,MQX,JPOS for call to MAN5
C      which generates relativistic CSFs
C
      NCF = NCFST
      MFAIL = .FALSE.
      JST1 = 0
      JST2 = 0
C
      DO JMAN = 1,NMAN
        IF (CORINC(JMAN).EQ.1) THEN
C
          DO IMAN = 1,JMAN
            IF (IMAN.EQ.JMAN) GOTO 10
            DO I = 1,NWM
              IF (NQY(I,IMAN).NE.NQY(I,JMAN)) GOTO 10
            ENDDO
            GOTO 60
   10       CONTINUE
          ENDDO
C
          DO I = 1,NW
            KCHOP(I) = JCHOP(I,JMAN)
          ENDDO
C
          DO III = 1,NWM
C
            NL = NLX(III)
            NQS = NQY(III,JMAN)
            NFULL = 4*NL+2
            IF (NQS.EQ.NFULL) GOTO 50
            NQY(III,JMAN) = NQS+1
C
            DO I = 1,NWM
              NQ = NQY(I,JMAN)
              NQY(I,JMAN) = NQ-1
              DO IMAN = 1,JMAN
                IF (IMAN.EQ.JMAN) GOTO 20
                DO II = 1,NWM
                  IF (NQY(II,IMAN).NE.NQY(II,JMAN)) GOTO 20
                ENDDO
                NQY(I,JMAN) = NQ
                GOTO 40
   20           CONTINUE
              ENDDO
              NQY(I,JMAN) = NQ
            ENDDO
C
            NOPEN = 0
C
            DO I = 1,NWM
              NL = NLX(I)
              NQ = NQY(I,JMAN)
              NFULL = 4*NL+2
              IF (NQ.EQ.NFULL .OR. NQ.EQ.0) GOTO 30
              NOPEN = NOPEN+1
              IF (NOPEN.GT.NOPENX) THEN
                WRITE (IWRITE,3000)
                CALL DMCHK2(26,NOPEN, IWRITE, IDMTST, NDIMAX)
              ENDIF
              MLX(NOPEN) = NL
              MQX(NOPEN) = NQ
              JPOS(NOPEN) = IPOS(I)
   30         CONTINUE
            ENDDO
C
            IF (NOPEN.GT.0) THEN
              ITEST = 0
              DO I = 1,NOPEN
                ITEST = ITEST+MLX(I)*MQX(I)
              ENDDO
              IPAR = 1
              IF (MOD(ITEST,2).NE.0) IPAR = -1
            ELSE
              IPAR = 1
              IF (JF.NE.0) GOTO 40
            ENDIF
C
            IF (IPAR.NE.NPTY) GOTO 40
C
C   Five open shells must always be defined in MAN5
C   and the following code includes extra dummy orbitals
C   to ensure this
C
            IF (NOPEN.LT.NOPENX) THEN
              NOPEN = NOPEN+1
              II = 1
              DO I = NOPEN,NOPENX
                MLX(I) = 0
                MQX(I) = 0
                JPOS(I) = NW+II
                II = II+1
              ENDDO
            ENDIF
C
            CALL MAN5(IWRITE,KCHOP,IPAR,JF,JF,MFAIL,JSTORE,ICHOP, IQ, IS!
     +PAR, ITAB, ITJPO, JCUP, JPOS, JQS, JTAB,MLX, MQX, NAK, NCF, NROWS,!
     + NTAB, NW)
C
            IF (JF.GE.0 .AND. JF.LE.JSTMAX) THEN
              JST = JF
              IF (JSTORE(JST).GT.0) THEN
                JST1 = JST2+1
                JST2 = JST1+JSTORE(JST)-1
                IF (MOD(JST,2).EQ.0) THEN
                  IF (IPAR.EQ.1) THEN
                    WRITE (IWRITE,3020) JMAN,JSTORE(JST),JST/2,JST1,JST2
                  ELSE
                    WRITE (IWRITE,3030) JMAN,JSTORE(JST),JST/2,JST1,JST2
                  ENDIF
                ELSE
                  IF (IPAR.EQ.1) THEN
                    WRITE (IWRITE,3040) JMAN,JSTORE(JST),JST,JST1,JST2
                  ELSE
                    WRITE (IWRITE,3050) JMAN,JSTORE(JST),JST,JST1,JST2
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
C
   40       CONTINUE
            NQY(III,JMAN) = NQS
   50       CONTINUE
          ENDDO
C
        ENDIF
   60   CONTINUE
      ENDDO
C
      IF (MFAIL) THEN
        WRITE (IWRITE,3010) NCF,MXNC
        CALL DMCHK2(07,NCF, IWRITE, IDMTST, NDIMAX)
        STOP
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/' GENCOR (generation of N+1 electron CSFs):'/            !
     +' There are too many open shells and MAN5 cannot be called'/      !
     +' The code is STOPPING')
 3010 FORMAT (/' GENCOR (generation of N+1 electron CSFs):'/            !
     +' The number of CSFs generated in MAN5 exceeds the dimensions'/   !
     +'     NCF is ',I6,'    maximum allowed is ',I6/' code is STOPPING'!
     +)
 3020 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'   even (',I5,',',I5,')')
 3030 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'   odd  (',I5,',',I5,')')
 3040 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'/2 even (',I5,',',I5,')')
 3050 FORMAT (' NR CSF ',I3,' ---> ',I4,' rel. CSFs with J = ',I3,      !
     +'/2 odd  (',I5,',',I5,')')
      END
CEND--------------------------------------------------------------------
CEND    IROW1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      FUNCTION IROW1(NELC,KSI,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/IROW1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C    NELC
C    KSI
C    IWRITE
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INTEGER IROW1
C
C  Argument variables
C
      INTEGER NELC
      INTEGER KSI
      INTEGER IWRITE
C
C  Local variables
C
      INTEGER KQ1,KQ2,KQL
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (NELC.LE.0 .OR. NELC.GT.KSI) THEN
        WRITE (IWRITE,3000) NELC,KSI
        STOP
      ENDIF
C
      KQ1 = NELC-1
      KQ2 = KSI-KQ1
      KQL = MIN(KQ1,KQ2)+1
C
      IF (KQL.EQ.1) THEN
        IROW1 = 1
      ELSE
        IROW1 = (KSI*(KSI-2))/8+KQL
      ENDIF
C
 3000 FORMAT (' STOPPING in routine IROW1'/                             !
     +' Termination because of data ERROR'/1X,I3,                       !
     +' electrons in shell with 2j+1 = ',I3)
      END
CEND--------------------------------------------------------------------
CEND    LOCBB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LOCBB(
     + K1, K2, K1S, K2S, RKSTO, ICTXB, IRK1, IRK2, ISTX1B,
     + ISTX2B, ISTXB, ITC, IWRITE, KBBN1, KBBN2, KBBR, KBMAX)
CRCS
CRCS $Source: /home/phn/DARC/RCS/LOCBB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   K1
C   K2
C   K1S
C   K2S
C   RKSTO
C   ICTXB
C   IRK1
C   IRK2
C   ISTX1B
C   ISTX2B
C   ISTXB
C   ITC
C   IWRITE
C   KBBN1
C   KBBN2
C   KBBR
C   KBMAX
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      INTEGER K1,K2
C
      DOUBLE PRECISION RKSTO(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBMAX
C
C  Local variables
C
      INTEGER I,IREC
      INTEGER J,K
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (K1.EQ.K1S .AND. K2.EQ.K2S) RETURN
      IF (K1.EQ.K2S .AND. K2.EQ.K1S) RETURN
C-----------------------------------------------------------------------
      IRK1 = KBBN1
      IRK2 = KBBN2
C
      IF (IRK1.GT.0) THEN
C
        IREC = KBBR
        CALL DA1('INTEGRAL.DAT',1,IREC,22,IRK1,RKSTO)
C
        IF (ITC(37).EQ.1) THEN
C
          WRITE (IWRITE,3050) KBMAX
          WRITE (IWRITE,3020) (ISTXB(I),I=1,KBMAX)
C
          WRITE (IWRITE,3060) KBMAX
          DO K = 1,KBMAX
            WRITE (IWRITE,3010) K
            DO I = 1,KBMAX
              WRITE (IWRITE,3020) (ICTXB(I,J,K),J=1,KBMAX)
            ENDDO
          ENDDO
C
          WRITE (IWRITE,3030) IRK2
          WRITE (IWRITE,3020) (ISTX1B(I),I=1,IRK2)
          WRITE (IWRITE,3040) IRK2
          WRITE (IWRITE,3020) (ISTX2B(I),I=1,IRK2)
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
      K1S = 0
      K2S = 0
      IF (ITC(36).EQ.1) THEN
        WRITE (IWRITE,3000) K1S,K2S,IRK1,IRK2
      ENDIF
C=======================================================================
 3000 FORMAT (' LOCBB  out; k1s,k2s,     irk1,irk2:',2I4,6X,2I6/)
 3010 FORMAT (/' K = ',I4/)
 3020 FORMAT (1X,7I8)
 3030 FORMAT (/' ISTX1(I) for I=1,IRK2  and  IRK2 = ',I4)
 3040 FORMAT (/' ISTX2(I) for I=1,IRK2  and  IRK2 = ',I4)
 3050 FORMAT (/' DEBUG from LOCBB routine ... bound-bound block'/
     +' ISTX(I) for I=1,KBMAX  and  KBMAX = ',I4)
 3060 FORMAT (/' ICTX(I,J,K) for K=1,KBMAX  and  KBMAX = ',I4)
      END
CEND--------------------------------------------------------------------
CEND    LOCBC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LOCBC(
     + IDISC2, K1, K2, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ISTX1,
     + ISTX2, IRK1, IRK2, ITC, IWRITE, KBCN1, KBCN2, KBCR, KBMAX,
     + KCMAX, KCMIN)
CRCS
CRCS $Source: /home/phn/DARC/RCS/LOCBC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IDISC2
C   K1
C   K2
C   K1S
C   K2S
C   KPOS
C   MAXP
C   RKSTO
C   ICTX
C   ISTX1
C   ISTX2
C   IRK1
C   IRK2
C   ITC
C   IWRITE
C   KBCN1
C   KBCN2
C   KBCR
C   KBMAX
C   KCMAX
C   KCMIN
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER NDX
      PARAMETER (NDX=9)
C
C  Argument variables
C
      INTEGER IDISC2,K1,K2
C
      DOUBLE PRECISION RKSTO(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER KPOS
      INTEGER MAXP
      INTEGER ICTX(NDX,NDX,*)
      INTEGER ISTX1(*)
      INTEGER ISTX2(*)
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KBCN1(*)
      INTEGER KBCN2(*)
      INTEGER KBCR(*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
C
C  Local variables
C
      INTEGER I,IREC
      INTEGER IS,IX,J,K
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (K1.EQ.K1S .AND. K2.EQ.K2S) RETURN
      IF (K1.EQ.K2S .AND. K2.EQ.K1S) RETURN
C-----------------------------------------------------------------------
C
C  determine IS
C
      IX = KCMAX-KCMIN+1
      IS = IX*(IX+1)/2+(K1-KCMIN+1)
C-----------------------------------------------------------------------
C
C  position the scratch file IDISC2
C
      IF (IS.GT.MAXP) THEN
        WRITE (IWRITE,3010) IS,K1,K2
        STOP
      ENDIF
C
      IX = IS-KPOS
C
      IF (IX.LT.0) THEN
        REWIND IDISC2
        IX = IS
      ENDIF
C
      IX = IX-1
C
      IF (IX.GT.0) THEN
        DO I = 1,IX
          READ (IDISC2)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IRK1 = KBCN1(K1)
      IRK2 = KBCN2(K1)
C
      IF (IRK1.EQ.0) THEN
C
        READ (IDISC2)
C
      ELSE
C
        READ (IDISC2)
     +  (((ICTX(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +  (ISTX1(I),I=1,IRK2),(ISTX2(I),I=1,IRK2)
C
        IREC = KBCR(K1)
        CALL DA1('INTEGRAL.DAT',1,IREC,22,IRK1,RKSTO)
C
        IF (ITC(37).EQ.1) THEN
C
          WRITE (IWRITE,3040) KBMAX
          DO K = 1,KBMAX
            WRITE (IWRITE,3020) K
            DO I = 1,KBMAX
              WRITE (IWRITE,3030) (ICTX(I,J,K),J=1,KBMAX)
            ENDDO
          ENDDO
C
          WRITE (IWRITE,3050) IRK2
          WRITE (IWRITE,3030) (ISTX1(I),I=1,IRK2)
          WRITE (IWRITE,3060) IRK2
          WRITE (IWRITE,3030) (ISTX2(I),I=1,IRK2)
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
      K1S = K1
      K2S = 0
      KPOS = IS
      IF (ITC(36).EQ.1) THEN
        WRITE (IWRITE,3000) K1S,K2S,KPOS,IRK1,IRK2
      ENDIF
C=======================================================================
 3000 FORMAT (' LOCBC  out; k1s,k2s,kpos,irk1,irk2:',2I4,3I6/)
 3010 FORMAT (/' ERROR in LOCBC for block ',I4/' K1=',I3,4X,'K2=',I3)
 3020 FORMAT (/' K = ',I4/)
 3030 FORMAT (1X,7I8)
 3040 FORMAT (/' DEBUG from LOCBC routine ... bound-continuum block'/
     +' ICTX(I,J,K) for K=1,KBMAX  and  KBMAX = ',I4)
 3050 FORMAT (/' ISTX1(I) for I=1,IRK2  and  IRK2 = ',I4)
 3060 FORMAT (/' ISTX2(I) for I=1,IRK2  and  IRK2 = ',I4)
      END
CEND--------------------------------------------------------------------
CEND    LOCCC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LOCCC(
     + IDISC2, K1, K2, K1S, K2S, KPOS, MAXP, RKSTO,ICTX, ICTY,
     + IRK1, ITC, IWRITE, KCCN, KCCR, KBMAX, KCMAX, KCMIN)
CRCS
CRCS $Source: /home/phn/DARC/RCS/LOCCC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IDISC2
C   K1
C   K2
C   K1S
C   K2S
C   KPOS
C   MAXP
C   RKSTO
C   ICTX
C   ICTY
C   IRK1
C   ITC
C   IWRITE
C   KCCN
C   KCCR
C   KBMAX
C   KCMAX
C   KCMIN
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
C
C  Argument variables
C
      INTEGER IDISC2,K1,K2
C
      DOUBLE PRECISION RKSTO(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER KPOS
      INTEGER MAXP
      INTEGER ICTX(NDX,NDX,*)
      INTEGER ICTY(NDX,NDX,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KCCN(MXNK,*)
      INTEGER KCCR(MXNK,*)
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER KCMIN
C
C  Local variables
C
      INTEGER I,I1,I2,IREC
      INTEGER IS,IX,J,K
      INTEGER K1P,K2P
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (K1.EQ.K1S .AND. K2.EQ.K2S) RETURN
      IF (K1.EQ.K2S .AND. K2.EQ.K1S) RETURN
C-----------------------------------------------------------------------
C
C  determine IS
C
      IF (K1.GT.K2) THEN
        K2P = K1
        K1P = K2
      ELSE
        K1P = K1
        K2P = K2
      ENDIF
C
      IX = (K1P-KCMIN+1)-1
      IS = IX*(KCMAX-KCMIN+1)-IX*(IX-1)/2+(K2P-KCMIN+1)-IX
C-----------------------------------------------------------------------
C
C  position the scratch file IDISC2
C
C
      IF (IS.GT.MAXP) THEN
        WRITE (IWRITE,3010) IS,K1,K2
        STOP
      ENDIF
C
      IX = IS-KPOS
C
      IF (IX.LT.0) THEN
        REWIND IDISC2
        IX = IS
      ENDIF
C
      IX = IX-1
C
      IF (IX.GT.0) THEN
        DO I = 1,IX
          READ (IDISC2)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IRK1 = KCCN(K1P,K2P)
C
      IF (IRK1.EQ.0) THEN
C
        READ (IDISC2)
C
      ELSE
C
        I1 = MIN(KBMAX+KBMAX,K1P+K2P)
        I2 = MIN(KBMAX+K1P,KBMAX+K2P)
        I1 = I1/4+1
        I2 = I2/4+1
        READ (IDISC2)
     +  (((ICTX(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I1),
     +  (((ICTY(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I2)
C
        IREC = KCCR(K1P,K2P)
        CALL DA1('INTEGRAL.DAT',1,IREC,22,IRK1,RKSTO)
C
        IF (ITC(37).EQ.1) THEN
          WRITE (IWRITE,3020) I1
          DO K = 1,I1
            WRITE (IWRITE,3030) K
            DO I = 1,KBMAX
              WRITE (IWRITE,3040) (ICTX(I,J,K),J=1,KBMAX)
            ENDDO
          ENDDO
          WRITE (IWRITE,3050) I2
          DO K = 1,I2
            WRITE (IWRITE,3030) K
            DO I = 1,KBMAX
              WRITE (IWRITE,3040) (ICTY(I,J,K),J=1,KBMAX)
            ENDDO
          ENDDO
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
      K1S = K1
      K2S = K2
      KPOS = IS
      IF (ITC(36).EQ.1) THEN
        WRITE (IWRITE,3000) K1S,K2S,KPOS,IRK1
      ENDIF
C=======================================================================
 3000 FORMAT (' LOCCC  out; k1s,k2s,kpos,irk1     :',2I4,2I6/)
 3010 FORMAT (/' ERROR in LOCCC for block ',I4/' K1=',I3,4X,'K2=',I3)
 3020 FORMAT (/' DEBUG from LOCCC routine ... continuum-continuum block'
     +/' ICTX(I,J,K) for K=1,I1  and  I1 = ',I4)
 3030 FORMAT (/' K = ',I4/)
 3040 FORMAT (1X,7I8)
 3050 FORMAT (/' ICTY(I,J,K) for K=1,I2  and  I2 = ',I4)
      END
CEND--------------------------------------------------------------------
CEND    LOCM.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LOCM(
     + K1, K2, K1S, K2S, RKSTO, IBBPOL, IBCPOL, ICCPOL, IRK1, ITC,
     + IWRITE, KMULTN, KMULTR, KBMAX, KCMAX, LAMBB, LAMBC, LAMCC)
CRCS
CRCS $Source: /home/phn/DARC/RCS/LOCM.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   K1
C   K2
C   K1S
C   K2S
C   RKSTO
C   IBBPOL
C   IBCPOL
C   ICCPOL
C   IRK1
C   ITC
C   IWRITE
C   KMULTN
C   KMULTR
C   KBMAX
C   KCMAX
C   LAMBB
C   LAMBC
C   LAMCC
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
C
C  Argument variables
C
      INTEGER K1,K2
C
      DOUBLE PRECISION RKSTO(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IBCPOL(NDX,MXNK,*)
      INTEGER ICCPOL(MXNK,MXNK,*)
      INTEGER IRK1
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER KMULTN
      INTEGER KMULTR
      INTEGER KBMAX
      INTEGER KCMAX
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
C
C  Local variables
C
      INTEGER I,IREC
      INTEGER J,K
      INTEGER LAMIND
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (K1.EQ.K1S .AND. K2.EQ.K2S) RETURN
      IF (K1.EQ.K2S .AND. K2.EQ.K1S) RETURN
C-----------------------------------------------------------------------
      IRK1 = KMULTN
C
      IF (IRK1.GT.0) THEN
C
        IREC = KMULTR
        CALL DA1('INTEGRAL.DAT',1,IREC,22,IRK1,RKSTO)
C
        IF (ITC(37).EQ.1) THEN
C
          IF (LAMBB.GT.0) THEN
            LAMIND = (LAMBB+1)/2
            WRITE (IWRITE,3030) LAMIND
            DO K = 1,LAMIND
              WRITE (IWRITE,3010) K
              DO I = 1,KBMAX
                WRITE (IWRITE,3020) (IBBPOL(I,J,K),J=1,KBMAX)
              ENDDO
            ENDDO
          ENDIF
C
          IF (KCMAX.GT.0) THEN
            IF (LAMBC.GT.0) THEN
              LAMIND = (LAMBC+1)/2
              WRITE (IWRITE,3040) LAMIND
              DO K = 1,LAMIND
                WRITE (IWRITE,3010) K
                DO I = 1,KBMAX
                  WRITE (IWRITE,3020) (IBCPOL(I,J,K),J=1,KCMAX)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
C
          IF (KCMAX.GT.0) THEN
            IF (LAMCC.GT.0) THEN
              LAMIND = (LAMCC+1)/2
              WRITE (IWRITE,3050) LAMIND
              DO K = 1,LAMIND
                WRITE (IWRITE,3010) K
                DO I = 1,KCMAX
                  WRITE (IWRITE,3020) (ICCPOL(I,J,K),J=1,KCMAX)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
      K1S = 999
      K2S = 999
      IF (ITC(36).EQ.1) THEN
        WRITE (IWRITE,3000) K1S,K2S,IRK1
      ENDIF
C=======================================================================
 3000 FORMAT (' LOCM   out; k1s,k2s,     irk1     :',2I4,6X,I6/)
 3010 FORMAT (/' K = ',I4/)
 3020 FORMAT (1X,7I8)
 3030 FORMAT (/' DEBUG from LOCM routine ... multipole block'/
     +' IBBPOL(I,J,K) for K=1,LAMIND  and  LAMIND = ',I4)
 3040 FORMAT (/' IBCPOL(I,J,K) for K=1,LAMIND  and  LAMIND = ',I4)
 3050 FORMAT (/' ICCPOL(I,J,K) for K=1,LAMIND  and  LAMIND = ',I4)
      END
CEND--------------------------------------------------------------------
CEND    LTAB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LTAB(IS,NQS,KS,IROWS,IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/LTAB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IS
C   NQS
C   KS
C   IROWS
C   IWRITE
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IROWS(*),IS(*),KS(*),NQS(*)
      INTEGER IWRITE
C
C  Local variables
C
      INTEGER I,KQ(4),KQ1,KQ2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IS(1).EQ.IS(2)) NQS(1) = NQS(2) - 1
      IF (IS(3).EQ.IS(4)) NQS(3) = NQS(4) - 1
C
      DO I = 1,4
C
C  Fault exit - input data inconsistent
C
        IF (NQS(I).LE.0 .OR. NQS(I).GT.KS(I)) THEN
          WRITE (IWRITE,3000) NQS(I),IS(I),KS(I)
          STOP
        ENDIF
C
        KQ1 = NQS(I)-1
        KQ2 = KS(I)-KQ1
        KQ(I) = MIN(KQ1,KQ2)+1
C
C  Normal status - pointer properly defined.
C
        IF (KQ(I).NE.1) THEN
          IROWS(I) = (KS(I)*(KS(I)-2))/8+KQ(I)
        ELSE
          IROWS(I) = 1
        ENDIF
C
      ENDDO
C
 3000 FORMAT (/' Termination in LTAB because of data ERROR'/1X,I3,      !
     +'electrons in shell ',I3,' with 2j+1 =',I3)
      END
CEND--------------------------------------------------------------------
CEND    MAN5.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MAN5(IWRITE,KCHOP,IPAR,JMIN,JMAX,MFAIL,JSTORE,ICHOP, IQ!
     +, ISPAR, ITAB, ITJPO, JCUP, JPOS, JQS, JTAB,MLX, MQX, NAK, NCF, NR!
     +OWS, NTAB, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MAN5.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IWRITE ... stream for output to printer
C   KCHOP
C   IPAR   ... parity
C   JMIN   ... min 2J
C   JMAX   ... max 2J
C   MFAIL  ... indicates a dimension error for NCF
C   JSTORE ... array that stores the number of CSFs for each J
C   ICHOP
C   IQ
C   ISPAR
C   ITAB
C   ITJPO
C   JCUP
C   JPOS
C   JQS
C   JTAB
C   MLX
C   MQX
C   NAK
C   NCF
C   NROWS
C   NTAB
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER ICASEX
      PARAMETER (ICASEX=100)
      INTEGER JSTMAX
      PARAMETER (JSTMAX=40)
C
C  Argument variables
C
      INTEGER IPAR,IWRITE,JMAX,JMIN
      INTEGER JSTORE(0:JSTMAX),KCHOP(*)
      LOGICAL MFAIL
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER JCUP(10,*)
      INTEGER JPOS(*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER MLX(*)
      INTEGER MQX(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      INTEGER ICUP,JF,JFMAX,JFMIN
      INTEGER JK,JL,JM,JN
      INTEGER JST,K,KC,KCASE
      INTEGER KCUP(ICASEX),KMAX,KMIN
      INTEGER KQ(2,ICASEX),KQS(3,2,ICASEX)
      INTEGER L,LA,LC,LCASE
      INTEGER LCUP(ICASEX),LL,LMAX
      INTEGER LMIN,LP,LQ(2,ICASEX)
      INTEGER LQS(3,2,ICASEX),MC,MCASE
      INTEGER MCUP(ICASEX),MMAX,MMIN
      INTEGER MODE,MQ(2,ICASEX)
      INTEGER MQS(3,2,ICASEX),NC,NCASE
      INTEGER NCUP(ICASEX),NMAX,NMIN
      INTEGER NQ(2,ICASEX),NQS(3,2,ICASEX)
      INTEGER OC,OCASE,OCUP(ICASEX)
      INTEGER OQ(2,ICASEX),OQS(3,2,ICASEX)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO JST = 0,JSTMAX
        JSTORE(JST) = 0
      ENDDO
C
      CALL COUP(IWRITE,MLX(1),MQX(1),0,0,KQS,KQ,KCUP,KCASE,0,ICASEX,ITAB!
     +, JTAB, NROWS, NTAB)
C
      IF (KCASE.GT.0) THEN
        DO KC = 1,KCASE
          KMIN = ABS(KCUP(KC)-KQS(3,2,KC))
          KMAX = KCUP(KC)+KQS(3,2,KC)-2
          DO JK = KMIN,KMAX,2
            CALL COUP(IWRITE,MLX(2),MQX(2),JK,0,LQS,LQ,LCUP,LCASE,0,ICAS!
     +EX,ITAB, JTAB, NROWS, NTAB)
C
            IF (LCASE.GT.0) THEN
              DO LC = 1,LCASE
                LMIN = ABS(LCUP(LC)-LQS(3,2,LC))
                LMAX = LCUP(LC)+LQS(3,2,LC)-2
                DO JL = LMIN,LMAX,2
                  CALL COUP(IWRITE,MLX(3),MQX(3),JL,0,MQS,MQ,MCUP,MCASE,!
     +0,ICASEX,ITAB, JTAB, NROWS, NTAB)
C
                  IF (MCASE.GT.0) THEN
                    DO MC = 1,MCASE
                      MMIN = ABS(MCUP(MC)-MQS(3,2,MC))
                      MMAX = MCUP(MC)+MQS(3,2,MC)-2
                      DO JM = MMIN,MMAX,2
                        CALL COUP(IWRITE,MLX(4),MQX(4),JM,0,NQS,NQ,NCUP,!
     +NCASE,0,ICASEX,ITAB, JTAB, NROWS, NTAB)
C
                        IF (NCASE.GT.0) THEN
                          DO NC = 1,NCASE
                            NMIN = ABS(NCUP(NC)-NQS(3,2,NC))
                            NMAX = NCUP(NC)+NQS(3,2,NC)-2
                            DO JN = NMIN,NMAX,2
C
                              DO JF = JMIN,JMAX,2
C
                                IF (JF.LT.0) THEN
                                  MODE = 0
                                ELSE
                                  MODE = 1
                                ENDIF
C
                                CALL COUP(IWRITE,MLX(5),MQX(5),JN,JF,OQS!
     +,OQ,OCUP,OCASE,MODE,ICASEX,ITAB, JTAB, NROWS, NTAB)
C
                                IF (OCASE.GT.0) THEN
                                  DO OC = 1,OCASE
C******
                                    IF (JF.LT.0) THEN
                                      JFMIN = ABS(OCUP(OC)-OQS(3,2,OC))
                                      JFMAX = OCUP(OC)+OQS(3,2,OC)-2
                                    ELSE
                                      JFMIN = JF
                                      JFMAX = JF
                                    ENDIF
C
   10                               CONTINUE
                                    NCF = NCF+1
                                    IF (NCF.GT.MXNC) THEN
                                      MFAIL = .TRUE.
                                      GOTO 150
                                    ENDIF
C
                                    DO L = 1,NW
                                      JQS(1,L,NCF) = 0
                                      JQS(2,L,NCF) = 0
                                      JQS(3,L,NCF) = 1
                                      ICHOP(L,NCF) = -1
                                      IF (KCHOP(L).EQ.-1) THEN
                                        IQ(L,NCF) = 0
                                      ELSE
                                        IQ(L,NCF) = 2*ABS(NAK(L))
                                      ENDIF
                                    ENDDO
C
                                    LP = JPOS(1)
                                    IF (LP.GT.NW) GOTO 20
                                    DO K = 1,3
                                      JQS(K,LP,NCF) = KQS(K,2,KC)
                                      IQ(LP,NCF) = KQ(2,KC)
                                      IF (MLX(1).GT.0) THEN
                                        JQS(K,LP-1,NCF) = KQS(K,1,KC)
                                        IQ(LP-1,NCF) = KQ(1,KC)
                                      ENDIF
                                    ENDDO
C
                                    LP = JPOS(2)
                                    IF (LP.GT.NW) GOTO 20
                                    DO K = 1,3
                                      JQS(K,LP,NCF) = LQS(K,2,LC)
                                      IQ(LP,NCF) = LQ(2,LC)
                                      IF (MLX(2).GT.0) THEN
                                        JQS(K,LP-1,NCF) = LQS(K,1,LC)
                                        IQ(LP-1,NCF) = LQ(1,LC)
                                      ENDIF
                                    ENDDO
C
                                    LP = JPOS(3)
                                    IF (LP.GT.NW) GOTO 20
                                    DO K = 1,3
                                      JQS(K,LP,NCF) = MQS(K,2,MC)
                                      IQ(LP,NCF) = MQ(2,MC)
                                      IF (MLX(3).GT.0) THEN
                                        JQS(K,LP-1,NCF) = MQS(K,1,MC)
                                        IQ(LP-1,NCF) = MQ(1,MC)
                                      ENDIF
                                    ENDDO
C
                                    LP = JPOS(4)
                                    IF (LP.GT.NW) GOTO 20
                                    DO K = 1,3
                                      JQS(K,LP,NCF) = NQS(K,2,NC)
                                      IQ(LP,NCF) = NQ(2,NC)
                                      IF (MLX(4).GT.0) THEN
                                        JQS(K,LP-1,NCF) = NQS(K,1,NC)
                                        IQ(LP-1,NCF) = NQ(1,NC)
                                      ENDIF
                                    ENDDO
C
                                    LP = JPOS(5)
                                    IF (LP.GT.NW) GOTO 20
                                    DO K = 1,3
                                      JQS(K,LP,NCF) = OQS(K,2,OC)
                                      IQ(LP,NCF) = OQ(2,OC)
                                      IF (MLX(5).GT.0) THEN
                                        JQS(K,LP-1,NCF) = OQS(K,1,OC)
                                        IQ(LP-1,NCF) = OQ(1,OC)
                                      ENDIF
                                    ENDDO
C
   20                               CONTINUE
                                    LA = 0
                                    DO L = 1,5
                                      LP = JPOS(L)
                                      IF (LP.GT.NW) GOTO 30
                                      LA = LA+2
                                      IF (IQ(LP,NCF).EQ.2*ABS(NAK(LP))) !
     +THEN
                                        ICHOP(LA,NCF) = 1
                                      ELSEIF (IQ(LP,NCF).EQ.0) THEN
                                        ICHOP(LA,NCF) = -1
                                      ELSE
                                        ICHOP(LA,NCF) = 0
                                      ENDIF
                                      IF (MLX(L).GT.0) THEN
                                        IF (IQ(LP-1,NCF).EQ.2*ABS(NAK(LP!
     +-1))) THEN
                                          ICHOP(LA-1,NCF) = 1
                                        ELSEIF (IQ(LP-1,NCF).EQ.0) THEN
                                          ICHOP(LA-1,NCF) = -1
                                        ELSE
                                          ICHOP(LA-1,NCF) = 0
                                        ENDIF
                                      ELSE
                                        ICHOP(LA-1,NCF) = -1
                                      ENDIF
                                    ENDDO
C
   30                               CONTINUE
                                    ICUP = 1
                                    DO L = 1,10
                                      LL = L
                                      IF (ICHOP(L,NCF).EQ.0) GOTO 40
                                    ENDDO
                                    GOTO 140
C
   40                               CONTINUE
                                    IF (LL.EQ.1) GOTO 50
                                    IF (LL.EQ.2) GOTO 60
                                    IF (LL.EQ.3) GOTO 70
                                    IF (LL.EQ.4) GOTO 80
                                    IF (LL.EQ.5) GOTO 90
                                    IF (LL.EQ.6) GOTO 100
                                    IF (LL.EQ.7) GOTO 110
                                    IF (LL.EQ.8) GOTO 120
                                    IF (LL.EQ.9) GOTO 130
                                    IF (LL.EQ.10) GOTO 140
C
   50                               CONTINUE
                                    IF (ICHOP(2,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = JK+1
                                      ICUP = ICUP+1
                                    ENDIF
   60                               CONTINUE
                                    IF (ICHOP(3,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = LCUP(LC)
                                      ICUP = ICUP+1
                                    ENDIF
   70                               CONTINUE
                                    IF (ICHOP(4,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = JL+1
                                      ICUP = ICUP+1
                                    ENDIF
   80                               CONTINUE
                                    IF (ICHOP(5,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = MCUP(MC)
                                      ICUP = ICUP+1
                                    ENDIF
   90                               CONTINUE
                                    IF (ICHOP(6,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = JM+1
                                      ICUP = ICUP+1
                                    ENDIF
  100                               CONTINUE
                                    IF (ICHOP(7,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = NCUP(NC)
                                      ICUP = ICUP+1
                                    ENDIF
  110                               CONTINUE
                                    IF (ICHOP(8,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = JN+1
                                      ICUP = ICUP+1
                                    ENDIF
  120                               CONTINUE
                                    IF (ICHOP(9,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = OCUP(OC)
                                      ICUP = ICUP+1
                                    ENDIF
  130                               CONTINUE
                                    IF (ICHOP(10,NCF).EQ.0) THEN
                                      JCUP(ICUP,NCF) = JFMIN+1
                                    ENDIF
  140                               CONTINUE
C
                                    DO L = 1,NW
                                      ICHOP(L,NCF) = 0
                                      IF (IQ(L,NCF).EQ.2*ABS(NAK(L))) IC!
     +HOP(L,NCF) = 1
                                      IF (IQ(L,NCF).EQ.0) ICHOP(L,NCF) =!
     +-1
                                    ENDDO
C
                                    ITJPO(NCF) = JFMIN+1
                                    ISPAR(NCF) = IPAR
C
  150                               CONTINUE
                                    IF (JFMIN .LE. JSTMAX) THEN
                                      JSTORE(JFMIN) = JSTORE(JFMIN)+1
                                    ENDIF
                                    JFMIN = JFMIN+2
                                    IF (JFMIN.LE.JFMAX) GOTO 10
C******
                                  ENDDO
                                ENDIF
C
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDIF
C
                      ENDDO
                    ENDDO
                  ENDIF
C
                ENDDO
              ENDDO
            ENDIF
C
          ENDDO
        ENDDO
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    MCPINB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MCPINB(IDISC1,NXX,IDMTST, ISLDR, ITC, IWRITE, JSLDR, ND!
     +IMAX, NH, NMCP,NNLDR, NP, NSLDF, NW1, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MCPINB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDISC1 ... scratch file for angular coefficients
C   NXX    ... number of CSFs
C   IDMTST
C   ISLDR
C   ITC
C   IWRITE
C   JSLDR
C   NDIMAX
C   NH
C   NMCP
C   NNLDR
C   NP
C   NSLDF
C   NW1    ... number of orbitals + 1
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER IDISC1
      INTEGER IDMTST(*)
      INTEGER ISLDR(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER JSLDR(*)
      INTEGER NDIMAX(*)
      INTEGER NMCP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NSLDF(*)
      INTEGER NW1
      INTEGER NXX
C
C  Local variables
C
      DOUBLE PRECISION X
      INTEGER I,IA,IB,IC
      INTEGER ICX,ID,IK,IR
      INTEGER IRS,IRSM,IRX,IS
      INTEGER ISTOR,ISTOR1,ISTOR2,ISX
      INTEGER ITYPE,NMCPD,NMCPE,NMCPR
      INTEGER NMCPT
      LOGICAL HEAD
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      HEAD = .FALSE.
      WRITE (IWRITE,3000)
C
      NMCPR = 0
      NMCPD = 0
      NMCPE = 0
      NMCPT = 0
      NMCP = 0
C
      IRSM = NXX*(NXX+1)/2
      ICX = 1
C-----------------------------------------------------------------------
C
C  loop over pairs of CSFs
C
      DO IRX = 1,NXX
        DO ISX = IRX,NXX
C
          READ (IDISC1) IR,IS
          IRS = (IR-1)*(NXX+NXX-IR)/2+IS
          NNLDR(IRS) = 0
C
C  read the MCP coefficients
C
C  ITYPE=7  - one-electron
C  ITYPE=8  - direct Slater
C  ITYPE=9  - exchange Slater
C
   10     CONTINUE
          READ (IDISC1) ISTOR1,ISTOR2,ITYPE,X
          IF (ISTOR2.EQ.0) GOTO 20
C
C  count coefficients
C
          NMCPR = NMCPR+1
C
          IF (ITYPE.EQ.7) THEN
            NMCPT = NMCPT+1
          ELSE
            IF (ITYPE.EQ.9) THEN
              NMCPE = NMCPE+1
            ELSE
              NMCPD = NMCPD+1
            ENDIF
          ENDIF
C
C  decode the coefficients
C
          IF (ITYPE.EQ.8 .OR. ITYPE.EQ.9) THEN
            ISTOR = ISTOR2
            ID = MOD(ISTOR,NW1)
            IC = ISTOR/NW1
            ISTOR = ISTOR1
            IB = MOD(ISTOR,NW1)
            ISTOR = ISTOR/NW1
            IA = MOD(ISTOR,NW1)
            IK = ISTOR/NW1
          ELSE
            ISTOR = ISTOR2
            ID = MOD(ISTOR,NW1)
            IC = ISTOR/NW1
          ENDIF
C
C  print coefficients if option 13 is set
C
          IF (ITC(13).EQ.1) THEN
C
            IF (.NOT.HEAD) THEN
              WRITE (IWRITE,3070)
              HEAD = .TRUE.
            ENDIF
C
            IF (ITYPE.EQ.7) THEN
              WRITE (IWRITE,3110) IR,IS,NP(IC),NH(IC),NP(ID),NH(ID),IK,X
            ELSE
              WRITE (IWRITE,3080) IR,IS,NP(IA),NH(IA),NP(IB),NH(IB),NP(I!
     +C),NH(IC),NP(ID),NH(ID),IK,X
            ENDIF
C
          ENDIF
C
          NMCP = NMCP+1
          NNLDR(IRS) = NNLDR(IRS)+1
          IF (NMCP.GT.IDMTST(10)) GOTO 10
          ISLDR(NMCP) = ISTOR1
          JSLDR(NMCP) = ISTOR2
          XSLDR(NMCP) = X
C
          GOTO 10
C-----------------------------------------------------------------------
   20     CONTINUE
          NSLDF(IRS) = ICX
          ICX = ICX+NNLDR(IRS)
C
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C  write information
C
C-----------------------------------------------------------------------
      WRITE (IWRITE,3100)
      WRITE (IWRITE,3090) NMCPR,NMCPD,NMCPE,NMCPT
      WRITE (IWRITE,3120) NMCP
C
      CALL DMCHK2(10,NMCP, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
C
C  debug print out if option 14 is set
C
C-----------------------------------------------------------------------
      IF (ITC(14).EQ.1) THEN
        WRITE (IWRITE,3010) NW1,NXX,IRSM
        WRITE (IWRITE,3020) (NNLDR(I),I=1,IRSM)
        WRITE (IWRITE,3030) (NSLDF(I),I=1,IRSM)
        WRITE (IWRITE,3040) (ISLDR(I),I=1,NMCP)
        WRITE (IWRITE,3050) (JSLDR(I),I=1,NMCP)
        WRITE (IWRITE,3060) (XSLDR(I),I=1,NMCP)
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/' ===== MCPINB entered. Read in angular coefficients.')
 3010 FORMAT (/'   NW1 : ',I9/'   NXX : ',I9/'  IRSM : ',I9)
 3020 FORMAT (' NNLDR : ',5I9)
 3030 FORMAT (' NSLDF : ',5I9)
 3040 FORMAT (' ISLDR : ',5I9)
 3050 FORMAT (' JSLDR : ',5I9)
 3060 FORMAT (' XSLDR : ',1P,5E9.1)
 3070 FORMAT (/' MCP coefficients'//5X,'r',4X,'s',33X,'k'/)
 3080 FORMAT (1X,2I5,3X,4(I4,A2),3X,I4,3X,1P,E14.7)
 3090 FORMAT (/1X,I9,' MCP coefficients read from disk'//1X,I9,         !
     +' direct       coefficients'/1X,I9,' exchange     coefficients'/1X!
     +,I9,' one-electron coefficients')
 3100 FORMAT (/' angular coefficients for the target Hamiltonian')
 3110 FORMAT (1X,2I5,3X,12X,2(I4,A2),3X,I4,3X,1P,E14.7)
 3120 FORMAT (/1X,I9,' MCP coefficients used in program')
      END
CEND--------------------------------------------------------------------
CEND    MCPINC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MCPINC(IDISC1,IDISC4,NXX,IDMTST, ISLDR, ITC, IWRITE, J2!
     +P1, JLASTX, JSLDR, NCCK, NCFCON,NCFGP, NDIMAX, NH, NMCP, NNLDR, NP!
     +, NSLDF, NW, NW1, XSLDR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MCPINC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDISC1 ... scratch file for angular coefficients
C   IDISC4 ... scratch file for direct slater coefficients
C   NXX    ... number of CSFs
C   IDMTST
C   ISLDR
C   ITC
C   IWRITE
C   J2P1
C   JLASTX
C   JSLDR
C   NCCK
C   NCFCON
C   NCFGP
C   NDIMAX
C   NH
C   NMCP
C   NNLDR
C   NP
C   NSLDF
C   NW
C   NW1    ... number of orbitals + 1
C   XSLDR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER IZ
      PARAMETER (IZ=0)
C
C  Argument variables
C
      CHARACTER*2 NH(*)
      DOUBLE PRECISION XSLDR(*)
      INTEGER IDISC1
      INTEGER IDISC4
      INTEGER IDMTST(*)
      INTEGER ISLDR(*)
      INTEGER ITC(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JLASTX
      INTEGER JSLDR(*)
      INTEGER NCCK(*)
      INTEGER NCFCON
      INTEGER NCFGP
      INTEGER NDIMAX(*)
      INTEGER NMCP
      INTEGER NNLDR(*)
      INTEGER NP(*)
      INTEGER NSLDF(*)
      INTEGER NW
      INTEGER NW1
      INTEGER NXX
C
C  Local variables
C
      DOUBLE PRECISION X
      INTEGER I,IA,IB,IC
      INTEGER ICX,ID,IK,IMCP
      INTEGER IR,IRS,IRSM,IRX
      INTEGER IS,ISTOR,ISTOR1,ISTOR2
      INTEGER ISX,ITEMP,ITYPE,NMCPD
      INTEGER NMCPE,NMCPR,NMCPT,NMCPV
      INTEGER NMCPW,NSKIP
      LOGICAL HEAD,READIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      HEAD = .FALSE.
C
C  READIN determines whether the direct Slater angular coefficients
C  are reused
C
      IF (ITC(49).EQ.0 .AND. NCFGP.EQ.0) THEN
        READIN = .TRUE.
      ELSE
        READIN = .FALSE.
      ENDIF
C
      IF (READIN) REWIND IDISC4
C
      WRITE (IWRITE,3000)
C
      NMCPV = 0
      NMCPW = 0
      NSKIP = 0
      NMCPR = 0
      NMCPD = 0
      NMCPE = 0
      NMCPT = 0
      NMCP = 0
C
      IRSM = NXX*(NXX+1)/2
      ICX = 1
C-----------------------------------------------------------------------
C
C  loop over pairs of CSFs
C
      DO IRX = 1,NXX
        DO ISX = IRX,NXX
C
          READ (IDISC1) IR,IS
          IRS = (IR-1)*(NXX+NXX-IR)/2+IS
          NNLDR(IRS) = 0
C
C  read the MCP coefficients
C
C  ITYPE=7  - one-electron
C  ITYPE=8  - direct Slater
C  ITYPE=9  - exchange Slater
C
   10     CONTINUE
          READ (IDISC1) ISTOR1,ISTOR2,ITYPE,X
          IF (ISTOR2.EQ.0) GOTO 40
C
C  count coefficients
C
          NMCPR = NMCPR+1
C
          IF (ITYPE.EQ.7) THEN
            NMCPT = NMCPT+1
          ELSE
            IF (ITYPE.EQ.9) THEN
              NMCPE = NMCPE+1
            ELSE
              NMCPD = NMCPD+1
            ENDIF
          ENDIF
C
C  decode the coefficients
C
          IF (ITYPE.EQ.8 .OR. ITYPE.EQ.9) THEN
            ISTOR = ISTOR2
            ID = MOD(ISTOR,NW1)
            IC = ISTOR/NW1
            ISTOR = ISTOR1
            IB = MOD(ISTOR,NW1)
            ISTOR = ISTOR/NW1
            IA = MOD(ISTOR,NW1)
            IK = ISTOR/NW1
          ELSE
            IA = 0
            IB = 0
            IK = 0
            ISTOR = ISTOR2
            ID = MOD(ISTOR,NW1)
            IC = ISTOR/NW1
          ENDIF
C
C  some of the MCP coefficients are skipped
C
C  This is for continuum-continuum matrix elements which involve
C  only bound orbitals. These can only occur on the diagonal i.e.
C  same channel function. They have been already calculated for
C  the target Hamiltonian.
C
          IF (IR.GT.NCFCON .OR. IS.GT.NCFCON) GOTO 30
          IF (NCCK(IR).NE.NCCK(IS)) GOTO 30
C
C  TWO-ELECTRON
C
          IF (IB.GT.NW) GOTO 30
          IF (IB.GT.0) GOTO 20
C
C  ONE-ELECTRON
C
          IF (IC.GT.NW .OR. ID.GT.NW) GOTO 30
C
C  print skipped coefficients if option 34 is set
C
   20     CONTINUE
C
          IF (ITC(34).EQ.1) THEN
C
            IF (.NOT.HEAD) THEN
              WRITE (IWRITE,3070)
              HEAD = .TRUE.
            ENDIF
C
            IF (ITYPE.EQ.7) THEN
              WRITE (IWRITE,3170) IR,IS,NP(IC),NH(IC),NP(ID),NH(ID),IK,X
            ELSE
              WRITE (IWRITE,3150) IR,IS,NP(IA),NH(IA),NP(IB),NH(IB),NP(I!
     +C),NH(IC),NP(ID),NH(ID),IK,X
            ENDIF
C
          ENDIF
C
          NSKIP = NSKIP+1
          GOTO 10
C
C  print stored coefficients if option 34 is set
C
   30     CONTINUE
C
          IF (ITC(34).EQ.1) THEN
C
            IF (.NOT.HEAD) THEN
              WRITE (IWRITE,3070)
              HEAD = .TRUE.
            ENDIF
C
            IF (ITYPE.EQ.7) THEN
              WRITE (IWRITE,3110) IR,IS,NP(IC),NH(IC),NP(ID),NH(ID),IK,X
            ELSE
              WRITE (IWRITE,3080) IR,IS,NP(IA),NH(IA),NP(IB),NH(IB),NP(I!
     +C),NH(IC),NP(ID),NH(ID),IK,X
            ENDIF
C
          ENDIF
C
          NMCP = NMCP+1
          IF (NMCP.GT.IDMTST(10)) GOTO 10
          ISLDR(NMCP) = ISTOR1
          JSLDR(NMCP) = ISTOR2
          XSLDR(NMCP) = X
          NNLDR(IRS) = NNLDR(IRS)+1
C
C  write the direct coefficients to scratch disk  (IDISC4)
C
C  Note that JLASTX=-2 means that the code could not form all possible
C  channels. In this case the angular coefficients are not reused.
C
          IF (READIN .AND. J2P1.NE.JLASTX .AND. ITYPE.EQ.8) THEN
            IF (JLASTX.NE.-2) THEN
              WRITE (IDISC4) IRS,ISTOR1,ISTOR2,X
              NMCPV = NMCPV+1
            ENDIF
          ENDIF
C
          GOTO 10
C-----------------------------------------------------------------------
   40     CONTINUE
          NSLDF(IRS) = ICX
          ICX = ICX+NNLDR(IRS)
C
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C  Finished reading from stream IDISC1.
C  Now read from IDISC4 if required.
C
      IF (READIN .AND. J2P1.EQ.JLASTX) THEN
C
C  read the stored coefficients
C  these need to be positioned correctly in the list
C
   50   CONTINUE
        READ (IDISC4) IRS,ISTOR1,ISTOR2,X
        IF (IRS.EQ.0) GOTO 70
        NMCPW = NMCPW+1
C
C  print the coefficients if option 34 is set
C
        IF (ITC(34).EQ.1) THEN
C
          IF (.NOT.HEAD) THEN
            WRITE (IWRITE,3070)
            HEAD = .TRUE.
          ENDIF
C
          ITEMP = NXX
          DO IR = 1,NXX
            IF (IRS.LE.ITEMP) GOTO 60
            ITEMP = ITEMP+(NXX-IR)
          ENDDO
   60     CONTINUE
          IS = NXX-ITEMP+IRS
C
          ISTOR = ISTOR2
          ID = MOD(ISTOR,NW1)
          IC = ISTOR/NW1
          ISTOR = ISTOR1
          IB = MOD(ISTOR,NW1)
          ISTOR = ISTOR/NW1
          IA = MOD(ISTOR,NW1)
          IK = ISTOR/NW1
C
          WRITE (IWRITE,3160) IR,IS,NP(IA),NH(IA),NP(IB),NH(IB),NP(IC),N!
     +H(IC),NP(ID),NH(ID),IK,X
C
        ENDIF
C
        IF (NMCP+1.GT.IDMTST(10)) GOTO 50
C
C  position correctly in the list
C
        IF (IRS.LT.IRSM) THEN
          DO I = NMCP,NSLDF(IRS+1),-1
            ISLDR(I+1) = ISLDR(I)
            JSLDR(I+1) = JSLDR(I)
            XSLDR(I+1) = XSLDR(I)
          ENDDO
          DO I = IRS+1,IRSM
            NSLDF(I) = NSLDF(I)+1
          ENDDO
        ENDIF
C
C  store coefficient
C
        NMCP = NMCP+1
        IMCP = NSLDF(IRS)+NNLDR(IRS)
        ISLDR(IMCP) = ISTOR1
        JSLDR(IMCP) = ISTOR2
        XSLDR(IMCP) = X
        NNLDR(IRS) = NNLDR(IRS)+1
        GOTO 50
C
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C  write a file-end to the scratch file
C
C  Note that JLASTX=-2 means that the code could not form all possible
C  channels. In this case the angular coefficients are not reused.
C  We reset JLASTX to -1 which is the initial value.
C
   70 CONTINUE
      IF (READIN .AND. J2P1.NE.JLASTX) THEN
C
        IF (JLASTX.EQ.-2) THEN
          JLASTX = -1
        ELSE
          WRITE (IDISC4) IZ,IZ,IZ,ZERO
          JLASTX = J2P1
        ENDIF
C
      ENDIF
C
C  write information
C
      WRITE (IWRITE,3100)
      WRITE (IWRITE,3090) NMCPR,NMCPD,NMCPE,NMCPT,NSKIP
      IF (NMCPV.GT.0) WRITE (IWRITE,3120) NMCPV
      IF (NMCPW.GT.0) WRITE (IWRITE,3130) NMCPW
      WRITE (IWRITE,3140) NMCP
      CALL DMCHK2(10,NMCP, IWRITE, IDMTST, NDIMAX)
C
C  debug print out if option 35 is set
C
      IF (ITC(35).EQ.1) THEN
        WRITE (IWRITE,3010) NW1,NXX,IRSM
        WRITE (IWRITE,3020) (NNLDR(I),I=1,IRSM)
        WRITE (IWRITE,3030) (NSLDF(I),I=1,IRSM)
        WRITE (IWRITE,3040) (ISLDR(I),I=1,NMCP)
        WRITE (IWRITE,3050) (JSLDR(I),I=1,NMCP)
        WRITE (IWRITE,3060) (XSLDR(I),I=1,NMCP)
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (/' ===== MCPINC entered. Read in angular coefficients.')
 3010 FORMAT (/'   NW1 : ',I9/'   NXX : ',I9/'  IRSM : ',I9)
 3020 FORMAT (' NNLDR : ',5I9)
 3030 FORMAT (' NSLDF : ',5I9)
 3040 FORMAT (' ISLDR : ',5I9)
 3050 FORMAT (' JSLDR : ',5I9)
 3060 FORMAT (' XSLDR : ',1P,5E9.1)
 3070 FORMAT (/' MCP coefficients'//5X,'r',4X,'s',33X,'k'/)
 3080 FORMAT (1X,2I5,3X,4(I4,A2),3X,I4,3X,1P,E14.7)
 3090 FORMAT (/1X,I9,' MCP coefficients read from disk'//1X,I9,         !
     +' direct       coefficients'/1X,I9,' exchange     coefficients'/1X!
     +,I9,' one-electron coefficients'//1X,I9,                          !
     +' MCP coefficients skipped')
 3100 FORMAT (/' angular coefficients for the continuum Hamiltonian')
 3110 FORMAT (1X,2I5,3X,12X,2(I4,A2),3X,I4,3X,1P,E14.7)
 3120 FORMAT (/1X,I9,' direct coefficients written to IDISC4')
 3130 FORMAT (/1X,I9,' direct coefficients read from IDISC4')
 3140 FORMAT (/1X,I9,' MCP coefficients used in program')
 3150 FORMAT (1X,2I5,3X,4(I4,A2),3X,I4,3X,1P,E14.7,' skipped')
 3160 FORMAT (1X,2I5,3X,4(I4,A2),3X,I4,3X,1P,E14.7,' stored')
 3170 FORMAT (1X,2I5,3X,12X,2(I4,A2),3X,I4,3X,1P,E14.7,' skipped')
      END
CEND--------------------------------------------------------------------
CEND    MODJ23.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MODJ23( J2, J3, J2S, J3S, NMOM)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MODJ23.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C o J2    ...
C o J3    ...
C   J2S   ...
C   J3S   ...
C   NMOM  ...
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
C
C  Argument variables
C
      INTEGER J2(MTRIAD,*)
      INTEGER J3(MTRIAD,*)
      INTEGER J2S(MTRIAD,*)
      INTEGER J3S(MTRIAD,*)
      INTEGER NMOM
C
C  Local variables
C
      INTEGER I,J,NS2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NS2 = NMOM-1
C
      DO J = 1,3
        DO I = 1,NS2
          J2(I,J) = J2S(I,J)
          J3(I,J) = J3S(I,J)
        ENDDO
      ENDDO
C
      I = J3(1,3)
      J3(1,3) = J2(1,1)
      J2(1,1) = I
C
      END
CEND--------------------------------------------------------------------
CEND    MUMDAD.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE MUMDAD(IBUG2, IS, IWRITE, JBQ1, JBQ2, JJQ1, JJQ2, JTQ1,!
     + JTQ2,KAPS, NQ1, NQ2, X)
CRCS
CRCS $Source: /home/phn/DARC/RCS/MUMDAD.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  Routines called:  CFP
C
C     IBUG2   ... debug parameter
C     IS      ...
C     IWRITE  ... output stream number
C     JBQ1    ...
C     JBQ2    ...
C     JJQ1    ...
C     JJQ2    ...
C     JTQ1    ...
C     JTQ2    ...
C     KAPS    ...
C     NQ1     ...
C     NQ2     ...
C     X       ... result
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
C
C  Argument variables
C
      DOUBLE PRECISION X
      INTEGER IBUG2
      INTEGER IS(2,*)
      INTEGER IWRITE
      INTEGER JBQ1(3,*)
      INTEGER JBQ2(3,*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JTQ1(*)
      INTEGER JTQ2(*)
      INTEGER KAPS(2,*)
      INTEGER NQ1(*)
      INTEGER NQ2(*)
C
C  Local variables
C
      DOUBLE PRECISION C
      INTEGER II,IJD,IJP,IVD
      INTEGER IVP,IWD,IWP,LOCK
      INTEGER NEL
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      X = ONE
C-----------------------------------------------------------------------
C
C   First index
C
C-----------------------------------------------------------------------
      LOCK = KAPS(1,1)
      IF (ABS(LOCK).EQ.2) GOTO 10
C
      II = IS(1,1)
      NEL = NQ1(II)
      IVP = JBQ1(1,II)
      IWP = JBQ1(2,II)
      IJP = JBQ1(3,II)-1
C
      IF (IS(1,1).NE.IS(2,1)) THEN
C
C  IA1<>IB1 and IA2<>IB2; use JJQ array.
C
        IVD = JJQ1(1,II)
        IWD = JJQ1(2,II)
        IJD = JJQ1(3,II)-1
C
      ELSE
C
C  IA1=IB1 or IA2=IB2; JTQ array needed.
C
        NEL = NEL-1
        IVD = JTQ1(1)
        IWD = JTQ1(2)
        IJD = JTQ1(3)-1
      ENDIF
C
      CALL CFP(IWRITE,LOCK,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
      IF (IBUG2.EQ.1) WRITE (IWRITE,3000) LOCK,NEL,IJD,IVD,IWD,IJP,IVP,I!
     +WP,C
      IF (ABS(C).LT.EPS10) GOTO 50
      X = X*C
C-----------------------------------------------------------------------
   10 CONTINUE
      LOCK = KAPS(2,1)
      IF (ABS(LOCK).EQ.2) GOTO 20
      II = IS(2,1)
      NEL = NQ1(II)
      IVD = JJQ1(1,II)
      IWD = JJQ1(2,II)
      IJD = JJQ1(3,II)-1
C
      IF (IS(1,1).NE.IS(2,1)) THEN
        IVP = JBQ1(1,II)
        IWP = JBQ1(2,II)
        IJP = JBQ1(3,II)-1
      ELSE
        IVP = JTQ1(1)
        IWP = JTQ1(2)
        IJP = JTQ1(3)-1
      ENDIF
C
      CALL CFP(IWRITE,LOCK,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
      IF (IBUG2.EQ.1) WRITE (IWRITE,3000) LOCK,NEL,IJD,IVD,IWD,IJP,IVP,I!
     +WP,C
      IF (ABS(C).LT.EPS10) GOTO 50
      X = X*C
C-----------------------------------------------------------------------
C
C   Second index
C
C-----------------------------------------------------------------------
   20 CONTINUE
      LOCK = KAPS(1,2)
      IF (ABS(LOCK).EQ.2) GOTO 30
      II = IS(1,2)
      NEL = NQ2(II)
      IVP = JBQ2(1,II)
      IWP = JBQ2(2,II)
      IJP = JBQ2(3,II)-1
C
      IF (IS(1,2).NE.IS(2,2)) THEN
C
C  IA1<>IB1 and IA2<>IB2; use JJQ array.
C
        IVD = JJQ2(1,II)
        IWD = JJQ2(2,II)
        IJD = JJQ2(3,II)-1
C
      ELSE
C
C  IA1=IB1 or IA2=IB2; JTQ array needed.
C
        NEL = NEL-1
        IVD = JTQ2(1)
        IWD = JTQ2(2)
        IJD = JTQ2(3)-1
      ENDIF
C
      CALL CFP(IWRITE,LOCK,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
      IF (IBUG2.EQ.1) WRITE (IWRITE,3000) LOCK,NEL,IJD,IVD,IWD,IJP,IVP,I!
     +WP,C
      IF (ABS(C).LT.EPS10) GOTO 50
      X = X*C
C-----------------------------------------------------------------------
   30 CONTINUE
      LOCK = KAPS(2,2)
      IF (ABS(LOCK).EQ.2) GOTO 40
      II = IS(2,2)
      NEL = NQ2(II)
      IVD = JJQ2(1,II)
      IWD = JJQ2(2,II)
      IJD = JJQ2(3,II)-1
C
      IF (IS(1,2).NE.IS(2,2)) THEN
        IVP = JBQ2(1,II)
        IWP = JBQ2(2,II)
        IJP = JBQ2(3,II)-1
      ELSE
        IVP = JTQ2(1)
        IWP = JTQ2(2)
        IJP = JTQ2(3)-1
      ENDIF
C
      CALL CFP(IWRITE,LOCK,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
      IF (IBUG2.EQ.1) WRITE (IWRITE,3000) LOCK,NEL,IJD,IVD,IWD,IJP,IVP,I!
     +WP,C
      IF (ABS(C).LT.EPS10) GOTO 50
      X = X*C
   40 CONTINUE
      RETURN
C-----------------------------------------------------------------------
   50 CONTINUE
      X = ZERO
C-----------------------------------------------------------------------
 3000 FORMAT (' CFP  ',I3,I4,I7,2I4,I7,2I4,1P,E20.9)
      END
CEND--------------------------------------------------------------------
CEND    OCON.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      FUNCTION OCON(IA1,IB1,IA2,IB2,NQ1,NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/OCON.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IA1
C   IB1
C   IA2
C   IB2
C   NQ1
C   NQ2
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      DOUBLE PRECISION OCON
C
C  Argument variables
C
      INTEGER IA1,IA2,IB1,IB2
      INTEGER NQ1(*),NQ2(*)
C
C  Local variables
C
      DOUBLE PRECISION WA,WB,WC
      INTEGER IDL,IDR,IPHAS,K
      INTEGER LLD1,LLD2,LRD1,LRD2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WA = DBLE(NQ1(IA1)*NQ1(IB1))
      IF (IA1.EQ.IB1) WA = WA - DBLE(NQ1(IA1))
      WB = DBLE(NQ2(IA2)*NQ2(IB2))
      IF (IA2.EQ.IB2) WB = WB - DBLE(NQ2(IB2))
C
      WC = WA*WB
      OCON = SQRT(WC)
C
C   Set phase factor (-1)**(DELTA P)
C
      LRD1 = MIN(IA2,IB2)+1
      LRD2 = MAX(IA2,IB2)
      IDR = 0
      IF (LRD1.LE.LRD2) THEN
        IDR = 1
        DO K = LRD1,LRD2
          IDR = IDR+NQ2(K)
        ENDDO
      ENDIF
C
      LLD1 = MIN(IA1,IB1)+1
      LLD2 = MAX(IA1,IB1)
      IDL = 0
      IF (LLD1.LE.LLD2) THEN
        IDL = 1
        DO K = LLD1,LLD2
          IDL = IDL+NQ1(K)
        ENDDO
      ENDIF
C
      IPHAS = IDR-IDL
      IF (MOD(IPHAS,2).NE.0) OCON = -OCON
C
      END
CEND--------------------------------------------------------------------
CEND    PLACE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PLACE(MODE,N1,K1,N2,K2,IS,MAXFUL,MAXNQN,MINNQN )
CRCS
CRCS $Source: /home/phn/DARC/RCS/PLACE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   MODE   ... =1 for direct and =2 for exchange
C   N1     ... principal quantum number for orbital 1
C   K1     ... K-value for orbital 1
C   N2     ... principal quantum number for orbital 2
C   K2     ... K-value for orbital 2
C o IS     ... index of integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IS,K1,K2,MODE
      INTEGER N1,N2
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MINNQN(MXNK)
C
C  Local variables
C
      INTEGER N,N1MAX,N1MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IS = 0
C
      IF (K1.EQ.K2) THEN
C
        N1MIN = MINNQN(K1)
        N1MAX = N1-1
C
        IF (N1MIN.LE.N1MAX) THEN
          DO N = N1MIN,N1MAX
            IF (N.LE.MAXFUL(K1)) THEN
              IS = IS+1
            ELSE
              IF (MODE.EQ.1) THEN
                IS = IS+N-(N1MIN-1)
              ELSE
                IS = IS+MAXNQN(K1)-(N1MIN-1)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
        IF (N1.GT.MAXFUL(K1)) THEN
          IS = IS+N2-N1MIN
        ENDIF
C
      ELSE
C
        N1MIN = MINNQN(K1)
        N1MAX = N1-1
C
        IF (N1MIN.LE.N1MAX) THEN
          DO N = N1MIN,N1MAX
            IF (N.LE.MAXFUL(K1)) THEN
              IS = IS+MAXNQN(K2)-MAXFUL(K2)
            ELSE
              IS = IS+MAXNQN(K2)-(MINNQN(K2)-1)
            ENDIF
          ENDDO
        ENDIF
C
        IF (N1.LE.MAXFUL(K1)) THEN
          IS = IS+N2-MAXFUL(K2)-1
        ELSE
          IS = IS+N2-MINNQN(K2)
        ENDIF
C
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    PLACE1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PLACE1(N1P,K1,N2P,K2,N3P,K3,IS,MAXFUL,MAXNQN,MINNQN )
CRCS
CRCS $Source: /home/phn/DARC/RCS/PLACE1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1P    ... principal quantum number for orbital 1
C   K1     ... K-value for orbital 1
C   N2P    ... principal quantum number for orbital 2
C   K2     ... K-value for orbital 2
C   N3P    ... principal quantum number for orbital 3
C   K3     ... K-value for orbital 3
C o IS     ... index of integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IS,K1,K2,K3
      INTEGER N1P,N2P,N3P
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MINNQN(MXNK)
C
C  Local variables
C
      INTEGER N1,N1MIN,N2,N2MAX
      INTEGER N2MIN,N3,N3MAX,N3MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IS = 0
C
      N1MIN = MINNQN(K1)
      N2MIN = MINNQN(K2)
      N2MAX = MAXNQN(K2)
      N3MIN = MINNQN(K3)
      N3MAX = MAXNQN(K3)
C
C   loop over the principal quantum numbers
C
      DO N1 = N1MIN,N1P
        IF (N1.LT.N1P) THEN
          IF (K3.EQ.K1) N3MAX = N1
          DO N3 = N3MIN,N3MAX
            DO N2 = N2MIN,N2MAX
C
              IF (N1.GT.MAXFUL(K1)) GOTO 10
              IF (N2.GT.MAXFUL(K2)) GOTO 10
              IF (N3.GT.MAXFUL(K3)) GOTO 10
              IF (N1.EQ.N2 .AND. K1.EQ.K2) GOTO 10
              IF (N1.EQ.N3 .AND. K1.EQ.K3) GOTO 10
              GOTO 20
C
   10         CONTINUE
              IS = IS+1
   20         CONTINUE
C
            ENDDO
          ENDDO
C
        ELSE
C
          N3MAX = N3P
          DO N3 = N3MIN,N3MAX
            IF (N3.LT.N3MAX) THEN
              DO N2 = N2MIN,N2MAX
C
                IF (N1.GT.MAXFUL(K1)) GOTO 30
                IF (N2.GT.MAXFUL(K2)) GOTO 30
                IF (N3.GT.MAXFUL(K3)) GOTO 30
                IF (N1.EQ.N2 .AND. K1.EQ.K2) GOTO 30
                IF (N1.EQ.N3 .AND. K1.EQ.K3) GOTO 30
                GOTO 40
C
   30           CONTINUE
                IS = IS+1
   40           CONTINUE
C
              ENDDO
C
            ELSE
C
              N2MAX = N2P
              DO N2 = N2MIN,N2MAX
C
                IF (N1.GT.MAXFUL(K1)) GOTO 50
                IF (N2.GT.MAXFUL(K2)) GOTO 50
                IF (N3.GT.MAXFUL(K3)) GOTO 50
                IF (N1.EQ.N2 .AND. K1.EQ.K2) GOTO 50
                IF (N1.EQ.N3 .AND. K1.EQ.K3) GOTO 50
                GOTO 60
C
   50           CONTINUE
                IS = IS+1
   60           CONTINUE
C
              ENDDO
            ENDIF
C
          ENDDO
        ENDIF
C
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    PLACE2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PLACE2(N1P,K1,N2P,IS,MAXFUL,MINNQN )
CRCS
CRCS $Source: /home/phn/DARC/RCS/PLACE2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1P    ... principal quantum number for orbital 1
C   K1     ... K-value for orbital 1
C   N2P    ... principal quantum number for orbital 2
C o IS     ... index of integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IS,K1,N1P,N2P
      INTEGER MAXFUL(MXNK)
      INTEGER MINNQN(MXNK)
C
C  Local variables
C
      INTEGER N1,N1MIN,N2,N2MAX
      INTEGER N2MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IS = 0
C
      N1MIN = MINNQN(K1)
      N2MIN = MINNQN(K1)
C
C   loop over the principal quantum numbers
C
      DO N1 = N1MIN,N1P
        IF (N1.LT.N1P) THEN
          N2MAX = N1
          DO N2 = N2MIN,N2MAX
C
            IF (N1.GT.MAXFUL(K1)) GOTO 10
            IF (N2.GT.MAXFUL(K1)) GOTO 10
            IF (N1.EQ.N2) GOTO 10
            GOTO 20
C
   10       CONTINUE
            IS = IS+1
   20       CONTINUE
C
          ENDDO
C
        ELSE
C
          N2MAX = N2P
          DO N2 = N2MIN,N2MAX
C
            IF (N1.GT.MAXFUL(K1)) GOTO 30
            IF (N2.GT.MAXFUL(K1)) GOTO 30
            IF (N1.EQ.N2) GOTO 30
            GOTO 40
C
   30       CONTINUE
            IS = IS+1
   40       CONTINUE
C
          ENDDO
        ENDIF
C
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    PLACE3.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PLACE3(N1P,K1,N2P,K2,N3P,K3,N4P,K4,IS,MAXFUL,MAXNQN,MIN!
     +NQN )
CRCS
CRCS $Source: /home/phn/DARC/RCS/PLACE3.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   N1P    ... principal quantum number for orbital 1
C   K1     ... K-value for orbital 1
C   N2P    ... principal quantum number for orbital 2
C   K2     ... K-value for orbital 2
C   N3P    ... principal quantum number for orbital 3
C   K3     ... K-value for orbital 3
C   N4P    ... principal quantum number for orbital 4
C   K4     ... K-value for orbital 4
C o IS     ... index of integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C
C o indicates output from routine
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IS,K1,K2,K3
      INTEGER K4,N1P,N2P,N3P
      INTEGER N4P
      INTEGER MAXFUL(MXNK)
      INTEGER MAXNQN(MXNK)
      INTEGER MINNQN(MXNK)
C
C  Local variables
C
      INTEGER N1,N1MIN,N2,N2MAX
      INTEGER N2MIN,N3,N3MAX,N3MIN
      INTEGER N4,N4MAX,N4MIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IS = 0
C
      N1MIN = MINNQN(K1)
      N2MIN = MINNQN(K2)
      N2MAX = MAXNQN(K2)
      N3MIN = MINNQN(K3)
      N3MAX = MAXNQN(K3)
      N4MIN = MINNQN(K4)
      N4MAX = MAXNQN(K4)
C
C   loop over the principal quantum numbers
C
      DO N1 = N1MIN,N1P
        IF (N1.LT.N1P) THEN
          IF (K3.EQ.K1) N3MAX = N1
          IF (K2.EQ.K1) N2MAX = N1
          DO N3 = N3MIN,N3MAX
            DO N2 = N2MIN,N2MAX
              IF (K4.EQ.K2) N4MAX = N2
              DO N4 = N4MIN,N4MAX
C
                IF (N1.GT.MAXFUL(K1)) GOTO 10
                IF (N2.GT.MAXFUL(K2)) GOTO 10
                IF (N3.GT.MAXFUL(K3)) GOTO 10
                IF (N4.GT.MAXFUL(K4)) GOTO 10
                IF (N1.EQ.N2 .AND. K1.EQ.K2 .AND. N3.EQ.N4 .AND.K3.EQ.K4!
     +) GOTO 10
                IF (N1.EQ.N3 .AND. K1.EQ.K3 .AND. N2.EQ.N4 .AND.K2.EQ.K4!
     +) GOTO 10
                GOTO 20
C
   10           CONTINUE
                IS = IS+1
   20           CONTINUE
C
              ENDDO
            ENDDO
          ENDDO
C
        ELSE
C
          N3MAX = N3P
          IF (K2.EQ.K1) N2MAX = N1
          DO N3 = N3MIN,N3MAX
            IF (N3.LT.N3MAX) THEN
              DO N2 = N2MIN,N2MAX
                IF (K4.EQ.K2) N4MAX = N2
                DO N4 = N4MIN,N4MAX
C
                  IF (N1.GT.MAXFUL(K1)) GOTO 30
                  IF (N2.GT.MAXFUL(K2)) GOTO 30
                  IF (N3.GT.MAXFUL(K3)) GOTO 30
                  IF (N4.GT.MAXFUL(K4)) GOTO 30
                  IF (N1.EQ.N2 .AND. K1.EQ.K2 .AND. N3.EQ.N4 .AND.K3.EQ.!
     +K4) GOTO 30
                  IF (N1.EQ.N3 .AND. K1.EQ.K3 .AND. N2.EQ.N4 .AND.K2.EQ.!
     +K4) GOTO 30
                  GOTO 40
C
   30             CONTINUE
                  IS = IS+1
   40             CONTINUE
C
                ENDDO
              ENDDO
C
            ELSE
C
              N2MAX = N2P
              DO N2 = N2MIN,N2MAX
                IF (N2.LT.N2MAX) THEN
                  IF (K4.EQ.K2) N4MAX = N2
                  DO N4 = N4MIN,N4MAX
C
                    IF (N1.GT.MAXFUL(K1)) GOTO 50
                    IF (N2.GT.MAXFUL(K2)) GOTO 50
                    IF (N3.GT.MAXFUL(K3)) GOTO 50
                    IF (N4.GT.MAXFUL(K4)) GOTO 50
                    IF (N1.EQ.N2 .AND. K1.EQ.K2 .AND. N3.EQ.N4 .AND.K3.E!
     +Q.K4) GOTO 50
                    IF (N1.EQ.N3 .AND. K1.EQ.K3 .AND. N2.EQ.N4 .AND.K2.E!
     +Q.K4) GOTO 50
                    GOTO 60
C
   50               CONTINUE
                    IS = IS+1
   60               CONTINUE
C
                  ENDDO
C
                ELSE
C
                  N4MAX = N4P
                  DO N4 = N4MIN,N4MAX
C
                    IF (N1.GT.MAXFUL(K1)) GOTO 70
                    IF (N2.GT.MAXFUL(K2)) GOTO 70
                    IF (N3.GT.MAXFUL(K3)) GOTO 70
                    IF (N4.GT.MAXFUL(K4)) GOTO 70
                    IF (N1.EQ.N2 .AND. K1.EQ.K2 .AND. N3.EQ.N4 .AND.K3.E!
     +Q.K4) GOTO 70
                    IF (N1.EQ.N3 .AND. K1.EQ.K3 .AND. N2.EQ.N4 .AND.K2.E!
     +Q.K4) GOTO 70
                    GOTO 80
C
   70               CONTINUE
                    IS = IS+1
   80               CONTINUE
C
                  ENDDO
                ENDIF
C
              ENDDO
            ENDIF
C
          ENDDO
        ENDIF
C
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    PROP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PROP(EIG, XMT, FACTAN, FACTCM, FACTEV, ISPAR, ITC, ITJP!
     +O,IWRITE, NAST, NCF)
CRCS
CRCS $Source: /home/phn/DARC/RCS/PROP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   EIG    ... target eigenvalues
C   XMT    ... target eigenvectors
C   FACTAN ...
C   FACTCM ...
C   FACTEV ...
C   ISPAR  ...
C   ITC    ...
C   ITJPO  ...
C   IWRITE ...
C   NAST   ... number of target levels to be included
C   NCF    ... number of target CSFs
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
C
C  Argument variables
C
      DOUBLE PRECISION EIG(*)
      DOUBLE PRECISION XMT(MXNL,*)
      DOUBLE PRECISION FACTAN
      DOUBLE PRECISION FACTCM
      DOUBLE PRECISION FACTEV
      INTEGER ISPAR(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER NAST
      INTEGER NCF
C
C  Local variables
C
      DOUBLE PRECISION AM,EAN,EAU,ECM
      DOUBLE PRECISION EEV,ERY
      INTEGER I,IA,II,IJ
      INTEGER IP,J,LEV(MXNL)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
C
      DO J = 1,NAST
C
        EAU = EIG(J)
        ERY = EAU+EAU
C
        IA = 0
        AM = ZERO
        DO I = 1,NCF
          IF (ABS(XMT(I,J)).GT.AM) THEN
            AM = ABS(XMT(I,J))
            IA = I
          ENDIF
        ENDDO
        LEV(J) = IA
C
        I = LEV(J)
        IJ = ITJPO(I)-1
        IP = ISPAR(I)
        AM = XMT(I,J)
C
        IF (AM.LT.ZERO) THEN
          DO II = 1,NCF
            XMT(II,J) = -XMT(II,J)
          ENDDO
          AM = -AM
        ENDIF
C
        IF (MOD(IJ,2).EQ.0) THEN
          IJ = IJ/2
          IF (IP.EQ.1) THEN
            WRITE (IWRITE,3060) J,IJ,I,AM,EAU,ERY
          ELSE
            WRITE (IWRITE,3070) J,IJ,I,AM,EAU,ERY
          ENDIF
        ELSE
          IF (IP.EQ.1) THEN
            WRITE (IWRITE,3040) J,IJ,I,AM,EAU,ERY
          ELSE
            WRITE (IWRITE,3050) J,IJ,I,AM,EAU,ERY
          ENDIF
        ENDIF
C
      ENDDO
C
      IF (ITC(10).EQ.1) THEN
C
        WRITE (IWRITE,3010)
C
        DO J = 1,NAST
C
          EAU = EIG(J)
          ECM = EAU*FACTCM
          EEV = EAU*FACTEV
C
          I = LEV(J)
          IJ = ITJPO(I)-1
          IP = ISPAR(I)
C
          IF (MOD(IJ,2).EQ.0) THEN
            IJ = IJ/2
            IF (IP.EQ.1) THEN
              WRITE (IWRITE,3140) J,IJ,ECM,EEV
            ELSE
              WRITE (IWRITE,3150) J,IJ,ECM,EEV
            ENDIF
          ELSE
            IF (IP.EQ.1) THEN
              WRITE (IWRITE,3120) J,IJ,ECM,EEV
            ELSE
              WRITE (IWRITE,3130) J,IJ,ECM,EEV
            ENDIF
          ENDIF
C
        ENDDO
C
      ENDIF
C
C   write out energy levels relative to groundstate
C
      IF (NAST.GT.1) THEN
C
        WRITE (IWRITE,3030)
C
        DO J = 1,NAST
C
          EAU = EIG(J)-EIG(1)
          ERY = EAU+EAU
C
          I = LEV(J)
          IJ = ITJPO(I)-1
          IP = ISPAR(I)
          AM = XMT(I,J)
C
          IF (MOD(IJ,2).EQ.0) THEN
            IJ = IJ/2
            IF (IP.EQ.1) THEN
              WRITE (IWRITE,3100) J,IJ,I,AM,EAU,ERY
            ELSE
              WRITE (IWRITE,3110) J,IJ,I,AM,EAU,ERY
            ENDIF
          ELSE
            IF (IP.EQ.1) THEN
              WRITE (IWRITE,3080) J,IJ,I,AM,EAU,ERY
            ELSE
              WRITE (IWRITE,3090) J,IJ,I,AM,EAU,ERY
            ENDIF
          ENDIF
C
        ENDDO
C
        IF (ITC(10).EQ.1) THEN
C
          WRITE (IWRITE,3020)
C
          DO J = 1,NAST
C
            EAU = EIG(J)-EIG(1)
            ECM = EAU*FACTCM
            EEV = EAU*FACTEV
            EAN = ZERO
            IF (J.GT.1) EAN = FACTAN/ECM
C
            I = LEV(J)
            IJ = ITJPO(I)-1
            IP = ISPAR(I)
C
            IF (MOD(IJ,2).EQ.0) THEN
              IJ = IJ/2
              IF (IP.EQ.1) THEN
                WRITE (IWRITE,3180) J,IJ,ECM,EEV,EAN
              ELSE
                WRITE (IWRITE,3190) J,IJ,ECM,EEV,EAN
              ENDIF
            ELSE
              IF (IP.EQ.1) THEN
                WRITE (IWRITE,3160) J,IJ,ECM,EEV,EAN
              ELSE
                WRITE (IWRITE,3170) J,IJ,ECM,EEV,EAN
              ENDIF
            ENDIF
C
          ENDDO
C
        ENDIF
C
      ENDIF
C
C   write out the eigenvectors
C
      IF (ITC(12).EQ.1) CALL MATOUT(IWRITE,XMT,NCF,NAST,MXNL,MXNL,3)
C
 3000 FORMAT (/' eigen-energies'/' --------------'//                    !
     +'                      dominant'/                                 !
     +'  level  J parity  CSF   mix         a.u.',16X,'Ryd.'/)
 3010 FORMAT (/'  level  J parity      cm-1',16X,'e.V.'/)
 3020 FORMAT (/'  level  J parity      cm-1',12X,'e.V.',12X,'Ang.'/)
 3030 FORMAT (/' eigen-energies relative to the lowest'/                !
     +' -------------------------------------'//                        !
     +'                      dominant'/                                 !
     +'  level  J parity  CSF   mix         a.u.',12X,'Ryd.'/)
 3040 FORMAT (1X,I4,2X,I3,'/2  e   ',I4,2X,F5.3,3X,1P,2E20.12)
 3050 FORMAT (1X,I4,2X,I3,'/2  o   ',I4,2X,F5.3,3X,1P,2E20.12)
 3060 FORMAT (1X,I4,2X,I3,'    e   ',I4,2X,F5.3,3X,1P,2E20.12)
 3070 FORMAT (1X,I4,2X,I3,'    o   ',I4,2X,F5.3,3X,1P,2E20.12)
 3080 FORMAT (1X,I4,2X,I3,'/2  e   ',I4,2X,F5.3,3X,1P,2E16.8)
 3090 FORMAT (1X,I4,2X,I3,'/2  o   ',I4,2X,F5.3,3X,1P,2E16.8)
 3100 FORMAT (1X,I4,2X,I3,'    e   ',I4,2X,F5.3,3X,1P,2E16.8)
 3110 FORMAT (1X,I4,2X,I3,'    o   ',I4,2X,F5.3,3X,1P,2E16.8)
 3120 FORMAT (1X,I4,2X,I3,'/2  e   ',1P,2E20.12)
 3130 FORMAT (1X,I4,2X,I3,'/2  o   ',1P,2E20.12)
 3140 FORMAT (1X,I4,2X,I3,'    e   ',1P,2E20.12)
 3150 FORMAT (1X,I4,2X,I3,'    o   ',1P,2E20.12)
 3160 FORMAT (1X,I4,2X,I3,'/2  e   ',1P,3E16.8)
 3170 FORMAT (1X,I4,2X,I3,'/2  o   ',1P,3E16.8)
 3180 FORMAT (1X,I4,2X,I3,'    e   ',1P,3E16.8)
 3190 FORMAT (1X,I4,2X,I3,'    o   ',1P,3E16.8)
      END
CEND--------------------------------------------------------------------
CEND    READ2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE READ2(
     + IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6, ICHOP, IDMTST, IHED,
     + IPOLPHy, IPOS, IQ, IREAD, ISPAR, ITAB, ITC, ITJPO, IWRITE,
     + JCHOP, JCUP, JQS, JTAB, NAK, NASTy, NCF, NDIMAX, NH, NLX,
     + NMANy, NP, NQX, NROWS, NTAB, NW, NWMy)
CRCS
CRCS $Source: /home/phn/DARC/RCS/READ2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICHOP
C   IDMTST
C   IHED
C   IPOLPHy
C   IPOS
C   IQ
C   IREAD
C   ISPAR
C   ITAB
C   ITC
C   ITJPO
C   IWRITE
C   JCHOP
C   JCUP
C   JQS
C   JTAB
C   NAK
C   NASTy
C   NCF
C   NDIMAX
C   NH
C   NLX
C   NMANy
C   NP
C   NQX
C   NROWS
C   NTAB
C   NW
C   NWMy
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      CHARACTER*2 NH(*)
      CHARACTER*80 IHED
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IPOLPHy
      INTEGER IPOS(*)
      INTEGER IQ(MXNW,*)
      INTEGER IREAD
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITC(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCHOP(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NASTy
      INTEGER NCF
      INTEGER NDIMAX(*)
      INTEGER NLX(*)
      INTEGER NMANy
      INTEGER NP(*)
      INTEGER NQX(MXNW,*)
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWMy
C
C  Local variables
C
      INTEGER I
C
      INTEGER IPOLPH
      INTEGER NAST
      INTEGER NMAN
      INTEGER NWM
      INTEGER OPT(50)
      integer inast
C
C  Namelists
C
      NAMELIST / DSTG2 / IPOLPH,NAST,NMAN,NWM,OPT   ,inast      !for par
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   Input record 1
C
C   Read the title (up to 80 characters)
C
      READ (IREAD,3000) IHED
C-----------------------------------------------------------------------
C
C   Input record 2
C
C   Namelist DSTG2
C
      IPOLPH = 1
      NAST = -1
      NMAN = -1
      NWM = -1
      DO I = 1,50
        OPT(I) = -1
        ITC(I) = 0
      ENDDO
C
      READ (IREAD,DSTG2)
C
      DO I = 1,50
        IF (OPT(I).GE.1 .AND. OPT(I).LE.50) THEN
          ITC(OPT(I)) = 1
        ENDIF
      ENDDO
C
      IF (NWM.LT.1) THEN
        WRITE (IWRITE,3030)
        STOP
      ENDIF
C
      IF (NWM.GT.0 .AND. NMAN.LT.1) THEN
        WRITE (IWRITE,3020)
        STOP
      ENDIF
C
      IF (IPOLPH.LT.1 .OR. IPOLPH.GT.2) THEN
        WRITE (IWRITE,3040)
        STOP
      ENDIF
C-----------------------------------------------------------------------
C
C   define the target CSFs
C
      CALL DATNR(
     + NMAN, NWM, IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6,
     + ICHOP, IDMTST, IPOS, IQ, IREAD, ISPAR, ITAB, ITJPO, IWRITE,
     + JCHOP, JCUP, JQS, JTAB, NAK, NCF, NDIMAX, NH, NLX, NP, NQX,
     + NROWS, NTAB, NW)
C
      CALL DMCHK2(09, NCF, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
      IF (NAST.LT.1) THEN
        NAST = NCF
      ENDIF
C
      IF (NAST.GT.NCF) THEN
        NAST = NCF
        WRITE (IWRITE,3010) NAST
      ENDIF
C
      IPOLPHy = IPOLPH
      NASTy = NAST
      NMANy = NMAN
      NWMy = NWM
C=======================================================================
 3000 FORMAT (A80)
 3010 FORMAT (/
     +' *** NAST (the number of levels to be included) has been reset ',
     +'to ',I6,' ***'/)
 3020 FORMAT (/' STOPPING in READ2.'/
     +' NMAN has not been set in the input data.')
 3030 FORMAT (/' STOPPING in READ2.'/
     +' NWM > 0 has not been set in the input data.')
 3040 FORMAT (/' STOPPING in READ2.'/
     +' IPOLPH must be set to 1 or 2 in the input data.')
      END
CEND--------------------------------------------------------------------
CEND    READ2C.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE READ2C(MORE,ICHOP, IDMTST, IPOS, IQ, IREAD, ISPAR, ITAB!
     +, ITJPO,IWRITE, J2P1, JCHOP, JCUP, JQS, JTAB, NAK, NCF, yNCFGP,NDI!
     +MAX, NLX, NMAN, yNPTY, NQX, NROWS, NTAB, NW, NWM)
CRCS
CRCS $Source: /home/phn/DARC/RCS/READ2C.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ICHOP
C   IDMTST
C   IPOS
C   IQ
C   IREAD
C   ISPAR
C   ITAB
C   ITJPO
C   IWRITE
C   J2P1
C   JCHOP
C   JCUP
C   JQS
C   JTAB
C   NAK
C   NCF
C   yNCFGP
C   NDIMAX
C   NLX
C   NMAN
C   yNPTY
C   NQX
C   NROWS
C   NTAB
C   NW
C   NWM
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
C
C  Argument variables
C
      INTEGER MORE
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IDMTST(*)
      INTEGER IPOS(*)
      INTEGER IQ(MXNW,*)
      INTEGER IREAD
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER J2P1
      INTEGER JCHOP(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER yNCFGP
      INTEGER NDIMAX(*)
      INTEGER NLX(*)
      INTEGER NMAN
      INTEGER yNPTY
      INTEGER NQX(MXNW,*)
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWM
C
C  Local variables
C
      INTEGER I,JF,NCFST
C
      DOUBLE PRECISION JTOT
      INTEGER CORINC(MXNC)
      INTEGER NCFGP
      INTEGER NPTY
C
C  Namelists
C
      NAMELIST / SYM / CORINC,JTOT,NCFGP,NPTY
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   Input record 7
C
C   NAMELIST ... SYM
C
C-----------------------------------------------------------------------
      JTOT = -1.D0
      NPTY = 0
      NCFGP = mxnl
C
C  set array CORINC to 1
C
C  CORINC(I) = 1 if NR CSF I is to be used to generate
C                correlation functions. This is the default.
C  CORINC(I) = 0 if the NR CSF is to be excluded.
C
      DO I = 1,MXNC
        CORINC(I) = 1
      ENDDO
C
      READ (IREAD,SYM,END=10)
C
      IF (JTOT.LT.ZERO) THEN
        MORE = 0
        RETURN
      ENDIF
C
      IF (NPTY.NE.1 .AND. NPTY.NE.-1) THEN
        WRITE (IWRITE,3010)
        STOP
      ENDIF
C
      J2P1 = NINT(JTOT+JTOT)+1
C
      DO I = 1,NMAN
        IF (CORINC(I).LT.0 .OR. CORINC(I).GT.1) THEN
          WRITE (IWRITE,3020)
          STOP
        ENDIF
      ENDDO
c
c Restrict N+1 to formation from first -NCFGP NR CSFs. nrb 20/08/09
c
      if(ncfgp.lt.0)then
        do i=-ncfgp+1,nman
          corinc(i)=0
        enddo
      endif
C-----------------------------------------------------------------------
C
C   define the correlation functions
C
C-----------------------------------------------------------------------
      IF (NWM.EQ.0) THEN
        NCFGP = 0
      ELSEIF (NCFGP.NE.0) THEN
        NCFST = NCF
        JF = J2P1-1
        CALL GENCOR(NMAN,NWM,NCFST,JF,NPTY,CORINC,ICHOP, IDMTST, IPOS, I!
     +Q, ISPAR, ITAB, ITJPO, IWRITE, JCHOP,JCUP, JQS, JTAB, NAK, NCF, ND!
     +IMAX, NLX,NQX, NROWS, NTAB, NW)
        NCFGP = NCF-NCFST
        CALL DMCHK2(09,NCFGP, IWRITE, IDMTST, NDIMAX)
        NCF = NCFST
      ENDIF
C
      WRITE (IWRITE,3000) NCFGP
C
      yNCFGP = NCFGP
      yNPTY = NPTY
C
      RETURN
C
   10 CONTINUE
      MORE = 0
C-----------------------------------------------------------------------
 3000 FORMAT (/I5,                                                      !
     +' correlation (N+1 electron) functions have been generated'/)
 3010 FORMAT (/' STOPPING in READ2C.'/                                  !
     +' NPTY must be set to 1 or -1 in the input data.')
 3020 FORMAT (/' STOPPING in READ2C.'/                                  !
     +' Array CORINC must be 0 or 1 in the input data.')
      END
CEND--------------------------------------------------------------------
CEND    REORDR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE REORDR(NST,ICHOP, IQ, ITJPO, JCUP, JQS, NCF, NW)
CRCS
CRCS $Source: /home/phn/DARC/RCS/REORDR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NST
C   ICHOP
C   IQ
C   ITJPO
C   JCUP
C   JQS
C   NCF
C   NW
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER NST
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER ITJPO(*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER NCF
      INTEGER NW
C
C  Local variables
C
      INTEGER I,ICF,IFIRST
      INTEGER IPOS(MXNC),J,K,L
      INTEGER M,MCHOP(MXNW),MCUP(10)
      INTEGER MQ(MXNW),MQS(3,MXNW),MTJPO,N
      INTEGER NEXT(MXNC),NST1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Exit if only one member in list.
C
      IF (NST.EQ.NCF) RETURN
C
C  Set up linked list giving desired order of CSF.
C
      NEXT(NST) = 0
      IFIRST = NST
      NST1 = NST+1
      DO ICF = NST1,NCF
        M = IFIRST
        L = 0
   10   CONTINUE
        IF (ITJPO(ICF).LT.ITJPO(M)) GOTO 20
        IF (L.NE.0) GOTO 30
        NEXT(ICF) = IFIRST
        IFIRST = ICF
        GOTO 40
C
   20   CONTINUE
        L = M
        M = NEXT(L)
        IF (M.NE.0) GOTO 10
   30   CONTINUE
        NEXT(ICF) = NEXT(L)
        NEXT(L) = ICF
   40   CONTINUE
      ENDDO
C
C  Invert list to give list of positions of CSF.
C
      L = IFIRST
      DO I = NST,NCF
        IPOS(L) = I
        L = NEXT(L)
      ENDDO
C
C  Reorder CSF.
C
      DO I = NST,NCF
        L = IPOS(I)
        IF (L.EQ.I) GOTO 60
        DO J = 1,NW
          DO M = 1,3
            MQS(M,J) = JQS(M,J,I)
          ENDDO
          MQ(J) = IQ(J,I)
          MCHOP(J) = ICHOP(J,I)
        ENDDO
        MTJPO = ITJPO(I)
        DO J = 1,10
          MCUP(J) = JCUP(J,I)
        ENDDO
   50   CONTINUE
        K = L
        DO J = 1,NW
          DO M = 1,3
            N = MQS(M,J)
            MQS(M,J) = JQS(M,J,K)
            JQS(M,J,K) = N
          ENDDO
          N = MQ(J)
          MQ(J) = IQ(J,K)
          IQ(J,K) = N
          N = MCHOP(J)
          MCHOP(J) = ICHOP(J,K)
          ICHOP(J,K) = N
        ENDDO
        N = MTJPO
        MTJPO = ITJPO(K)
        ITJPO(K) = N
        DO J = 1,10
          N = MCUP(J)
          MCUP(J) = JCUP(J,K)
          JCUP(J,K) = N
        ENDDO
        L = IPOS(K)
        IPOS(K) = K
        IF (L.NE.I) GOTO 50
        DO J = 1,NW
          DO M = 1,3
            JQS(M,J,I) = MQS(M,J)
          ENDDO
          IQ(J,I) = MQ(J)
          ICHOP(J,I) = MCHOP(J)
        ENDDO
        ITJPO(I) = MTJPO
        DO J = 1,10
          JCUP(J,I) = MCUP(J)
        ENDDO
        IPOS(I) = I
   60   CONTINUE
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    RKCO.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RKCO(JA,JB,NH, IBUG1, IBUG2, IBUG3, ICHOP, IEXC, IQ, IM!
     +E, ISPAR, ITJPO,IWRITE, JCUP, JQS, NAK, NCF, NOUTX, NP, NW, NWA,IT!
     +AB, JTAB, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RKCO.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C The CSFs are JA, JB.
C The following conventions are in force:
C labels 1,2 refer to L, R sides of matrix element respectively
C Pointers:
C JA1, JB1, JA2, JB2 point to the JLIST array of active orbitals,
C IA1, IB1, IA2, IB2 point to the complete list of orbitals.
C
C   NH
C   IBUG1
C   IBUG2
C   IBUG3
C   ICHOP
C   IEXC
C   IQ
C   IME
C   ISPAR
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   NAK
C   NCF
C   NOUTX
C   NP
C   NW
C   NWA
C   ITAB
C   JTAB
C   NTAB
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NPLX
      PARAMETER (NPLX=14)
C
C  Argument variables
C
      INTEGER JA,JB
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER ICHOP(MXNW,*)
      INTEGER IEXC
      INTEGER IQ(MXNW,*)
      INTEGER IME
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NOUTX
      INTEGER NP(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWA
C
C  Local variables
C
      INTEGER I,IDQ,IT1,IT2
      INTEGER J,JA1,JA2,JB1
      INTEGER JB2,JC1S(NPLX),JC2S(NPLX)
      INTEGER JLIS(NPLX),JT1,JT2,K1
      INTEGER KLAST,KW1,KW2,KWA
      INTEGER NPEELM
C
      INTEGER JJC1(NPLX),JJC2(NPLX)
      INTEGER NQ1(MXNW),NQ2(MXNW)
      INTEGER JJQ1(3,MXNW),JJQ2(3,MXNW)
      INTEGER JLIST(NPLX),KLIST(MXNW),NCORE,NPEEL
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (ITJPO(JA).NE.ITJPO(JB)) RETURN
      CALL SETUP(IBUG2,IWRITE,JA,JB,ICHOP, IQ, JCUP, JQS, NCF, NW,JJC1, !
     +JJC2, JJQ1, JJQ2, JLIST, KLIST, NCORE, NPEEL, NQ1, NQ2)
      IF (IBUG2.EQ.1) CALL VIJOUT(IWRITE,JA,JB,JJC1, JJC2, JJQ1, JJQ2, J!
     +LIST, NCORE, NQ1, NQ2, NPEEL)
C-----------------------------------------------------------------------
      IF (IBUG2.EQ.1) THEN
        WRITE (IWRITE,3000) NW,NCF,JA,JB
        WRITE (IWRITE,3010) (ITJPO(I),I=1,NCF)
        WRITE (IWRITE,3020) (ISPAR(I),I=1,NCF)
        DO I = 1,NW
          WRITE (IWRITE,3030) (IQ(I,J),J=1,NCF)
        ENDDO
        WRITE (IWRITE,3040) NPEEL,NCORE
        WRITE (IWRITE,3050) (JLIST(I),I=1,NPEEL)
        IF (NPEEL.GT.1) THEN
          NPEELM = NPEEL-1
          WRITE (IWRITE,3060) (JJC1(I),I=1,NPEELM)
          WRITE (IWRITE,3070) (JJC2(I),I=1,NPEELM)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C   Analyse peel shell interactions
C
C   Analyse electron distribution in peel.
C   (The full procedure is needed only if the number of peel orbitals
C    NPEEL.GE.2)
C
C-----------------------------------------------------------------------
      IF (NW.LT.1) GOTO 40
      IF (NPEEL.EQ.0) RETURN
      IF (NPEEL.EQ.1) GOTO 20
C-----------------------------------------------------------------------
C
C   Find differences in occupations, NDQ, for each peel orbital
C   in turn and use to set up labels of active orbitals maintaining
C   the convention   JA1.LE.JB1 and JA2.LE.JB2.
C
C-----------------------------------------------------------------------
      CALL RKCO1(IDQ,JA1,JB1,JA2,JB2,JLIST,NPEEL,NQ1,NQ2)
C-----------------------------------------------------------------------
C
C   Calculate coefficients for all possible sets of active shells.
C
C   There are 4 cases, depending on the value of IDQ, the sum of the
C   absolute differences NDQ -
C
C   IDQ.GT.4 - matrix element null
C
C-----------------------------------------------------------------------
      IF (IDQ.GT.4) RETURN
C-----------------------------------------------------------------------
C
C   IDQ.EQ.4 - matrix element uniquely defined
C
C-----------------------------------------------------------------------
      IF (IDQ.EQ.4) THEN
        IF (JB1.EQ.0) JB1 = JA1
        IF (JB2.EQ.0) JB2 = JA2
        IF (IBUG2.EQ.1) WRITE (IWRITE,3090) JA1,JB1,JA2,JB2
        CALL COR(JA1,JB1,JA2,JB2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRITE,!
     + JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2, J!
     +JQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
        RETURN
      ENDIF
C-----------------------------------------------------------------------
C
C   IDQ.EQ.2 - one orbital fixed each side. Include all possible
C              spectators.
C
C   IDQ.EQ.0 - for a matrix element off-diagonal in coupling.
C              Sum over all pairs of orbitals excluding core-core.
C
C-----------------------------------------------------------------------
      IF (IDQ.EQ.2) THEN
        KLAST = 1
      ELSE
        IF (IDQ.NE.0) GOTO 40
        IF (JA.EQ.JB) GOTO 20
        KLAST = NPEEL
      ENDIF
C-----------------------------------------------------------------------
C
C   Store the arrays JLIST,JJC1 and JJC2.
C
C-----------------------------------------------------------------------
      DO I = 1,NPEEL
        JLIS(I) = JLIST(I)
      ENDDO
      NPEELM = NPEEL-1
      DO I = 1,NPEELM
        JC1S(I) = JJC1(I)
        JC2S(I) = JJC2(I)
      ENDDO
C-----------------------------------------------------------------------
      DO KWA = 1,KLAST
        IF (IDQ.EQ.0) THEN
          JA1 = KWA
          JA2 = KWA
        ENDIF
C
        JT1 = JA1
        JT2 = JA2
        IT1 = JLIST(JA1)
        IT2 = JLIST(JA2)
C
        CALL RKCO2(KWA,JT1,JT2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRITE, J!
     +A, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2, JJQ!
     +1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
C
        IF (IDQ.EQ.0 .AND. NCORE.EQ.0) GOTO 10
        IF (NCORE.EQ.0 .OR. NAK(IT1).NE.NAK(IT2)) RETURN
C-----------------------------------------------------------------------
C
C   This section calculates the terms arising from active electrons
C   which are in closed shells.
C
C-----------------------------------------------------------------------
        CALL RKCO3(JT1,JT2,JLIS,JC1S,JC2S,IBUG1, IBUG2, IBUG3, IEXC, IME!
     +, IWRITE, JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1!
     +, JJC2, JJQ1, JJQ2, JLIST, KLIST, NCORE, NPEEL, NQ1, NQ2)
C-----------------------------------------------------------------------
   10   CONTINUE
      ENDDO
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
C
C   IDQ.EQ.0 - diagonal case. Include all pairs with JA1=JA2, JB1=JB2.
C
C-----------------------------------------------------------------------
   20 CONTINUE
      DO KW1 = 1,NPEEL
        K1 = JLIST(KW1)
        JB1 = KW1
        DO KW2 = 1,KW1
          JA1 = KW2
          IF (JA1.EQ.JB1 .AND. NQ1(K1).LE.1) GOTO 30
          IF (IBUG2.EQ.1) WRITE (IWRITE,3090) JA1,JB1,JA1,JB1
          CALL COR(JA1,JB1,JA1,JB1,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRIT!
     +E, JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2,!
     + JJQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
   30     CONTINUE
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
C
C   Diagnostic print.
C
C-----------------------------------------------------------------------
   40 CONTINUE
      WRITE (IWRITE,3080)
      STOP
C-----------------------------------------------------------------------
 3000 FORMAT (' RKCO called : NW NCF JA JB : ',4I4)
 3010 FORMAT (' RKCO called : ITJPO        : ',10I4)
 3020 FORMAT (' RKCO called : ISPAR        : ',10I4)
 3030 FORMAT (' RKCO called : IQ           : ',10I4)
 3040 FORMAT (' RKCO called : NPEEL,NCORE  : ',2I4)
 3050 FORMAT (' RKCO called : JLIST        : ',10I4)
 3060 FORMAT (' RKCO called : JJC1         : ',10I4)
 3070 FORMAT (' RKCO called : JJC2         : ',10I4)
 3080 FORMAT (' ERROR in RKCO')
 3090 FORMAT (//10X,'  JA1 =',I3,4X,'  JB1 =',I3,4X,'  JA2 =',I3,       !
     +'  JB2 =',I3/)
      END
CEND--------------------------------------------------------------------
CEND    RKCO1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RKCO1(IDQ,JA1,JB1,JA2,JB2,JLIST,NPEEL,NQ1,NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RKCO1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IDQ
C   JA1
C   JB1
C   JA2
C   JB2
C   JLIST
C   NPEEL
C   NQ1
C   NQ2
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IDQ,JA1,JA2,JB1
      INTEGER JB2,JLIST(*),NPEEL
      INTEGER NQ1(*),NQ2(*)
C
C  Local variables
C
      INTEGER J,JWW,NDQ
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IDQ = 0
C
      JA1 = 0
      JB1 = 0
      JA2 = 0
      JB2 = 0
C
      DO JWW = 1,NPEEL
C
        J = JLIST(JWW)
        NDQ = NQ1(J)-NQ2(J)
C
        IF (ABS(NDQ).GT.2) THEN
          IDQ = 99
          RETURN
        ENDIF
C
        IF (NDQ.EQ.0) GOTO 10
C
        IF (NDQ.GT.0) THEN
C
          IF (NDQ.EQ.1) THEN
C
            IF (JA1.GT.0) THEN
              JB1 = JWW
            ELSE
              JA1 = JWW
            ENDIF
C
            IDQ = IDQ+1
C
          ELSE
C
            JA1 = JWW
            IDQ = IDQ+2
C
          ENDIF
C
        ELSE
C
          IF (NDQ.EQ.-1) THEN
C
            IF (JA2.GT.0) THEN
              JB2 = JWW
            ELSE
              JA2 = JWW
            ENDIF
C
            IDQ = IDQ+1
C
          ELSE
C
            JA2 = JWW
            IDQ = IDQ+2
C
          ENDIF
C
        ENDIF
C
   10   CONTINUE
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    RKCO2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RKCO2(KWA,JT1,JT2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRIT!
     +E, JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2,!
     + JJQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RKCO2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IEXC
C   IME
C   IWRITE
C   JA
C   JB
C   NAK
C   NH
C   NOUTX
C   NP
C   NW
C   NWA
C   ITAB
C   JTAB
C   NTAB
C   JJC1
C   JJC2
C   JJQ1
C   JJQ2
C   JLIST
C   NPEEL
C   NQ1
C   NQ2
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER JT1
      INTEGER JT2
      INTEGER KWA
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IEXC
      INTEGER IME
      INTEGER ITAB(*)
      INTEGER IWRITE
      INTEGER JA
      INTEGER JB
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NOUTX
      INTEGER NP(*)
      INTEGER NPEEL
      INTEGER NQ1(*)
      INTEGER NQ2(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWA
C
C  Local variables
C
      INTEGER IB1,IB2,JA1,JA2
      INTEGER JB1,JB2,JT3,K1
      INTEGER KW
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO KW = KWA,NPEEL
C
        K1 = JLIST(KW)
        IF (NQ1(K1)*NQ2(K1).EQ.0) GOTO 10
C
        JA1 = JT1
        JA2 = JT2
        JB1 = KW
        JB2 = KW
C
C  Interchange JA1 and JB1 if necessary.
C
        IF (JA1.GT.JB1) THEN
          JT3 = JB1
          JB1 = JA1
          JA1 = JT3
        ELSE
          IF (JA1.EQ.JB1) THEN
            IB1 = JLIST(JB1)
            IF (NQ1(IB1).LE.1) GOTO 10
          ENDIF
        ENDIF
C
C  Interchange JA2 and JB2 if necessary
C
        IF (JA2.GT.JB2) THEN
          JT3 = JB2
          JB2 = JA2
          JA2 = JT3
        ELSE
          IF (JA2.EQ.JB2) THEN
            IB2 = JLIST(JB2)
            IF (NQ2(IB2).LE.1) GOTO 10
          ENDIF
        ENDIF
C
        IF (IBUG2.EQ.1) WRITE (IWRITE,3000) JA1,JB1,JA2,JB2
        CALL COR(JA1,JB1,JA2,JB2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRITE,!
     + JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2, J!
     +JQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
C
   10   CONTINUE
      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (//10X,'  JA1 =',I3,4X,'  JB1 =',I3,4X,'  JA2 =',I3,       !
     +'  JB2 =',I3)
      END
CEND--------------------------------------------------------------------
CEND    RKCO3.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RKCO3(JT1,JT2,JLIS,JC1S,JC2S,IBUG1, IBUG2, IBUG3, IEXC,!
     + IME, IWRITE, JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,!
     +JJC1, JJC2, JJQ1, JJQ2, JLIST, KLIST, NCORE, NPEEL, NQ1, NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RKCO3.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG1
C   IBUG2
C   IBUG3
C   IEXC
C   IME
C   IWRITE
C   JA
C   JB
C   NAK
C   NH
C   NOUTX
C   NP
C   NW
C   NWA
C   ITAB
C   JTAB
C   NTAB
C   JJC1
C   JJC2
C   JJQ1
C   JJQ2
C   JLIST
C   KLIST
C   NCORE
C   NPEEL
C   NQ1
C   NQ2
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER JC1S(*)
      INTEGER JC2S(*)
      INTEGER JLIS(*)
      INTEGER JT1
      INTEGER JT2
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IEXC
      INTEGER IME
      INTEGER ITAB(*)
      INTEGER IWRITE
      INTEGER JA
      INTEGER JB
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER JTAB(*)
      INTEGER KLIST(*)
      INTEGER NAK(*)
      INTEGER NCORE
      INTEGER NOUTX
      INTEGER NP(*)
      INTEGER NPEEL
      INTEGER NQ1(*)
      INTEGER NQ2(*)
      INTEGER NTAB(*)
      INTEGER NW
      INTEGER NWA
C
C  Local variables
C
      INTEGER I,I1,II,IJ
      INTEGER IJW,IM,JA1,JA2
      INTEGER JB1,JB2,JT3,KW
      INTEGER NPEELM
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO KW = 1,NCORE
C
        IJW = KLIST(KW)
        DO I = 1,NPEEL
          IJ = JLIST(I)
          IF (IJW.LT.IJ) GOTO 10
        ENDDO
        I = NPEEL+1
        GOTO 20
C
   10   CONTINUE
        IM = NPEEL-I+1
        DO II = 1,IM
          JLIST(NPEEL+2-II) = JLIST(NPEEL+1-II)
          IF (NPEEL.EQ.II) GOTO 20
          JJC1(NPEEL+1-II) = JJC1(NPEEL-II)
          JJC2(NPEEL+1-II) = JJC2(NPEEL-II)
        ENDDO
C
   20   CONTINUE
        IF (I.LT.3) THEN
          I1 = JLIST(1)
          JJC1(1) = JJQ1(3,I1)
          JJC2(1) = JJQ2(3,I1)
        ELSE
          JJC1(I-1) = JJC1(I-2)
          JJC2(I-1) = JJC2(I-2)
        ENDIF
C
        JLIST(I) = IJW
        JA1 = JT1
        IF (JT1.GE.I) JA1 = JA1 + 1
        JB1 = I
        JA2 = JT2
        IF (JT2.GE.I) JA2 = JA2 + 1
        JB2 = I
        IF (JA1.GT.JB1) THEN
          JT3 = JB1
          JB1 = JA1
          JA1 = JT3
        ENDIF
C
        IF (JA2.GT.JB2) THEN
          JT3 = JB2
          JB2 = JA2
          JA2 = JT3
        ENDIF
C
        NPEEL = NPEEL+1
        IF (IBUG2.EQ.1) THEN
          WRITE (IWRITE,3000) JA1,JB1,JA2,JB2,KW,KLIST(KW)
          WRITE (IWRITE,3010) (JLIST(I),I=1,NPEEL)
          NPEELM = NPEEL-1
          WRITE (IWRITE,3020) (JJC1(I),I=1,NPEELM)
          WRITE (IWRITE,3030) (JJC2(I),I=1,NPEELM)
        ENDIF
C
        CALL COR(JA1,JB1,JA2,JB2,IBUG1, IBUG2, IBUG3, IEXC, IME, IWRITE,!
     + JA, JB,NAK, NH, NOUTX, NP, NW, NWA,ITAB, JTAB, NTAB,JJC1, JJC2, J!
     +JQ1, JJQ2, JLIST, NPEEL, NQ1, NQ2)
        NPEEL = NPEEL-1
C
C  Reset the arrays JLIST, JJC1 and JJC2
C
        DO I = 1,NPEEL
          JLIST(I) = JLIS(I)
        ENDDO
        NPEELM = NPEEL-1
        DO I = 1,NPEELM
          JJC1(I) = JC1S(I)
          JJC2(I) = JC2S(I)
        ENDDO
C
      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (//10X,'  JA1 =',I3,4X,'  JB1 =',I3,4X,'  JA2 =',I3,       !
     +'  JB2 =',I3,'  K2 =',I3,'  KW =',I3)
 3010 FORMAT (' JLIST : ',15I4)
 3020 FORMAT (' JJC1  : ',15I4)
 3030 FORMAT (' JJC2  : ',15I4)
      END
CEND--------------------------------------------------------------------
CEND    SETJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SETJ(IS, JBQ1, JJC1, JJC2, JJQ1, JJQ2, JLIST, JS, JTQ1,!
     + JTQ2,KS, NS,J1, J2, J2S, J3, J3S, KJ23, MMOM, NMOM)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SETJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IS      ...
C   JBQ1    ...
C   JJC1    ...
C   JJC2    ...
C   JJQ1    ...
C   JJQ2    ...
C   JLIST   ...
C   JS      ...
C   JTQ1    ...
C   JTQ2    ...
C   KS      ...
C   NS      ...
C o J1      ... (J1(I),I=1,MMOM) is the angular momentum values stored
C               as 2J+1
C o J2      ... ((J2(I,J),I=1,(NMOM-1)),J=1,3) is the position in the
C               J1 array of the initial state triads
C o J2S     ... stored version of J2
C o J3      ... ((J3(I,J),I=1,(NMOM-1)),J=1,3) is the position in the
C               J1 array of the final state triads
C o J3S     ... stored version of J3
C o KJ23    ...
C o MMOM    ... the total number of angular momentum values in the
C               initial and final states
C o NMOM    ... the number of basic angular momentum values that
C               are coupled
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
C
C  Argument variables
C
      INTEGER IS(2,*)
      INTEGER J1(*)
      INTEGER J2(MTRIAD,*)
      INTEGER J2S(MTRIAD,*)
      INTEGER J3(MTRIAD,*)
      INTEGER J3S(MTRIAD,*)
      INTEGER JBQ1(3,*)
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER JS(2,*)
      INTEGER JTQ1(*)
      INTEGER JTQ2(*)
      INTEGER KJ23
      INTEGER KS(2,*)
      INTEGER MMOM
      INTEGER NMOM
      INTEGER NS
C
C  Local variables
C
      INTEGER I,II,IJ,IJ1
      INTEGER IJ2,J,NS1,NS2
      INTEGER NS3
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C    1.0  Set J1 array
C
C-----------------------------------------------------------------------
      II = 0
      DO IJ = 1,NS
        I = JLIST(IJ)
        II = II+1
        J1(II) = JBQ1(3,I)
      ENDDO
      IF (NS.EQ.1) GOTO 10
      NS1 = NS-1
      DO I = 1,NS1
        II = II+1
        J1(II) = JJC1(I)
      ENDDO
      DO I = 1,NS1
        II = II+1
        J1(II) = JJC2(I)
      ENDDO
   10 CONTINUE
      DO I = 1,2
        II = II+1
        IJ = IS(I,1)
        J1(II) = JJQ1(3,IJ)
        IF (I.EQ.1 .AND. IS(1,1).EQ.IS(2,1)) J1(II) = JTQ1(3)
        J1(II+4) = KS(I,1)
      ENDDO
      DO I = 1,2
        II = II+1
        IJ = IS(I,2)
        J1(II) = JJQ2(3,IJ)
        IF (I.EQ.1 .AND. IS(1,2).EQ.IS(2,2)) J1(II) = JTQ2(3)
        J1(II+4) = KS(I,2)
      ENDDO
C-----------------------------------------------------------------------
C
C    2.0  Set J2,J3 arrays if not already available
C
C-----------------------------------------------------------------------
      NS2 = MAX(4,NS+2)
      IF (KJ23.GT.0) GOTO 60
C
      DO I = 4,NS2
        J2(I,1) = NS+I-4
        J2(I,2) = I-2
        J2(I,3) = NS+I-3
        J3(I,1) = J2(I,1)+NS-1
        J3(I,2) = I-2
        J3(I,3) = J2(I,3)+NS-1
      ENDDO
      J2(4,1) = 1
      J3(4,1) = 1
C-----------------------------------------------------------------------
C
C   At this stage, the entries in rows corresponding to active shells
C   are set incorrectly.
C
C    3.0  Set rows 1 thro' 3
C
C-----------------------------------------------------------------------
      NS3 = 3*NS
      J2(1,1) = NS3+5
      J2(1,2) = NS3+7
      J2(1,3) = NS3+3
      J2(2,1) = JS(1,1)
      J2(2,2) = NS3+3
      J2(2,3) = NS3-1
      J2(3,1) = JS(2,1)
      J2(3,2) = NS3+4
      J2(3,3) = NS3
C
      J3(1,1) = NS3+7
      J3(1,2) = NS3+4
      J3(1,3) = NS3+6
      J3(2,1) = JS(1,2)
      J3(2,2) = NS3+5
      J3(2,3) = NS3+1
      J3(3,1) = JS(2,2)
      J3(3,2) = NS3+6
      J3(3,3) = NS3+2
C-----------------------------------------------------------------------
C
C    4.0  Set remaining resultants
C
C-----------------------------------------------------------------------
      IJ1 = JS(1,1)
      IJ2 = JS(2,1)
      IF (IJ2.GT.1) J2(IJ2+2,2) = J2(3,3)
      IF (IJ2.EQ.1) J2(4,1) = J2(3,3)
      IF (IJ1.NE.IJ2) GOTO 20
      J2(3,1) = J2(2,3)
      GOTO 30
C
   20 CONTINUE
      IF (IJ1.GT.1) J2(IJ1+2,2) = J2(2,3)
      IF (IJ1.EQ.1) J2(4,1) = J2(2,3)
C
   30 CONTINUE
      IJ1 = JS(1,2)
      IJ2 = JS(2,2)
      IF (IJ2.GT.1) J3(IJ2+2,2) = J3(3,3)
      IF (IJ2.EQ.1) J3(4,1) = J3(3,3)
      IF (IJ1.NE.IJ2) GOTO 40
      J3(3,1) = J3(2,3)
      GOTO 50
C
   40 CONTINUE
      IF (IJ1.GT.1) J3(IJ1+2,2) = J3(2,3)
      IF (IJ1.EQ.1) J3(4,1) = J3(2,3)
C-----------------------------------------------------------------------
C
C    All arrays now set. Put up flag KJ23.
C
C-----------------------------------------------------------------------
   50 CONTINUE
      KJ23 = 1
      MMOM = NS3+7
      NMOM = NS+3
C-----------------------------------------------------------------------
C
C    5.0  Save J2,J3 and return
C
C-----------------------------------------------------------------------
      DO J = 1,3
        DO I = 1,NS2
          J2S(I,J) = J2(I,J)
          J3S(I,J) = J3(I,J)
        ENDDO
      ENDDO
      RETURN
C-----------------------------------------------------------------------
C
C    6.0  Reset J2,J3 from buffers if KJ23 has been set
C
C-----------------------------------------------------------------------
   60 CONTINUE
      DO J = 1,3
        DO I = 1,NS2
          J2(I,J) = J2S(I,J)
          J3(I,J) = J3S(I,J)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      END
CEND--------------------------------------------------------------------
CEND    SETUP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SETUP(IBUG2,IWRITE,JA,JB,ICHOP, IQ, JCUP, JQS, NCF, NW,!
     +JJC1, JJC2, JJQ1, JJQ2, JLIST, KLIST, NCORE, NPEEL, NQ1, NQ2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SETUP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IBUG2
C   IWRITE
C   JA
C   JB
C   ICHOP
C   IQ
C   JCUP
C   JQS
C   NCF
C   NW
C o JJC1
C o JJC2
C o JJQ1
C o JJQ2
C o JLIST
C o KLIST
C o NCORE
C o NPEEL
C o NQ1
C o NQ2
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      INTEGER NPLX
      PARAMETER (NPLX=14)
C
C  Argument variables
C
      INTEGER IBUG2,IWRITE,JA,JB
C
      INTEGER ICHOP(MXNW,*)
      INTEGER IQ(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER JQS(3,MXNW,*)
      INTEGER KLIST(*)
      INTEGER NCF
      INTEGER NCORE
      INTEGER NPEEL
      INTEGER NQ1(*)
      INTEGER NQ2(*)
      INTEGER NW
C
C  Local variables
C
      INTEGER I,J,JCNT,JCNTOP
      INTEGER JW1,JW2,JWW,K
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IBUG2.EQ.1) THEN
        DO J = 1,NCF
          WRITE (IWRITE,3000) (JCUP(I,J),I=1,10)
        ENDDO
        DO J = 1,NCF
          WRITE (IWRITE,3010) (ICHOP(I,J),I=1,NW)
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C  List parameters defining all shells in both CSFs
C  whether participating or not.
C
C-----------------------------------------------------------------------
      DO J = 1,NW
        NQ1(J) = IQ(J,JA)
        NQ2(J) = IQ(J,JB)
      ENDDO
C-----------------------------------------------------------------------
      DO K = 1,3
        DO J = 1,NW
          JJQ1(K,J) = JQS(K,J,JA)
          JJQ2(K,J) = JQS(K,J,JB)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C
C  Define coupling schemes
C
C  Set JLIST array to define those shells which are open in
C  either CSF, and KLIST array to locate the rest.
C  Exclude shells which are empty in both CSFs.
C
C-----------------------------------------------------------------------
      NPEEL = 0
      NCORE = 0
      DO J = 1,NW
C
        IF (ICHOP(J,JA).NE.-1 .OR. ICHOP(J,JB).NE.-1) THEN
          IF (ICHOP(J,JA).EQ.1 .AND. ICHOP(J,JB).EQ.1) THEN
            NCORE = NCORE+1
            KLIST(NCORE) = J
          ELSE
            NPEEL = NPEEL+1
C
C  Note that in some cases an extra peel orbital is added to
C  the true peel orbitals. Therefore the dimension check is
C  against NPLX-1.
C
            IF (NPEEL.GT.NPLX-1) THEN
              WRITE (IWRITE,3030) JA,JB,NPLX
              STOP
            ENDIF
            JLIST(NPEEL) = J
          ENDIF
        ENDIF
C
      ENDDO
C-----------------------------------------------------------------------
C
C  Return if not more than one shell is open
C
C-----------------------------------------------------------------------
      IF (NPEEL.LE.1) RETURN
C-----------------------------------------------------------------------
C
C  Set arrays of coupling angular momenta interpolating
C  closed shells where necessary.
C
C  Left hand side first ...
C
C-----------------------------------------------------------------------
      JCNT = 1
      JCNTOP = 0
      JW1 = JLIST(1)
      JW2 = JLIST(2)
      IF (ICHOP(JW1,JA).EQ.0) THEN
        JCNTOP = 1
        IF (ICHOP(JW2,JA).EQ.0) THEN
          JJC1(1) = JCUP(JCNT,JA)
          JCNT = JCNT+1
        ELSE
          JJC1(1) = JQS(3,JW1,JA)
        ENDIF
      ELSE
        JJC1(1) = JQS(3,JW2,JA)
        IF (ICHOP(JW2,JA).EQ.0) JCNTOP = 1
      ENDIF
C-----------------------------------------------------------------------
      IF (NPEEL.GE.3) THEN
        DO J = 3,NPEEL
          JWW = JLIST(J)
          IF (ICHOP(JWW,JA).EQ.0) THEN
            IF (JCNTOP.EQ.0) THEN
              JJC1(J-1) = JQS(3,JWW,JA)
            ELSE
              JJC1(J-1) = JCUP(JCNT,JA)
              JCNT = JCNT+1
            ENDIF
            JCNTOP = JCNTOP+1
          ELSE
            JJC1(J-1) = JJC1(J-2)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IF (IBUG2.EQ.1) THEN
        WRITE (IWRITE,3020) JCNT,JCNTOP
        IF (JCNT.GT.1) WRITE (IWRITE,3000) (JCUP(J,JA),J=1,JCNT-1)
      ENDIF
C-----------------------------------------------------------------------
C
C  ... and repeat for right hand side
C
C-----------------------------------------------------------------------
      JCNT = 1
      JCNTOP = 0
      JW1 = JLIST(1)
      JW2 = JLIST(2)
      IF (ICHOP(JW1,JB).EQ.0) THEN
        JCNTOP = 1
        IF (ICHOP(JW2,JB).EQ.0) THEN
          JJC2(1) = JCUP(JCNT,JB)
          JCNT = JCNT+1
        ELSE
          JJC2(1) = JQS(3,JW1,JB)
        ENDIF
      ELSE
        JJC2(1) = JQS(3,JW2,JB)
        IF (ICHOP(JW2,JB).EQ.0) JCNTOP = 1
      ENDIF
C-----------------------------------------------------------------------
      IF (NPEEL.GE.3) THEN
        DO J = 3,NPEEL
          JWW = JLIST(J)
          IF (ICHOP(JWW,JB).EQ.0) THEN
            IF (JCNTOP.EQ.0) THEN
              JJC2(J-1) = JQS(3,JWW,JB)
            ELSE
              JJC2(J-1) = JCUP(JCNT,JB)
              JCNT = JCNT+1
            ENDIF
            JCNTOP = JCNTOP+1
          ELSE
            JJC2(J-1) = JJC2(J-2)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      IF (IBUG2.EQ.1) THEN
        WRITE (IWRITE,3020) JCNT,JCNTOP
        IF (JCNT.GT.1) WRITE (IWRITE,3000) (JCUP(J,JB),J=1,JCNT-1)
      ENDIF
C-----------------------------------------------------------------------
 3000 FORMAT (' SETUP called JCUP        : ',10I4)
 3010 FORMAT (' SETUP called ICHOP       : ',10I4)
 3020 FORMAT (' SETUP called JCNT JCNTOP : ',2I4)
 3030 FORMAT (/' ERROR in routine SETUP'/                               !
     +' Too many peel shells for CSFs : ',2I4/                          !
     +' The present version is limited to NPLX =',I4/                   !
     +' You must change the parameter NPLX.')
      END
CEND--------------------------------------------------------------------
CEND    SKRC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SKRC(IS,KAPS,KS,KD1,KD2,KE1,KE2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SKRC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IS     ...
C     KAPS   ...
C     KS     ...
C     KD1    ... minimum K (direct)
C     KD2    ... number of terms (direct)
C     KE1    ... minimum K (exchange)
C     KE2    ... number of terms (exchange)
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IS(4),KAPS(4),KD1,KD2
      INTEGER KE1,KE2,KS(4)
C
C  Local variables
C
      INTEGER ISD1,ISD2,ISE1,ISE2
      INTEGER KD1A,KD1B,KD2A,KD2B
      INTEGER KE1A,KE1B,KE2A,KE2B
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      KD1 = -1
      KD2 = 0
      KE1 = -1
      KE2 = 0
C
C  Direct terms -  KD1=minimum K , KD2=number of terms
C
      ISD1 = 1
      IF (KAPS(1)*KAPS(3).LT.0) ISD1 = -1
      ISD2 = 1
      IF (KAPS(2)*KAPS(4).LT.0) ISD2 = -1
      KD1A = ABS(KS(1)-KS(3))
      IF (ISD1.LT.0) KD1A = KD1A + 2
      KD1B = ABS(KS(2)-KS(4))
      IF (ISD2.LT.0) KD1B = KD1B + 2
      IF (MOD((KD1A-KD1B)/2,2).NE.0) GOTO 10
      KD2A = KS(1)+KS(3)-2
      IF (ISD1.GT.0) KD2A = KD2A - 2
      KD2B = KS(2)+KS(4)-2
      IF (ISD2.GT.0) KD2B = KD2B - 2
      KD1 = MAX(KD1A,KD1B)/2
      KD2 = MIN(KD2A,KD2B)/2
      KD2 = (KD2-KD1)/2+1
C
C  Exchange terms -  KE1=minimum K , KE2=number of terms
C
   10 CONTINUE
      IF (IS(1).EQ.IS(2) .OR. IS(3).EQ.IS(4)) RETURN
      ISE1 = 1
      IF (KAPS(1)*KAPS(4).LT.0) ISE1 = -1
      ISE2 = 1
      IF (KAPS(2)*KAPS(3).LT.0) ISE2 = -1
      KE1A = ABS(KS(1)-KS(4))
      IF (ISE1.LT.0) KE1A = KE1A + 2
      KE1B = ABS(KS(2)-KS(3))
      IF (ISE2.LT.0) KE1B = KE1B + 2
      IF (MOD((KE1A-KE1B)/2,2).NE.0) RETURN
      KE2A = KS(1)+KS(4)-2
      IF (ISE1.GT.0) KE2A = KE2A - 2
      KE2B = KS(2)+KS(3)-2
      IF (ISE2.GT.0) KE2B = KE2B - 2
      KE1 = MAX(KE1A,KE1B)/2
      KE2 = MIN(KE2A,KE2B)/2
      KE2 = (KE2-KE1)/2+1
C
      END
CEND--------------------------------------------------------------------
CEND    SMCP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SMCP(NOUT,ISMCP,IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG!
     +6,ICHOP, IEXC, IQ, ISPAR, ITJPO, IWRITE, JCUP, JQS,NAK, NCF, NH, N!
     +P, NW,ITAB, JTAB, NROWS, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SMCP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NOUT   ... stream number used to write coefficients to a file
C   ISMCP  ... =0 only one-electron integrals are calculated,
C              =1 all integrals are calculated
C   IBUG1
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG5
C   IBUG6
C   ICHOP
C   IEXC
C   IQ
C   ISPAR
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   NAK
C   NCF
C   NH
C   NP
C   NW
C   ITAB
C   JTAB
C   NROWS
C   NTAB
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER IZ
      PARAMETER (IZ=0)
C
C  Argument variables
C
      INTEGER ISMCP,NOUT
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG5
      INTEGER IBUG6
      INTEGER ICHOP(MXNW,*)
      INTEGER IEXC
      INTEGER IQ(MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITAB(*)
      INTEGER ITJPO(*)
      INTEGER IWRITE
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER JTAB(*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NP(*)
      INTEGER NROWS
      INTEGER NTAB(*)
      INTEGER NW
C
C  Local variables
C
      DOUBLE PRECISION COEF
      DOUBLE PRECISION VSHELL(MXNW)
      INTEGER IA1,IA2,IMF,IMT
      INTEGER IMV,IMX,IOPAR,KA
      INTEGER IME,JA,JB,NOUTX
      INTEGER NWA
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      NOUTX = NOUT
      NWA = NW+1
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
C-----------------------------------------------------------------------
      IF (IEXC.EQ.1) THEN
        WRITE (IWRITE,3040)
      ELSE
        IF (IEXC.EQ.-1) THEN
          WRITE (IWRITE,3050)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C  Print out table of allowed levels in jj-coupling if IBUG5 is set.
C
C-----------------------------------------------------------------------
      IF (IBUG5.EQ.1) CALL TMSOUT(IWRITE, ITAB, JTAB, NROWS, NTAB)
      IBUG5 = 0
C-----------------------------------------------------------------------
      KA = 0
      IOPAR = 1
      IF (IBUG1.EQ.1) WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
      IMV = 0
      IMT = 0
C-----------------------------------------------------------------------
C
C   JA and JB refer to the initial and final states in the list of
C   NCF CSFs, respectively
C
C-----------------------------------------------------------------------
      DO JA = 1,NCF
        DO JB = JA,NCF
C
          IF (NOUT.GT.0) WRITE (NOUT) JA,JB
C-----------------------------------------------------------------------
C
C   two-electron coefficients
C   call MCP package
C
C-----------------------------------------------------------------------
          IME = 0
          IF (ISMCP.EQ.1) CALL RKCO(JA,JB,NH, IBUG1, IBUG2, IBUG3, ICHOP!
     +, IEXC, IQ, IME, ISPAR, ITJPO,IWRITE, JCUP, JQS, NAK, NCF, NOUTX, !
     +NP, NW, NWA,ITAB, JTAB, NTAB)
C-----------------------------------------------------------------------
C
C   one-electron coefficients
C   call MCT package and output results
C   off-diagonal case only
C
C-----------------------------------------------------------------------
          IMF = 0
C
          IF (JA.NE.JB) THEN
            CALL TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG!
     +4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,!
     +ITAB, JTAB, NTAB)
            IF (IA1.GT.0) THEN
              IF (IA1.EQ.IA2) THEN
                DO IA1 = 1,NW
                  COEF = VSHELL(IA1)
                  IF (ABS(COEF).GE.EPS10) THEN
                    CALL SPEAK(7,IZ,IZ,IA1,IA1,IZ,COEF,IBUG1, IME, IWRIT!
     +E, JA, JB, NOUTX, NH, NP, NWA)
                    IMF = IMF+1
                  ENDIF
                ENDDO
              ELSE
                COEF = VSHELL(1)
                IF (ABS(COEF).GE.EPS10) THEN
                  CALL SPEAK(7,IZ,IZ,IA1,IA2,IZ,COEF,IBUG1, IME, IWRITE,!
     + JA, JB, NOUTX, NH, NP, NWA)
                  IMF = IMF+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
          IMX = IME+IMF
          IMT = IMT+IMF
          IMV = IMV+IME
          IF (IMX.GT.0 .AND. IBUG1.EQ.1) WRITE (IWRITE,3030) IMX,JA,JB
C
          IF (NOUT.GT.0) WRITE (NOUT) IZ,IZ,IZ,ZERO
C
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      WRITE (IWRITE,3020) IMV,IMT
C-----------------------------------------------------------------------
 3000 FORMAT (/                                                         !
     +' ===== SMCP has been entered. This routine will calculate'/      !
     +' ===== the angular coefficients required for the Dirac Hamilton',!
     +'ian.')
 3010 FORMAT (/5X,'r',4X,'s',4X,'a',4X,'b',4X,'c',4X,'d',4X,'k',4X,     !
     +'coefficient'/)
 3020 FORMAT (/1X,I9,' V-coefficients calculated'/1X,I9,                !
     +' T-coefficients calculated')
 3030 FORMAT (1X,I9,' coefficients between ',I5,' and ',I5)
 3040 FORMAT (/' exchange coefficients neglected')
 3050 FORMAT (/' direct coefficients neglected')
      END
CEND--------------------------------------------------------------------
CEND    SNRC.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SNRC(IS,KAPS,KS,ID1,ID2,NE1,NE2,IBRD,IBRE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SNRC.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER IBRD,IBRE,ID1,ID2
      INTEGER IS(4),KAPS(4),KS(4),NE1
      INTEGER NE2
C
C  Local variables
C
      INTEGER IAC,IAD,ID1A,ID2A
      INTEGER NE1A,NE2A
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      ID2 = 0
      NE2 = 0
C
      IAC = 1
      IF ((KAPS(1)*KAPS(3)).LT.0) IAC = -1
C
      IAD = 1
      IF ((KAPS(2)*KAPS(4)).LT.0) IAD = -1
C
      ID1 = ABS(KS(1)-KS(3))/2-1
      IF (IAC.EQ.-1) ID1 = ID1 + 1
      IF (ID1.EQ.-1) ID1 = 1
      ID1A = ABS(KS(2)-KS(4))/2-1
      IF (IAD.EQ.-1) ID1A = ID1A + 1
      IF (ID1A.EQ.-1) ID1A = 1
C
      IF (MOD(ID1-ID1A,2).EQ.0) GOTO 10
C
      IBRD = -1
      GOTO 20
C
   10 CONTINUE
      ID2 = ABS(KS(1)+KS(3))/2
      IF (IAC.EQ.-1) ID2 = ID2 + 1
      ID2A = ABS(KS(2)+KS(4))/2
      IF (IAD.EQ.-1) ID2A = ID2A + 1
      ID1 = MAX(ID1,ID1A)
      ID2 = MIN(ID2,ID2A)
      ID2 = (ID2-ID1)/2+1
      IBRD = 1
      IF (IS(1).EQ.IS(3) .AND. IS(2).NE.IS(4)) IBRD = 2
      IF (IS(1).NE.IS(3) .AND. IS(2).EQ.IS(4)) IBRD = 2
      IF (IS(1).EQ.IS(3) .AND. IS(2).EQ.IS(4)) IBRD = 3
C
   20 CONTINUE
      IF (IS(1).NE.IS(2) .AND. IS(3).NE.IS(4)) GOTO 30
      IBRE = -1
      RETURN
C
   30 CONTINUE
      IAC = 1
      IF ((KAPS(1)*KAPS(4)).LT.0) IAC = -1
      IAD = 1
      IF ((KAPS(2)*KAPS(3)).LT.0) IAD = -1
C
      NE1 = ABS(KS(1)-KS(4))/2-1
      IF (IAC.EQ.-1) NE1 = NE1 + 1
      IF (NE1.EQ.-1) NE1 = 1
      NE1A = ABS(KS(2)-KS(3))/2-1
      IF (IAD.EQ.-1) NE1A = NE1A + 1
      IF (NE1A.EQ.-1) NE1A = 1
C
      IF (MOD(NE1-NE1A,2).EQ.0) GOTO 40
C
      IBRE = -1
      RETURN
C
   40 CONTINUE
      NE2 = ABS(KS(1)+KS(4))/2
      IF (IAC.EQ.-1) NE2 = NE2 + 1
      NE2A = ABS(KS(2)+KS(3))/2
      IF (IAD.EQ.-1) NE2A = NE2A + 1
      NE1 = MAX(NE1,NE1A)
      NE2 = MIN(NE2,NE2A)
      NE2 = (NE2-NE1)/2+1
      IBRE = 1
      IF (IS(1).EQ.IS(4) .AND. IS(2).NE.IS(3)) IBRE = 2
      IF (IS(1).NE.IS(4) .AND. IS(2).EQ.IS(3)) IBRE = 2
      IF (IS(1).EQ.IS(3) .AND. IS(2).EQ.IS(4)) IBRE = 4
C
      END
CEND--------------------------------------------------------------------
CEND    SPEAK.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SPEAK(ITYPE,IA,IB,IC,ID,K,COEF,IBUG1, IME, IWRITE, JA, !
     +JB, NOUTX, NH, NP, NWA)
CRCS
CRCS $Source: /home/phn/DARC/RCS/SPEAK.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C  No routines called.
C
C   ITYPE
C   IA
C   IB
C   IC
C   ID
C   K
C   COEF
C   IBUG1
C   IME
C   IWRITE
C   JA
C   JB
C   NOUTX
C   NH
C   NP
C   NWA
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      DOUBLE PRECISION COEF
      INTEGER IA,IB,IC,ID
      INTEGER ITYPE,K
C
      CHARACTER*2 NH(*)
      INTEGER IBUG1
      INTEGER IME
      INTEGER IWRITE
      INTEGER JA
      INTEGER JB
      INTEGER NOUTX
      INTEGER NP(*)
      INTEGER NWA
C
C  Local variables
C
      INTEGER ISTOR1,ISTOR2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (ITYPE.EQ.7) THEN
        ISTOR1 = 0
        ISTOR2 = IC*NWA+ID
        IF (IBUG1.EQ.1) WRITE (IWRITE,3010) JA,JB,NP(IC),NH(IC),NP(ID),N!
     +H(ID),COEF,ISTOR1,ISTOR2,ITYPE
C
      ELSE
        ISTOR1 = (K*NWA+IA)*NWA+IB
        ISTOR2 = IC*NWA+ID
        IF (IBUG1.EQ.1) WRITE (IWRITE,3000) JA,JB,NP(IA),NH(IA),NP(IB),N!
     +H(IB),NP(IC),NH(IC),NP(ID),NH(ID),K,COEF,ISTOR1,ISTOR2,ITYPE
        IME = IME+1
      ENDIF
C
      IF (NOUTX.GT.0) WRITE (NOUTX) ISTOR1,ISTOR2,ITYPE,COEF
C-----------------------------------------------------------------------
 3000 FORMAT (1X,2I5,2X,4(I2,A2,1X),1X,I2,2X,1P,E16.9,2X,2I6,2X,I1)
 3010 FORMAT (1X,2I5,2X,2(1X,'**',2X),2(I2,A2,1X),5X,1P,E16.9,2X,2I6,2X,!
     +I1)
      END
CEND--------------------------------------------------------------------
CEND    TMSOUT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE TMSOUT(IWRITE, ITAB, JTAB, NROWS, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/TMSOUT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C     IWRITE
C     ITAB
C     JTAB
C     NROWS
C     NTAB
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Local variables
C
      INTEGER I,II,JI,JJ
      INTEGER K,KK,KKK
C
C  Argument variables
C
      INTEGER IWRITE
      INTEGER ITAB(*)
      INTEGER JTAB(*)
      INTEGER NROWS
      INTEGER NTAB(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
      WRITE (IWRITE,3010) NROWS, (ITAB(I),I=1,NROWS)
      WRITE (IWRITE,3020)
      DO I = 1,NROWS
        JI = JTAB(I)
        JJ = 3*ITAB(I)
        KK = JI+1
        KKK = JJ+JI
        II = I-1
        WRITE (IWRITE,3030) II, (NTAB(K),K=KK,KKK)
      ENDDO
C-----------------------------------------------------------------------
 3000 FORMAT (/40X,'Table of possible terms'//)
 3010 FORMAT (1X,I3,' rows of this table (NTAB) are defined.'/          !
     +' The lengths (ITAB) of the rows are, respectively,'/(1X,12I5))
 3020 FORMAT (/' List of NTAB. '/                                       !
     +' Each triad of numbers corresponds to (V,W,2J+1),'/              !
     +' where V is seniority,'/                                         !
     +'   and W distinguishes states with the same values of V,J.'/)
 3030 FORMAT (' Row ',I2,' ,',4(I8,2I3)/(1X,I16,2I3,3(I8,2I3)))
      END
CEND--------------------------------------------------------------------
CEND    TNSRJJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE TNSRJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL,IBUG2, IBUG3, IBUG!
     +4, IBUG6, ICHOP, IQ, ISPAR, ITJPO, IWRITE,JCUP, JQS, NAK, NCF, NW,!
     +ITAB, JTAB, NTAB)
CRCS
CRCS $Source: /home/phn/DARC/RCS/TNSRJJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   KA
C   IOPAR
C   JA
C   JB
C   IA1
C   IA2
C   VSHELL
C   IBUG2
C   IBUG3
C   IBUG4
C   IBUG6
C   ICHOP
C   IQ
C   ISPAR
C   ITJPO
C   IWRITE
C   JCUP
C   JQS
C   NAK
C   NCF
C   NW
C   ITAB
C   JTAB
C   NTAB
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  External functions
C
      EXTERNAL IROW1
      INTEGER IROW1
C
C  Parameter variables
C
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M3MNGM
      PARAMETER (M3MNGM=3*MANGM)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER NPLX
      PARAMETER (NPLX=14)
      INTEGER MANGMP
      PARAMETER (MANGMP=2*(MANGM/3))
      INTEGER MSUM
      PARAMETER (MSUM=10)
C
C  Argument variables
C
      INTEGER IA1,IA2,IOPAR,JA
      INTEGER JB,KA
      DOUBLE PRECISION VSHELL(*)
C
      INTEGER IBUG2
      INTEGER IBUG3
      INTEGER IBUG4
      INTEGER IBUG6
      INTEGER IWRITE
      INTEGER IQ(MXNW,*)
      INTEGER NAK(*)
      INTEGER NCF
      INTEGER NW
      INTEGER ICHOP(MXNW,*)
      INTEGER JCUP(10,*)
      INTEGER JQS(3,MXNW,*)
      INTEGER ISPAR(*)
      INTEGER ITJPO(*)
      INTEGER ITAB(*)
      INTEGER JTAB(*)
      INTEGER NTAB(*)
C
C  Local variables
C
      DOUBLE PRECISION C,RECUPS,VAL
      INTEGER I,I0,IDL,IDQ
      INTEGER II,IJ,IJD,IJP
      INTEGER IM,IS(2),ISH,IT1
      INTEGER IT2,IT3,IVD,IVP
      INTEGER IWD,IWP,IX,JA1
      INTEGER JA2,JC1S(NPLX),JC2S(NPLX)
      INTEGER JLIS(NPLX),JWW,K,KJ23
      INTEGER KK,KS(2),KS1,KS2
      INTEGER L1,L2,LA,LB
      INTEGER LLD1,LLD2,LLS1,LLS2
      INTEGER LS1,LS2,NDQ,NEL
      INTEGER NELCTS,NPEELM,NS,NX
      LOGICAL FAIL
      INTEGER JBQ1(3,MXNW)
      INTEGER JBQ2(3,MXNW)
      INTEGER JJC1(NPLX)
      INTEGER JJC2(NPLX)
      INTEGER NQ1(MXNW)
      INTEGER NQ2(MXNW)
      INTEGER JJQ1(3,MXNW)
      INTEGER JJQ2(3,MXNW)
      INTEGER JLIST(NPLX)
      INTEGER KLIST(MXNW)
      INTEGER NCORE
      INTEGER NPEEL
C
      INTEGER J1(MANGM)
      INTEGER J2(MTRIAD,3)
      INTEGER J3(MTRIAD,3)
      INTEGER MMOM
      INTEGER NMOM
      LOGICAL FREE(MANGM)
C
      INTEGER J6(M3MNGM)
      INTEGER J6C
      INTEGER J7(M3MNGM)
      INTEGER J7C
      INTEGER J8(M3MNGM)
      INTEGER J8C
      INTEGER J9(MANGMP)
      INTEGER J9C
      INTEGER JDEL
      INTEGER JW(6,M6J)
      INTEGER JWC
      INTEGER LDEL(M6J,2)
      INTEGER MP
      INTEGER INV6J(M6J)
      INTEGER J6P(MANGMP)
      INTEGER J7P(MANGMP)
      INTEGER J8P(MANGMP)
      INTEGER J9P(MANGMP)
      INTEGER JSUM4(MTRIAD,M6J)
      INTEGER JSUM5(MTRIAD,M6J)
      INTEGER JSUM6(MTRIAD)
      INTEGER JWORD(6,M6J)
      INTEGER K6CP(MSUM)
      INTEGER K7CP(MSUM)
      INTEGER K8CP(MSUM)
      INTEGER K9CP(MSUM)
      INTEGER NB6J(MSUM)
      INTEGER NBJ(MSUM)
      INTEGER NLSUM
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER I1,I2,I3
      LOGICAL ITRIG
      ITRIG(I1,I2,I3) = I1.GE.(ABS(I2-I3)+1).AND.I1.LE.(I2+I3-1)
C-----------------------------------------------------------------------
      IA1 = 0
      KK = KA+KA+1
      IF (.NOT.ITRIG(ITJPO(JA),ITJPO(JB),KK)) RETURN
      IF (IOPAR.NE.0 .AND. ISPAR(JA)*ISPAR(JB)*IOPAR.NE.1) RETURN
C
      CALL SETUP(IBUG2,IWRITE,JA,JB,ICHOP, IQ, JCUP, JQS, NCF, NW,JJC1, !
     +JJC2, JJQ1, JJQ2, JLIST, KLIST, NCORE, NPEEL, NQ1, NQ2)
      IF (IBUG4.EQ.1) CALL VIJOUT(IWRITE,JA,JB,JJC1, JJC2, JJQ1, JJQ2, J!
     +LIST, NCORE, NQ1, NQ2, NPEEL)
C
      DO IJ = 1,NW
        VSHELL(IJ) = ZERO
      ENDDO
C-----------------------------------------------------------------------
C
C  Analyse peel shell interactions
C
      IDQ = 0
      JA1 = 0
      JA2 = 0
C
      IF (NPEEL.EQ.0) GOTO 50
C
      DO JWW = 1,NPEEL
        IJ = JLIST(JWW)
        NDQ = NQ1(IJ)-NQ2(IJ)
        IF (ABS(NDQ).GT.1) GOTO 240
        IF (NDQ) 20,40,10
   10   CONTINUE
        JA1 = JWW
        GOTO 30
C
   20   CONTINUE
        JA2 = JWW
   30   CONTINUE
        IDQ = IDQ+1
   40   CONTINUE
      ENDDO
C
      IF (IDQ.GT.2) GOTO 240
C-----------------------------------------------------------------------
C
C  Evaluate the array VSHELL
C
C  There are two possibilities :
C
C  If IDQ=0, then loop over all shells by index ISH
C  If IDQ=2, then one orbital fixed on each side
C
C-----------------------------------------------------------------------
      NS = NPEEL
      IF (IDQ.EQ.2) GOTO 130
C-----------------------------------------------------------------------
C
C  loop over shells when IDQ=0
C
C-----------------------------------------------------------------------
   50 CONTINUE
      ISH = 0
      IF (NPEEL.EQ.0) GOTO 60
      DO I = 1,NPEEL
        JLIS(I) = JLIST(I)
      ENDDO
      IF (NPEEL.EQ.1) GOTO 60
      NPEELM = NPEEL-1
      DO I = 1,NPEELM
        JC1S(I) = JJC1(I)
        JC2S(I) = JJC2(I)
      ENDDO
C-----------------------------------------------------------------------
C
C  If ISH > NW, then loop is over and return
C
C-----------------------------------------------------------------------
   60 CONTINUE
      ISH = ISH+1
      IF (ISH.GT.NW) RETURN
      IF (ICHOP(ISH,JA).EQ.-1) GOTO 60
      IF (IBUG6.EQ.1) WRITE (IWRITE,3040) ISH
      IF (ICHOP(ISH,JA).EQ.0) GOTO 110
C-----------------------------------------------------------------------
C
C  Case one
C  The ISHth shell is in the core or in the peel and closed for both
C  sides
C
C-----------------------------------------------------------------------
      I = 1
      IF (NPEEL.EQ.0) GOTO 100
      DO I = 1,NPEEL
        IJ = JLIST(I)
        IF (ISH.LT.IJ) GOTO 70
      ENDDO
      I = NPEEL+1
      GOTO 80
C
   70 CONTINUE
      IM = NPEEL-I+1
      DO II = 1,IM
        JLIST(NPEEL+2-II) = JLIST(NPEEL+1-II)
        IF (NPEEL.EQ.II) GOTO 80
        JJC1(NPEEL+1-II) = JJC1(NPEEL-II)
        JJC2(NPEEL+1-II) = JJC2(NPEEL-II)
      ENDDO
   80 CONTINUE
      IF (I.LT.3) GOTO 90
      JJC1(I-1) = JJC1(I-2)
      JJC2(I-1) = JJC2(I-2)
      GOTO 100
C
   90 CONTINUE
      I0 = JLIST(1)
      JJC1(1) = JJQ1(3,I0)
      JJC2(1) = JJQ2(3,I0)
  100 CONTINUE
      JLIST(I) = ISH
      JA1 = I
      JA2 = I
      NS = NPEEL+1
      GOTO 130
C-----------------------------------------------------------------------
C
C  Case two
C  The ISHth shell is in the peel and open for either side
C
C-----------------------------------------------------------------------
  110 CONTINUE
      NS = NPEEL
      DO JWW = 1,NPEEL
        NX = ISH-JLIST(JWW)
        IF (NX.EQ.0) GOTO 120
      ENDDO
  120 CONTINUE
      JA1 = JWW
      JA2 = JWW
C-----------------------------------------------------------------------
C
C  ****** main computation ******
C
C  JA1 , JA2 are the indices of interacting shells in JLIST
C  IA1 , IA2 are the indices of interacting shells in NW
C
C-----------------------------------------------------------------------
  130 CONTINUE
      IA1 = JLIST(JA1)
      IA2 = JLIST(JA2)
      KS1 = 2*ABS(NAK(IA1))
      KS2 = 2*ABS(NAK(IA2))
C-----------------------------------------------------------------------
C
C  check triangular condition for the active shells
C
C-----------------------------------------------------------------------
      IF (ITRIG(KS1,KS2,KK)) GOTO 140
      IF (IDQ.EQ.2) RETURN
      GOTO 220
C-----------------------------------------------------------------------
C
C  set tables of quantum numbers of non-interacting spectator shells
C
C-----------------------------------------------------------------------
  140 CONTINUE
      DO JWW = 1,NS
        IJ = JLIST(JWW)
        IF (IJ.EQ.IA1) GOTO 150
        DO K = 1,3
          JBQ1(K,IJ) = JJQ1(K,IJ)
        ENDDO
C
  150   CONTINUE
        IF (IJ.EQ.IA2) GOTO 160
        DO K = 1,3
          JBQ2(K,IJ) = JJQ2(K,IJ)
        ENDDO
        IF (IJ.EQ.IA1 .OR. IJ.EQ.IA2) GOTO 160
        DO K = 1,3
* correction due to Froese Fischer, Gaigalas and Ralchenko (2006)
* detected in GRASP92 - replace 250 by 220.
*           IF (JBQ1(K,IJ).NE.JBQ2(K,IJ)) GOTO 220      !Suppress
          IF (JBQ1(K,IJ).NE.JBQ2(K,IJ)) GOTO 250        !Keep original
        ENDDO
  160   CONTINUE
      ENDDO
C-----------------------------------------------------------------------
C
C  ****** loop over parent states ******
C
C-----------------------------------------------------------------------
      IS(1) = IA1
      IS(2) = IA2
      KS(1) = KS1
      KS(2) = KS2
      VAL = ZERO
      KJ23 = 0
      IX = 0
      FAIL = .FALSE.
C**2
      NELCTS = NQ2(IA2)
      L2 = IROW1(NELCTS,KS2,IWRITE)
      LLS2 = ITAB(L2)
      LS2 = JTAB(L2)
C
      DO LB = 1,LLS2
        LS2 = LS2+3
        IT1 = NTAB(LS2)
        IT2 = KS2
        IT3 = JJQ2(3,IA2)
        IF (.NOT.ITRIG(IT1,IT2,IT3)) GOTO 200
        IF (ABS(NTAB(LS2-2)-JJQ2(1,IA2)).NE.1) GOTO 200
        DO K = 1,3
          JBQ2(K,IA2) = NTAB(LS2+K-3)
        ENDDO
C**1
        NELCTS = NQ1(IA1)
        L1 = IROW1(NELCTS,KS1,IWRITE)
        LLS1 = ITAB(L1)
        LS1 = JTAB(L1)
C
        DO LA = 1,LLS1
          LS1 = LS1+3
          IT1 = NTAB(LS1)
          IT2 = KS1
          IT3 = JJQ1(3,IA1)
          IF (.NOT.ITRIG(IT1,IT2,IT3)) GOTO 190
          IF (ABS(NTAB(LS1-2)-JJQ1(1,IA1)).NE.1) GOTO 190
          DO K = 1,3
            JBQ1(K,IA1) = NTAB(LS1+K-3)
          ENDDO
C
          DO K = 1,3
            IF (JBQ1(K,IA1).NE.JBQ2(K,IA1)) GOTO 190
            IF (JBQ1(K,IA2).NE.JBQ2(K,IA2)) GOTO 190
          ENDDO
C-----------------------------------------------------------------------
C
C  parent shells now defined
C
C-----------------------------------------------------------------------
          CALL FIXJ(JA1,JA2,KA,IS,KS,NS,KJ23,J1, J2, J3, MMOM, NMOM,JBQ2!
     +, JJC1, JJC2, JJQ1, JJQ2, JLIST)
          KJ23 = 1
C-----------------------------------------------------------------------
C
C  evaluate recoupling coefficient
C
C-----------------------------------------------------------------------
          IF (IX.EQ.0) THEN
      DO I=1,MMOM
        FREE(I)=.FALSE.
      ENDDO
      IF (LLS2.NE.1) FREE(JA2)=.TRUE.
      CALL NJGRAF (
     + RECUPS,-1,FAIL,
     + J1,J2,J3,MMOM,NMOM,FREE,
     + IBUG3,IWRITE,
     + J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     + JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     + K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
            IX = 1
          ENDIF
C
          IF (FAIL) GOTO 210
      CALL GENSUM (
     + RECUPS,
     + J1,MMOM,
     + IBUG3,
     + J6C,J7C,J8C,J9C,JWC,
     + J6,J7,J8,J9,JW,
     + JDEL,LDEL,MP,J6P,J7P,J8P,J9P,
     + JWORD,NLSUM,NBJ,NB6J,
     + K6CP,K7CP,K8CP,K9CP,
     + JSUM4,JSUM5,JSUM6,INV6J
     + )
          IF (IBUG6.EQ.1) WRITE (IWRITE,3030) RECUPS
          IF (ABS(RECUPS).LT.EPS10) GOTO 190
C-----------------------------------------------------------------------
C
C  evaluates 2 CFPs
C
C-----------------------------------------------------------------------
          IF (KS1.EQ.2) GOTO 170
          II = IA1
          NEL = NQ1(II)
          IVP = JBQ1(1,II)
          IWP = JBQ1(2,II)
          IJP = JBQ1(3,II)-1
          IVD = JJQ1(1,II)
          IWD = JJQ1(2,II)
          IJD = JJQ1(3,II)-1
          CALL CFP(IWRITE,KS1,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
          IF (IBUG6.EQ.1) WRITE (IWRITE,3020) KS1,NEL,IJD,IVD,IWD,IJP,IV!
     +P,IWP,C
          IF (ABS(C).LT.EPS10) GOTO 190
          RECUPS = RECUPS*C
C
  170     CONTINUE
          IF (KS2.EQ.2) GOTO 180
          II = IA2
          NEL = NQ2(II)
          IVD = JJQ2(1,II)
          IWD = JJQ2(2,II)
          IJD = JJQ2(3,II)-1
          IVP = JBQ2(1,II)
          IWP = JBQ2(2,II)
          IJP = JBQ2(3,II)-1
          CALL CFP(IWRITE,KS2,NEL,IJD,IVD,IWD,IJP,IVP,IWP,C)
          IF (IBUG6.EQ.1) WRITE (IWRITE,3020) KS2,NEL,IJD,IVD,IWD,IJP,IV!
     +P,IWP,C
          IF (ABS(C).LT.EPS10) GOTO 190
          RECUPS = RECUPS*C
C
  180     CONTINUE
          VAL = VAL+RECUPS
  190     CONTINUE
        ENDDO
  200   CONTINUE
      ENDDO
C
C  *** end of loop over parent states ***
C
  210 CONTINUE
      IF (IDQ.EQ.2) GOTO 230
C-----------------------------------------------------------------------
C
C  ****** IDQ=0 case ******
C
C-----------------------------------------------------------------------
      VSHELL(ISH) = VAL*DBLE(NQ1(IA1))
C-----------------------------------------------------------------------
C
C  loop over all shells when IDQ=0
C
C-----------------------------------------------------------------------
  220 CONTINUE
      IF (NPEEL.EQ.0) GOTO 60
      DO I = 1,NPEEL
        JLIST(I) = JLIS(I)
      ENDDO
      IF (NPEEL.EQ.1) GOTO 60
      NPEELM = NPEEL-1
      DO I = 1,NPEELM
        JJC1(I) = JC1S(I)
        JJC2(I) = JC2S(I)
      ENDDO
      GOTO 60
C-----------------------------------------------------------------------
C
C  ****** IDQ=2 case ******
C
C  permutation factor for IDQ=2
C
C-----------------------------------------------------------------------
  230 CONTINUE
      VAL = VAL*SQRT(DBLE(NQ1(IA1)*NQ2(IA2)))
      LLD1 = MIN(IA1,IA2)+1
      LLD2 = MAX(IA1,IA2)
      IDL = 1
      IF (IA1.LT.IA2) IDL = 0
      DO K = LLD1,LLD2
        IDL = IDL+NQ1(K)
      ENDDO
      IF (MOD(IDL,2).NE.0) VAL = -VAL
      VSHELL(1) = VAL
      RETURN
C-----------------------------------------------------------------------
  240 CONTINUE
      IF (IBUG6.EQ.1) WRITE (IWRITE,3000)
      RETURN
C-----------------------------------------------------------------------
  250 CONTINUE
      IF (IBUG6.EQ.1) WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
 3000 FORMAT (' one side has more than one interacting electron')
 3010 FORMAT (                                                          !
     +' spectator quantum numbers not diagonal for non-interacting she',!
     +'lls')
 3020 FORMAT (' CFP  ',I3,I4,I7,2I4,I7,2I4,1P,E20.9)
 3030 FORMAT (/' recoupling coeff=',1P,E20.9)
 3040 FORMAT (//' ISH=',I3)
      END
CEND--------------------------------------------------------------------
CEND    VIJOUT.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE VIJOUT(IWRITE,JA,JB,JJC1, JJC2, JJQ1, JJQ2, JLIST, NCOR!
     +E, NQ1, NQ2, NPEEL)
CRCS
CRCS $Source: /home/phn/DARC/RCS/VIJOUT.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:03:27 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C  No routines called.
C
C   IWRITE
C   JA
C   JB
C   JJC1
C   JJC2
C   JJQ1
C   JJQ2
C   JLIST
C   NCORE
C   NQ1
C   NQ2
C   NPEEL
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      CHARACTER*2 IJ1
      PARAMETER (IJ1 = '/2')
      CHARACTER*2 IJ2
      PARAMETER (IJ2 = '  ')
C
C  Argument variables
C
      INTEGER IWRITE,JA,JB
      INTEGER JJC1(*)
      INTEGER JJC2(*)
      INTEGER JJQ1(3,*)
      INTEGER JJQ2(3,*)
      INTEGER JLIST(*)
      INTEGER NCORE
      INTEGER NQ1(*)
      INTEGER NQ2(*)
      INTEGER NPEEL
C
C  Local variables
C
      CHARACTER*2 IC(2)
      INTEGER I,J,JC(2),JWW
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (NPEEL.EQ.0) GOTO 50
C-----------------------------------------------------------------------
C
C  Print active shell quantum numbers from JLIST table
C
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000) JA,JB
      WRITE (IWRITE,3010)
C-----------------------------------------------------------------------
      DO J = 1,NPEEL
        JWW = JLIST(J)
        JC(1) = JJQ1(3,JWW)-1
        IC(1) = IJ1
        IF (MOD(JC(1),2).EQ.1) GOTO 10
        JC(1) = JC(1)/2
        IC(1) = IJ2
   10   CONTINUE
        JC(2) = JJQ2(3,JWW)-1
        IC(2) = IJ1
        IF (MOD(JC(2),2).EQ.1) GOTO 20
        JC(2) = JC(2)/2
        IC(2) = IJ2
   20   CONTINUE
        WRITE (IWRITE,3020) JWW,NQ1(JWW),JJQ1(1,JWW),JJQ1(2,JWW),JC(1),I!
     +C(1),NQ2(JWW),JJQ2(1,JWW),JJQ2(2,JWW),JC(2),IC(2)
      ENDDO
C-----------------------------------------------------------------------
      IF (NPEEL.LT.2) GOTO 50
C-----------------------------------------------------------------------
C
C  Print coupling angular momenta if NPEEL.GE.2
C
C-----------------------------------------------------------------------
      WRITE (IWRITE,3030)
C-----------------------------------------------------------------------
      DO J = 2,NPEEL
        JC(1) = JJC1(J-1)-1
        IC(1) = IJ1
        IF (MOD(JC(1),2).EQ.1) GOTO 30
        JC(1) = JC(1)/2
        IC(1) = IJ2
   30   CONTINUE
        JC(2) = JJC2(J-1)-1
        IC(2) = IJ1
        IF (MOD(JC(2),2).EQ.1) GOTO 40
        JC(2) = JC(2)/2
        IC(2) = IJ2
   40   CONTINUE
        WRITE (IWRITE,3040) (JC(I),IC(I),I=1,2)
      ENDDO
C-----------------------------------------------------------------------
   50 CONTINUE
      WRITE (IWRITE,3050) NCORE
C-----------------------------------------------------------------------
 3000 FORMAT (//'   CSF(',I3,') -',23X,'CSF(',I3,') -'//)
 3010 FORMAT (6X,'Shell',4X,'Q',4X,'V',2X,'W',2X,'J',19X,'Q',4X,'V',2X, !
     +'W',2X,'J'//)
 3020 FORMAT (7X,I3,I6,I5,2I3,A2,15X,I3,I5,2I3,A2)
 3030 FORMAT (//'    Coupling schemes - ')
 3040 FORMAT (14X,I2,A2,27X,I2,A2)
 3050 FORMAT (//'   There are ',I3,' inactive closed shells.'/)
      END
CEND--------------------------------------------------------------------
CEND    WRITP2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE WRITP2(
     + ITAPE1, ITAPE2, IDISC2, RECORD, IHED, IBBPOL,
     + IBCPOL, ICCPOL, ICTXB, IDMTST, IPOLPH, ISTX1B, ISTX2B, ISTXB,
     + IWRITE, JAG, K1S, K2S, KAG, KBBN1, KBBN2, KBBR, KBCN1, KBCN2,
     + KBCR, KBMAX, KCCN, KCCR, KCMAX, KCMIN, KMULTN, KMULTR, KPOS,
     + KX, LAG, LAMBB, LAMBC, LAMCC, MAXFUL, MAXNQN, MAXP, MINNQN,
     + NDIMAX, NRANG2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/WRITP2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/12/05 19:35:02 $
CRCS $Revision: 11.2 $
CRCS
C-----------------------------------------------------------------------
C
C   ITAPE1 ... stream number of DSTG1.DAT (dstg1 dump file)
C   ITAPE2 ... stream number of DSTG2.DAT (dstg2 dump file)
C   IDISC2 ... stream number of DA file containing radial integrals
C   RECORD ... 20-character record of time/date at execution
C   IHED   ... 80-character title for the calculation
C   IBBPOL ... array giving positions of the b-b integrals
C   IBCPOL ... array giving positions of the b-c integrals
C   ICCPOL ... array giving positions of the c-c integrals
C   ICTXB  ... array ICTXB(K1,K2,K3) gives index in ISTX1B/ISTX2B
C              arrays
C   IDMTST ... array containing maximum dimensions set in the code
C   IPOLPH ...
C   ISTX1B ... array stores LAM*(KBMAX+1)+K4 for b-b Slater int.
C   ISTX2B ... array gives position of Slater int. corresponding to
C              ISTX1B
C   ISTXB  ... array ISTXB(K1) gives position of one-electron int.
C   IWRITE ... stream number for printed output
C   JAG    ... array of orbital 2j quantum numbers
C   K1S    ...
C   K2S    ...
C   KAG    ... array of orbital kappa quantum numbers
C   KBBN1  ...
C   KBBN2  ...
C   KBBR   ...
C   KBCN1  ...
C   KBCN2  ...
C   KBCR   ...
C   KBMAX  ... maximum bound K-value
C   KCCN   ...
C   KCCR   ...
C   KCMAX  ... maximum continuum K-value
C   KCMIN  ... minimum continuum K-value
C   KMULTN ...
C   KMULTR ...
C   KPOS   ...
C   KX     ... maximum of KBMAX and KCMAX
C   LAG    ... array of orbital l quantum numbers
C   LAMBB  ... maximum lambda value allowed for bound-bound
C              multipole integrals
C   LAMBC  ... maximum lambda value allowed for bound-continuum
C              multipole integrals
C   LAMCC  ... maximum lambda value allowed for continuum-continuum
C              multipole integrals
C   MAXFUL ... array containing maximum principal quantum number
C              for shells that are always full
C   MAXNQN ... array containing maximum principal quantum number
C              for each K-value for the bound orbitals
C   MAXP   ...
C   MINNQN ... array containing minimum principal quantum number
C              for each K-value for the bound orbitals
C   NDIMAX ... array containing maximum dimensions used in the code
C   NRANG2 ... number of continuum orbitals per K-value
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
      INTEGER NBUT
      PARAMETER (NBUT=2*(MXNB-1))
      INTEGER NIRX
      PARAMETER (NIRX=20)
      INTEGER NDX
      PARAMETER (NDX=9)
      INTEGER NDY
      PARAMETER (NDY=(NDX+MXNK)/4+1)
C
C  Argument variables
C
      INTEGER IDISC2,ITAPE1,ITAPE2
C
      CHARACTER*20 RECORD
      CHARACTER*80 IHED
      INTEGER IBBPOL(NDX,NDX,*)
      INTEGER IBCPOL(NDX,MXNK,*)
      INTEGER ICCPOL(MXNK,MXNK,*)
      INTEGER ICTXB(NDX,NDX,*)
      INTEGER IDMTST(*)
      INTEGER IPOLPH
      INTEGER IRK1
      INTEGER IRK2
      INTEGER ISTX1B(*)
      INTEGER ISTX2B(*)
      INTEGER ISTXB(*)
      INTEGER IWRITE
      INTEGER JAG(*)
      INTEGER K1S
      INTEGER K2S
      INTEGER KAG(*)
      INTEGER KBBN1
      INTEGER KBBN2
      INTEGER KBBR
      INTEGER KBCN1(*)
      INTEGER KBCN2(*)
      INTEGER KBCR(*)
      INTEGER KBMAX
      INTEGER KCCN(MXNK,*)
      INTEGER KCCR(MXNK,*)
      INTEGER KCMAX
      INTEGER KCMIN
      INTEGER KMULTN
      INTEGER KMULTR
      INTEGER KPOS
      INTEGER KX
      INTEGER LAG(*)
      INTEGER LAMBB
      INTEGER LAMBC
      INTEGER LAMCC
      INTEGER MAXFUL(*)
      INTEGER MAXNQN(*)
      INTEGER MAXP
      INTEGER MINNQN(*)
      INTEGER NDIMAX(*)
      INTEGER NRANG2
C
C  Local variables
C
      CHARACTER*80 IHEDIN
      CHARACTER*20 RECIN
      DOUBLE PRECISION BSTO,CL,DR1,HINT
      DOUBLE PRECISION RA
      DOUBLE PRECISION DUMBAS(MXNB)
      DOUBLE PRECISION DUMBUT(NBUT)
      INTEGER I,I1,I2,IJK
      INTEGER IREC,IRX(NIRX),ITOT,IX
      INTEGER J,K,KC
      INTEGER KCP,L,LAMIND
      INTEGER MBUT,N,NELC
      INTEGER NIX,NUMORB,NZ
      INTEGER ICTYB(NDX,NDX,NDY)
      INTEGER MAXNLG(MXNK)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      WRITE (IWRITE,3000)
C-----------------------------------------------------------------------
C
C  Read from the DSTG1 dump
C
C-----------------------------------------------------------------------
      READ (ITAPE1) IHEDIN,RECIN
      READ (ITAPE1) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      READ (ITAPE1) LAMBB,LAMBC,LAMCC
C
      WRITE (IWRITE,3010) IHEDIN(1:40),IHEDIN(41:80),RECIN
      WRITE (IWRITE,3020) KBMAX,KCMIN,KCMAX,KX
      WRITE (IWRITE,3030) NRANG2,NELC,NZ
      WRITE (IWRITE,3040) LAMBB,LAMBC,LAMCC
C-----------------------------------------------------------------------
      CALL DMCHK2(08,KX, IWRITE, IDMTST, NDIMAX)
C-----------------------------------------------------------------------
      READ (ITAPE1) (MINNQN(L),L=1,KX),(MAXNQN(L),L=1,KX),
     +              (MAXNLG(L),L=1,KX),(MAXFUL(L),L=1,KBMAX),
     +              (LAG(L),L=1,KX),(KAG(L),L=1,KX),(JAG(L),L=1,KX)
C
      WRITE (IWRITE,3050) (MINNQN(L),L=1,KBMAX)
      WRITE (IWRITE,3060) (MAXFUL(L),L=1,KBMAX)
      WRITE (IWRITE,3070) (MAXNQN(L),L=1,KBMAX)
      WRITE (IWRITE,3080) (MAXNLG(L),L=1,KBMAX)
      WRITE (IWRITE,3090) (LAG(L),L=1,KX)
      WRITE (IWRITE,3100) (KAG(L),L=1,KX)
      WRITE (IWRITE,3110) (JAG(L),L=1,KX)
C-----------------------------------------------------------------------
      READ (ITAPE1) RA,BSTO,HINT,CL,DR1
C
      WRITE (IWRITE,3120) RA,BSTO,HINT,CL,DR1
C-----------------------------------------------------------------------
      READ (ITAPE1) NIX
      CALL DMCHK2(24,NIX, IWRITE, IDMTST, NDIMAX)
      READ (ITAPE1) (IRX(I),I=1,NIX)
C
      WRITE (IWRITE,3130) NIX, (IRX(I),I=1,NIX)
C-----------------------------------------------------------------------
      READ (ITAPE1) NUMORB
C
      WRITE (IWRITE,3140) NUMORB
C-----------------------------------------------------------------------
C
C  Problem if the dump does not contain dipole radial integrals
C  and a photo-ionisation calculation has been requested
C
      IF (IPOLPH.EQ.2) THEN
        IF (LAMBC.EQ.0 .OR. LAMCC.EQ.0) THEN
          WRITE (IWRITE,3180)
          STOP
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C  Write to the DSTG2 dump
C
      WRITE (ITAPE2) IHED,RECORD
      WRITE (ITAPE2) KBMAX,KCMIN,KCMAX,KX,NRANG2,NELC,NZ
      WRITE (ITAPE2) LAMBB,LAMBC,LAMCC
      WRITE (ITAPE2) (MINNQN(L),L=1,KX),(MAXNQN(L),L=1,KX),
     +               (MAXNLG(L),L=1,KX),(MAXFUL(L),L=1,KBMAX),
     +               (LAG(L),L=1,KX),(KAG(L),L=1,KX),(JAG(L),L=1,KX)
      WRITE (ITAPE2) RA,BSTO,HINT,CL,DR1
      WRITE (ITAPE2) NIX
      WRITE (ITAPE2) (IRX(I),I=1,NIX)
      WRITE (ITAPE2) NUMORB
C
      WRITE (IWRITE,3150)
C
C  Copy over the continuum orbital data and Buttle corrections
C
      CALL DMCHK2(06,NRANG2, IWRITE, IDMTST, NDIMAX)
      DO L = KCMIN,KCMAX
        READ (ITAPE1) (DUMBAS(N),N=1,NRANG2)
        WRITE (ITAPE2) (DUMBAS(N),N=1,NRANG2)
        READ (ITAPE1) (DUMBAS(N),N=1,NRANG2)
        WRITE (ITAPE2) (DUMBAS(N),N=1,NRANG2)
        READ (ITAPE1) MBUT
        WRITE (ITAPE2) MBUT
        READ (ITAPE1) (DUMBUT(N),N=1,MBUT)
        WRITE (ITAPE2) (DUMBUT(N),N=1,MBUT)
        READ (ITAPE1) (DUMBUT(N),N=1,MBUT)
        WRITE (ITAPE2) (DUMBUT(N),N=1,MBUT)
      ENDDO
C
      WRITE (IWRITE,3170)
C-----------------------------------------------------------------------
C
C  multipole integral data
C
      READ (ITAPE1) IRK1,IREC
      KMULTN = IRK1
      KMULTR = IREC
C
      IF (IRK1.GT.0) THEN
        CALL DMCHK2(03,IRK1, IWRITE, IDMTST, NDIMAX)
        IF (LAMBB.GT.0) THEN
          LAMIND = (LAMBB+1)/2
          READ (ITAPE1)
     +  (((IBBPOL(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,LAMIND)
        ENDIF
        IF (KCMAX.GT.0) THEN
          IF (LAMBC.GT.0) THEN
            LAMIND = (LAMBC+1)/2
            READ (ITAPE1)
     +  (((IBCPOL(I,J,K),I=1,KBMAX),J=1,KCMAX),K=1,LAMIND)
          ENDIF
        ENDIF
        IF (KCMAX.GT.0) THEN
          IF (LAMCC.GT.0) THEN
            LAMIND = (LAMCC+1)/2
            READ (ITAPE1)
     +  (((ICCPOL(I,J,K),I=1,KCMAX),J=1,KCMAX),K=1,LAMIND)
          ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C  continuum-continuum integral data
C
      IX = KCMAX-KCMIN+1
      IX = IX*(IX+1)/2
      DO IJK = 1,IX
        READ (ITAPE1) IRK1,KC,KCP,IREC
        KCCN(KC,KCP) = IRK1
        KCCR(KC,KCP) = IREC
C
        IF (IRK1.GT.0 ) THEN
          CALL DMCHK2(03,IRK1, IWRITE, IDMTST, NDIMAX)
          I1 = MIN(KBMAX+KBMAX,KC+KCP)
          I2 = MIN(KBMAX+KC,KBMAX+KCP)
          I1 = I1/4+1
          I2 = I2/4+1
          READ (ITAPE1)
     +  (((ICTXB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I1),
     +  (((ICTYB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I2)
          WRITE (IDISC2)
     +  (((ICTXB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I1),
     +  (((ICTYB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,I2)
        ELSE
          WRITE (IDISC2) ZERO
        ENDIF
C
      ENDDO
C-----------------------------------------------------------------------
C
C  bound-continuum integral data
C
        DO IJK = KCMIN,KCMAX
          READ (ITAPE1) IRK1,IRK2,KC,IREC
          KBCN1(KC) = IRK1
          KBCN2(KC) = IRK2
          KBCR(KC) = IREC
C
          IF (IRK1.GT.0) THEN
            CALL DMCHK2(03,IRK1, IWRITE, IDMTST, NDIMAX)
            CALL DMCHK2(04,IRK2, IWRITE, IDMTST, NDIMAX)
            READ (ITAPE1)
     +   (((ICTXB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +   (ISTX1B(I),I=1,IRK2),(ISTX2B(I),I=1,IRK2)
            WRITE (IDISC2)
     +   (((ICTXB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +   (ISTX1B(I),I=1,IRK2),(ISTX2B(I),I=1,IRK2)
          ELSE
            WRITE (IDISC2) ZERO
          ENDIF
C
        ENDDO
C-----------------------------------------------------------------------
C
C  bound-bound integral data
C
        READ (ITAPE1) IRK1,IRK2,IREC
        KBBN1 = IRK1
        KBBN2 = IRK2
        KBBR = IREC
C
        IF (IRK1.GT.0) THEN
          CALL DMCHK2(03,IRK1, IWRITE, IDMTST, NDIMAX)
          CALL DMCHK2(04,IRK2, IWRITE, IDMTST, NDIMAX)
          READ (ITAPE1)
     +  (ISTXB(I),I=1,KBMAX),
     +  (((ICTXB(I,J,K),I=1,KBMAX),J=1,KBMAX),K=1,KBMAX),
     +  (ISTX1B(I),I=1,IRK2),(ISTX2B(I),I=1,IRK2)
        ENDIF
C-----------------------------------------------------------------------
      IX = KCMAX-KCMIN+1
      ITOT = 1+IX*(IX+1)/2+IX+1
      WRITE (IWRITE,3160) ITOT
      REWIND ITAPE1
C-----------------------------------------------------------------------
C
C   set parameters that are used in the LOC... routines
C
      K1S = -1
      K2S = -1
      MAXP = IX+IX*(IX+1)/2
      KPOS = MAXP+1
C=======================================================================
 3000 FORMAT (/31X,'Routine WRITP2'/31X,'--------------')
 3010 FORMAT (/' Title of dump : '/1X,A40/1X,A40/1X,A20)
 3020 FORMAT (/
     +' KBMAX  (largest  bound     angular momentum K-value) : ',I5/
     +' KCMIN  (smallest continuum angular momentum K-value) : ',I5/
     +' KCMAX  (largest  continuum angular momentum K-value) : ',I5/
     +' KX                                                   : ',I5)
 3030 FORMAT (
     +' NRANG2 (continuum orbitals per ang.mom.)             : ',I5/
     +' NELC   (number of electrons)                         : ',I5/
     +' NZ     (atomic number)                               : ',I5)
 3040 FORMAT (
     +' LAMBB  (bound-bound multipole parameter)             : ',I5/
     +' LAMBC  (bound-continuum multipole parameter)         : ',I5/
     +' LAMCC  (continuum-continuum multipole parameter)     : ',I5)
 3050 FORMAT (/(' MINNQN : ',10I5))
 3060 FORMAT (' MAXFUL : ',10I5)
 3070 FORMAT (' MAXNQN : ',10I5)
 3080 FORMAT (' MAXNLG : ',10I5)
 3090 FORMAT (' LAG    : ',10I5)
 3100 FORMAT (' KAG    : ',10I5)
 3110 FORMAT (' JAG    : ',10I5)
 3120 FORMAT (/1P,
     +' RA     (R-matrix boundary)                           : ',E14.7/
     +' BSTO   (log. derivative parameter)                   : ',E14.7/
     +' HINT   (basic step-size)                             : ',E14.7/
     +' CL     (speed of light)                              : ',E14.7/
     +' DR1    (first mesh point)                            : ',E14.7)
 3130 FORMAT (/
     + ' NIX    (mesh sub-intervals)           = ',I5//
     +(' IRX    (sub-interval endpoints)       : ',6I5))
 3140 FORMAT (/
     +' NUMORB (number of bound orbitals)                    : ',I5/)
 3150 FORMAT (/
     +' *** basic data'/
     +' *** read from  DSTG1 dump (ITAPE1)'/
     +' *** written to DSTG2 dump (ITAPE2)')
 3160 FORMAT (/
     +' *** integrals',8X,I4,' blocks'/
     +' *** read from    DSTG1 dump (ITAPE1)'/
     +' *** written to scratch file (IDISC2)')
 3170 FORMAT (/
     +' *** amplitudes, eigenvalues and Buttle corrections'/
     +' *** read from  DSTG1 dump (ITAPE1)'/
     +' *** written to DSTG2 dump (ITAPE2)')
 3180 FORMAT (/
     +' *******************************************'/
     +' ***    The code is STOPPING in WRITP2.  ***'/
     +' ***  For a photo-ionisation calculation ***'/
     +' ***            IPOLPH=2                 ***'/
     +' ***    you must have LAMBC AND LAMCC    ***'/
     +' ***           non-zero.                 ***'/
     +' *******************************************'/)
      END
CEND--------------------------------------------------------------------
CEND    DA1.f    Wed Jan 21 10:09:58 GMT 2004
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
CEND    DRACAH.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DRACAH(I,J,K,L,M,N,RAC)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DRACAH.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 13:57:16 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MXNY
      PARAMETER (MXNY=8)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.D0)
      DOUBLE PRECISION THIRTY
      PARAMETER (THIRTY=3.D1)
      DOUBLE PRECISION EPS10
      PARAMETER (EPS10=1.D-10)
C
C  Argument variables
C
      DOUBLE PRECISION RAC
      INTEGER I,J,K,L,M,N
C
C  Local variables
C
      DOUBLE PRECISION RAC1,RAC2
      DOUBLE PRECISION X
      INTEGER ICODE,ICOUNT,IMARK,J1
      INTEGER J2,J3,J4,J5
      INTEGER J6,J7,KI,KJ
      INTEGER NUMAX,NUMIN
      INTEGER II,JJ,KK,LL,MM,NN
C
      DOUBLE PRECISION GAM(500)
      DOUBLE PRECISION XSTO(2171)
      INTEGER*2 ISTO(0:MXNY,0:MXNY,0:MXNY,0:MXNY,0:MXNY,0:MXNY)
      INTEGER NUMC,NUMR,NUMSTO
      LOGICAL FIRST
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE GAM,XSTO,ISTO
      SAVE NUMC,NUMR,NUMSTO
      SAVE FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      IF (FIRST) THEN
C
C  initialise counters
C
        NUMC = 0
        NUMR = 0
        NUMSTO = 0
C
C  calculate the logs of factorials
C
        GAM(1) = ONE
        GAM(2) = ONE
        X = TWO
        DO II = 3,30
          GAM(II) = GAM(II-1)*X
          X = X+ONE
        ENDDO
C
        DO II = 1,30
          GAM(II) = LOG(GAM(II))
        ENDDO
C
        X = THIRTY
        DO II = 31,500
          GAM(II) = GAM(II-1)+LOG(X)
          X = X+ONE
        ENDDO
C
C  initialise the ISTO array
C
        DO II = 0,MXNY
          DO JJ = 0,MXNY
            DO KK = 0,MXNY
              DO LL = 0,MXNY
                DO MM = 0,MXNY
                  DO NN = 0,MXNY
                    ISTO(II,JJ,KK,LL,MM,NN) = -1
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
        FIRST = .FALSE.
C
      ENDIF
C-----------------------------------------------------------------------
C
C  use I<0 to return the counters
C
      IF (I.LT.0) THEN
        J = NUMC
        K = NUMR
        L = NUMSTO
        RETURN
      ENDIF
C-----------------------------------------------------------------------
      IF (I.LE.MXNY .AND. J.LE.MXNY .AND.
     +    K.LE.MXNY .AND. L.LE.MXNY .AND.
     +    M.LE.MXNY .AND. N.LE.MXNY) THEN
C
        IF (ISTO(I,J,K,L,M,N).GE.0) THEN
          ICODE = ISTO(I,J,K,L,M,N)
          IF (ICODE.EQ.0) THEN
            RAC = ZERO
          ELSE
            RAC = XSTO(ICODE)
          ENDIF
          NUMR = NUMR+1
          GOTO 20
        ENDIF
C
        IMARK = 1
      ELSE
        IMARK = 0
      ENDIF
C-----------------------------------------------------------------------
C
C   Test the triangular conditions
C
C-----------------------------------------------------------------------
      RAC = ZERO
C
      J1 = I+J+M
      IF ((2*MAX(I,J,M)-J1).GT.0 .OR. MOD(J1,2).NE.0) GOTO 10
      J2 = K+L+M
      IF ((2*MAX(K,L,M)-J2).GT.0 .OR. MOD(J2,2).NE.0) GOTO 10
      J3 = I+K+N
      IF ((2*MAX(I,K,N)-J3).GT.0 .OR. MOD(J3,2).NE.0) GOTO 10
      J4 = J+L+N
      IF ((2*MAX(J,L,N)-J4).GT.0 .OR. MOD(J4,2).NE.0) GOTO 10
C
      J1 = J1/2
      J2 = J2/2
      J3 = J3/2
      J4 = J4/2
      J5 = (I+J+K+L)/2
      J6 = (I+L+M+N)/2
      J7 = (J+K+M+N)/2
C
      NUMIN = MAX(J1,J2,J3,J4)+1
      NUMAX = MIN(J5,J6,J7)+1
      RAC = ONE
C
      IF (NUMIN.NE.NUMAX) THEN
        NUMIN = NUMIN+1
        ICOUNT = 0
        DO KJ = NUMIN,NUMAX
          KI = NUMAX-ICOUNT
          RAC1 = KI*(J5-KI+2)*(J6-KI+2)*(J7-KI+2)
          RAC2 = (KI-1-J1)*(KI-1-J2)*(KI-1-J3)*(KI-1-J4)
          RAC = ONE-(RAC*RAC1/RAC2)
          ICOUNT = ICOUNT+1
        ENDDO
        NUMIN = NUMIN-1
      ENDIF
C
      RAC = RAC*((-ONE)**(J5+NUMIN+1))*EXP((GAM(NUMIN+1)-GAM(NUMIN-J1)-G!
     +AM(NUMIN-J2)-GAM(NUMIN-J3)-GAM(NUMIN-J4)-GAM(J5+2-NUMIN)-GAM(J6+2-!
     +NUMIN)-GAM(J7+2-NUMIN))+((GAM(J1+1-I)+GAM(J1+1-J)+GAM(J1+1-M)-GAM(!
     +J1+2)+GAM(J2+1-K)+GAM(J2+1-L)+GAM(J2+1-M)-GAM(J2+2)+GAM(J3+1-I)+GA!
     +M(J3+1-K)+GAM(J3+1-N)-GAM(J3+2)+GAM(J4+1-J)+GAM(J4+1-L)+GAM(J4+1-N!
     +)-GAM(J4+2))/TWO))
C-----------------------------------------------------------------------
   10 CONTINUE
      NUMC = NUMC+1
      IF (IMARK.EQ.1) THEN
C
        IF (ABS(RAC).GT.EPS10) THEN
          IF (NUMSTO.EQ.2171) GOTO 20
          NUMSTO = NUMSTO+1
          XSTO(NUMSTO) = RAC
          ICODE = NUMSTO
        ELSE
          ICODE = 0
        ENDIF
C
        ISTO(I,J,K,L,M,N) = ICODE
        ISTO(L,K,J,I,M,N) = ICODE
        ISTO(L,J,K,I,N,M) = ICODE
        ISTO(I,K,J,L,N,M) = ICODE
        ISTO(J,I,L,K,M,N) = ICODE
        ISTO(K,L,I,J,M,N) = ICODE
        ISTO(K,I,L,J,N,M) = ICODE
        ISTO(J,L,I,K,N,M) = ICODE
C
      ENDIF
C-----------------------------------------------------------------------
   20 CONTINUE
      END
CEND--------------------------------------------------------------------
CEND    MATOUT.f    Wed Jan 21 10:09:58 GMT 2004
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
CEND    CALEN.f    Wed Jan 21 10:09:58 GMT 2004
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
CEND    QUARTZ.f    Wed Jan 21 10:09:58 GMT 2004
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
CEND--------------------------------------------------------------------
CEND    NJGRAF.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE NJGRAF (
     +RECUP,IGEN,FAIL,
     +J1,J2,J3,M,N,FREE,
     +IBUG3,IWRITE,
     +J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     +JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     +K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/NJGRAF.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
C   The changes are as follows:                                        *
C                                                                      *
C      1. PARAMETER IGEN has been included in the argument list:       *
C            IGEN =  0  normal call to NJGRAF                          *
C            IGEN = -1  GENSUM is not called                           *
C      2. The contents of COMMON blocks /ARGU/ and /SUMARG/ are used   *
C         by  GENSUM  to calculate the recoupling coefficient. These   *
C         COMMON  blocks  have  been removed from GENSUM. Their con-   *
C         tents are passed to  GENSUM  through the argument list in-   *
C         stead, so that NJGRAF can be called to set up formulae for   *
C         both the direct and exchange cases in COR and BREIT.         *
C      3. Extra  dimension  tests  have  been  included  in routines   *
C         NJGRAF, PRINTJ, SPRATE, VAR and ZERO. These are  discussed   *
C         below.                                                       *
C      4. An  extra routine  RDIAG  has been introduced to remove an   *
C         extended  DO loop from GENSUM, to conform with the FORTRAN   *
C         77 standard.                                                 *
C                                                                      *
C   Description of some COMMON blocks. A full discussion is given in   *
C   the NJGRAF program description (Bar-Shalom and Klapisch op cit).   *
C                                                                      *
C      COMMON block COUPLE                                             *
C                                                                      *
C         M                The total number of angular momentum val-   *
C                          ues in the initial and final states         *
C         N                The number of basic angular momentum val-   *
C                          ues that are coupled                        *
C         J1(I),           The angular momentum values stored as 2J+1  *
C            I = 1,M                                                   *
C         J2(I,J),         The position in the J1 array of the init-   *
C            I = 1,(N-1),  ial state triads                            *
C            J = 1,3                                                   *
C         J3(I,J),         The position in the J1 array of the final   *
C            I = 1,(N-1),  state triads                                *
C            J = 1,3                                                   *
C         FREE(I),         If FREE(I) = .TRUE., no reference is made   *
C            I = 1,M       to the value of J1(I) when establishing a   *
C                          formula in  NJGRAF .  GENSUM  may then be   *
C                          called  for  repeated  occurences of this   *
C                          formula  with  differing values of J1(I).   *
C                          If J1(I) does  not  vary between calls to   *
C                          GENSUM then FREE(I) should be set .FALSE.   *
C                          so that zero branches  can be removed be-   *
C                          fore the formula is established.            *
C                                                                      *
C      COMMON block DEBUG                                              *
C                                                                      *
C         IBUG1            Not used                                    *
C         IBUG2            Not used                                    *
C         IBUG3            Debug prints in NJGRAF and GENSUM if 1      *
C         IBUG4            Not used                                    *
C         IBUG5            Not used                                    *
C         IBUG6            Not used                                    *
C                                                                      *
C      COMMON block ARGU                                               *
C                                                                      *
C         J6C              The number of elements in the K6 array      *
C         J7C              The number of elements in the K7 array      *
C         J8C              The number of elements in the K8 array      *
C         J9C              The number of elements in the K9 array      *
C         JWC              The number of columns in the JW array       *
C         J6(I),           Each entry corresponds to a  factor  SQRT   *
C            I = 1,J6C     (2J+1) in RECUP. The value  of  J6  GIVES   *
C                          position in  J1  array where  J  value is   *
C                          found                                       *
C         J7(I),           Each entry corresponds to factor  (-1)**J   *
C            I = 1,J7C     in RECUP                                    *
C         J8(I),           Each entry corresponds to a factor (-1)**   *
C            I = 1,J8C     (2J) in RECUP                               *
C         J9(I),           Each entry corresponds to a factor (2J+1)   *
C            I = 1,J9C     **(-0.5) in RECUP                           *
C         JW(I,J),         Each column corresponds to a Racah coeff-   *
C            I = 1,6,      icient in RECUP                             *
C            J = 1,JWC                                                 *
C         JDEL             The number of delta functions               *
C         LDEL(I,J),       The arguments of the delta functions        *
C              J = 1,2                                                 *
C         SUMVAR(I)        .TRUE. for ang. mom. I (a summation vari-   *
C                          able                                        *
C         MP               The index of the last variable              *
C                                                                      *
C   The arrays  J6, J7, J8, J9 and  JW, are evaluated by NJGRAF. The   *
C   summation over the variables in  J6, J7, J8, J9 and  JW, and the   *
C   evaluation of RECUP is carried out in GENSUM. GENSUM  can be re-   *
C   entered directly to evaluate different  recoupling  coefficients   *
C   with the same structure  by just  altering the numbers in the J1   *
C   array.                                                             *
C                                                                      *
C                                                                      *
C   This version holds the array dimensions in parameter statements.   *
C   The dimensions are labelled:                                       *
C                                                                      *
C      MANGM  : Dimension of the J1 and FREE arrays in /COUPLE/, and   *
C               the  first  dimension of the LINE and LCOL arrays in   *
C               /TREE/. Also  the  dimension  of the SUMVAR array in   *
C               /ARGU/, AND OF THE INVER array in routine SPRATE. It   *
C               is tested for  M  on entry to  NJGRAF, and for MP in   *
C               routine SPRATE.                                        *
C      MTRIAD : Dimension of the  J2 and  J3 arrays in /COUPLE/. The   *
C               dimensions of these  arrays  are checked on entry to   *
C               NJGRAF in addition  MTRIAD sets the dimension of the   *
C               JSUM6 array and the first dimension of the JSUM4 and   *
C               JSUM5  arrays in /SUMARG/. Also gives the dimensions   *
C               of some  temporary working arrays in SPRATE and GEN-   *
C               SUM. In these  cases  mtriad sets the maximum number   *
C               of summation variables  in any particular sum, which   *
C               is tested in SPRATE.                                   *
C      M2TRD  : (=2*MTRIAD) Dimension of the J23 ,  ARROW  and  TABS   *
C               arrays in /TREE/. Also  the  dimension of the npoint   *
C               array in /GRAPH/.                                      *
C      M4TRD  : (=4*MTRIAD) Dimension of the  JDIAG,  ARR, IL and IH   *
C               arrays in /GRAPH/, and of the IAL array in /BUILD/.    *
C      M3MNGM : Dimension of the J6 array in /ARGU/, tested in SPRATE  *
C               Dimension of the J7 array in /ARGU/, tested in SPRATE  *
C               Dimension of the J8 array in /ARGU/, tested in SPRATE  *
C      MANGMP : Dimension of the J9 array in /ARGU/, tested in SPRATE  *
C               MANGMP also sets the dimension of the J6P,  J7P, J8P   *
C               and J9P arrays in /SUMARG/, And of the JNS  array in   *
C               routine VAR. The dimension of the JNS array is  tes-   *
C               ted in VAR.                                            *
C      M6J    : Dimension of the JW and LDEL arrays in /ARGU/,         *
C               and of the JWORD and INV6J arrays in /SUMARG/.  Also   *
C               the second dimension of the  JSUM4 and  JSUM5 arrays   *
C               in /SUMARG/. In addition it  gives the dimensions of   *
C               a  number of  temporary  working  arrays in routines   *
C               SPRATE and GENSUM. M6J is tested in SPRATE.            *
C      MFACT  : The dimension of the factorial array GAM in /FACTS /.  *
C      MSUM   : Dimension of the NBJ, NB6J, K6CP, K7CP, K8CP and K9CP  *
C               arrays in /SUMARG/. MSUM is the  maximum  number  of   *
C               sums allowed, and is tested in routine SPRATE.         *
C      MTAB   : The dimension of the JTAB array in  routine  PRINTJ.   *
C               MTAB is tested in PRINTJ.                              *
C      MZERO  : Dimension of the JZERO array in /ZER/. MZERO is tes-   *
C               ted in routine ZEROJ.                                  *
C                                                                      *
C                                           Last update: 16 Oct 1992   *
C                                                                      *
C***********************************************************************
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MZERO
      PARAMETER (MZERO=20)
C
C  Argument variables
C
      DOUBLE PRECISION RECUP
      INTEGER IGEN
      LOGICAL FAIL
C
      INTEGER IBUG3
      INTEGER INV6J(*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J2(MTRIAD,*)
      INTEGER J3(MTRIAD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6P(*)
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7P(*)
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8P(*)
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9P(*)
      INTEGER JDEL
      INTEGER JSUM4(MTRIAD,*)
      INTEGER JSUM5(MTRIAD,*)
      INTEGER JSUM6(*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWORD(6,*)
      INTEGER K6CP(*)
      INTEGER K7CP(*)
      INTEGER K8CP(*)
      INTEGER K9CP(*)
      INTEGER LDEL(M6J,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NB6J(*)
      INTEGER NBJ(*)
      INTEGER NLSUM
      LOGICAL FREE(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,J,JF
      INTEGER JF1,JPOL,JUMP,NCP
      LOGICAL FIND,INUSE
C
      CHARACTER*6 NAMSUB
      INTEGER JZERO(MZERO),NZERO
      INTEGER IAL(M4TRD),IF1,IF2,NODE
      INTEGER J6CC,J7CC,J8CC,J9CC
      INTEGER JDELC,JWCC
      INTEGER IT2,IT3,IT5
      INTEGER JARR(2,3),JKP(2,3)
      INTEGER ARR(M4TRD,3),ICROSS,IFIRST
      INTEGER IH(M4TRD),IL(M4TRD),ILAST,IPARTL
      INTEGER IPARTS,ITFREE(M6J),JDIAG(M4TRD,3)
      INTEGER NBNODE,NC,NFIN,NFREE
      INTEGER NPART,NPOINT(M2TRD)
      INTEGER TAB1(MANGM,2)
      INTEGER ARROW(M2TRD,3),J23(M2TRD,3)
      INTEGER LCOL(MANGM,2),LINE(MANGM,2)
      INTEGER NBTR
      LOGICAL CUT
      LOGICAL SUMVAR(MANGM)
      LOGICAL TABS(M2TRD)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'NJGRAF'/
C-----------------------------------------------------------------------
C
C   Debug printout
C
      IF (IBUG3 .EQ. 1) THEN
        INQUIRE (UNIT = 99,OPENED = INUSE)
        IF (.NOT.INUSE) THEN
          OPEN (FILE='NJGRAF.OUT',UNIT=99,STATUS='UNKNOWN',
     +          FORM='FORMATTED')
        ENDIF
        WRITE (99,3000)
        WRITE (99,3010) M,N-1
        WRITE (99,3020)
        WRITE (99,3030) (J1(I),I = 1,M)
        WRITE (99,3040) (FREE(I),I = 1,M)
        WRITE (99,3050)
        WRITE (99,3060) ((J2(I,J),J = 1,3),(J3(I,J),J = 1,3),I = 1,N-1)
      ENDIF
C
C   Test the dimension of the J1 array
C
      IF (M+1 .GT. MANGM) THEN
        WRITE (IWRITE,3070)
        WRITE (IWRITE,3080) M+1,MANGM
        STOP
      ENDIF
C
C   Test the dimensions of the J2 and J3 arrays
C
      IF (N-1 .GT. MTRIAD) THEN
        WRITE (IWRITE,3070)
        WRITE (IWRITE,3090) N-1,MTRIAD
        STOP
      ENDIF
C
C   Initializations
C
      DO I = N,MTRIAD
        DO J = 1,3
          J2(I,J) = 0
          J3(I,J) = 0
        ENDDO
      ENDDO
C
      FAIL = .FALSE.
      J6C = 0
      J7C = 0
      J8C = 0
      J9C = 0
      JWC = 0
      JDEL = 0
      CALL SETDM
     + ( J6C, J6CC, J7C, J7CC, J8C
     + , J8CC, J9C, J9CC, JDEL, JDELC, JWC
     + , JWCC
     + )
      NFIN = 0
      CUT = .FALSE.
C
C   Building up of the unstructured graph
C
      CALL SETTAB (FAIL
     + , ARR, ARROW, IAL, IBUG3, ICROSS, IFIRST
     + , IH, IL, ILAST, IPARTL, IPARTS, IWRITE, J1
     + , J2, J23, J3, J6, J6C, J6CC, J7, J7C
     + , J7CC, J8, J8C, J8CC, J9, J9C, J9CC, JDEL
     + , JDELC, JDIAG, JW, JWC, JWCC, JZERO, LCOL, LDEL
     + , LINE, M, MP, N, NBNODE, NBTR, NC, NFIN
     + , NFREE, NPART, NPOINT, NZERO, TAB1, CUT, FREE
     + , SUMVAR, TABS
     + )
C
C   Exit with RECUP set to zero if any angular momentum is
C   impossible
C
      M = M+1
      IF (FAIL) GOTO 90
C
      M = M-1
C
C   Locate and eliminate any zero angular momentum; simplify the
C   graph
C
      JF = 0
      JF1 = 0
      CALL ZEROJ (JF1,JF,FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS,
     + IFIRST, IH, IL, ILAST, IPARTL, IPARTS, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C,
     + J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC,
     + NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
      IF (FAIL) GOTO 90
C
      MP = M
      IF (NBTR .EQ. 0) GOTO 100
      JUMP = 1
C
   10 CONTINUE
      CALL CHKLP1 (FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS, IFIRST, IH,
     + IL, ILAST, IPARTL, IPARTS, IWRITE, J1, J23, J6, J6C,
     + J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9, J9C, J9CC,
     + JDEL, JDELC, JDIAG, JW, JWC, JWCC, JZERO, LCOL, LDEL,
     + LINE, M, MP, N, NAMSUB, NBNODE, NBTR, NC, NFIN, NFREE,
     + NPART, NPOINT, NZERO, SUMVAR, TAB1, TABS)
      IF (FAIL) GOTO 90
C
C   Build a flat diagram out of the unstructured graph; several flat
C   diagrams may constitute the original graph, in which case there
C   are possible cuts; the flat diagrams will have free ends if cut
C
      CALL DIAGRM (JUMP,
     + ARR, ARROW, IAL, IBUG3, ICROSS, IF1, IF2, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J7, J7C, J8, J8C, J9, J9C, JDEL, JDIAG, JW,
     + JWC, JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR,
     + NC, NFIN, NFREE, NODE, NPART, NPOINT, NZERO, TAB1,
     + SUMVAR, TABS)
      NFIN = MAX(0,NFREE-2)
C
      IF (NFIN .NE. 0) THEN
        JUMP = 3
C
C   Handling of free ends if a cut was found
C
        CALL CUTNL (FAIL,
     + ARR, ARROW, CUT, FREE, IBUG3, ICROSS, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, IT2, IT3, IT5, ITFREE, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8,
     + J8C, J8CC, J9, J9C, J9CC, JARR, JDEL, JDELC, JDIAG,
     + JKP, JW, JWC, JWCC, JZERO, LCOL, LDEL, LINE, M, MP,
     + N, NBNODE, NBTR, NC, NFIN, NFREE, NPART, NPOINT,
     + NZERO, SUMVAR, TAB1, TABS)
        IF (FAIL) GOTO 90
      ELSE
        JUMP = 2
        IF (NFREE .EQ. 1) THEN
          CALL CUT1L (FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS, IFIRST, IH,
     + IL, ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9,
     + J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC, JZERO,
     + LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC, NFIN,
     + NFREE, NPART, NPOINT, NZERO, SUMVAR, TAB1, TABS)
          IF (FAIL) GOTO 90
        ELSEIF (NFREE .GT. 1) THEN
          CALL CUT2L (FAIL,
     + ARR, ARROW, CUT, FREE, IBUG3, ICROSS, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C, J8CC,
     + J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR,
     + NC, NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
          IF (FAIL) GOTO 90
        ENDIF
      ENDIF
C
      NBTR = NBTR+NFIN
      IF (NBTR .NE. 0) CUT = .TRUE.
C
C   Analysis of the flat diagram.
C   Closed circuits of increasing order NC are searched, analysed,
C   and taken out of the flat diagram, thus reducing the number of
C   nodes, NBNODE.
C
      NC = 0
   20 CONTINUE
      NC = NC+1
      CALL SEARCH (FIND
     + , ARR, ARROW, IBUG3, ICROSS, IFIRST, IH, IL, ILAST
     + , IPARTL, IPARTS, IWRITE, J1, J23
     + , J6, J6C, J7, J7C, J8, J8C, J9, J9C
     + , JDEL, JDIAG, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
      IF (.NOT. FIND) GOTO 20
      NCP = NC-2
      JPOL = 0
      IF ((M .EQ. MP) .AND. (NC.GT.3)) CALL SETDM
     + ( J6C, J6CC, J7C, J7CC, J8C
     + , J8CC, J9C, J9CC, JDEL, JDELC, JWC
     + , JWCC
     + )
      IF (IPARTL .GT. 2) CALL POLYGN (JPOL
     + , ARR, ARROW, IBUG3, ICROSS, IFIRST, IH, IL, ILAST
     + , IPARTL, IPARTS, IWRITE, J1, J23
     + , J6, J6C, J7, J7C, J8, J8C, J9, J9C
     + , JDEL, JDIAG, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
      GOTO (30,40,50,60),NC
   30 CONTINUE
      CALL LOLPOP (FAIL
     + , NAMSUB, ARR, IBUG3, IH, IL, ILAST
     + , J1, J6, J6C
     + , J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9
     + , J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC
     + , LDEL, MP, NBNODE
     + , NPOINT, CUT, FREE, SUMVAR
     + )
      IF (FAIL) GOTO 90
      GOTO 70
   40 CONTINUE
      CALL BUBBLE (JPOL,FAIL,
     + ARR, CUT, FREE, IBUG3, IFIRST, IH, IL, ILAST,
     + J1, J6, J6C, J6CC, J7, J7C, J7CC,
     + J8, J8C, J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW,
     + JWC, JWCC, LDEL, MP, NAMSUB, NBNODE,
     + NPOINT, SUMVAR, TAB1)
      IF (FAIL) GOTO 90
      GOTO 70
   50 CONTINUE
      CALL TRIANG (FAIL
     + , NAMSUB, ARR, IFIRST, IH, IL, ILAST
     + , J1
     + , J8, J8C, JDIAG, JW
     + , JWC, NBNODE
     + , NPOINT, TAB1, CUT, FREE, SUMVAR
     + )
      IF (FAIL) GOTO 90
      GOTO 70
   60 CONTINUE
      CALL SQUARE
     + ( NAMSUB, ARR, ICROSS, IH, IL
     + , J6, J6C, J7, J7C, J8, J8C
     + , JDIAG, JW, JWC, MP
     + , NBNODE, NPART, NPOINT, TAB1, SUMVAR
     + )
   70 CONTINUE
      NBNODE = NBNODE-2
      IF (NBNODE .EQ. 0) GOTO 80
      IFIRST = IH(1)
      ILAST = IH(NBNODE)
C
C   PRINTJ is an all purpose printing SUBROUTINE called from many
C   places
C
      CALL PRINTJ (NAMSUB,8,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
      IF (NBNODE .EQ. NFIN) GOTO 80
      NC = NCP
C
C   Proceed to other circuits of order NC-1
C
      GOTO 20
   80 CONTINUE
      IF (NBTR .EQ. 0) GOTO 100
      IF (JUMP .EQ. 3) CALL ORDTRI
     + ( ARR, ARROW, IAL, IBUG3, ICROSS, IF1, IF2, IFIRST
     + , IH, IL, ILAST, IPARTL, IPARTS, IT2, IT3, IT5
     + , ITFREE, IWRITE, J1, J23, J6, J6C
     + , J7, J7C, J8, J8C, J9, J9C, JARR, JDEL
     + , JDIAG, JKP, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NODE, NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
C
C   At this stage, the flat diagram has been reduced to nodes
C   involving free ends. Proceed to build other flat diagrams
C   if necessary.
C
      GOTO 10
C
C   All parts of the original graph have been reduced.
C
   90 CONTINUE
      RECUP = ZERO
      M = M-1
      RETURN
  100 CONTINUE
      CALL PRINTJ (NAME,0,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
C   Preparation of the results, and separation in several sums
C   if cuts have been detected, also in the flat diagram itself
C
      CALL SPRATE (M
     + , INV6J, IWRITE, J6, J6C, J6CC, J6P, J7, J7C
     + , J7CC, J7P, J8, J8C, J8CC, J8P, J9, J9C
     + , J9CC, J9P, JSUM4, JSUM5, JSUM6, JW
     + , JWC, JWORD, K6CP, K7CP, K8CP, K9CP
     + , MP, NB6J, NBJ, NLSUM, CUT, SUMVAR
     + )
      M = M-1
C
C   GENSUM computes the numerical value of the recoupling
C   coefficient
C
      IF (IGEN .NE. -1)
     +CALL GENSUM (
     +RECUP,
     +J1,M,
     +IBUG3,
     +J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     +JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     +K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
C
 3000 FORMAT (//' ++++++++++ NJGRAF ++++++++++'/)
 3010 FORMAT (' Total number of angular momenta (M) = ',1I3//           !
     +' Number of triads in each of the left-hand and right-hand state',!
     +'s (N-1) = ',1I3)
 3020 FORMAT (/' (2J+1)-value for each angular momentum:')
 3030 FORMAT (1X,42I3)
 3040 FORMAT (1X,42L3)
 3050 FORMAT (/' Left-hand triads',10X,'Right-hand triads')
 3060 FORMAT (1X,3I3,19X,3I3)
 3070 FORMAT (/' ***** Error in NJGRAF *****'/)
 3080 FORMAT (' M+1 = ',1I3,', exceeds PARAMETER MANGM = ',1I3)
 3090 FORMAT (' N-1 = ',1I3,', exceeds PARAMETER MTRIAD = ',1I3)
      END
CEND--------------------------------------------------------------------
CEND    BUBBLE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE BUBBLE (JPOL,FAIL,
     + ARR, CUT, FREE, IBUG3, IFIRST, IH, IL, ILAST,
     + J1, J6, J6C, J6CC, J7, J7C, J7CC,
     + J8, J8C, J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW,
     + JWC, JWCC, LDEL, MP, NAMSUB, NBNODE,
     + NPOINT, SUMVAR, TAB1)
CRCS
CRCS $Source: /home/phn/DARC/RCS/BUBBLE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   CUT
C   FREE
C   IBUG3
C   IFIRST
C   IH
C   IL
C   ILAST
C   J1
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   LDEL
C   MP
C   NAMSUB
C   NBNODE
C   NPOINT
C   SUMVAR
C   TAB1
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      INTEGER JPOL
      LOGICAL FAIL
C
      CHARACTER*6 NAMSUB
      INTEGER ARR(M4TRD,*)
      INTEGER IBUG3
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER J1(*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER LDEL(M6J,*)
      INTEGER MP
      INTEGER NBNODE
      INTEGER NPOINT(*)
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,I1,I2,IL1
      INTEGER IT,IT1,IT2,K
      INTEGER K2,K23,L,L1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'BUBBLE'/
C-----------------------------------------------------------------------
      NAMSUB = NAME
      K2 = 2
      K23 = 3
      I1 = 1
      I2 = 1
      IT1 = NPOINT(1)
      IT2 = NPOINT(2)
C
      IF (IT2 .EQ. ILAST) THEN
        IF (IT1 .NE. IFIRST) THEN
          IT2 = IT1
          IT1 = ILAST
        ENDIF
        I1 = -1
        K23 = 2
        I2 = 2
      ENDIF
C
      CALL PHASE (IT1,JDIAG,M4TRD,
     + J7,J7C)
      K = ABS((3*ARR(IT2,1)+2*ARR(IT2,2)+ARR(IT2,3))/2)+1
      IF (K .NE. 4) CALL PHASE2 (JDIAG(IT2,K),
     + J8,J8C)
      IF (NBNODE .EQ. 2) RETURN
      IL1 = IL(IT2)+I1
      IT = IH(IL1)
      ARR(IT,K23) = ARR(IT1,K23)
      L = JDIAG(IT1,K23)
      L1 = JDIAG(IT,K23)
      JDIAG(IT,K23) = L
C
      IF (JPOL .NE. 1) THEN
        CALL DELTA (L,L1,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
        IF (FAIL) RETURN
      ELSE
        MP = MP-1
        JW(2,JWC) = L
        J6(J6C-1) = L
        J6(J6C) = L
        IF (K .EQ. 2) J8(J8C) = L
      ENDIF
C
      TAB1(L,I2) = IT
C
      IF (IT1 .NE. ILAST) THEN
        IF (IT2 .EQ. ILAST) THEN
          TAB1(L,1) = IH(2)
          IL1 = 2
          K2 = 1
        ENDIF
C
        DO I = IL1,NBNODE
          IT = IH(I)
          IL(IT) = I-K2
          IH(I-K2) = IT
        ENDDO
C
      ENDIF
C
      J9(J9C+1) = L
      J9C = J9C+2
      J9(J9C) = L
C
      END
CEND--------------------------------------------------------------------
CEND    CHANGE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CHANGE (L,K,
     + ARR, J7, J7C, JDIAG)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CHANGE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   J7
C   J7C
C   JDIAG
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      INTEGER K,L
C
C  Local variables
C
      INTEGER JAR,JP
C
      INTEGER ARR(M4TRD,*)
      INTEGER J7(*)
      INTEGER J7C
      INTEGER JDIAG(M4TRD,*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      CALL PHASE (L,JDIAG,M4TRD,
     + J7,J7C)
      JP = JDIAG(L,K)
      JDIAG(L,K) = JDIAG(L,1)
      JDIAG(L,1) = JP
      JAR = ARR(L,K)
      ARR(L,K) = ARR(L,1)
      ARR(L,1) = JAR
      END
CEND--------------------------------------------------------------------
CEND    CHKLP1.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CHKLP1 (FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS, IFIRST, IH,
     + IL, ILAST, IPARTL, IPARTS, IWRITE, J1, J23, J6, J6C,
     + J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9, J9C, J9CC,
     + JDEL, JDELC, JDIAG, JW, JWC, JWCC, JZERO, LCOL, LDEL,
     + LINE, M, MP, N, NAMSUB, NBNODE, NBTR, NC, NFIN, NFREE,
     + NPART, NPOINT, NZERO, SUMVAR, TAB1, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CHKLP1.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C
C   ARR
C   ARROW
C   CUT
C   FREE
C   IAL
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NAMSUB
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   SUMVAR
C   TAB1
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      LOGICAL FAIL
C
      CHARACTER*6 NAMSUB
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER JDIF,L,NBTR1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'CHKLP1'/
C-----------------------------------------------------------------------
      NAMSUB = NAME
      NBTR1 = 2*(N-1)
      DO L = 1,NBTR1
        IF (.NOT. TABS(L)) THEN
          JDIF = 0
          IF (J23(L,1) .EQ. J23(L,2)) THEN
            JDIF = J23(L,3)
          ELSEIF (J23(L,1) .EQ. J23(L,3)) THEN
            JDIF = J23(L,2)
          ELSEIF (J23(L,2) .EQ. J23(L,3)) THEN
            JDIF = J23(L,1)
          ENDIF
          IF (JDIF .NE. 0) THEN
C
C   Putting the link to 0. ZEROJ changes NBTR
C
            FAIL = .FALSE.
            IF (J1(JDIF) .NE. 1.AND. .NOT. FREE(JDIF)) THEN
              FAIL = .TRUE.
              IF (IBUG3 .EQ. 1) WRITE (99,3000) JDIF,J1(JDIF)
              RETURN
            ELSE
              CALL ZEROJ (1,JDIF,FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS,
     + IFIRST, IH, IL, ILAST, IPARTL, IPARTS, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C,
     + J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC,
     + NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
              IF (FAIL) RETURN
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF (JDIF .NE. 0) CALL PRINTJ (NAME,4,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
 3000 FORMAT (1X,'JDIF = ',1I2,'; should be 0; J1(JDIF) = ',1I2,        !
     +'; RECUP -> 0.')
      END
CEND--------------------------------------------------------------------
CEND    CHVAR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CHVAR (JP,NBC,KBC,JT,JINV)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CHVAR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER NBC
      INTEGER JINV(*),JP(*),KBC
      LOGICAL JT(*)
C
C  Local variables
C
      INTEGER I,JK,KB
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      KB = KBC+1
C
      IF (KB .LE. NBC) THEN
        DO I = KB,NBC
          JK = JP(I)
          IF (JT(JK)) THEN
            KBC = KBC+1
            JP(I) = JP(KBC)
            JP(KBC) = JINV(JK)
          ENDIF
        ENDDO
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    CUT1L.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CUT1L (FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS, IFIRST, IH,
     + IL, ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9,
     + J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC, JZERO,
     + LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC, NFIN,
     + NFREE, NPART, NPOINT, NZERO, SUMVAR, TAB1, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CUT1L.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   CUT
C   FREE
C   IAL
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   ITFREE
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   SUMVAR
C   TAB1
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      LOGICAL FAIL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER ITFREE(*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,IL1,ILP,IT
      INTEGER J,J0
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'CUT1L '/
C-----------------------------------------------------------------------
      IT = ITFREE(1)
      J0 = JDIAG(IT,1)
      CALL DELTA (J0,M,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) GOTO 10
      CALL DELTA (JDIAG(IT,3),JDIAG(IT,2),FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) GOTO 10
      JDIAG(IT+1,3) = JDIAG(IT,3)
C
      IF (ARR(IT,2) .EQ. ARR(IT,3)) THEN
        ARR(IT+1,3) = 1
        ARR(IT-1,2) = -1
      ELSEIF (ARR(IT,2) .LT. ARR(IT,3)) THEN
        ARR(IT+1,3) = -1
        ARR(IT-1,2) = 1
      ENDIF
C
      J9C = J9C+1
      J9(J9C) = JDIAG(IT,3)
      J = 2
      CALL ZEROJ (J,J0,FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS,
     + IFIRST, IH, IL, ILAST, IPARTL, IPARTS, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C,
     + J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC,
     + NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
      IF (FAIL) GOTO 10
      IL1 = IL(IT+1)
C
      DO I = IL1,NBNODE
        IT = IH(I)
        ILP = I-1
        IL(IT) = ILP
        IH(ILP) = IT
      ENDDO
C
      NBNODE = NBNODE-1
C
   10 CONTINUE
      CALL PRINTJ (NAME,12,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      END
CEND--------------------------------------------------------------------
CEND    CUT2L.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CUT2L (FAIL,
     + ARR, ARROW, CUT, FREE, IBUG3, ICROSS, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C, J8CC,
     + J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR,
     + NC, NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CUT2L.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   CUT
C   FREE
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   ITFREE
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   SUMVAR
C   TAB1
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      LOGICAL FAIL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER ITFREE(*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER IT1,IT2,JT1,JT2
      INTEGER K1,K2,L1,L2
      INTEGER LC1,LC2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'CUT2L '/
C-----------------------------------------------------------------------
      IT1 = ITFREE(1)
      IT2 = ITFREE(2)
      JT1 = JDIAG(IT1,1)
      JT2 = JDIAG(IT2,1)
      CALL DELTA (JT1,JT2,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) GOTO 10
      IF (ARR(IT1,1) .EQ. ARR(IT2,1)) CALL PHASE2 (JT1,
     + J8,J8C)
      ARR(IT2,1) = -ARR(IT1,1)
      JDIAG(IT2,1) = JT1
      TAB1(JT1,2) = IT2
      J9(J9C+1) = JT1
      J9C = J9C+2
      J9(J9C) = JT1
      CALL OTHERJ (0,JT1,L1,LC1,K1,
     + LCOL,LINE,TABS)
      CALL OTHERJ (0,JT2,L2,LC2,K2,
     + LCOL,LINE,TABS)
      J23(L2,LC2) = JT1
      LINE(JT1,K1) = L2
      LCOL(JT1,K1) = LC2
      ARROW(L2,LC2) = -ARROW(L1,LC1)
C
   10 CONTINUE
      CALL PRINTJ (NAME,12,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      END
CEND--------------------------------------------------------------------
CEND    CUTNL.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE CUTNL (FAIL,
     + ARR, ARROW, CUT, FREE, IBUG3, ICROSS, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, IT2, IT3, IT5, ITFREE, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8,
     + J8C, J8CC, J9, J9C, J9CC, JARR, JDEL, JDELC, JDIAG,
     + JKP, JW, JWC, JWCC, JZERO, LCOL, LDEL, LINE, M, MP,
     + N, NBNODE, NBTR, NC, NFIN, NFREE, NPART, NPOINT,
     + NZERO, SUMVAR, TAB1, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/CUTNL.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   CUT
C   FREE
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IT2
C   IT3
C   IT5
C   ITFREE
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JARR
C   JDEL
C   JDELC
C   JDIAG
C   JKP
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   SUMVAR
C   TAB1
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      LOGICAL FAIL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IT2
      INTEGER IT3
      INTEGER IT5
      INTEGER ITFREE(*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JARR(2,*)
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JKP(2,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,IL1,ILP,IT
      INTEGER IT1,IT4,J,JT
      INTEGER K,NFR,NTF
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'CUTNL '/
C-----------------------------------------------------------------------
      NTF = ITFREE(NFREE)-ITFREE(1)
      IF (NTF .GT. NFREE) GOTO 30
      IT2 = ITFREE(1)
      IT3 = ITFREE(NFREE)
      IT1 = IT2-1
      IT4 = IT3+1
C
      IF (NTF .NE. NFREE) THEN
C
        JT = JDIAG(IT2,3)
        CALL DELTA (JT,JDIAG(IT3,2),FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
C
        IF (FAIL) GOTO 20
C
        IF (ARR(IT2,3) .EQ. ARR(IT3,2)) THEN
          CALL PHASE2 (JT,
     + J8,J8C)
          ARR(IT2,3) = -ARR(IT2,3)
          ARR(IT1,2) = -ARR(IT1,2)
        ENDIF
C
        JDIAG(IT3,2) = JT
        JDIAG(IT4,3) = JT
        J9(J9C+1) = JT
        J9C = J9C+2
        J9(J9C) = JT
        NBTR = NBTR+NFREE
        IT5 = 0
C
      ELSE
C
        NFR = 0
C
        DO IT5 = IT2,IT3
          NFR = NFR+1
          IF (ITFREE(NFR) .GT. IT5) GOTO 10
        ENDDO
C
   10   CONTINUE
        JKP(1,1) = JDIAG(IT5,1)
        JARR(1,1) = -ARR(IT5,1)
        JKP(1,2) = JDIAG(IT2,3)
        JARR(1,2) = -ARR(IT2,3)
        JKP(1,3) = JDIAG(IT3,2)
        JARR(1,3) = -ARR(IT3,2)
C
        DO J = 1,3
          JKP(2,J) = JDIAG(IT5,J)
          JARR(2,J) = ARR(IT5,J)
        ENDDO
C
        JDIAG(IT5,2) = JDIAG(IT3,2)
        ARR(IT5,2) = ARR(IT3,2)
        JDIAG(IT5,3) = JDIAG(IT2,3)
        ARR(IT5,3) = ARR(IT2,3)
        ILP = IL(IT2)
        IL(IT5) = ILP
        IH(ILP) = IT5
        NBTR = NBTR+NFREE+2
        CALL PHASE (IT5,JDIAG,M4TRD,
     + J7,J7C)
        K = ABS((3*ARR(IT5,1)+2*ARR(IT5,2)+ARR(IT5,3))/2)+1 ! +1) -> )+1 nrb
        IF (K .NE. 4) CALL PHASE2 (JDIAG(IT5,K),
     + J8,J8C)
C
      ENDIF
C
      IL1 = IL(IT4)
C
      DO I = IL1,NBNODE
        IT = IH(I)
        ILP = I-NFREE
        IL(IT) = ILP
        IH(ILP) = IT
      ENDDO
C
      NBNODE = NBNODE-NFREE
      NFIN = 0
      GOTO 30
C
   20 CONTINUE
      FAIL = .TRUE.
   30 CONTINUE
      CALL PRINTJ (NAME,8,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      END
CEND--------------------------------------------------------------------
CEND    DELTA.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DELTA (JA,JB,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DELTA.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      INTEGER JA,JB
      LOGICAL FAIL
C
      INTEGER IBUG3
      INTEGER J1(*)
      INTEGER J6(*),J6C,J7(*),J7C
      INTEGER J8(*),J8C,J9(*),J9C
      INTEGER JDEL,JW(6,*),JWC
      INTEGER LDEL(M6J,*)
      INTEGER J6CC,J7CC,J8CC,J9CC
      INTEGER JDELC,JWCC
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
C
C  Local variables
C
      INTEGER I,J,J61,J71
      INTEGER J81,J91,JDEL1,JW1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (IBUG3 .EQ. 1) WRITE (99,3000) JA,SUMVAR(JA),JB,SUMVAR(JB)
      IF (SUMVAR(JA) .AND. SUMVAR(JB)) GOTO 10
      IF (FREE(JA) .OR. FREE(JB)) THEN
        JDEL = JDEL+1
        LDEL(JDEL,1) = JA
        LDEL(JDEL,2) = JB
        SUMVAR(JA) = .FALSE.
        SUMVAR(JB) = .FALSE.
        RETURN
      ENDIF
C
      IF (J1(JA) .NE. J1(JB)) FAIL = .TRUE.
      CUT = .TRUE.
      RETURN
C
   10 CONTINUE
      IF (J6C .NE. J6CC) THEN
        J61 = J6CC+1
C
        DO I = J61,J6C
          IF (J6(I) .EQ. JB) J6(I) = JA
        ENDDO
C
      ENDIF
C
      IF (J7C .NE. J7CC) THEN
        J71 = J7CC+1
C
        DO I = J71,J7C
          IF (J7(I) .EQ. JB) J7(I) = JA
        ENDDO
      ENDIF
C
      IF (J8C .NE. J8CC) THEN
        J81 = J8CC+1
C
        DO I = J81,J8C
          IF (J8(I) .EQ. JB) J8(I) = JA
        ENDDO
      ENDIF
C
      IF (J9C .NE. J9CC) THEN
        J91 = J9CC+1
C
        DO I = J91,J9C
          IF (J9(I) .EQ. JB) J9(I) = JA
        ENDDO
      ENDIF
C
      IF (JWC .NE. JWCC) THEN
        JW1 = JWCC+1
C
        DO I = JW1,JWC
          DO J = 1,6
            IF (JW(J,I) .EQ. JB) JW(J,I) = JA
          ENDDO
        ENDDO
      ENDIF
C
      IF (JDEL .NE. JDELC) THEN
        JDEL1 = JDELC+1
C
        DO I = JDEL1,JDEL
          DO J = 1,2
            IF (LDEL(I,J) .EQ. JB) LDEL(I,J) = JA
          ENDDO
        ENDDO
C
        SUMVAR(JB) = .FALSE.
      ENDIF
C
 3000 FORMAT (/'From DELTA: JA = ',I2,L2,5X,'JB = ',I2,L2)
      END
CEND--------------------------------------------------------------------
CEND    DIAGRM.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE DIAGRM (JUMP,
     + ARR, ARROW, IAL, IBUG3, ICROSS, IF1, IF2, IFIRST, IH, IL,
     + ILAST, IPARTL, IPARTS, ITFREE, IWRITE, J1, J23,
     + J6, J6C, J7, J7C, J8, J8C, J9, J9C, JDEL, JDIAG, JW,
     + JWC, JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR,
     + NC, NFIN, NFREE, NODE, NPART, NPOINT, NZERO, TAB1,
     + SUMVAR, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/DIAGRM.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   IAL
C   IBUG3
C   ICROSS
C   IF1
C   IF2
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   ITFREE
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J7
C   J7C
C   J8
C   J8C
C   J9
C   J9C
C   JDEL
C   JDIAG
C   JW
C   JWC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NODE
C   NPART
C   NPOINT
C   NZERO
C   TAB1
C   SUMVAR
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      INTEGER JUMP
C
      INTEGER ARR(M4TRD,*)
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER ARROW(M2TRD,*)
      INTEGER J23(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IF1
      INTEGER IF2
      INTEGER NODE
      INTEGER IBUG3
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER ITFREE(*)
      INTEGER JDIAG(M4TRD,*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER M
      INTEGER N
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J9(*)
      INTEGER J9C
      INTEGER JDEL
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JZERO(*)
      INTEGER NZERO
      INTEGER LCOL(MANGM,*)
      INTEGER LINE(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER MP
      INTEGER NBNODE
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NBTR
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER TAB1(MANGM,*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,I1,ICH,ICT
      INTEGER J,JAR,JB,JT
      INTEGER K1,K2,K3,KP
      INTEGER L,L1,L2,LC
      INTEGER LP,NB,NBP,ND
      INTEGER NTIME
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE NB
C
      DATA NAME/'DIAGRM'/
      DATA NB/0/
C-----------------------------------------------------------------------
C
C   Initialization
C
      IF (JUMP .GT. 2) GOTO 20
      IF (JUMP .LT. 2) NB = 0
   10 CONTINUE
      NB = NB+1
      IF (TABS(NB)) GOTO 10
      NODE = NBTR
      ILAST = NBTR
C
      DO J = 1,3
        JDIAG(NODE,J) = J23(NB,J)
        ARR(NODE,J) = ARROW(NB,J)
      ENDDO
C
      TABS(NB) = .TRUE.
C
      DO I = 1,MP
        IAL(I) = 0
      ENDDO
C
      IF1 = JDIAG(NODE,1)
      IF2 = JDIAG(NODE,3)
      IAL(IF1) = 1
      IAL(IF2) = 1
   20 CONTINUE
      NTIME = 0
      I1 = 1
      K1 = 1
      K2 = 2
      K3 = 3
   30 CONTINUE
      JB = JDIAG(NODE,K2)
      CALL OTHERJ (0,JB,L,LC,KP,
     + LCOL,LINE,TABS)
      CALL NEIBOR (LC,L1,L2)
C
C   Check consistency of triads
C
      IF (TABS(L)) THEN
        WRITE (IWRITE,3000)
        STOP
      ENDIF
C
      CALL WAY (L,L1,L2,ICH,ND,
     + IAL, IF1, IF2, J23, LCOL, LINE, TABS)
      NODE = NODE+I1
      TABS(L) = .TRUE.
      JDIAG(NODE,K3) = J23(L,LC)
      ARR(NODE,K3) = ARROW(L,LC)
      ICT = ICH*I1
C
      IF (ICH .LE. 0) THEN
        LP = L1
        L1 = L2
        L2 = LP
      ENDIF
C
      IF (ICT .LE. 0) CALL PHASE (L,J23,M2TRD,
     + J7,J7C)
      JDIAG(NODE,K1) = J23(L,L1)
      ARR(NODE,K1) = ARROW(L,L1)
      JDIAG(NODE,K2) = J23(L,L2)
      ARR(NODE,K2) = ARROW(L,L2)
      J = J23(L,L1)
      IAL(J) = IAL(J)+1
      J = J23(L,L2)
      IAL(J) = IAL(J)+1
      IF (ND .LT. 1) GOTO 30
      NTIME = NTIME+1
      ILAST = MAX(NODE,ILAST)
      IFIRST = MIN(NODE,NBTR)
      NBP = IAL(IF1)+IAL(IF2)
      IF ((NBP .GT. 3) .OR. (NTIME .GT. 1)) THEN
        NBNODE = ILAST-IFIRST+1
        NBTR = NBTR-NBNODE
C
C   Definition of free ends and other quantities.
C
        CALL INTAB
     + (M,IAL,IFIRST,IH,IL,ILAST,ITFREE,JDIAG,NFREE,TAB1)
        CALL PRINTJ (NAME,12,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
        GOTO 40
      ENDIF
C
      IF (NBP .GT. 2) THEN
        IF (IAL(IF1) .LE. IAL(IF2)) THEN
          JT = JDIAG(NBTR,1)
          JAR = ARR(NBTR,1)
          JDIAG(NBTR,1) = JDIAG(NBTR,3)
          ARR(NBTR,1) = ARR(NBTR,3)
          JDIAG(NBTR,3) = JT
          ARR(NBTR,3) = JAR
          CALL PHASE (NBTR,JDIAG,M4TRD,
     + J7,J7C)
        ENDIF
      ENDIF
C
      NODE = NBTR
      I1 = -1
      K2 = 3
      K3 = 2
      GOTO 30
C
   40 CONTINUE
      RETURN
C
 3000 FORMAT ('DIAGRM: Flat graph impossible to build.')
      END
CEND--------------------------------------------------------------------
CEND    GENSUM.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE GENSUM (
     +RECUP,
     +J1,M,
     +IBUG3,
     +J6C,J7C,J8C,J9C,JWC,J6,J7,J8,J9,JW,
     +JDEL,LDEL,MP,J6P,J7P,J8P,J9P,JWORD,NLSUM,NBJ,NB6J,
     +K6CP,K7CP,K8CP,K9CP,JSUM4,JSUM5,JSUM6,INV6J
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/GENSUM.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IBUG3
C   J1
C   M
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      DOUBLE PRECISION DBLE
C
C  Parameter variables
C
      DOUBLE PRECISION EPSIL
      PARAMETER (EPSIL=1.0D-10)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      INTEGER INV6J(*),J6(*),J6C
      INTEGER J6P(*),J7(*),J7C
      INTEGER J7P(*),J8(*),J8C
      INTEGER J8P(*),J9(*),J9C
      INTEGER J9P(*),JDEL,JSUM4(MTRIAD,*)
      INTEGER JSUM5(MTRIAD,*),JSUM6(*)
      INTEGER JW(6,*),JWC,JWORD(6,*)
      INTEGER K6CP(*),K7CP(*),K8CP(*)
      INTEGER K9CP(*),LDEL(M6J,*),MP
      INTEGER NB6J(*),NBJ(*),NLSUM
      DOUBLE PRECISION RECUP
C
      INTEGER IBUG3
      INTEGER J1(*)
      INTEGER M
C
C  Local variables
C
      DOUBLE PRECISION SPR,SQR,STOR,STOR1,DIJ6CP                    !nrb
      DOUBLE PRECISION WSTOR(M6J),X1,XJ1(MANGM)
      INTEGER I,I1,I1T,I2
      INTEGER I3,I4,I5,IAS
      INTEGER IASTOR,ICHAN,IK                                !,IJ6CP nrb
      INTEGER IK1,IK2,IP
      INTEGER IPAIR(2,2),IST(6),IX2,J
      INTEGER J12(4,MTRIAD,MTRIAD),J6CP,J6F
      INTEGER J7CP,J7F,J8CP,J8F
      INTEGER J9CP,J9F,JB,JJ
      INTEGER JJ1,JJ2,JMAX,JMIN
      INTEGER JSUM(2,M6J),JSUM2(MTRIAD)
      INTEGER JSUM3(MTRIAD),JT1,JT2
      INTEGER JT4,JWJ,JWR,JWRD
      INTEGER JWTEST(M6J),K,KM,LL
      INTEGER MAT(MTRIAD,MTRIAD),MAXLP(MTRIAD)
      INTEGER MM,MXCSVR,NFS,NJ
      INTEGER NO1,NOLP,NPS,NSUM
      INTEGER NSUM1
      LOGICAL LDIAG(MTRIAD),NOEL(MTRIAD)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA MXCSVR/4/
C-----------------------------------------------------------------------
C
C   evaluates all terms in J6, J7, J8, J9, LDEL, JW which do not
C   involve a summation. The result is stored in RECUP and IASTOR
C
      IF (IBUG3 .EQ. 1) THEN
C
        DO I = 1,M
          XJ1(I) = 0.5D00*DBLE(J1(I)-1)
        ENDDO
C
        WRITE (99,3070) (XJ1(I),I = 1,M)
        WRITE (99,3040) NLSUM
        WRITE (99,3080)
      ENDIF
C
      MM = M+1
      J1(MM) = 1
C
C   Test delta functions
C
      J1(MM) = 1
      IF (JDEL .LE. 0) GOTO 10
C
      DO I = 1,JDEL
        I1 = LDEL(I,1)
        I2 = LDEL(I,2)
        IF ((I1 .GT. MM) .OR. (I2 .GT. MM)) THEN
          IF (I1.GT.MM) J1(I1) = J1(I2)
          IF (I2.GT.MM) J1(I2) = J1(I1)
        ELSE
          IF (J1(I1) .NE. J1(I2)) THEN
            RECUP = 0.0D00
            RETURN
          ENDIF
        ENDIF
      ENDDO
C
   10 CONTINUE
      RECUP = 1.0D00
      IF (JWC .NE. 0) THEN
C
C   Multiply RECUP by all Racah coefficients which do not involve a
C   summation
C
        IF (IBUG3 .EQ. 1) WRITE (99,3060)
C
        DO I = 1,JWC
          IF (INV6J(I) .GT. 0) GOTO 20
          DO J = 1,6
            I1 = JW(J,I)
            IST(J) = J1(I1)-1
          ENDDO
C
          CALL DRACAH (IST(1),IST(2),IST(3),IST(4),IST(5),IST(6),X1)
          IF (IBUG3 .EQ. 1) WRITE (99,3030) (XJ1(JW(K,I)),K = 1,6),X1
          RECUP = RECUP*X1
C
   20     CONTINUE
        ENDDO
C
      ENDIF
C
      SQR = 1.0D00
C
      IF (J6C .NE. 0) THEN
        DO I = 1,J6C
          I1 = J6(I)
          SQR = SQR*J1(I1)
        ENDDO
      ENDIF
C
      SPR = 1.0D00
C
      IF (J9C .NE. 0) THEN
        DO I = 1,J9C
          I1 = J9(I)
          SPR = SPR*J1(I1)
        ENDDO
      ENDIF
C
      RECUP = RECUP*SQRT(SQR/SPR)
      IF (ABS(RECUP) .LT. EPSIL) GOTO 270
      IASTOR = 0
C
      IF (J7C .NE. 0) THEN
        DO I = 1,J7C
          I1 = J7(I)
          IASTOR = IASTOR+J1(I1)-1
        ENDDO
      ENDIF
C
      IF (J8C .NE. 0) THEN
        DO I = 1,J8C
          I1 = J8(I)
          IASTOR = IASTOR+2*(J1(I1)-1)
        ENDDO
      ENDIF
C
      IF (NLSUM .LE. 0) THEN
        IASTOR = IASTOR/2
C
C   No summation involved. End of computation
C
        STOR1 = 1.0D00
        STOR = 1.0D00
        IF (MOD (IASTOR,2) .EQ. 1) RECUP = -RECUP
        IF (IBUG3 .EQ. 1) WRITE (99,3010) RECUP
        RETURN
C
      ENDIF
C
C   Evaluation of the part involving summations.
C
      NFS = 0
      JWR = 0
      J6F = 0
      J7F = 0
      J8F = 0
      J9F = 0
      NPS = 0
   30 CONTINUE
      NPS = NPS+1
      IF (IBUG3 .EQ. 1) WRITE (99,3000) NPS
C
C   Loop on the disconnected summations
C
      IAS = 0
      NSUM = NBJ(NPS)-NFS
      JWRD = NB6J(NPS)-JWR
      J6CP = K6CP(NPS)
      J7CP = K7CP(NPS)
      J8CP = K8CP(NPS)
      J9CP = K9CP(NPS)
C
C   The range of values of each summation variable is defined by
C   establishing a matrix of the links between variables.
C   MAT(I,J) contains:
C       I = J  Number of possible values of I due to triangular
C              relations with non-variables, i.e. constants.
C       I > J  Number of links between I and J through constants
C       I < J  Value of the constant, if the above is 1. If not,
C              these values are srored in J12(L,I,J) where there
C              is room for MXCSVR such values (L .LE. 4)
C
      DO I = 1,NSUM
        DO J = 1,NSUM
          MAT(I,J) = 0
        ENDDO
      ENDDO
C
      DO I1 = 1,NSUM
        I1T = I1+NFS
        I2 = JSUM6(I1T)
        DO I3 = 1,I2
          I = JSUM5(I1T,I3)
          J = JSUM4(I1T,I3)
          GOTO (40,50,60,70,80,90),J
C
C   The rows of the IPAIR arrays give limits of summation imposed
C
   40     CONTINUE
          IPAIR(1,1) = JWORD(2,I)
          IPAIR(1,2) = JWORD(5,I)
          IPAIR(2,1) = JWORD(3,I)
          IPAIR(2,2) = JWORD(6,I)
          GOTO 100
C
   50     CONTINUE
          IPAIR(1,1) = JWORD(1,I)
          IPAIR(1,2) = JWORD(5,I)
          IPAIR(2,1) = JWORD(4,I)
          IPAIR(2,2) = JWORD(6,I)
          GOTO 100
C
   60     CONTINUE
          IPAIR(1,1) = JWORD(1,I)
          IPAIR(1,2) = JWORD(6,I)
          IPAIR(2,1) = JWORD(4,I)
          IPAIR(2,2) = JWORD(5,I)
          GOTO 100
C
   70     CONTINUE
          IPAIR(1,1) = JWORD(2,I)
          IPAIR(1,2) = JWORD(6,I)
          IPAIR(2,1) = JWORD(3,I)
          IPAIR(2,2) = JWORD(5,I)
          GOTO 100
C
   80     CONTINUE
          IPAIR(1,1) = JWORD(1,I)
          IPAIR(1,2) = JWORD(2,I)
          IPAIR(2,1) = JWORD(3,I)
          IPAIR(2,2) = JWORD(4,I)
          GOTO 100
C
   90     CONTINUE
          IPAIR(1,1) = JWORD(1,I)
          IPAIR(1,2) = JWORD(3,I)
          IPAIR(2,1) = JWORD(2,I)
          IPAIR(2,2) = JWORD(4,I)
C
  100     CONTINUE
          DO I4 = 1,2
            KM = 0
            DO I5 = 1,2
              IF (IPAIR(I4,I5) .GT. MP) KM = KM+1
            ENDDO
C
            JJ1 = IPAIR(I4,1)
            JJ2 = IPAIR(I4,2)
            IF (KM .EQ. 1) GOTO 110
            IF (KM .GT. 1) GOTO 130
C
C   One variable linked to two constants. Fix the diagonal MAT(I,I)
C
            JT1 = J1(JJ1)-1
            JT2 = J1(JJ2)-1
            JMIN = ABS(JT1-JT2)
            JMAX = JT1+JT2
C
            IF (MAT(I1,I1) .GT. 1) THEN
C
C   If there are several couples of constants, take the more
C   stringent combination
C
              JMIN = MAX(JMIN,JSUM(1,I1))
              JMAX = MIN(JMAX,JSUM(2,I1))
              IF (JMAX .GE. JMIN) THEN
                JSUM(1,I1) = JMIN
                JSUM(2,I1) = JMAX
                MAT(I1,I1) = (JMAX-JMIN)/2+1
                GOTO 130
              ELSE
                RECUP = 0.0D00
                GOTO 260
              ENDIF
            ELSEIF (MAT(I1,I1) .LT. 1) THEN
C
C   First time
C
              MAT(I1,I1) = (JMAX-JMIN)/2+1
              JSUM(1,I1) = JMIN
              JSUM(2,I1) = JMAX
            ENDIF
C
            GOTO 130
C
C   One variable linked to one constant and one variable  non diagonal
C   element
C
  110       CONTINUE
            JT1 = MIN(JJ1,JJ2)
            JT2 = MAX(JJ1,JJ2)-MP
            IF (JT2 .GT. I1) GOTO 130
            JT4 = J1(JT1)-1
            K = MAT(I1,JT2)
            IF (K .EQ. 0) GOTO 120
C
            DO LL = 1,K
              IF (JT4 .EQ. J12(LL,JT2,I1)) GOTO 130
            ENDDO
C
  120       CONTINUE
            K = K+1
            IF (K .GT. MXCSVR) GOTO 130
            MAT(I1,JT2) = K
            J12(K,JT2,I1) = JT4
C
  130       CONTINUE
          ENDDO
        ENDDO
      ENDDO
C
C   Reduce the diagonal elements by taking into account the non
C   diagonal elements, and keep the latter only if needed
C
  140 CONTINUE
      ICHAN = 0
C
      DO I = 1,NSUM
        NOEL(I) = .TRUE.
        I1 = I-1
        IF (I1 .EQ. 0) GOTO 160
        DO J = 1,I1
          IF ((MAT(I,J) .EQ. 0) .OR. (MAT(J,J) .EQ. 0)) GOTO 150
          IK1 = I
          IK2 = J
          CALL RDIAG (I,J,IK1,IK2,ICHAN,MAT,JSUM,J12)
          NOEL(I) = .FALSE.
  150     CONTINUE
        ENDDO
  160   CONTINUE
        IF (I .EQ. NSUM) GOTO 180
        I2 = I+1
C
        DO J = I2,NSUM
          IF ((MAT(J,I) .EQ. 0) .OR. (MAT(J,J) .EQ. 0)) GOTO 170
          IK1 = J
          IK2 = I
          CALL RDIAG (I,J,IK1,IK2,ICHAN,MAT,JSUM,J12)
  170     CONTINUE
        ENDDO
  180   CONTINUE
      ENDDO
C
      IF (ICHAN .NE. 0) GOTO 140
      GOTO 190
C
C   Carry out the summations.
C
  190 CONTINUE
      DO I = 1,NSUM
        JSUM3(I) = 1
        LDIAG(I) = .FALSE.
        IF (MAT(I,I) .EQ. 1) LDIAG(I) = .TRUE.
      ENDDO
C
      DO I = 1,JWRD
        JWTEST(I) = 1
      ENDDO
C
      STOR = 0.0D00
      STOR1 = 1.0D00
      NOLP = 0
      IP = 1
  200 CONTINUE
      NOLP = NOLP+1
C
C   Find the range of JSUM2(NOLP)
C   NOLP is the index  of the summation variable
C
      JMIN = JSUM(1,NOLP)
      JMAX = JSUM(2,NOLP)
      IF (NOEL(NOLP)) GOTO 210
      NO1 = NOLP-1
C
      DO NJ = 1,NO1
        IF (MAT(NOLP,NJ) .EQ. 1) THEN
          JJ1 = MAT(NJ,NOLP)
          JJ2 = JSUM2(NJ)
          JMIN = MAX(JMIN,ABS(JJ2-JJ1))
          JMAX = MIN(JMAX,JJ1+JJ2)
        ELSEIF (MAT(NOLP,NJ) .GT. 1) THEN
          K = MAT(NOLP,NJ)
          JJ2 = JSUM2(NJ)
C
          DO I = 1,K
            JJ1 = J12(I,NJ,NOLP)
            JMIN = MAX(JMIN,ABS(JJ2-JJ1))
            JMAX = MIN(JMAX,JJ1+JJ2)
          ENDDO
C
        ENDIF
C
      ENDDO
C
  210 CONTINUE
      JSUM2(NOLP) = JMIN
      MAXLP(NOLP) = JMAX
      IF (LDIAG(NOLP)) JSUM3(NOLP) = 0
      IF (NOLP .LT. NSUM) GOTO 200
C
      DO JJ = JMIN,JMAX,2
        JSUM2(NSUM) = JJ
C
C   Determine which RACAH coefficients need re-evaluating and
C   set JWTEST appropriately
C
        DO J = IP,NSUM
          IF (JSUM3(J) .LE. 0) GOTO 220
          I2 = JSUM6(J)
C
          DO I1 = 1,I2
            I3 = JSUM5(J,I1)
            JWTEST(I3) = 1
          ENDDO
  220     CONTINUE
        ENDDO
C
        DO J = 1,JWRD
          IF (JWTEST(J) .EQ. 0) GOTO 230
          JWJ = J+JWR
C
          DO I = 1,6
            IF (JWORD(I,JWJ) .LE. MP) THEN
              I1 = JWORD(I,JWJ)
              IST(I) = J1(I1)-1
            ELSE
              I1 = JWORD(I,JWJ)-MP-NFS
              IST(I) = JSUM2(I1)
            ENDIF
          ENDDO
C
          CALL DRACAH (IST(1),IST(2),IST(3),IST(4),IST(5),IST(6),X1)
          WSTOR(J) = X1
          IF (IBUG3 .EQ. 1) THEN
            DO I = 1,6
              XJ1(I) = 0.5D00*DBLE(IST(I))
            ENDDO
C
            WRITE (99,3030) (XJ1(I), I = 1,6),X1
          ENDIF
  230     CONTINUE
        ENDDO
C
C   Form product of Racah coefficients, (2J+1) factors and (-1)
C   factors in STOR1
C
        DO I = 1,JWRD
          STOR1 = STOR1*WSTOR(I)
        ENDDO
C
C   IASTOR contains the power of (-1) which is common to all terms
C
        IX2 = 0
C        IJ6CP = 1                                      !*4 overflwo nrb
        DIJ6CP = 1.0D0
        IF (J6CP .NE. J6F) THEN
          JB = J6F+1
C
          DO I = JB,J6CP
            I1 = J6P(I)-NFS
            DIJ6CP = DIJ6CP*(JSUM2(I1)+1)             !IJ6CP->DIJ6CP nrb
          ENDDO
        ENDIF
C
        IF (J9CP .NE. J9F) THEN
          JB = J9F+1
C
          DO I = JB,J9CP
            I1 = J9P(I)-NFS
            DIJ6CP = DIJ6CP/(JSUM2(I1)+1)             !IJ6CP->DIJ6CP nrb
          ENDDO
        ENDIF
C
C        STOR1 = STOR1*SQRT(DBLE(IJ6CP))                !*4 overflwo nrb
        STOR1 = STOR1*SQRT(DIJ6CP)
C
        IF (J7CP .NE. J7F) THEN
          JB = J7F+1
C
          DO I = JB,J7CP
            I1 = J7P(I)-NFS
            IX2 = IX2+JSUM2(I1)
          ENDDO
        ENDIF
C
        IF (J8CP .NE. J8F) THEN
          JB = J8F+1
C
          DO I = JB,J8CP
            I1 = J8P(I)-NFS
            IX2 = IX2+2*JSUM2(I1)
          ENDDO
        ENDIF
C
        IF (MOD(IX2,2) .EQ. 1) THEN
          IAS = -1
          IX2 = IX2+1
        ENDIF
C
        IX2 = IX2/2
C
C   Add term into STOR and reset STOR1 to 1 ready for next term
C
        IF (MOD(IX2,2) .EQ. 1) STOR1 = -STOR1
        STOR = STOR+STOR1
        STOR1 = 1.0D00
        NSUM1 = NSUM-1
        IF (NSUM1 .EQ. 0) GOTO 240
C
        DO IK = 1,NSUM1
          JSUM3(IK) = 0
        ENDDO
C
        DO IK = 1,JWRD
          JWTEST(IK) = 0
        ENDDO
C
  240   CONTINUE
      ENDDO
C
  250 CONTINUE
      NOLP = NOLP-1
C
      IF (NOLP .NE. 0) THEN
        IF (LDIAG(NOLP)) GOTO 250
        JSUM3(NOLP) = 1
        JSUM2(NOLP) = JSUM2(NOLP)+2
        IF (JSUM2(NOLP) .GT. MAXLP(NOLP)) GOTO 250
        IP = NOLP
C
C   Proceed to next variable
C
        GOTO 200
C
      ENDIF
C
      RECUP = RECUP*STOR
      IF (IBUG3 .EQ. 1) WRITE (99,3050) NPS,STOR,RECUP
      IF (ABS(RECUP) .LT. EPSIL) GOTO 270
      JWR = JWRD+JWR
      NFS = NSUM+NFS
      J6F = J6CP
      J7F = J7CP
      J8F = J8CP
      J9F = J9CP
      IASTOR = IASTOR+IAS
C
C   Proceed to next sum
C
      IF (NPS .LT. NLSUM) GOTO 30
      IASTOR = IASTOR/2
      IF (MOD (IASTOR,2) .NE. 0) RECUP = -RECUP
      IF (IBUG3 .EQ. 1) WRITE (99,3020) RECUP
  260 CONTINUE
      RETURN
C
C   No summations. Check that there are no inconsistencies. Then
C   multiply by (-1) factor and exit
C
  270 CONTINUE
      RECUP = 0.0D00
C
 3000 FORMAT (' Sum Nr.',I3)
 3010 FORMAT (' No summation. Recoupling coefficient = ',G15.8)
 3020 FORMAT (' Recoupling coefficient = ',G15.8)
 3030 FORMAT (6F5.1,10X,G15.8)
 3040 FORMAT (' Number of independent sums:',I3)
 3050 FORMAT (' Sum Nr.',I2,' Sum value = ',G15.8,' RECUP = ',G15.8)
 3060 FORMAT (' Not involving summation variable')
 3070 FORMAT (//' Printout from SUBROUTINE GENSUM'//                    !
     +' Values of angular momenta in *REAL* FORMAT'/(14F5.1))
 3080 FORMAT (/' Racah W functions (6J)'/' Arguments in *REAL* FORMAT',1!
     +8X,'value')
      END
CEND--------------------------------------------------------------------
CEND    INTAB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE INTAB
     + (M,IAL,IFIRST,IH,IL,ILAST,ITFREE,JDIAG,NFREE,TAB1)
CRCS
CRCS $Source: /home/phn/DARC/RCS/INTAB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Local variables
C
      INTEGER I,IFR,IT,ITT
      INTEGER J,K
C
C  Argument variables
C
      INTEGER M
      INTEGER IAL(*)
      INTEGER IFIRST
      INTEGER IH(*),IL(*),ILAST
      INTEGER ITFREE(*),JDIAG(M4TRD,*)
      INTEGER NFREE
      INTEGER TAB1(MANGM,*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      DO I = 1,M
        IAL(I) = 1
      ENDDO
C
      DO I = IFIRST,ILAST
        J = JDIAG(I,1)
        K = IAL(J)
        TAB1(J,K) = I
        IAL(J) = K+1
      ENDDO
C
      IFR = IFIRST-1
C
      DO I = IFIRST,ILAST
        IT = I-IFR
        IL(I) = IT
        IH(IT) = I
      ENDDO
C
      J = JDIAG(IFIRST,3)
      K = IAL(J)
      IF (K .GT. 1) TAB1(J,2) = TAB1(J,1)
      TAB1(J,1) = IFIRST
      IAL(J) = 3
      J = JDIAG(ILAST,2)
      TAB1(J,2) = ILAST
      IAL(J) = 3
      NFREE = 0
C
      DO I = IFIRST,ILAST
        J = JDIAG(I,1)
        IF (IAL(J) .NE. 3) THEN
          NFREE = NFREE+1
          ITT = ILAST+NFREE
          TAB1(J,2) = ITT
          IL(ITT) = NFREE*1000
          ITFREE(NFREE) = I
        ENDIF
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    LOLPOP.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE LOLPOP (FAIL
     + , NAMSUB, ARR, IBUG3, IH, IL, ILAST
     + , J1, J6, J6C
     + , J6CC, J7, J7C, J7CC, J8, J8C, J8CC, J9
     + , J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC
     + , LDEL, MP, NBNODE
     + , NPOINT, CUT, FREE, SUMVAR
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/LOLPOP.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NAMSUB
C   ARR
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   ITFREE
C   J1
C   J2
C   J3
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   LDEL
C   M
C   MP
C   N
C   NBNODE
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   TAB1
C   CUT
C   FREE
C   SUMVAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      LOGICAL FAIL
C
      CHARACTER*6 NAMSUB
      INTEGER ARR(M4TRD,*)
      INTEGER IBUG3
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER J1(*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER LDEL(M6J,*)
      INTEGER MP
      INTEGER NBNODE
      INTEGER NPOINT(*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,I1,I2,I3
      INTEGER IL1,IL2,ILP,IT
      INTEGER K,K1,K2,K3
      INTEGER KP(3),KS(3),L,L1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'LOLPOP'/
      DATA KP/2,3,1/
      DATA KS/0,1,-1/
C-----------------------------------------------------------------------
      NAMSUB = NAME
      I1 = NPOINT(1)
      K3 = 2
      IF (I1 .EQ. ILAST) K3 = 3
      L = JDIAG(I1,K3)
      CALL DELTA (L,MP,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) RETURN
      K = KP(K3)
      IF (ARR(I1,K) .LT. 0) CALL PHASE2 (JDIAG(I1,K),
     + J8,J8C)
      K1 = KS(K3)
      IL1 = IL(I1)+K1
      I2 = IH(IL1)
      L1 = JDIAG(I2,1)
      CALL DELTA (L1,JDIAG(I2,K3),FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) RETURN
      IF (ARR(I2,K3) .EQ. K1) CALL PHASE2 (L1,
     + J8,J8C)
      IL2 = IL(I2)+K1
      I3 = IH(IL2)
      K2 = K3+K1
      JDIAG(I3,K2) = L1
      ARR(I3,K2) = ARR(I2,1)
      J9C = J9C+1
      J9(J9C) = L1
      J6C = J6C+1
      J6(J6C) = JDIAG(I1,1)
      IF (K3 .EQ. 3) RETURN
C
      DO I = 3,NBNODE
        IT = IH(I)
        ILP = I-2
        IL(IT) = ILP
        IH(ILP) = IT
      ENDDO
C
      END
CEND--------------------------------------------------------------------
CEND    NEIBOR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE NEIBOR (LC,L1,L2)
CRCS
CRCS $Source: /home/phn/DARC/RCS/NEIBOR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER L1,L2,LC
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (LC .LT. 2) THEN
        L1 = 2
        L2 = 3
      ELSEIF (LC .EQ. 2) THEN
        L1 = 3
        L2 = 1
      ELSE
        L1 = 1
        L2 = 2
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    ORDTRI.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ORDTRI
     + ( ARR, ARROW, IAL, IBUG3, ICROSS, IF1, IF2, IFIRST
     + , IH, IL, ILAST, IPARTL, IPARTS, IT2, IT3, IT5
     + , ITFREE, IWRITE, J1, J23, J6, J6C
     + , J7, J7C, J8, J8C, J9, J9C, JARR, JDEL
     + , JDIAG, JKP, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NODE, NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/ORDTRI.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   IAL
C   IBUG3
C   ICROSS
C   IF1
C   IF2
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IT2
C   IT3
C   IT5
C   ITFREE
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J7
C   J7C
C   J8
C   J8C
C   J9
C   J9C
C   JARR
C   JDEL
C   JDIAG
C   JKP
C   JW
C   JWC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NODE
C   NPART
C   NPOINT
C   NZERO
C   TAB1
C   SUMVAR
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IF1
      INTEGER IF2
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IT2
      INTEGER IT3
      INTEGER IT5
      INTEGER ITFREE(*)
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J9(*)
      INTEGER J9C
      INTEGER JARR(2,*)
      INTEGER JDEL
      INTEGER JDIAG(M4TRD,*)
      INTEGER JKP(2,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NODE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,I1,I2,IK
      INTEGER ISW,IT,J,K
      INTEGER N0,NB,NBT,NBT1
      INTEGER NBTT,NF,NFR,NFT
      INTEGER NM
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'ORDTRI'/
C-----------------------------------------------------------------------
      DO I = 1,MP
        IAL(I) = 0
      ENDDO
C
      IF (NFIN .NE. 0) THEN
        NBT1 = NBTR-1
        NBT = NBT1+NFIN
        NBTT = NBT+1
        NB = 0
        GOTO 40
      ENDIF
C
      NF = NBTR-ITFREE(1)
C
      IF (IT5 .EQ. 0) THEN
        NBT1 = NBTR-1
        N0 = 0
        NFT = NFREE
        ISW = 2
        GOTO 90
      ENDIF
C
      NFT = IT5-IT2
      NM = NFT+NBTR+1
      NBT1 = NBTR
C
      DO J = 1,3
        JDIAG(NBTR,J) = JKP(1,J)
        ARR(NBTR,J) = JARR(1,J)
      ENDDO
C
C     JT = JDIAG(NM,1)
      N0 = 0
      ISW = 1
      GOTO 90
C
   10 CONTINUE
      N0 = NFT
C
      DO J = 1,3
        JDIAG(NM,J) = JKP(2,J)
        ARR(NM,J) = JARR(2,J)
      ENDDO
C
      NBT1 = NBT1+1
      NFT = IT3-IT5
      ISW = 3
      GOTO 90
C
   20 CONTINUE
      NBT1 = K-NFT
C
   30 CONTINUE
      NODE = NBT1+NFT
      CALL CHANGE (NODE,2,
     + ARR, J7, J7C, JDIAG)
      GOTO 80
C
   40 CONTINUE
      DO I = 1,NBNODE
        I1 = IH(I)
        IF (IL(I1) .GT. ILAST) GOTO 70
        I2 = NBT1+I
        IF (I1 .GT. NBTT) GOTO 50
        IF (I1 .EQ. I2) GOTO 60
        IF (IL(I2) .LE. NBNODE) GOTO 70
C
   50   CONTINUE
        DO J = 1,3
          JDIAG(I2,J) = JDIAG(I1,J)
          ARR(I2,J) = ARR(I1,J)
        ENDDO
C
        IL(I1) = ILAST+I
   60   CONTINUE
        NB = NB+1
        IL(I2) = 0
C
   70   CONTINUE
      ENDDO
C
      IF (NB .NE. NFIN) GOTO 40
      NODE = NBT
   80 CONTINUE
      IF1 = JDIAG(NBTR,1)
      IF2 = JDIAG(NBTR,3)
C
      DO I = NBTR,NODE
        DO K = 1,3
          J = JDIAG(I,K)
          IAL(J) = IAL(J)+1
        ENDDO
      ENDDO
C
      ILAST = NODE
      CALL PRINTJ (NAME,8,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      RETURN
C
   90 CONTINUE
      IF (NF .LE. 0) THEN
        NFR = N0
        I1 = 1
      ELSE
        NFR = NFT+1
        I1 = -1
      ENDIF
C
      DO I = 1,NFT
        IK = NFR+I1*I
        IT = ITFREE(IK)
        K = NBT1+IK
C
        DO J = 1,3
          JDIAG(K,J) = JDIAG(IT,J)
          ARR(K,J) = ARR(IT,J)
        ENDDO
C
      ENDDO
C
      GOTO (10,30,20),ISW
C
      END
CEND--------------------------------------------------------------------
CEND    OTHERJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE OTHERJ (LIN,J,LO,LCO,K,
     + LCOL,LINE,TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/OTHERJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
C
C  Argument variables
C
      INTEGER J,K,LCO,LIN
      INTEGER LO
C
      INTEGER LCOL(MANGM,*)
      INTEGER LINE(MANGM,*)
      LOGICAL TABS(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      LO = LINE(J,1)
C
      IF ((LO .EQ. LIN) .OR. TABS(LO)) THEN
        K = 1
        LO = LINE(J,2)
        LCO = LCOL(J,2)
      ELSE
        K = 2
        LCO = LCOL(J,1)
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    PHASE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PHASE (L,JM,NDIM,
     + J7,J7C)
CRCS
CRCS $Source: /home/phn/DARC/RCS/PHASE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER NDIM
      INTEGER JM(NDIM,*),L
C
      INTEGER J7(*),J7C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      J7(J7C+1) = JM(L,1)
      J7(J7C+2) = JM(L,2)
      J7C = J7C+3
      J7(J7C) = JM(L,3)
      END
CEND--------------------------------------------------------------------
CEND    PHASE2.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PHASE2 (J,
     + J8,J8C)
CRCS
CRCS $Source: /home/phn/DARC/RCS/PHASE2.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER J
C
      INTEGER J8(*),J8C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      J8C = J8C+1
      J8(J8C) = J
      END
CEND--------------------------------------------------------------------
CEND    POLYGN.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE POLYGN (JPOL
     + , ARR, ARROW, IBUG3, ICROSS, IFIRST, IH, IL, ILAST
     + , IPARTL, IPARTS, IWRITE, J1, J23
     + , J6, J6C, J7, J7C, J8, J8C, J9, J9C
     + , JDEL, JDIAG, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/POLYGN.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J7
C   J7C
C   J8
C   J8C
C   J9
C   J9C
C   JDEL
C   JDIAG
C   JW
C   JWC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   TAB1
C   SUMVAR
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Argument variables
C
      INTEGER JPOL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J9(*)
      INTEGER J9C
      INTEGER JDEL
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,IT1,IT2,JAR
      INTEGER JB,JC,JE,NBC
      INTEGER NC1,NC2
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'POLYGN'/
C-----------------------------------------------------------------------
      NC1 = NC+1
      NC2 = NC
      NBC = IPARTL-2
C
   10 CONTINUE
      DO I = 1,NBC
        IT2 = NPOINT(NC1-I)
        IT1 = NPOINT(NC2-I)
        JB = JDIAG(IT1,1)
        JC = JDIAG(IT2,1)
        JDIAG(IT1,1) = JC
        JDIAG(IT2,1) = JB
        JAR = ARR(IT1,1)
        ARR(IT1,1) = ARR(IT2,1)
        ARR(IT2,1) = JAR
        JE = JDIAG(IT1,2)
        MP = MP+1
        SUMVAR(MP) = .TRUE.
        JDIAG(IT1,2) = MP
        JDIAG(IT2,3) = MP
C
        IF (TAB1(JB,1) .EQ. IT1) THEN
          TAB1(JB,1) = IT2
        ELSE
          TAB1(JB,2) = IT2
        ENDIF
C
        IF (TAB1(JC,1) .EQ. IT2) THEN
          TAB1(JC,1) = IT1
        ELSE
          TAB1(JC,2) = IT1
        ENDIF
C
        IF (ARR(IT1,2) .LE. 0) THEN
          CALL PHASE2 (JE,
     + J8,J8C)
          ARR(IT1,2) = 1
          ARR(IT2,3) = -1
        ENDIF
C
        JWC = JWC+1
        JW(1,JWC) = JB
        JW(2,JWC) = MP
        JW(3,JWC) = JE
        JW(4,JWC) = JC
        JW(5,JWC) = JDIAG(IT2,2)
        JW(6,JWC) = JDIAG(IT1,3)
        J6(J6C+1) = MP
        J6C = J6C+2
        J6(J6C) = MP
      ENDDO
C
      NC = NC-NBC
C
      IF (NC .GT. 4) THEN
        NBC = IPARTS-2
        NC1 = IPARTS+1
        NC2 = IPARTS
        GOTO 10
      ENDIF
C
      IF (NPART .NE. 1) THEN
        NPOINT(3) = NPOINT(NC1)
        NPOINT(4) = NPOINT(NC1+1)
      ENDIF
C
      IF (NC .EQ. 2) JPOL = 1
      CALL PRINTJ (NAME,10,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      END
CEND--------------------------------------------------------------------
CEND    PRINTJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE PRINTJ (NAMES,JP,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/PRINTJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER MTAB
      PARAMETER (MTAB=30)
C
C  Argument variables
C
      CHARACTER*6 NAMES
      INTEGER JP
C
      INTEGER J1(*)
      INTEGER M,N
      INTEGER IBUG3
      INTEGER IWRITE
      INTEGER J6(*),J6C,J7(*),J7C
      INTEGER J8(*),J8C,J9(*),J9C
      INTEGER JDEL,JW(6,*),JWC
      INTEGER LDEL(M6J,*),MP
      INTEGER ARR(M4TRD,*),ICROSS,IFIRST
      INTEGER IH(*),IL(*),ILAST,IPARTL
      INTEGER IPARTS,JDIAG(M4TRD,*)
      INTEGER NBNODE,NC,NFIN,NFREE
      INTEGER NPART,NPOINT(*)
      INTEGER TAB1(MANGM,*)
      INTEGER ARROW(M2TRD,*),J23(M2TRD,*)
      INTEGER LCOL(MANGM,*),LINE(MANGM,*)
      INTEGER NBTR
      INTEGER JZERO(*),NZERO
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*4 I6,I7,I8,I9
      CHARACTER*8 IBLANK,IFR,IFREE
      CHARACTER*4 IJ1
      CHARACTER IM,IP,IS(3)
      CHARACTER*6 NSETTB
      INTEGER I,IT,J,JT
      INTEGER JTAB(MTAB,3),JUMP,K
      INTEGER MM,NBTR1,NTIME
      INTEGER I6C,I7C,I8C,I9C,IDEL,IWC
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SAVE I6C,I7C,I8C,I9C,IDEL,IWC
C
      DATA IBLANK,IFREE,IP,IM/'        ','FREE END','+','-'/
      DATA NSETTB/'SETTAB'/
      DATA I6,I7,I8,I9,IJ1/'I6=','I7=','I8=','I9=','J1='/
      DATA I6C,I7C,I8C,I9C,IDEL,IWC/1,1,1,1,1,1/
C-----------------------------------------------------------------------
      IF (IBUG3 .NE. 1) RETURN
      WRITE (99,3190) NAMES
C
      JUMP = JP
      IF (JUMP .EQ. 0) THEN
C
        I6C = 1
        I7C = 1
        I8C = 1
        I9C = 1
        IDEL = 1
        IWC = 1
C
        WRITE (99,3140) IJ1,(J1(I),I = 1,M)
C
      ENDIF
C
      IF (JUMP .LT. 8) GOTO 30
      WRITE (99,3020) NBNODE,NBTR,NFIN,IFIRST,ILAST,NFREE
      JUMP = JUMP-8
      WRITE (99,3030)
      K = 0
C
      DO I = 1,NBNODE
        IT = IH(I)
        IFR = IBLANK
        JT = JDIAG(IT,1)
C
        IF ((TAB1(JT,2) .NE. IT) .OR. (JT .EQ. JDIAG(IFIRST,3))) THEN
          K = K+1
          IF (K .GT. MTAB) THEN
            WRITE (IWRITE,3000) K,MTAB
            STOP
          ENDIF
          JTAB(K,1) = JT
          JTAB(K,2) = TAB1(JT,1)
          JTAB(K,3) = TAB1(JT,2)
        ENDIF
C
        IF (TAB1(JT,2) .GT. ILAST) IFR = IFREE
C
        DO J = 1,3
          IS(J) = IP
          IF (ARR(IT,J) .LT. 1) IS(J) = IM
        ENDDO
C
        WRITE (99,3040) (IS(J),J = 1,3)
        WRITE (99,3050) IL(IT),IT,IFR,(JDIAG(IT,J),J = 1,3)
C
      ENDDO
C
      WRITE (99,3060)
      NTIME = 0
      JT = JDIAG(IFIRST,3)
      IF (JT .NE. JDIAG(ILAST,2)) THEN
        IF (TAB1(JT,2) .LT. 1000) GOTO 20
      ENDIF
   10 CONTINUE
      K = K+1
      IF (K .GT. MTAB) THEN
        WRITE (IWRITE,3010) K,MTAB
        STOP
      ENDIF
      JTAB(K,1) = JT
      JTAB(K,2) = TAB1(JT,1)
      JTAB(K,3) = TAB1(JT,2)
   20 CONTINUE
      NTIME = NTIME+1
C
      IF (NTIME .NE. 2) THEN
        JT = JDIAG(ILAST,2)
        IF (TAB1(JT,2) .EQ. 1000) GOTO 10
      ENDIF
C
      WRITE (99,3070) ((JTAB(I,J),J = 1,3),I = 1,K)
      WRITE (99,3080) (I,SUMVAR(I),I = 1,MP)
   30 CONTINUE
      IF (JUMP .LT. 4) GOTO 50
      JUMP = JUMP-4
      NBTR1 = 2*N-2
      WRITE (99,3090) NBTR1
      K = 0
C
      DO I = 1,NBTR1
        IF (TABS(I)) GOTO 40
        K = K+1
C
        DO J = 1,3
          IS(J) = IP
          IF (ARROW(I,J) .LT. 1) IS(J) = IM
        ENDDO
C
        WRITE (99,3100) (IS(J),J = 1,3)
        WRITE (99,3110) K,I,(J23(I,J),J = 1,3)
C
   40   CONTINUE
      ENDDO
C
      WRITE (99,3120)
      MM = M
      IF (NAMES .NE. NSETTB) MM = M-1
      WRITE (99,3130) (I,(LINE(I,J),LCOL(I,J),J = 1,2),I = 1,MM)
C
   50 CONTINUE
      IF (JUMP .GE. 2) THEN
        JUMP = JUMP-2
        WRITE (99,3170) NC,NPART,IPARTL,IPARTS,ICROSS,
     +                  (NPOINT(I),I = 1,NC)
      ENDIF
C
      IF (JUMP .GE. 1) WRITE (99,3180) NZERO,(I,JZERO(I),I = 1,NZERO)
      IF (J6C .GE. I6C) WRITE (99,3140) I6,(J6(I),I = I6C,J6C)
      IF (J7C .GE. I7C) WRITE (99,3140) I7,(J7(I),I = I7C,J7C)
      IF (J8C .GE. I8C) WRITE (99,3140) I8,(J8(I),I = I8C,J8C)
      IF (J9C .GE. I9C) WRITE (99,3140) I9,(J9(I),I = I9C,J9C)
      IF (JDEL .GE. IDEL)
     +             WRITE (99,3150) ((LDEL(I,J),J = 1,2),I = IDEL,JDEL)
      IF (JWC .GE. IWC) WRITE (99,3160) ((JW(J,I),J = 1,6),I = IWC,JWC)
      I6C = J6C+1
      I7C = J7C+1
      I8C = J8C+1
      I9C = J9C+1
      IDEL = JDEL+1
      IWC = JWC+1
C
 3000 FORMAT (' Dimension error in PRINTJ. K = ',I5,' MTAB = ',I5)
 3010 FORMAT (' Dimension error IN PRINTJ. K = ',I5,' MTAB = ',I5)
 3020 FORMAT (/10X,'NBNODE = ',I3,10X,'NBTR = ',I3,10X,'NFIN = ',I3/10X,!
     +'IFIRST = ',I3,10X,'ILAST = ',I3,9X,'NFREE = ',I3)
 3030 FORMAT (//7X,'IL',3X,'IH',14X,'JDIAG'//)
 3040 FORMAT (28X,3(A1,2X))
 3050 FORMAT (7X,I2,3X,I2,2X,A8,2X,3I3/)
 3060 FORMAT (/5X,'TAB1'/)
 3070 FORMAT (4(I3,')',2X,I3,I5,5X))
 3080 FORMAT (/2X,'SUMVAR = ',15(I3,L1))
 3090 FORMAT (//10X,'J23',10X,'NBTR1 = ',I3//)
 3100 FORMAT (18X,3(A1,2X))
 3110 FORMAT (I9,I5,2X,3I3/)
 3120 FORMAT (/3X,'J  L1 K1  L2 K2')
 3130 FORMAT (4(I4,')',I3,I3,I4,I3))
 3140 FORMAT (/3X,A4,3X,3(20I3/))
 3150 FORMAT (/3X,'DELTA = ',7(I5,I3))
 3160 FORMAT (/3X,'JW(ARG. OF 6J)',6I3)
 3170 FORMAT (//2X,'NC = ',I2,4X,'NPART = ',I2,4X,'IPARTL = ',I2,4X,    !
     +'IPARTS = ',I2,4X,'ICROSS = ',I2,4X/2X,'NPOINT = ',20I3)
 3180 FORMAT (//2X,'NZERO = ',I2,5X,12(I4,')',I3))
 3190 FORMAT (///3X,'Printout after calling SUBROUTINE ',A7)
      END
CEND--------------------------------------------------------------------
CEND    RDIAG.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE RDIAG (I,J,IK1,IK2,ICHAN,MAT,JSUM,J12)
CRCS
CRCS $Source: /home/phn/DARC/RCS/RDIAG.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
C
C  Argument variables
C
      INTEGER I,ICHAN,IK1,IK2
      INTEGER J,J12(4,MTRIAD,*)
      INTEGER JSUM(2,*),MAT(MTRIAD,*)
C
C  Local variables
C
      INTEGER JJ1,JMAX,JMAX1,JMIN
      INTEGER JMIN1,JMN,JMNP(5),JMX
      INTEGER JMXP(5),JND,K,K1
      INTEGER L1,L2,L3
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      JMIN1 = 0
      JMAX1 = 1000
      K = MAT(IK1,IK2)
C
      DO L1 = 1,K
C
        L3 = MAT(J,J)
        JJ1 = JSUM(1,J)
        JND = J12(L1,IK2,IK1)
        JMIN = 1000
        JMAX = 0
        JMNP(L1) = 0
        JMXP(L1) = 1000
C
        DO L2 = 1,L3
C
          JMN = ABS(JND-JJ1)
          JMX = JND+JJ1
          JMIN = MIN(JMN,JMIN)
          JMAX = MAX(JMX,JMAX)
          JMNP(L1) = MAX(JMN,JMNP(L1))
          JMXP(L1) = MIN(JMX,JMXP(L1))
          JJ1 = JJ1+2
C
        ENDDO
C
        JMIN1 = MAX(JMIN1,JMIN)
        JMAX1 = MIN(JMAX1,JMAX)
C
      ENDDO
C
      IF (MAT(I,I) .EQ. 0) THEN
        JSUM(1,I) = JMIN1
        JSUM(2,I) = JMAX1
        MAT(I,I) = (JMAX1-JMIN1)/2+1
        ICHAN = ICHAN+1
        GOTO 10
      ENDIF
C
      IF (JSUM(1,I) .LT. JMIN1) THEN
        JSUM(1,I) = JMIN1
        ICHAN = ICHAN+1
      ENDIF
C
      IF (JSUM(2,I) .GT. JMAX1) THEN
        JSUM(2,I) = JMAX1
        ICHAN = ICHAN+1
      ENDIF
C
   10 CONTINUE
      K1 = 0
C
      DO L1 = 1,K
        IF ((JMNP(L1) .LE. JSUM(1,I)) .AND.(JMXP(L1) .GE. JSUM(2,I)))
     +      GOTO 20
        K1 = K1+1
        J12(K1,IK2,IK1) = J12(L1,IK2,IK1)
   20   CONTINUE
      ENDDO
C
      IF (K1 .NE. K) THEN
        MAT(IK1,IK2) = K1
        ICHAN = ICHAN+1
      ENDIF
C
      MAT(IK2,IK1) = J12(1,IK2,IK1)
C
      END
CEND--------------------------------------------------------------------
CEND    SEARCH.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SEARCH (FIND
     + , ARR, ARROW, IBUG3, ICROSS, IFIRST, IH, IL, ILAST
     + , IPARTL, IPARTS, IWRITE, J1, J23
     + , J6, J6C, J7, J7C, J8, J8C, J9, J9C
     + , JDEL, JDIAG, JW, JWC, JZERO, LCOL, LDEL, LINE
     + , M, MP, N, NBNODE, NBTR, NC, NFIN, NFREE
     + , NPART, NPOINT, NZERO, TAB1, SUMVAR, TABS
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/SEARCH.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J7
C   J7C
C   J8
C   J8C
C   J9
C   J9C
C   JDEL
C   JDIAG
C   JW
C   JWC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   TAB1
C   SUMVAR
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      LOGICAL FIND
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J9(*)
      INTEGER J9C
      INTEGER JDEL
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,I1,I2,I20
      INTEGER I21,I3,I30,I31
      INTEGER I4,IC,IDIST,II
      INTEGER II1,II2,IN,IPS
      INTEGER IT,JA,JB,JC
      INTEGER JD,JPS,JT,K2
      INTEGER K3,K4,NBN,NC2
      INTEGER NCM,NCM1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'SEARCH'/
C-----------------------------------------------------------------------
C
C   Initialization
C
      FIND = .FALSE.
      NCM1 = NC-1
      NCM = NC-2
      ICROSS = 0
C
C   First treat two cases that do not involve do loops:
C
C   1. One isolated point, either the first or the last
C
      NPART = 1
      IPARTL = NC-1
      IPARTS = 1
C
C   A. First
C
      I1 = IFIRST
      K3 = 3
      K2 = 2
   10 CONTINUE
      JA = JDIAG(I1,1)
      JC = JDIAG(I1,K3)
C
      IF (JA .EQ. JC) THEN
        IF (NC .GT. 1) THEN
          WRITE (IWRITE,3000) I1,K3,JA,JC,NC
          STOP
        ENDIF
        NPOINT(1) = I1
        GOTO 80
      ENDIF
C
      I2 = TAB1(JA,K2)
      I3 = TAB1(JC,K2)
C
      IF (ABS(IL(I3)-IL(I2))-NCM .LT. 0) THEN
        WRITE (IWRITE,3010) I2,I3,JA,JC,K2,NC
        STOP
      ENDIF
C
      IF (ABS(IL(I3)-IL(I2))-NCM .GT. 0) THEN
C
C   B. Last
C
        IF (I1 .NE. IFIRST) GOTO 20
        I1 = ILAST
        K3 = 2
        K2 = 1
        GOTO 10
      ENDIF
C
      IC = 1
      NPOINT(IC) = I1
      I20 = MIN(I2,I3)
      I21 = IL(I20)
      I31 = I21+NCM1
C
      DO II = I21,I31
        IC = IC+1
        NPOINT(IC) = IH(II)
      ENDDO
C
      IF (NC .LE. 2) THEN
        IF (JDIAG(IFIRST,1) .NE. JDIAG(ILAST,1))
     +    CALL PHASE (I1,JDIAG,M4TRD,
     + J7,J7C)
        GOTO 80
      ENDIF
C
      IF (I1 .NE. ILAST) THEN
        IT = I2
        JT = JDIAG(ILAST,2)
        K4 = 2
        I4 = ILAST
      ELSE
        IT = I3
        JT = JDIAG(IFIRST,3)
        K4 = 3
        I4 = IFIRST
      ENDIF
C
      IF (IT .EQ. I20) CALL PHASE (I1,JDIAG,M4TRD,
     + J7,J7C)
      IF ((JT .EQ.JA) .OR. (JT.EQ.JC)) CALL CHANGE (I4,K4,
     + ARR, J7, J7C, JDIAG)
      GOTO 80
C
C   2. Two isolated points,first and last
C
   20 CONTINUE
      IF (NC .EQ. 1) RETURN
      IF (NC .LE. 3) GOTO 30
      IPARTL = NC-2
      IPARTS = 1
      I1 = IFIRST
      I2 = ILAST
      JA = JDIAG(I1,1)
      JB = JDIAG(I1,3)
C
      IF (TAB1(JA,2) .NE. I2) THEN
        JA = JDIAG(I1,3)
        JB = JDIAG(I1,1)
        IF (TAB1(JA,2) .NE. I2) GOTO 30
      ENDIF
C
      IF (JA .EQ. JDIAG(I2,1)) THEN
        JC = JDIAG(I2,2)
      ELSE
        JC = JDIAG(ILAST,1)
      ENDIF
C
      I3 = TAB1(JB,2)
      I4 = TAB1(JC,1)
      IDIST = IL(I4)-IL(I3)
C
      IF (ABS(IDIST)-(NCM-1) .LT. 0) THEN
        WRITE (IWRITE,3020) I3,I4,JB,JC,IDIST,NC
        STOP
      ENDIF
      IF (ABS(IDIST)-(NCM-1) .EQ. 0) THEN
        NPOINT(1) = ILAST
        NPOINT(2) = IFIRST
        ICROSS = SIGN(1,IDIST)
        IC = 2
        I20 = MIN(I3,I4)
        I21 = IL(I20)
        I31 = I21+NCM
C
        DO II = I21,I31
          IC = IC+1
          NPOINT(IC) = IH(II)
        ENDDO
C
        IF (JA .EQ. JDIAG(IFIRST,1)) CALL CHANGE (IFIRST,3,
     + ARR, J7, J7C, JDIAG)
        IF (JA .EQ. JDIAG(ILAST,1)) CALL CHANGE (ILAST,2,
     + ARR, J7, J7C, JDIAG)
        GOTO 80
      ENDIF
C
C   First general case: all points in one group
C
   30 CONTINUE
      NPART = 1
      IPARTS = 0
      IPARTL = NC
      K3 = 1
C
      DO IN = 1,NBNODE
        I = IH(IN)
   40   CONTINUE
        JA = JDIAG(I,K3)
        IF (I .NE. TAB1(JA,2)) THEN
          I2 = TAB1(JA,2)
C
          IF (IL(I2)-IN-NCM1 .LT. 0) THEN
            WRITE (IWRITE,3030) IN,I,I2,IL(I2),JA,NC
            STOP
          ENDIF
          IF (IL(I2)-IN-NCM1 .EQ. 0) THEN
            I21 = IL(I2)
            IC = 0
C
            DO II = IN,I21
              IC = IC+1
              NPOINT(IC) = IH(II)
            ENDDO
C
            IF (JA .EQ. JDIAG(IFIRST,3)) CALL CHANGE (IFIRST,3,
     + ARR, J7, J7C, JDIAG)
            IF (JA .EQ. JDIAG(ILAST,2)) CALL CHANGE (ILAST,2,
     + ARR, J7, J7C, JDIAG)
            GOTO 80
          ENDIF
        ENDIF
C
        IF (IN .EQ. 1) THEN
          IF (K3 .NE. 3) THEN
            K3 = 3
            GOTO 40
          ELSE
            K3 = 1
          ENDIF
        ENDIF
C
      ENDDO
C
C   Search did not find loop NC .LE. 3
C
      IF (NC .LE. 3) RETURN
C
C   General case of loop partitionned in 2 groups. DO loop
C   on IPARTS
C
      NPART = 2
      NC2 = NC/2
      K3 = 1
      K2 = 1
C
      DO IPS = 2,NC2
        JPS = IPS-1
        NBN = NBNODE-JPS
C
        DO I1 = 1,NBN
          I = IH(I1)
          I2 = IH(I1+JPS)
   50     CONTINUE
          JA = JDIAG(I,K3)
          JD = JDIAG(I2,K2)
C
          IF (I .EQ. TAB1(JA,1)) THEN
            II2 = TAB1(JD,2)
            II1 = TAB1(JA,2)
          ELSE
            II1 = TAB1(JA,1)
            II2 = TAB1(JD,1)
          ENDIF
C
          IDIST = IL(II1)-IL(II2)
C
          IF (ABS (IDIST)-(NCM-JPS) .LT. 0) THEN
            WRITE (IWRITE,3040) JPS,I1,I,I2,JA,JD,II1,II2,IDIST,NC
            STOP
          ENDIF
          IF (ABS (IDIST)-(NCM-JPS) .GT. 0) GOTO 60
          ICROSS = SIGN(1,IDIST)
          IC = 0
          I21 = IL(I2)
C
          DO II = I1,I21
            IC = IC+1
            NPOINT(IC) = IH(II)
          ENDDO
C
          I20 = MIN(II1,II2)
          I30 = MAX(II1,II2)
          I21 = IL(I20)
          I31 = IL(I30)
C
          DO II = I21,I31
            IC = IC+1
            NPOINT(IC) = IH(II)
          ENDDO
C
          IPARTS = IPS
          IPARTL = NC-IPS
          IF ((JDIAG(IFIRST,3) .EQ. JA) .OR.(JDIAG(IFIRST,3) .EQ. JD))
     +       CALL CHANGE (IFIRST,3,
     + ARR, J7, J7C, JDIAG)
          IF ((JDIAG(ILAST,2) .EQ. JA) .OR.(JDIAG(ILAST,2) .EQ. JD))
     +       CALL CHANGE (ILAST,2,
     + ARR, J7, J7C, JDIAG)
          GOTO 80
C
   60     CONTINUE
          IF (I1 .EQ. 1) THEN
            IF (K3 .EQ. 3) THEN
              K3 = 1
              GOTO 70
            ELSE
              K3 = 3
              GOTO 50
            ENDIF
          ENDIF
C
          IF (I2 .EQ. ILAST) THEN
            IF (K2 .NE. 2) THEN
              K2 = 2
              GOTO 50
            ENDIF
          ENDIF
C
   70     CONTINUE
        ENDDO
      ENDDO
C
C   SEARCH did not find circuit of order NC
C
      RETURN
C
C   Loop found
C
   80 CONTINUE
      FIND = .TRUE.
      CALL PRINTJ (NAME,10,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
C   Error printout
C
 3000 FORMAT (' Error in SEARCH. I1,K3,JA,JC,NC = ',5I5)
 3010 FORMAT (' Error in SEARCH. I2,I3,JA,JC,K2,NC = ',6I5)
 3020 FORMAT (' Error in SEARCH. I3,I4,JB,JC,IDIST,NC = ',6I5)
 3030 FORMAT (' Error in SEARCH. IN,I,I2,IL(I2),JA,NC = ',6I5)
 3040 FORMAT (' Error in SEARCH. JPS,I1,I,I2,JA,JD,II1,II2,IDIST,NC = ',!
     +10I5)
      END
CEND--------------------------------------------------------------------
CEND    SETDM.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SETDM
     + ( J6C, J6CC, J7C, J7CC, J8C
     + , J8CC, J9C, J9CC, JDEL, JDELC, JWC
     + , JWCC
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/SETDM.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   J6C
C   J6CC
C   J7C
C   J7CC
C   J8C
C   J8CC
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JWC
C   JWCC
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JWC
      INTEGER JWCC
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      JWCC = JWC
      JDELC = JDEL
      J6CC = J6C
      J7CC = J7C
      J8CC = J8C
      J9CC = J9C
      END
CEND--------------------------------------------------------------------
CEND    SETTAB.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SETTAB (FAIL
     + , ARR, ARROW, IAL, IBUG3, ICROSS, IFIRST
     + , IH, IL, ILAST, IPARTL, IPARTS, IWRITE, J1
     + , J2, J23, J3, J6, J6C, J6CC, J7, J7C
     + , J7CC, J8, J8C, J8CC, J9, J9C, J9CC, JDEL
     + , JDELC, JDIAG, JW, JWC, JWCC, JZERO, LCOL, LDEL
     + , LINE, M, MP, N, NBNODE, NBTR, NC, NFIN
     + , NFREE, NPART, NPOINT, NZERO, TAB1, CUT, FREE
     + , SUMVAR, TABS
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/SETTAB.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   IAL
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IWRITE
C   J1
C   J2
C   J23
C   J3
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   TAB1
C   CUT
C   FREE
C   SUMVAR
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
C
C  Argument variables
C
      LOGICAL FAIL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J2(MTRIAD,*)
      INTEGER J23(M2TRD,*)
      INTEGER J3(MTRIAD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,II,IPR,IPR1
      INTEGER IT,J,JI,JT
      INTEGER JT1,K,KC,L
      INTEGER LC,NB1
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'SETTAB'/
C-----------------------------------------------------------------------
      DO I = 1,M2TRD
        DO J = 1,3
          J23(I,J) = 0
        ENDDO
      ENDDO
C
      IPR = N-1
      NBTR = IPR+IPR
C
      DO I = 1,IPR
        DO J = 1,2
          J23(I,J) = J2(I,J)
          ARROW(I,J) = 1
        ENDDO
        TABS(I) = .FALSE.
        J23(I,3) = J2(I,3)
        ARROW(I,3) = -1
      ENDDO
C
      IPR1 = IPR+1
C
      DO I = IPR1,NBTR
        II = I-IPR
        DO J = 1,2
          J23(I,J) = J3(II,J)
          ARROW(I,J) = -1
        ENDDO
        TABS(I) = .FALSE.
        J23(I,3) = J3(II,3)
        ARROW(I,3) = 1
      ENDDO
C
      DO J = 1,NBTR
        J8(J) = J23(J,1)
      ENDDO
C
      J8C = NBTR+IPR
      NB1 = NBTR+1
C
      DO J = NB1,J8C
        I = J-IPR
        J8(J) = J23(I,3)
      ENDDO
C
      J6C = NBTR
C
      DO J = 1,J6C
        J6(J) = J23(J,3)
      ENDDO
C
      DO I = 1,M
        SUMVAR(I) = .FALSE.
        IAL(I) = 1
      ENDDO
C
      DO I = 1,NBTR
        DO J = 1,3
          JI = J23(I,J)
          K = IAL(JI)
          LINE(JI,K) = I
          LCOL(JI,K) = J
          IAL(JI) = K+1
        ENDDO
      ENDDO
C
      IT = 0
C
      DO I = 1,NBTR
C
        JT = J23(I,3)
C
        IF (IAL(JT) .EQ. 3) THEN
C
          CALL OTHERJ (I,JT,L,LC,K,
     + LCOL,LINE,TABS)
          IF (LC .EQ. 3) GOTO 10
C
        ELSE
C
          IF (IT .EQ. 1) THEN
            CALL DELTA (JT1,JT,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
            IF (FAIL) GOTO 20
            K = LINE(JT,1)
            KC = LCOL(JT,1)
            LINE(JT1,2) = K
            LCOL(JT1,2) = KC
            LINE(JT,2) = LINE(JT1,1)
            LCOL(JT,2) = LCOL(JT1,1)
            J23(K,KC) = JT1
            IAL(JT) = 1
            GOTO 10
          ENDIF
C
          JT1 = JT
          IT = 1
C
        ENDIF
C
      ENDDO
C
   10 CONTINUE
      J9(J9C+1) = JT
      J9C = J9C+2
      J9(J9C) = JT
C
   20 CONTINUE
      CALL PRINTJ (NAME,4,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
C
      END
CEND--------------------------------------------------------------------
CEND    SPRATE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SPRATE (M
     + , INV6J, IWRITE, J6, J6C, J6CC, J6P, J7, J7C
     + , J7CC, J7P, J8, J8C, J8CC, J8P, J9, J9C
     + , J9CC, J9P, JSUM4, JSUM5, JSUM6, JW
     + , JWC, JWORD, K6CP, K7CP, K8CP, K9CP
     + , MP, NB6J, NBJ, NLSUM, CUT, SUMVAR
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/SPRATE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   INV6J
C   IWRITE
C   J6
C   J6C
C   J6CC
C   J6P
C   J7
C   J7C
C   J7CC
C   J7P
C   J8
C   J8C
C   J8CC
C   J8P
C   J9
C   J9C
C   J9CC
C   J9P
C   JSUM4
C   JSUM5
C   JSUM6
C   JW
C   JWC
C   JWORD
C   K6CP
C   K7CP
C   K8CP
C   K9CP
C   MP
C   NB6J
C   NBJ
C   NLSUM
C   CUT
C   SUMVAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M3MNGM
      PARAMETER (M3MNGM=3*MANGM)
      INTEGER MANGMP
      PARAMETER (MANGMP=2*(MANGM/3))
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER MSUM
      PARAMETER (MSUM=10)
C
C  Argument variables
C
      INTEGER M
C
      INTEGER INV6J(*)
      INTEGER IWRITE
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J6P(*)
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J7P(*)
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J8P(*)
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER J9P(*)
      INTEGER JSUM4(MTRIAD,*)
      INTEGER JSUM5(MTRIAD,*)
      INTEGER JSUM6(*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWORD(6,*)
      INTEGER K6CP(*)
      INTEGER K7CP(*)
      INTEGER K8CP(*)
      INTEGER K9CP(*)
      INTEGER MP
      INTEGER NB6J(*)
      INTEGER NBJ(*)
      INTEGER NLSUM
      LOGICAL CUT
      LOGICAL SUMVAR(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      CHARACTER*5 NME
      INTEGER I,I1,I2,I3
      INTEGER I6J,IK,IN6J(M6J)
      INTEGER INVER(MANGM),ISK,ISU
      INTEGER J,J1,J6CP,J6J
      INTEGER J6SUM(M6J),J7CP,J8CP,J9CP
      INTEGER JINV(MTRIAD),JJ,JK
      INTEGER JNSUM(MTRIAD),JS6
      INTEGER JSUMT(M6J,6),JTEM4(MTRIAD,M6J)
      INTEGER JTEM5(MTRIAD,M6J),JTEM6(MTRIAD)
      INTEGER K,K6C,K7C,K8C
      INTEGER K9C,KT,M1,N6J
      INTEGER N6JN(M6J),NJ,NMX,NPX
      INTEGER NSUM,NSUM6J(M6J)
      LOGICAL JS(MTRIAD),JT(MTRIAD),SUM6J(M6J)
      LOGICAL T6J(M6J)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C   Test that array dimensions have not been exceeded.
C
      IF (MP .GT. MANGM) THEN
        NMX = MANGM
        NPX = MP
        NAME = 'MANGM '
        NME = 'MP   '
      ELSEIF (JWC .GT. M6J) THEN
        NMX = M6J
        NPX = JWC
        NAME = 'M6J   '
        NME = 'JWC  '
      ELSEIF (J6C .GT. M3MNGM) THEN
        NMX = M3MNGM
        NPX = J6C
        NAME = 'M3MNGM'
        NME = 'J6C  '
      ELSEIF (J7C .GT. M3MNGM) THEN
        NMX = M3MNGM
        NPX = J7C
        NAME = 'M3MNGM'
        NME = 'J7C  '
      ELSEIF (J8C .GT. M3MNGM) THEN
        NMX = M3MNGM
        NPX = J8C
        NAME = 'M3MNGM'
        NME = 'J8C  '
      ELSE
        IF (J9C .LE. MANGMP) GOTO 20
        NMX = MANGMP
        NPX = J9C
        NAME = 'MANGMP'
        NME = 'J9C  '
      ENDIF
C
   10 CONTINUE
      WRITE (IWRITE,3000) NAME,NME,NPX,NMX
      STOP
C
C   Determination of effective summation variables and their
C   relationships with 6j coefficients.
C
   20 CONTINUE
      DO I = 1,JWC
        INV6J(I) = 0
        SUM6J(I) = .FALSE.
      ENDDO
C
      NSUM = 0
      NLSUM = 0
      IF (MP .EQ. M) RETURN
      M1 = M+1
C
      DO I = M1,MP
        IF (SUMVAR(I)) THEN
          NSUM = NSUM+1
          JSUM6(NSUM) = 0
          INVER(I) = NSUM
        ENDIF
      ENDDO
C
      IF (NSUM .EQ. 0) RETURN
C
      IF (NSUM .GT. MTRIAD) THEN
        NMX = MTRIAD
        NPX = NSUM
        NAME = 'MTRIAD'
        NME = 'NSUM '
        GOTO 10
      ENDIF
C
      KT = 0
C
      DO I = 1,JWC
        DO J = 1,6
          IK = JW(J,I)
          IF (.NOT. SUMVAR(IK)) GOTO 30
C
          IF (.NOT. SUM6J(I)) THEN
            SUM6J(I) = .TRUE.
            KT = KT+1
            J6SUM(KT) = 0
            NSUM6J(KT) = I
            INV6J(I) = KT
          ENDIF
C
          ISK = INVER(IK)
          I2 = JSUM6(ISK)+1
          JSUM6(ISK) = I2
          JSUM4(ISK,I2) = J
          JSUM5(ISK,I2) = KT
          I3 = J6SUM(KT)+1
          J6SUM(KT) = I3
          JSUMT(KT,I3) = ISK
   30     CONTINUE
        ENDDO
      ENDDO
C
      CALL VAR (J6,J6P,J6C,J6CP,J6CC,SUMVAR,M,INVER,
     + IWRITE)
      CALL VAR (J7,J7P,J7C,J7CP,J7CC,SUMVAR,M,INVER,
     + IWRITE)
      CALL VAR (J8,J8P,J8C,J8CP,J8CC,SUMVAR,M,INVER,
     + IWRITE)
      CALL VAR (J9,J9P,J9C,J9CP,J9CC,SUMVAR,M,INVER,
     + IWRITE)
C
      IF (.NOT. CUT) THEN
        NLSUM = 1
        NBJ(1) = NSUM
        NB6J(1) = KT
        K6CP(1) = J6CP
        K7CP(1) = J7CP
        K8CP(1) = J8CP
        K9CP(1) = J9CP
C
        DO I = 1,KT
          I1 = NSUM6J(I)
          DO J = 1,6
            JWORD(J,I) = JW(J,I1)
          ENDDO
        ENDDO
C
        DO I = 1,NSUM
          ISU = JSUM6(I)
          DO J = 1,ISU
            I1 = JSUM5(I,J)
            J1 = JSUM4(I,J)
            JWORD(J1,I1) = MP+I
          ENDDO
        ENDDO
C
        RETURN
      ENDIF
C
C   Separation of variables and sums in case a cut was detected.
C
      K6C = 0
      K7C = 0
      K8C = 0
      K9C = 0
      NJ = 0
      N6J = 0
C
      DO I = 1,KT
        T6J(I) = .FALSE.
      ENDDO
C
      DO I = 1,NSUM
        JT(I) = .FALSE.
        JS(I) = .FALSE.
      ENDDO
C
      J = 1
C
   40 CONTINUE
      NJ = NJ+1
      JNSUM(NJ) = J
      JINV(J) = NJ
      JT(J) = .TRUE.
   50 CONTINUE
      JS(J) = .TRUE.
      JS6 = JSUM6(J)
C
      DO I = 1,JS6
        I6J = JSUM5(J,I)
C
        IF (.NOT. T6J(I6J)) THEN
          T6J(I6J) = .TRUE.
          N6J = N6J+1
          N6JN(N6J) = NSUM6J(I6J)
          IN6J(I6J) = N6J
        ENDIF
C
        J6J = J6SUM(I6J)
C
        DO K = 1,J6J
          JK = JSUMT(I6J,K)
          IF (.NOT. JT(JK)) THEN
            NJ = NJ+1
            JNSUM(NJ) = JK
            JINV(JK) = NJ
            JT(JK) = .TRUE.
          ENDIF
        ENDDO
C
      ENDDO
C
      DO JJ = 1,NSUM
        J = JJ
        IF ((.NOT. JS(JJ)) .AND. JT(JJ)) GOTO 50
      ENDDO
C
      NLSUM = NLSUM+1
C
      IF (NLSUM .GT. MSUM) THEN
        NMX = MSUM
        NPX = NLSUM
        NAME = 'MSUM  '
        NME = 'NLSUM'
        GOTO 10
      ENDIF
      NBJ(NLSUM) = NJ
      NB6J(NLSUM) = N6J
C
      IF (J6CP .NE. 0) CALL CHVAR (J6P,J6CP,K6C,JT,JINV)
      K6CP(NLSUM) = K6C
      IF (J7CP .NE. 0) CALL CHVAR (J7P,J7CP,K7C,JT,JINV)
      K7CP(NLSUM) = K7C
      IF (J8CP .NE. 0) CALL CHVAR (J8P,J8CP,K8C,JT,JINV)
      K8CP(NLSUM) = K8C
      IF (J9CP .NE. 0) CALL CHVAR (J9P,J9CP,K9C,JT,JINV)
      K9CP(NLSUM) = K9C
C
      IF (NJ .NE. NSUM) THEN
        DO JJ = 1,NSUM
          J = JJ
          IF (.NOT. JT(JJ)) GOTO 40
        ENDDO
      ENDIF
C
      DO I = 1,KT
        I1 = N6JN(I)
        DO J = 1,6
          JWORD(J,I) = JW(J,I1)
        ENDDO
      ENDDO
C
      DO I = 1,NSUM
        IK = JNSUM(I)
        I2 = JSUM6(IK)
        JTEM6(I) = I2
        DO J = 1,I2
          JTEM4(I,J) = JSUM4(IK,J)
          K = JSUM5(IK,J)
          JTEM5(I,J) = IN6J(K)
        ENDDO
      ENDDO
C
      DO I = 1,NSUM
        I2 = JTEM6(I)
        JSUM6(I) = I2
        DO J = 1,I2
          I1 = JTEM5(I,J)
          J1 = JTEM4(I,J)
          JSUM4(I,J) = J1
          JSUM5(I,J) = I1
          JWORD(J1,I1) = I+MP
        ENDDO
      ENDDO
C
 3000 FORMAT (' Dimension error for ',A6/2X,A5,' = ',I5,                !
     +' is out of allowed range',I4)
      END
CEND--------------------------------------------------------------------
CEND    SQUARE.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE SQUARE
     + ( NAMSUB, ARR, ICROSS, IH, IL
     + , J6, J6C, J7, J7C, J8, J8C
     + , JDIAG, JW, JWC, MP
     + , NBNODE, NPART, NPOINT, TAB1, SUMVAR
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/SQUARE.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NAMSUB
C   ARR
C   ICROSS
C   IH
C   IL
C   J6
C   J6C
C   J7
C   J7C
C   J8
C   J8C
C   JDIAG
C   JW
C   JWC
C   MP
C   NBNODE
C   NPART
C   NPOINT
C   TAB1
C   SUMVAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,ILP,IT,IT1
      INTEGER IT2,IT3,IT4,ITHL
      INTEGER ITLL,ITMAX,ITMIN,ITMN
      INTEGER ITMX,JJ1,JJ2,JJ3
      INTEGER K23,K32,L2,L4
C
      CHARACTER*6 NAMSUB
      INTEGER ARR(M4TRD,*)
      INTEGER ICROSS
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J8(*)
      INTEGER J8C
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER MP
      INTEGER NBNODE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER TAB1(MANGM,*)
      LOGICAL SUMVAR(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'SQUARE'/
C-----------------------------------------------------------------------
      NAMSUB = NAME
      MP = MP+1
      SUMVAR(MP) = .TRUE.
      IT1 = NPOINT(1)
      IT2 = NPOINT(2)
C
      IF (ICROSS .EQ. 1) THEN
        IT3 = NPOINT(3)
        IT4 = NPOINT(4)
        K23 = 3
        K32 = 2
      ELSE
        IT3 = NPOINT(4)
        IT4 = NPOINT(3)
        K23 = 2
        K32 = 3
      ENDIF
C
      L4 = JDIAG(IT2,1)
C
      IF (ARR(IT2,1) .LE. 0) THEN
        CALL PHASE2 (L4,
     + J8,J8C)
        ARR(IT2,1) = 1
        ARR(IT3,1) = -1
      ENDIF
C
      L2 = JDIAG(IT1,1)
      IF (ARR(IT1,1) .GT. 0) CALL PHASE2 (L2,
     + J8,J8C)
      JWC = JWC+1
      JW(1,JWC) = L4
      JW(2,JWC) = L2
      JW(3,JWC) = JDIAG(IT2,2)
      JJ1 = JDIAG(IT1,3)
      JW(4,JWC) = JJ1
      JW(5,JWC) = MP
      JW(6,JWC) = JDIAG(IT1,2)
      IF (ARR(IT1,2) .LT. 0) CALL PHASE2 (JDIAG(IT1,2),
     + J8,J8C)
      JWC = JWC+1
      JW(1,JWC) = L4
      JW(2,JWC) = L2
      JJ3 = JDIAG(IT3,K23)
      JJ2 = JDIAG(IT4,K32)
      JW(3,JWC) = JJ3
      JW(4,JWC) = JJ2
      JW(5,JWC) = MP
      JW(6,JWC) = JDIAG(IT3,K32)
      IF (ARR(IT3,K32) .LT. 0) CALL PHASE2 (JDIAG(IT3,K32),
     + J8,J8C)
      J6(J6C+1) = MP
      J6C = J6C+2
      J6(J6C) = MP
C
      IF (NPART .EQ. 1) THEN
        ITMIN = IT2
        ITMAX = IT3
      ELSE
        ITMIN = MIN(IT2,IT3)
        ITMAX = MAX(IT2,IT3)
      ENDIF
      ITMN = MIN(IT1,IT4)
      ITMX = MAX(IT1,IT4)
C
      TAB1(MP,1) = ITMIN
      TAB1(MP,2) = ITMAX
      JDIAG(IT2,1) = MP
      JDIAG(IT3,1) = MP
      JDIAG(IT2,3) = JJ1
      ARR(IT2,3) = ARR(IT1,3)
      JDIAG(IT3,K32) = JJ2
      ARR(IT3,K32) = ARR(IT4,K32)
C
      IF (ICROSS .EQ. 1) THEN
        J7(J7C+1) = L2
        J7(J7C+2) = L4
        CALL PHASE2 (L4,
     + J8,J8C)
        J7C = J7C+3
        J7(J7C) = MP
      ELSE
        CALL PHASE2 (JJ2,
     + J8,J8C)
      ENDIF
C
      ITLL = IL(ITMN)
      ITHL = IL(ITMX)
C
      DO I = ITLL+1,ITHL-1
        IT = IH(I)
        ILP = I-1
        IL(IT) = ILP
        IH(ILP) = IT
      ENDDO
      IF (ITHL .NE. NBNODE) THEN
        DO I = ITHL+1,NBNODE
          IT = IH(I)
          ILP = I-2
          IL(IT) = ILP
          IH(ILP) = IT
        ENDDO
      ENDIF
C
      IF (NPART .NE. 2) THEN
        TAB1(JJ1,1) = IH(1)
        TAB1(JJ1,2) = IH(NBNODE-2)
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    TRDEL.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE TRDEL (JJ1,JJ2,JJ3,NBN,FAIL,
     + J1, CUT, FREE, SUMVAR)
CRCS
CRCS $Source: /home/phn/DARC/RCS/TRDEL.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   J1
C   CUT
C   FREE
C   SUMVAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Argument variables
C
      INTEGER JJ1,JJ2,JJ3,NBN
      LOGICAL FAIL
C
C  Local variables
C
      INTEGER I1,I2,I3
C
      INTEGER J1(*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      IF (SUMVAR(JJ1) .OR. SUMVAR(JJ2) .OR. SUMVAR(JJ3)) RETURN
      IF (NBN .GT. 4) CUT = .TRUE.
C
      IF ((.NOT. FREE(JJ1)) .AND.
     +    (.NOT. FREE(JJ2)) .AND.
     +    (.NOT. FREE(JJ3))) THEN
        I1 = J1(JJ1)
        I2 = J1(JJ2)
        I3 = J1(JJ3)
        IF ((I1 .LT. (ABS (I2-I3)+1)) .OR. (I1 .GT. (I2+I3-1)))
     +     FAIL = .TRUE.
      ENDIF
C
      END
CEND--------------------------------------------------------------------
CEND    TRIANG.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE TRIANG (FAIL
     + , NAMSUB, ARR, IFIRST, IH, IL, ILAST
     + , J1
     + , J8, J8C, JDIAG, JW
     + , JWC, NBNODE
     + , NPOINT, TAB1, CUT, FREE, SUMVAR
     + )
CRCS
CRCS $Source: /home/phn/DARC/RCS/TRIANG.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   NAMSUB
C   ARR
C   IFIRST
C   IH
C   IL
C   ILAST
C   J1
C   J8
C   J8C
C   JDIAG
C   JW
C   JWC
C   NBNODE
C   NPOINT
C   TAB1
C   CUT
C   FREE
C   SUMVAR
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
C
C  Argument variables
C
      LOGICAL FAIL
C
      CHARACTER*6 NAMSUB
      INTEGER ARR(M4TRD,*)
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER J1(*)
      INTEGER J8(*)
      INTEGER J8C
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER NBNODE
      INTEGER NPOINT(*)
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,IL2,IL3,ILP
      INTEGER IT,IT1,IT2,IT3
      INTEGER JT1,K12,K23
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'TRIANG'/
C-----------------------------------------------------------------------
      NAMSUB = NAME
      IT1 = NPOINT(1)
      IT2 = NPOINT(2)
      IT3 = NPOINT(3)
      JWC = JWC+1
      JW(1,JWC) = JDIAG(IT3,2)
      JW(2,JWC) = JDIAG(IT2,3)
      JW(3,JWC) = JDIAG(IT3,1)
      IF (ARR(IT3,1) .GT. 0) CALL PHASE2 (JW(3,JWC),
     + J8,J8C)
      JW(4,JWC) = JDIAG(IT2,1)
      IF (ARR(IT2,1) .LT. 0) CALL PHASE2 (JW(4,JWC),
     + J8,J8C)
      K23 = 3
      IF (IT1 .EQ. IFIRST) K23 = 2
      JW(5,JWC) = JDIAG(IT1,K23)
      JW(6,JWC) = JDIAG(IT3,3)
C
      CALL TRDEL (JW(1,JWC),JW(2,JWC),JW(5,JWC),NBNODE,FAIL,
     + J1, CUT, FREE, SUMVAR)
C
      IF (FAIL) GOTO 10
C
      IF (ARR(IT3,3) .GT. 0) CALL PHASE2 (JW(6,JWC),
     + J8,J8C)
C
      JT1 = JW(5,JWC)
      JDIAG(IT3,1) = JT1
      JDIAG(IT3,3) = JW(2,JWC)
      ARR(IT3,1) = ARR(IT1,K23)
      ARR(IT3,3) = ARR(IT2,3)
C
      IF (IT1 .NE. IFIRST) THEN
        TAB1(JT1,1) = IT3
        TAB1(JT1,2) = IH(NBNODE-1)
        K12 = 1
      ELSE
        TAB1(JT1,1) = IH(2)
        TAB1(JT1,2) = IT3
        K12 = 2
      ENDIF
C
      IL3 = IL(IT3)
C
      IF (IT1 .NE. ILAST) THEN
        IL2 = IL(IT2)-1
        DO I = 2,IL2
          IT = IH(I)
          ILP = I-1
          IL(IT) = ILP
          IH(ILP) = IT
        ENDDO
      ENDIF
C
      DO I = IL3,NBNODE
        IT = IH(I)
        ILP = I-K12
        IL(IT) = ILP
        IH(ILP) = IT
      ENDDO
C
   10 CONTINUE
      END
CEND--------------------------------------------------------------------
CEND    VAR.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE VAR (JN,JNS,JNC,JNSC,JBC,SUMVAR,M,INVER,
     + IWRITE)
CRCS
CRCS $Source: /home/phn/DARC/RCS/VAR.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MANGMP
      PARAMETER (MANGMP=2*(MANGM/3))
C
C  Argument variables
C
      INTEGER JNC
      INTEGER INVER(*),JBC,JN(*)
      INTEGER JNS(*),JNSC,M
      LOGICAL SUMVAR(*)
      INTEGER IWRITE
C
C  Local variables
C
      INTEGER I,I1,J,JBBC
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      JNSC = 0
C
      IF (JBC .NE. JNC) THEN
C
        JBBC = JBC+1
C
        DO I = JBBC,JNC
          I1 = JN(I)
          IF (SUMVAR(I1)) THEN
            JNSC = JNSC+1
            IF (JNSC .GT. MANGMP) THEN
              WRITE (IWRITE,3000) JNSC,MANGMP
              STOP
            ENDIF
            J = INVER(I1)
            JNS(JNSC) = J
            JN(I) = M
          ENDIF
        ENDDO
C
      ENDIF
C
 3000 FORMAT (' Dimension error in VAR. JNSC = ',I5,' MANGMP = ',I5)
      END
CEND--------------------------------------------------------------------
CEND    WAY.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE WAY (L,KA,KB,ICH,NB,
     + IAL, IF1, IF2, J23, LCOL, LINE, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/WAY.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   IAL
C   IF1
C   IF2
C   J23
C   LCOL
C   LINE
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
C
C  Argument variables
C
      INTEGER ICH,KA,KB,L
      INTEGER NB
C
      INTEGER IAL(*)
      INTEGER IF1
      INTEGER IF2
      INTEGER J23(M2TRD,*)
      INTEGER LCOL(MANGM,*)
      INTEGER LINE(MANGM,*)
      LOGICAL TABS(*)
C
C  Local variables
C
      INTEGER I1,I2,I3,I4
      INTEGER IA,IB,JI1,JI2
      INTEGER JI3,JI4,K1,K2
      INTEGER L1,L2,LA,LB
      INTEGER LC1,LC2,NB1,NBM
      INTEGER NBP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      K1 = J23(L,KA)
      K2 = J23(L,KB)
      NB = IAL(K1)+IAL(K2)-1
      IF (NB) 20,10,60
   10 CONTINUE
      NB1 = IAL(K1)-IAL(K2)
      IF (NB1) 70,60,60
   20 CONTINUE
      CALL OTHERJ (L,K1,L1,LC1,LA,
     + LCOL,LINE,TABS)
      CALL OTHERJ (L,K2,L2,LC2,LB,
     + LCOL,LINE,TABS)
      CALL NEIBOR (LC1,I1,I2)
      CALL NEIBOR (LC2,I3,I4)
      JI1 = J23(L1,I1)
      JI2 = J23(L1,I2)
      JI3 = J23(L2,I3)
      JI4 = J23(L2,I4)
      IA = IAL(JI1)+IAL(JI2)
      IB = IAL(JI3)+IAL(JI4)
      NBP = IB+IA+1
      NBM = IB-IA
      GOTO (60,30,40,30,50),NBP
   30 CONTINUE
      IF (NBM) 70,60,60
   40 CONTINUE
      IF (NBM) 70,50,60
   50 CONTINUE
      IF ((JI3 .EQ. IF1) .OR.
     +    (JI3 .EQ. IF2) .OR.
     +    (JI4 .EQ. IF1) .OR.
     +    (JI4 .EQ. IF2)) GOTO 70
   60 CONTINUE
      ICH = 1
      GOTO 80
   70 CONTINUE
      ICH = -1
   80 CONTINUE
      RETURN
C
      END
CEND--------------------------------------------------------------------
CEND    ZEROJ.f    Wed Jan 21 10:09:58 GMT 2004
CEND--------------------------------------------------------------------
      SUBROUTINE ZEROJ (J,JZ,FAIL,
     + ARR, ARROW, CUT, FREE, IAL, IBUG3, ICROSS,
     + IFIRST, IH, IL, ILAST, IPARTL, IPARTS, IWRITE,
     + J1, J23, J6, J6C, J6CC, J7, J7C, J7CC, J8, J8C,
     + J8CC, J9, J9C, J9CC, JDEL, JDELC, JDIAG, JW, JWC, JWCC,
     + JZERO, LCOL, LDEL, LINE, M, MP, N, NBNODE, NBTR, NC,
     + NFIN, NFREE, NPART, NPOINT, NZERO, SUMVAR,
     + TAB1, TABS)
CRCS
CRCS $Source: /home/phn/DARC/RCS/ZEROJ.f,v $
CRCS $Author: phn $
CRCS $Date: 2001/10/31 14:13:50 $
CRCS $Revision: 11.1 $
CRCS
C-----------------------------------------------------------------------
C
C
C   ARR
C   ARROW
C   CUT
C   FREE
C   IAL
C   IBUG3
C   ICROSS
C   IFIRST
C   IH
C   IL
C   ILAST
C   IPARTL
C   IPARTS
C   IWRITE
C   J1
C   J23
C   J6
C   J6C
C   J6CC
C   J7
C   J7C
C   J7CC
C   J8
C   J8C
C   J8CC
C   J9
C   J9C
C   J9CC
C   JDEL
C   JDELC
C   JDIAG
C   JW
C   JWC
C   JWCC
C   JZERO
C   LCOL
C   LDEL
C   LINE
C   M
C   MP
C   N
C   NBNODE
C   NBTR
C   NC
C   NFIN
C   NFREE
C   NPART
C   NPOINT
C   NZERO
C   SUMVAR
C   TAB1
C   TABS
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
C
C  Parameter variables
C
      INTEGER MTRIAD
      PARAMETER (MTRIAD=12)
      INTEGER M2TRD
      PARAMETER (M2TRD=2*MTRIAD)
      INTEGER M4TRD
      PARAMETER (M4TRD=4*MTRIAD)
      INTEGER MANGM
      PARAMETER (MANGM=60)
      INTEGER M6J
      PARAMETER (M6J=20)
      INTEGER MZERO
      PARAMETER (MZERO=20)
C
C  Argument variables
C
      INTEGER J,JZ
      LOGICAL FAIL
C
      INTEGER ARR(M4TRD,*)
      INTEGER ARROW(M2TRD,*)
      INTEGER IAL(*)
      INTEGER IBUG3
      INTEGER ICROSS
      INTEGER IFIRST
      INTEGER IH(*)
      INTEGER IL(*)
      INTEGER ILAST
      INTEGER IPARTL
      INTEGER IPARTS
      INTEGER IWRITE
      INTEGER J1(*)
      INTEGER J23(M2TRD,*)
      INTEGER J6(*)
      INTEGER J6C
      INTEGER J6CC
      INTEGER J7(*)
      INTEGER J7C
      INTEGER J7CC
      INTEGER J8(*)
      INTEGER J8C
      INTEGER J8CC
      INTEGER J9(*)
      INTEGER J9C
      INTEGER J9CC
      INTEGER JDEL
      INTEGER JDELC
      INTEGER JDIAG(M4TRD,*)
      INTEGER JW(6,*)
      INTEGER JWC
      INTEGER JWCC
      INTEGER JZERO(*)
      INTEGER LCOL(MANGM,*)
      INTEGER LDEL(M6J,*)
      INTEGER LINE(MANGM,*)
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NBNODE
      INTEGER NBTR
      INTEGER NC
      INTEGER NFIN
      INTEGER NFREE
      INTEGER NPART
      INTEGER NPOINT(*)
      INTEGER NZERO
      INTEGER TAB1(MANGM,*)
      LOGICAL CUT
      LOGICAL FREE(*)
      LOGICAL SUMVAR(*)
      LOGICAL TABS(*)
C
C  Local variables
C
      CHARACTER*6 NAME
      INTEGER I,JJ1,JJ2,JJX
      INTEGER JJZ,JT,K1,K2
      INTEGER L,L1,L2,LC
      INTEGER LCO1,LCO2,LIN,LO1
      INTEGER LO2
      LOGICAL NOCUT
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA NAME/'ZEROJ '/
C-----------------------------------------------------------------------
      NOCUT = .FALSE.
      NZERO = 0
C
      IF (J .GE. 1) THEN
        CALL OTHERJ (0,JZ,LIN,LC,K1,
     + LCOL,LINE,TABS)
        I = NZERO
        GOTO 40
      ENDIF
C
      DO I = 1,M
        IF ((J1(I) .NE. 1) .OR. FREE(I) .OR. (IAL(I).LE.1)) GOTO 10
        NZERO = NZERO+1
        IF (NZERO .GT. MZERO) THEN
          WRITE (IWRITE,3000) NZERO,MZERO
          STOP
        ENDIF
        JZERO(NZERO) = I
   10   CONTINUE
      ENDDO
C
      NOCUT = .TRUE.
      M = M+1
      J1(M) = 1
      SUMVAR(M) = .FALSE.
      FREE(M) = .FALSE.
      IF (NZERO .EQ. 0) GOTO 90
      CALL PRINTJ (NAME,1,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
      I = 0
   20 CONTINUE
      I = I+1
      JZ = JZERO(I)
      J = 0
   30 CONTINUE
      J = J+1
      LIN = LINE(JZ,J)
      IF (TABS(LIN)) GOTO 80
      LC = LCOL(JZ,J)
   40 CONTINUE
      CALL NEIBOR (LC,L1,L2)
      JJ1 = J23(LIN,L1)
      JJ2 = J23(LIN,L2)
C
      IF (JJ1 .EQ. JJ2) THEN
        J6C = J6C+1
        J6(J6C) = JJ1
        LO1 = LIN
        LO2 = LIN
        LCO1 = L1
        LCO2 = L2
        GOTO 70
      ENDIF
C
      CALL DELTA (JJ1,JJ2,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
      IF (FAIL) GOTO 90
C
      IF ((J1(JJ1) .NE. 1) .AND. (J1(JJ2) .NE. 1)) GOTO 60
      IF (J1(JJ1) .LT. J1(JJ2)) GOTO 60
      IF (J1(JJ1) .GT. J1(JJ2)) GOTO 50
C
      IF (NZERO .NE. 0) THEN
        DO JJX = I,NZERO
          JJZ = JZERO(JJX)
          IF (JJ1 .EQ. JJZ) GOTO 60
          IF (JJ2 .EQ. JJZ) GOTO 50
        ENDDO
      ENDIF
C
      GOTO 60
C
   50 CONTINUE
      JJZ = JJ2
      JJ2 = JJ1
      JJ1 = JJZ
C
   60 CONTINUE
      CALL OTHERJ (LIN,JJ1,LO1,LCO1,K1,
     + LCOL,LINE,TABS)
      CALL OTHERJ (LIN,JJ2,LO2,LCO2,K2,
     + LCOL,LINE,TABS)
      J9C = J9C+1
      J9(J9C) = JJ1
      J23(LO2,LCO2) = JJ1
      LINE(JJ1,K1) = LO2
      LCOL(JJ1,K1) = LCO2
C
   70 CONTINUE
      IF (ARROW(LIN,L1) .LT. ARROW(LIN,L2)) THEN
        CALL PHASE2 (JJ1,
     + J8,J8C)
      ELSEIF (ARROW(LIN,L1) .EQ. ARROW(LIN,L2)) THEN
        ARROW(LO1,LCO1) = 1
        ARROW(LO2,LCO2) = -1
      ENDIF
C
      TABS(LIN) = .TRUE.
      NBTR = NBTR-1
      IF (NBTR .EQ. 0) GOTO 90
      IF (LO1 .EQ. LO2) THEN
        L = 6-LCO1-LCO2
        JT = J23(LO1,L)
        IF ((J1(JT) .EQ. 1) .AND. (.NOT.FREE(JT))) GOTO 80
        CALL DELTA (JT,M,FAIL,
     + J1,FREE,
     + IBUG3,
     + J6,J6C,J7,J7C,
     + J8,J8C,J9,J9C,
     + JDEL,JW,JWC,
     + LDEL,
     + SUMVAR,
     + CUT,
     + J6CC,J7CC,J8CC,J9CC,
     + JDELC,JWCC)
        IF (FAIL) GOTO 90
        NZERO = NZERO+1
        JZERO(NZERO) = JT
      ENDIF
   80 CONTINUE
      IF (J .EQ. 1) GOTO 30
C
      IF (NBTR .NE. 0) THEN
        IF (I .LT. NZERO) GOTO 20
      ENDIF
C
   90 CONTINUE
      CALL PRINTJ (NAME,4,
     + J1, M,N, IBUG3, IWRITE, J6,J6C,J7,J7C, J8,J8C,J9,J9C,
     + JDEL,JW,JWC, LDEL,MP, ARR,ICROSS,IFIRST, IH,IL,ILAST,IPARTL,
     + IPARTS,JDIAG, NBNODE,NC,NFIN,NFREE, NPART,NPOINT, TAB1,
     + ARROW,J23, LCOL,LINE, NBTR, JZERO,NZERO, SUMVAR, TABS)
      IF (NOCUT) CUT = .FALSE.
C
 3000 FORMAT (' Dimension error in ZEROJ. NZERO = ',I5,' MZERO = ',I5)
      END
