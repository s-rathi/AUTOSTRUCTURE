C     DSTG0                    v11.1                     phn 31 Oct 2001
c
c                              v12.1                     nrb 19 May 2008
c
      PROGRAM DSTG0
C
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
      CHARACTER*5 CODE
      CHARACTER*60 AUT,DAT,REV,SOU
      CHARACTER*9 FILE1
      INTEGER IREAD,IWRITE
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DATA CODE/'DSTG0'/
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
      FILE1 = CODE//'.OUT'
C
C  Open IWRITE file
C
      OPEN (UNIT=IWRITE,FILE=FILE1,STATUS='UNKNOWN')
C
C  Set version information
C
      SOU = '$Source: /home/badnell/rmatrix/ser/darc $'
      AUT = '$Author: nrb $'
      DAT = '$Mon May 19 13:38:27 BST 2008 $'
      REV = "$Revision: 12.1, derived from phn's 11.1 $"
C
C  Write out title
C
      WRITE (IWRITE,3000) SOU,AUT,DAT,REV,CODE
      PRINT 3010,SOU,AUT,DAT,REV,CODE
C
C  Now call the rest of the program
C
      CALL AAMN0(IREAD,IWRITE)
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/1X,A60/1X,A60/1X,A60/1X,A60/1X,71('*')//
     +' DARC, the Dirac Atomic R-matrix Codes.'/
     +' homepage: www.am.qub.ac.uk/DARC/'/1X,A5,' module'/
     +' Coded in double precision FORTRAN'//
     +' Any inquiries or comments on the code can be directed to'/
     +' Dr Patrick Norrington at :'//
     +' email     : p.norrington@qub.ac.uk'/
     +' homepage  : www.am.qub.ac.uk/users/p.norrington'//1X,71('*'))
 3010 FORMAT (/1X,71('*')/1X,A60/1X,A60/1X,A60/1X,A60/1X,71('*')//
     +' DARC, the Dirac Atomic R-matrix Codes.'/
     +' homepage: www.am.qub.ac.uk/DARC/'/1X,A5,' module'//1X,71('*'))
      END
C
C                             *******************
C
      SUBROUTINE AAMN0(IREAD,IWRITE)
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      INCLUDE 'darc.inc'
C
C  Argument variables
C
      INTEGER IREAD,IWRITE
C
C  Local variables
C
      CHARACTER*50 FILEN
      CHARACTER JANS
      CHARACTER*3 LABEL
      CHARACTER*2 NH(MXNW)
      CHARACTER*6 ORBNAM
      CHARACTER*20 RECORD
      CHARACTER STRING
      DOUBLE PRECISION CP(MXP1),CQ(MXP1),D(MXNP)
      DOUBLE PRECISION DR(MXNP),P(MXNP),Q(MXNP)
      DOUBLE PRECISION UCF(MXNW),ZZ(MXNP)
      INTEGER I,IANS,ILAST
      INTEGER IMCDF(MXNW),J,JREAD,JWRITE
      INTEGER N,NAK(MXNW),NCOUNT
      INTEGER NP(MXNW),NW
      INTEGER IDMTST(30)
      INTEGER NDIMAX(30)
      LOGICAL EX
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
C
C  Now make calls to:
C
C   CALEN  ... get RECORD of current time/date
C   DMSET0 ... set dimensions
C
      CALL CALEN(IWRITE,RECORD)
      CALL DMSET0(IWRITE, IDMTST, NDIMAX)
C
      JREAD = 10
      JWRITE = 20
C
      PRINT 3000
      WRITE (IWRITE,3000)
      PRINT 3020
      WRITE (IWRITE,3020)
C
C  Check existence of TARGET.INP
C  You cannot overwrite it
C
      INQUIRE (FILE='TARGET.INP',EXIST=EX)
      IF (EX) THEN
        PRINT 3030
        WRITE (IWRITE,3030)
c        CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
c        STOP
      ENDIF
C
   10 CONTINUE
      PRINT 3010
c
      string=' '
C
      READ (IREAD,3150) STRING
c
      if(string.eq.' ')string='1'
C
      IF (STRING.EQ.'0') THEN
        PRINT 3090
        WRITE (IWRITE,3090)
        STOP
      ENDIF
C-----------------------------------------------------------------------
      IF (STRING.EQ.'1') THEN
C
        PRINT 3100
        READ (IREAD,3120) FILEN
        IF (FILEN.EQ.' ') FILEN='MCDF.DAT'
        INQUIRE (FILE=FILEN,EXIST=EX)
        IF (.NOT.EX) THEN
          PRINT 3130,FILEN
          WRITE (IWRITE,3130) FILEN
          GOTO 10
        ENDIF
        OPEN (UNIT=JREAD,FILE=FILEN,ACCESS='SEQUENTIAL',STATUS='OLD',
     +        FORM='UNFORMATTED')
C
C  IWRITE ... I/O number for writing output
C  JREAD  ... I/O number for reading dump
C  N      ... number of mesh points
C  DR     ... array containing radial mesh
C  ZZ     ... array containing nuclear potential
C  NW     ... number of orbitals
C  NH     ... array containing orbital labels (character format)
C  NP     ... array containing orbital principal quantum numbers
C  NAK    ... array containing orbital kappa quantum numbers (l/j combo)
C  IMCDF  ... array containing orbital initial pointers in CP/CQ arrays
C  UCF    ... array containing orbital generalised occupation numbers
C  CP     ... array containing orbital large components
C  CQ     ... array containing orbital small components
C
        CALL MCDF09(IWRITE,JREAD,N,DR,ZZ,NW,NH,NP,NAK,IMCDF,UCF,CP,CQ,
     +              IDMTST,NDIMAX)
C
        GOTO 11
C
      ENDIF
C-----------------------------------------------------------------------
      IF (STRING.EQ.'2') THEN
C
        PRINT 3110
        READ (IREAD,3120) FILEN
        IF (FILEN.EQ.' ') GOTO 10
        INQUIRE (FILE=FILEN,EXIST=EX)
        IF (.NOT.EX) THEN
          PRINT 3130,FILEN
          WRITE (IWRITE,3130) FILEN
          GOTO 10
        ENDIF
        OPEN (UNIT=JREAD,FILE=FILEN,ACCESS='SEQUENTIAL',STATUS='OLD',
     +        FORM='UNFORMATTED')
C
        CALL MCDF10(IWRITE,JREAD,N,DR,ZZ,NW,NH,NP,NAK,IMCDF,UCF,CP,CQ,
     +              IDMTST,NDIMAX)
C
        GOTO 11
C
      ENDIF
C-----------------------------------------------------------------------
      GOTO 10
C-----------------------------------------------------------------------
   11 CONTINUE
C
C  Write information about the orbitals.
C
c      PRINT 3070
      WRITE (IWRITE,3070)
      DO J = 1,NW
c        PRINT 3080,J,NP(J),NH(J),NP(J),NAK(J),UCF(J)
        WRITE (IWRITE,3080) J,NP(J),NH(J),NP(J),NAK(J),UCF(J)
      ENDDO
c      PRINT 3020
      WRITE (IWRITE,3020)
C-----------------------------------------------------------------------
C
C  Write TARGET.INP
C  Write the following cards.
C  The cards D to G are repeated for J=1,NW.
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
      OPEN (UNIT=JWRITE,FILE='TARGET.INP',STATUS='UNKNOWN'        !'NEW'
     x     ,FORM='FORMATTED')
C
      WRITE (JWRITE,3040) NW,N
      WRITE (JWRITE,3050) (UCF(J),J=1,NW)
      WRITE (JWRITE,3050) (DR(I),I=1,N)
C
      DO J = 1,NW
        ILAST = IMCDF(J)
        WRITE (JWRITE,3060) NP(J),NAK(J)
        WRITE (JWRITE,3050) (CP(ILAST+I),I=1,N)
        WRITE (JWRITE,3060) NP(J),NAK(J)
        WRITE (JWRITE,3050) (CQ(ILAST+I),I=1,N)
      ENDDO
C
      CLOSE(JWRITE)
      PRINT 3220
      WRITE (IWRITE,3220)
C-----------------------------------------------------------------------
C
C  Write the file TARGET.SH if requested
C
   20 CONTINUE
      PRINT 3140
c
      jans=' '
      READ (IREAD,3150) JANS
      if(jans.eq.' ')jans='n'
c
      IANS = -1
      IF (JANS.EQ.'Y' .OR. JANS.EQ.'y') IANS=1
      IF (JANS.EQ.'N' .OR. JANS.EQ.'n') IANS=0
      IF (IANS.LT.0 .OR. IANS.GT.1) GOTO 20
C
      IF (IANS.EQ.1) THEN
C
        INQUIRE (FILE='TARGET.SH',EXIST=EX)
        IF (EX) THEN
          PRINT 3160
          WRITE (IWRITE,3160)
c          CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
c          STOP
        ENDIF
        OPEN (UNIT=JWRITE,FILE='TARGET.SH',STATUS='UNKNOWN'      !'NEW',
     +       ,FORM='FORMATTED')
C
        NCOUNT = 0
        DO J = 1,NW
          NCOUNT = NCOUNT+1
          CALL IN2CH3 (NCOUNT,LABEL,IWRITE)
          ORBNAM = 'orb'//LABEL
          ILAST = IMCDF(J)
          DO I = 1,N
            P(I) = CP(ILAST+I)
            Q(I) = CQ(ILAST+I)
            D(I) = P(I)*P(I)+Q(I)*Q(I)
          ENDDO
          WRITE (JWRITE,3170) ORBNAM,NP(J),NH(J),NAK(J),N
          WRITE (JWRITE,3180) ORBNAM
          WRITE (JWRITE,3190) (DR(I),P(I),Q(I),D(I),I=1,N)
          WRITE (JWRITE,3200)
        ENDDO
C
        CLOSE(JWRITE)
        PRINT 3210
        WRITE (IWRITE,3210)
C
      ENDIF
C-----------------------------------------------------------------------
      PRINT 3020
      WRITE (IWRITE,3020)
      CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
C
 3000 FORMAT (/                                                         !
     +' This program will read from a GRASP dump file and write the'/   !
     +' orbitals to a formatted file with the name TARGET.INP. This'/   !
     +' can be used as input to DARC DSTG1. You will be prompted for'/  !
     +' the version of GRASP.')
 3010 FORMAT (/'     What is the version of GRASP ?'
     +       //'            0 = exit'
     +        /'            1 = version 0.9 MCDF dump'
     +        /'            2 = version 1   MCDF dump'
     +       //'     Default is version 0.9 MCDF dump'/)
 3020 FORMAT (/1X,71('*'))
c 3030 FORMAT (/' TARGET.INP exists ... STOPPING here, no overwrite.')
 3030 FORMAT (/' TARGET.INP exists ... OVERWRITING!!!')
 3040 FORMAT (1X,2I7)
 3050 FORMAT (1X,1P,4E16.8)
 3060 FORMAT (1X,I4,2X,I4)
 3070 FORMAT (//1X,'orbital',3X,'N',3X,'K   occupation number'/)
 3080 FORMAT (1X,I2,1X,I2,A2,2I4,3X,1P,E13.5)
 3090 FORMAT (/'     STOPPING as requested.'/)
 3100 FORMAT (/'     What is the name (max. 50 characters)'
     +        ,' of the GRASP0 MCDF dump ?'
     +        /'     Default is MCDF.DAT')
 3110 FORMAT (/'     What is the name (max. 50 characters)'
     +        ,' of the GRASP1 MCDF dump ?')
 3120 FORMAT (A50)
 3130 FORMAT (/5X,A50/'     does not exist ... try again')
 3140 FORMAT (/' The file TARGET.SH can be used for plotting.'/         !
     +' Do you want to write TARGET.SH (y/n) ?'/
     +' Default is No.')
 3150 FORMAT (A1)
c 3160 FORMAT (/' TARGET.SH exists ... STOPPING here, no overwrite.')
 3160 FORMAT (/' TARGET.SH exists ... OVERWRITING!!!')
 3170 FORMAT ('cat << eof >> INDEX'/A6,' : ',I2,1X,A2,1X,I3,            !
     +' : r (a.u.) : ',I4/'eof')
 3180 FORMAT ('cat << eof >> ',A6)
 3190 FORMAT (1X,1P,4E12.4)
 3200 FORMAT ('eof')
 3210 FORMAT (/' TARGET.SH has been written')
 3220 FORMAT (/' TARGET.INP has been written')
      END
C
C                             *******************
C
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
C
C                             *******************
C
      SUBROUTINE DMCHK0(I,ID, IWRITE, IDMTST, NDIMAX)
C
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
      IF (I.EQ.7) THEN
        PRINT 3010
        WRITE (IWRITE,3010)
        GOTO 10
      ENDIF
C
      IF (I.EQ.11) THEN
        PRINT 3020
        WRITE (IWRITE,3020)
        GOTO 10
      ENDIF
C
      IF (I.EQ.12) THEN
        PRINT 3030
        WRITE (IWRITE,3030)
        GOTO 10
      ENDIF
C
      IF (I.EQ.13) THEN
        PRINT 3040
        WRITE (IWRITE,3040)
        GOTO 10
      ENDIF
C
   10 CONTINUE
      CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
      PRINT 3050
      WRITE (IWRITE,3050)
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
     +/' IDMTST(7) contains the maximum number of CSFs.'
     +/' It is set by variable MXNC in file darc.inc.')
 3020 FORMAT (
     +/' IDMTST(11) contains the dimension of arrays used to'
     +/' tabulate the mesh, orbitals and potentials.'
     +/' It is set by variable MXNP in file darc.inc.')
 3030 FORMAT (
     +/' IDMTST(12) contains the maximum number of target orbitals.'
     +/' It is set by variable MXNW in file darc.inc.')
 3040 FORMAT (
     +/' IDMTST(13) contains the maximum value of points'
     +/' that can be used for storing orbitals.'
     +/' It is set by variable MXP1 in file darc.inc.')
 3050 FORMAT (
     +/'      *******************************************'
     +/'      *** Module terminates in routine DMCHK0 ***'
     +/'      *******************************************'
     +)
      END
C
C                             *******************
C
      SUBROUTINE DMPRT0(IWRITE, IDMTST, NDIMAX)
C
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
      WRITE (IWRITE,3010) IDMTST(07),NDIMAX(07)
      WRITE (IWRITE,3020) IDMTST(11),NDIMAX(11)
      WRITE (IWRITE,3030) IDMTST(12),NDIMAX(12)
      WRITE (IWRITE,3040) IDMTST(13),NDIMAX(13)
C-----------------------------------------------------------------------
 3000 FORMAT (/31X,'Routine DMPRT0'/31X,'--------------'//              !
     +' The following information relates to the dimensions set in the',!
     +' code.'/)
 3010 FORMAT (' IDMTST(07)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNC')
 3020 FORMAT (' IDMTST(11)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNP')
 3030 FORMAT (' IDMTST(12)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXNW')
 3040 FORMAT (' IDMTST(13)   set to : ',I9,'   maximum used : ',I9,     !
     +'    MXP1')
      END
C
C                             *******************
C
      SUBROUTINE DMSET0(IWRITE, IDMTST, NDIMAX)
C
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
      WRITE (IWRITE,3020) MXNC
      WRITE (IWRITE,3030) MXNP
      WRITE (IWRITE,3040) MXNW
      WRITE (IWRITE,3050) MXP1
      WRITE (IWRITE,3060)
C-----------------------------------------------------------------------
      IDMTST(07) = MXNC
      IDMTST(11) = MXNP
      IDMTST(12) = MXNW
      IDMTST(13) = MXP1
C-----------------------------------------------------------------------
 3000 FORMAT (/1X,71('*')/                                              !
     +' routine DMSET0 : the code has been dimensioned as follows'/1X,71!
     +('*'))
 3010 FORMAT (/' The following dimensions have been set :'/)
 3020 FORMAT (1X,I9,' = NC  target orbitals')
 3030 FORMAT (1X,I9,' = NP  points in radial mesh')
 3040 FORMAT (1X,I9,' = NW  target CSFs')
 3050 FORMAT (1X,I9,' = P1  mesh points used to store orbitals')
 3060 FORMAT (/1X,71('*'))
      END
C
C                             *******************
C
      SUBROUTINE IN2CH3 (i,name,iwrite)
C
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
C
C                             *******************
C
      SUBROUTINE MCDF09(IWRITE,ITAPE3,N,DR,ZZ,NW,NH,NP,NAK,IMCDF,UCF,CP,!
     +CQ, IDMTST, NDIMAX)
C
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
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C   LM   number of lagrange multipliers
C   OL   number of levels to optimize on
C
      INTEGER YYLM
      PARAMETER (YYLM=2*MXNW)
      INTEGER YYOL
      PARAMETER (YYOL=10)
      INTEGER N10
      PARAMETER (N10=YYOL*MXNC)
C
C  Argument variables
C
      INTEGER IMCDF(*),ITAPE3,IWRITE,N
      INTEGER NAK(*),NP(*),NW
      CHARACTER*2 NH(*)
      DOUBLE PRECISION CP(*),CQ(*),DR(*)
      DOUBLE PRECISION UCF(*),ZZ(*)
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
C
C  Local variables
C
      CHARACTER*80 IHEDIN
      CHARACTER*20 RECIN
      DOUBLE PRECISION BREENG(MXNC),C
      DOUBLE PRECISION CCR(N10),COUENG(MXNC)
      DOUBLE PRECISION COUVEC(MXNC,MXNC),E(MXNW),EAV
      DOUBLE PRECISION ECV(YYLM),EPH,H,PARM(4)
      DOUBLE PRECISION PZ(MXNW),QZ(MXNW),RNT,RR
      DOUBLE PRECISION STEP1,STEP2,Z,Z1
      INTEGER I,ICCMIN(MXNC)
      INTEGER ICHOP(MXNW,MXNC),IECC(YYLM),ILAST
      INTEGER IQ(MXNW,MXNC),ISPAR(MXNC)
      INTEGER ITJPO(MXNC),J,JCUP(10,MXNC)
      INTEGER JQS(3,MXNW,MXNC),K,NCF
      INTEGER NCFTT,NCMIN,NEC,NPARM
      INTEGER NUCTYP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      READ (ITAPE3) IHEDIN,RECIN
      READ (ITAPE3) NCMIN,NW,NCF,N
      READ (ITAPE3) Z,RNT,H,C
C
      WRITE (IWRITE,3000) IHEDIN(1:40),IHEDIN(41:80),RECIN
      WRITE (IWRITE,3020) NCMIN,NW,NCF,N
      WRITE (IWRITE,3030) Z,RNT,H,C
C
      IF (Z.LT.ZERO) THEN
        PRINT 3010
        WRITE (IWRITE,3010)
        STOP
      ENDIF
C
      CALL DMCHK0(07,NCF, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK0(11,N+1, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK0(12,NW, IWRITE, IDMTST, NDIMAX)
C
C   An extra point is included for the wave-functions
C   and nuclear potential at r=0.
C
      ILAST = 0
      DO I = 1,NW
        IMCDF(I) = ILAST
        READ (ITAPE3) NH(I),NP(I),NAK(I),E(I)
        READ (ITAPE3) PZ(I),QZ(I)
        CALL DMCHK0(13,ILAST+1+N, IWRITE, IDMTST, NDIMAX)
        CP(ILAST+1) = ZERO
        CQ(ILAST+1) = ZERO
        READ (ITAPE3) (CP(ILAST+1+J),J=1,N), (CQ(ILAST+1+J),J=1,N)
        ILAST = ILAST+1+N
      ENDDO
C-----------------------------------------------------------------------
      READ (ITAPE3) (COUENG(I),I=1,NCF)
      READ (ITAPE3) (UCF(I),I=1,NW)
      IF (NCMIN.GT.0) THEN
        IF (NCMIN.GT.YYOL) THEN
          PRINT 3050,YYOL,NCMIN
          WRITE (IWRITE,3050) YYOL,NCMIN
          CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
          STOP
        ENDIF
        NCFTT = NCF*NCMIN
        READ (ITAPE3) (CCR(I),I=1,NCFTT)
        READ (ITAPE3) (ICCMIN(I),I=1,NCMIN)
      ENDIF
C
      READ (ITAPE3) ((IQ(I,J),J=1,NCF),I=1,NW)
      READ (ITAPE3) EAV, ((COUVEC(I,J),J=1,NCF),I=1,NCF),(BREENG(I),I=1,!
     +NCF)
C
      READ (ITAPE3) NEC
      IF (NEC.GT.YYLM) THEN
        PRINT 3060,YYLM,NEC
        WRITE (IWRITE,3060) YYLM,NEC
        CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
        STOP
      ENDIF
      IF (NEC.GT.0) READ (ITAPE3) (IECC(I),I=1,NEC), (ECV(I),I=1,NEC)
C
      READ (ITAPE3) NUCTYP
C
      IF (NUCTYP.EQ.0) THEN
        ZZ(1) = Z
      ELSE
        ZZ(1) = ZERO
      ENDIF
C
      READ (ITAPE3) (ZZ(I+1),I=1,N)
      READ (ITAPE3) Z1
      READ (ITAPE3) NPARM
      IF (NPARM.GT.0) READ (ITAPE3) (PARM(I),I=1,NPARM)
C
      READ (ITAPE3) ((ICHOP(I,J),I=1,NW),J=1,NCF)
      READ (ITAPE3) (((JQS(K,I,J),I=1,NW),J=1,NCF),K=1,3)
      READ (ITAPE3) ((JCUP(I,J),I=1,8),J=1,NCF)
      READ (ITAPE3) (ITJPO(J),J=1,NCF)
      READ (ITAPE3) (ISPAR(J),J=1,NCF)
C-----------------------------------------------------------------------
      CLOSE (ITAPE3)
C-----------------------------------------------------------------------
C
C  Set the logarithmic mesh
C
C-----------------------------------------------------------------------
      EPH = EXP(H)
      RR = RNT
      DR(1) = ZERO
      DO I = 1,N
        DR(I+1) = RR
        RR = RR*EPH
      ENDDO
      N = N+1
C-----------------------------------------------------------------------
      STEP1 = DR(2)-DR(1)
      STEP2 = DR(N)-DR(N-1)
      WRITE (IWRITE,3040) N,H,EPH,DR(1),DR(N),STEP1,STEP2
C-----------------------------------------------------------------------
 3000 FORMAT (/' Read GRASP dump with title :'//1X,A40/1X,A40/1X,A20//  !
     +' GRASP problem'/' -------------')
 3010 FORMAT (/' ********************************************'/         !
     +' *** WARNING : code is stopping in MCDF09 ***'/                  !
     +' ***       unconverged GRASP dump         ***'/                  !
     +' ********************************************')
 3020 FORMAT (/' NCMIN (MCDF calculation type)        : ',I6/           !
     +' NW    (orbitals)                     : ',I6/                    !
     +' NCF   (configurations)               : ',I6/                    !
     +' N     (log. mesh number of points)   : ',I6)
 3030 FORMAT (' Z     (atomic number)                : ',F7.2/          !
     +' RNT   (log. mesh 1st point)          : ',1P,E14.7/              !
     +' H     (log. mesh step-size)          : ',E14.7/                 !
     +' C     (speed of light)               : ',E14.7)
 3040 FORMAT (/'  Logarithmic mesh is defined as follows :'//           !
     +'  number of points (N) = ',I4/'  stepsize         (H) = ',1P,E12.!
     +5/'  EXP(H)               = ',E12.5/'  first mesh point     = ',E1!
     +2.5/'  last  mesh point     = ',E12.5/'  initial stepsize     = ',!
     +E12.5/'  final   stepsize     = ',E12.5)
 3050 FORMAT (/' Dimension ERROR in routine MCDF09'/                    !
     +' You must increase parameter YYOL (now ',I6,') to at least ',I6)
 3060 FORMAT (/' Dimension ERROR in routine MCDF09'/                    !
     +' You must increase parameter YYLM (now ',I6,') to at least ',I6)
      END
C
C                             *******************
C
      SUBROUTINE MCDF10(IWRITE,ITAPE3,N,DR,ZZ,NW,NH,NP,NAK,IMCDF,UCF,CP,!
     +CQ, IDMTST, NDIMAX)
C
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
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.D0)
C
C   LM   number of lagrange multipliers
C   NUC  number of parameters in nuclear charge distribution
C   OL   number of levels to optimize on
C
      INTEGER YYLM
      PARAMETER (YYLM=2*MXNW)
      INTEGER YYNUC
      PARAMETER (YYNUC=6)
      INTEGER YYOL
      PARAMETER (YYOL=10)
C
C  Argument variables
C
      INTEGER IMCDF(*),ITAPE3,IWRITE,N
      INTEGER NAK(*),NP(*),NW
      CHARACTER*2 NH(*)
      DOUBLE PRECISION CP(*),CQ(*),DR(*)
      DOUBLE PRECISION UCF(*),ZZ(*)
      INTEGER IDMTST(*)
      INTEGER NDIMAX(*)
C
C  Local variables
C
      CHARACTER*8 IDATE
      CHARACTER*72 IHED
      CHARACTER*8 ITIME
      CHARACTER*4 STAT,TYPE
      DOUBLE PRECISION ATW,C,CCR(MXNC,YYOL)
      DOUBLE PRECISION COWMAN(MXNW),DIFEIG(MXNW)
      DOUBLE PRECISION DIFPQ(MXNW),E(MXNW),EAV
      DOUBLE PRECISION ECV(YYLM),EMT(MXNC,MXNC),EPH
      DOUBLE PRECISION H,PARM(YYNUC),PZ(MXNW)
      DOUBLE PRECISION QZ(MXNW),RNT,RR
      DOUBLE PRECISION SA(MXNC),STEP1,STEP2
      DOUBLE PRECISION XCAMAX(MXNW),Z,Z1
      DOUBLE PRECISION Z3,ZNORM
      INTEGER I,IASPAR(MXNC)
      INTEGER IATJPO(MXNC),ICCMIN(YYOL)
      INTEGER IECC(YYLM),ILAST,IQ(MXNC,MXNW)
      INTEGER ISPAR(MXNC),ITJPO(MXNC),J
      INTEGER MCOW(MXNW),MIX(MXNW),NCF,NCMIN
      INTEGER NEC,NNUC,NPARM,NUCTYP
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-----------------------------------------------------------------------
      READ (ITAPE3) IHED,ITIME,IDATE,TYPE,STAT
      WRITE (IWRITE,3000) IHED,ITIME,IDATE,TYPE,STAT
      READ (ITAPE3) NCF,NW
      WRITE (IWRITE,3010) NCF,NW
C
      CALL DMCHK0(07,NCF, IWRITE, IDMTST, NDIMAX)
      CALL DMCHK0(12,NW, IWRITE, IDMTST, NDIMAX)
C
      READ (ITAPE3) (NH(J),NP(J),NAK(J),J = 1,NW)
      READ (ITAPE3) ((IQ(I,J),I = 1,NCF),J = 1,NW)
      READ (ITAPE3) (ITJPO(I),ISPAR(I),I = 1,NCF)
C
      READ (ITAPE3) Z,ATW,RNT,H,C,NCMIN,N
      WRITE (IWRITE,3020) Z,ATW,RNT,H,C,NCMIN,N
C
      CALL DMCHK0(11,N+1, IWRITE, IDMTST, NDIMAX)
C
C   An extra point is included for the wave-functions
C   and nuclear potential at r=0.
C
      ILAST = 0
      DO I = 1,NW
        READ (ITAPE3) E(I),PZ(I),QZ(I)
        READ (ITAPE3) XCAMAX(I),MCOW(I),COWMAN(I)
        READ (ITAPE3) MIX(I),DIFEIG(I),DIFPQ(I)
        IMCDF(I) = ILAST
        CALL DMCHK0(13,ILAST+1+N, IWRITE, IDMTST, NDIMAX)
        CP(ILAST+1) = ZERO
        CQ(ILAST+1) = ZERO
        READ (ITAPE3) (CP(ILAST+1+J),J=1,N), (CQ(ILAST+1+J),J=1,N)
        ILAST = ILAST+1+N
      ENDDO
C
      READ (ITAPE3) (UCF(J),J = 1,NW)
      IF (NCMIN .NE. 0) THEN
        IF (NCMIN.GT.YYOL) THEN
          PRINT 3040,YYOL,NCMIN
          WRITE (IWRITE,3040) YYOL,NCMIN
          CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
          STOP
        ENDIF
        READ (ITAPE3) ((CCR(I,J),I = 1,NCF),J = 1,NCMIN)
        READ (ITAPE3) (ICCMIN(I),I = 1,NCMIN)
      ENDIF
C
      READ (ITAPE3) NEC
      IF (NEC.GT.YYLM) THEN
        PRINT 3050,YYLM,NEC
        WRITE (IWRITE,3050) YYLM,NEC
        CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
        STOP
      ENDIF
      IF (NEC .NE. 0)READ (ITAPE3) (IECC(I),I = 1,NEC),(ECV(I),I = 1,NEC!
     +)
C
      READ (ITAPE3) NUCTYP,NPARM,NNUC
      IF (NPARM.GT.YYNUC) THEN
        PRINT 3060,YYNUC,NPARM
        WRITE (IWRITE,3060) YYNUC,NPARM
        CALL DMPRT0(IWRITE, IDMTST, NDIMAX)
        STOP
      ENDIF
      READ (ITAPE3) (ZZ(I),I = 1,N)
      READ (ITAPE3) ZNORM,Z1,Z3
      IF (NPARM .NE. 0) READ (ITAPE3) (PARM(I),I = 1,NPARM)
C
      READ (ITAPE3) (IATJPO(I),IASPAR(I),I = 1,NCF)
      READ (ITAPE3) EAV,(SA(I),I = 1,NCF)
      DO J = 1,NCF
        READ (ITAPE3) (EMT(I,J),I = 1,NCF)
      ENDDO
C-----------------------------------------------------------------------
      CLOSE (ITAPE3)
C
C  Set the logarithmic mesh
C
      EPH = EXP(H)
      RR = RNT
      DR(1) = ZERO
      DO I = 1,N
        DR(I+1) = RR
        RR = RR*EPH
      ENDDO
      N = N+1
C
      STEP1 = DR(2)-DR(1)
      STEP2 = DR(N)-DR(N-1)
      WRITE (IWRITE,3030) N,H,EPH,DR(1),DR(N),STEP1,STEP2
C-----------------------------------------------------------------------
 3000 FORMAT (/' Read GRASP1 MCDF dump with title :'//1X,A72/1X,A8/1X,A8!
     +/1X,A4/1X,A4//' GRASP problem'/' -------------')
 3010 FORMAT (/' NW    (orbitals)                     : ',I6/           !
     +' NCF   (configurations)               : ',I6)
 3020 FORMAT (' Z     (atomic number)                : ',F7.2/          !
     +' ATW   (atomic weight)                : ',1P,E14.7/              !
     +' RNT   (log. mesh 1st point)          : ',E14.7/                 !
     +' H     (log. mesh step-size)          : ',E14.7/                 !
     +' C     (speed of light)               : ',E14.7/                 !
     +' NCMIN (MCDF calculation type)        : ',I6/                    !
     +' N     (log. mesh number of points)   : ',I6)
 3030 FORMAT (/'  Logarithmic mesh is defined as follows :'//           !
     +'  number of points (N) = ',I4/'  stepsize         (H) = ',1P,E12.!
     +5/'  EXP(H)               = ',E12.5/'  first mesh point     = ',E1!
     +2.5/'  last  mesh point     = ',E12.5/'  initial stepsize     = ',!
     +E12.5/'  final   stepsize     = ',E12.5)
 3040 FORMAT (/' Dimension ERROR in routine MCDF10'/                    !
     +' You must increase parameter YYOL (now ',I6,') to at least ',I6)
 3050 FORMAT (/' Dimension ERROR in routine MCDF10'/                    !
     +' You must increase parameter YYLM (now ',I6,') to at least ',I6)
 3060 FORMAT (/' Dimension ERROR in routine MCDF10'/                    !
     +' You must increase parameter YYNUC (now ',I6,') to at least ',I6)
      END
