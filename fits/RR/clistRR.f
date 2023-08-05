C N.R. BADNELL                     UoS                            03/03/06
      PROGRAM CLIST
      IMPLICIT REAL*4(A-H,O-Z)
C
C READ A SINGLE OR ARBITRARY CONCATENATED SET OF "cfout" RR FIT FILES
C AND ORDER THEM FOR OUTPUT WITH OUTER LOOP OVER SEQUENCE AND INNER
C LOOP OVER ELEMENT. A, B, T0, T1 COEFFICIENTS WRITTEN WITH ALL VALUES 
C FOR A SINGLE ION PER LINE.
C *OBVIOUSLY*, USERS CAN TAIL THE FORMAT OF THE OUTPUT TO THEIR OWN DESIRE.
C
C USEFUL VARIABLES/ARRAYS INCLUDE:
C 
C NION= NUMBER OF IONIC DATASETS
C NZ(NION)= NUCLEAR CHARGE OF DATASET NION I.E. ELEMENT
C NE(NION)= NUMBER OF TARGET ELECTRONS FOR DATASET NION I.E. SEQUENCE.
C INDX(M,IZ,IN)=NION POSITION OF METASTABLE M, ELEMENT IZ AND SEQUENCE IN.
C IWGHT(NION)= STATISTICAL WEIGHT OF METASTABLE M OF NION.
C IWD= DEFAULT GROUND LEVEL WEIGHT FOR (OLD) FILES MISSING WEIGHT INFO.
C NCF(NION)=NUMBER OF COEFFICIENT SETS FOR DATASET NION.
C
C NO USER INPUT REQUIRED, OTHER THAN
C INPUT FILE OF FIT DATA: cfout
C OUTPUT FILE FIT LIST:   clist
C
C
      PARAMETER(MAXION=2500)              !MAX NUMBER OF DATASETS
      PARAMETER(MAXCF =   1)              !MAX NUMBER OF COEFFS PER ION
      PARAMETER(MXMETA=10)                !MAX NUMBER OF METASTABLES PER ION
C
      LOGICAL EX
      CHARACTER*1 CHAR(70)
      CHARACTER*8 DATE
C
      DIMENSION INDX(MXMETA,100,0:100),NCF(MAXION)
     X         ,IWGHT(MAXION),IWD(0:100),IMX(0:100)
      DIMENSION CF(6,MAXCF,MAXION),CC(6)
C
      DATA DATE/'19581027'/
C
      INQUIRE(FILE='cfout',EXIST=EX)
      IF(.NOT.EX)STOP'*** NO cfout FILE OF FIT DATA FOUND!'
C  
      OPEN(UNIT=7,FILE='cfout',STATUS='OLD')
      OPEN(UNIT=8,FILE='clist',STATUS='UNKNOWN')
C
      IZX=0
      INX=0
      METAX=0
      NION=0   
      DO IN=0,100
        DO IZ=1,100
          DO M=1,MXMETA
            INDX(M,IZ,IN)=0
          ENDDO
        ENDDO
      ENDDO 
      DO IN=0,100
        IMX(IN)=0
      ENDDO
      DO IN=1,MAXION                 
        CF(6,1,IN)=0.0D0
      ENDDO
C
C DEFAULT GROUND-LEVEL WEIGHTS (FOR OLD FILES MISSING W).
C
      IWD(0)=1
      IWD(1)=2
      IWD(2)=1
      IWD(3)=2
      IWD(4)=1
      IWD(5)=2
      IWD(6)=1
      IWD(7)=4
      IWD(8)=5
      IWD(9)=4
      IWD(10)=1
      IWD(11)=2
      IWD(12)=1           
C
C LOOK FOR A NEW HEADER
C
  100 READ(7,1000,END=9999)CHAR
 1000 FORMAT(70A1)
C
      IF(CHAR(1).EQ.'Z')THEN                  !WE HAVE A HEADER
        IF(CHAR(3).EQ.'X')THEN
          WRITE(*,*)'*** MISSING ION SPECIFICATION',CHAR
          STOP'*** MISSING ION SPECIFICATION'
        ENDIF
C
        BACKSPACE(7)
        READ(7,1010)IZ,IN,META,IWJ
 1010   FORMAT(2X,I2,3X,I2,3X,I2,3X,I2)
c        write(*,1010)iz,in,meta,iwj
C
        IF(IZ.EQ.0)THEN
          WRITE(*,*)'*** MIX-UP ON HEADER Z,N=',IZ,IN
          STOP '*** ABORT, CHECK cfout FILE'
        ENDIF
        IF(META.EQ.0)THEN
          META=1
          IF(IN.GT.12)STOP '*** EXTEND DEFAULT GROUND LEVEL WEIGHTS'
          IWJ=IWD(IN)
        ENDIF
C
        NION=NION+1
        IF(NION.GT.MAXION)STOP '*** INCREASE PARAMETER MAXION'
        IF(META.GT.MXMETA)STOP '*** INCREASE PARAMETER MXMETA'
C
        IF(INDX(META,IZ,IN).NE.0)THEN
          WRITE(*,*)'*** DUPLICATE DATASES FOR Z,N,M=',IZ,IN,META
          STOP '*** ABORT, CHECK cfout FILE'
        ENDIF
        INDX(META,IZ,IN)=NION
        NCF(NION)=0
        IWGHT(NION)=IWJ
        IZX=MAX(IZX,IZ)
        INX=MAX(INX,IN)
        IMX(IN)=MAX(IMX(IN),META)
        METAX=MAX(METAX,META)
      ELSE
        IF(CHAR(3).EQ.'N')THEN
          WRITE(*,*)'*** MISSING HEADER? Z,N,M=',IZ,IN,META
          STOP'*** MISSING HEADER??'
        ENDIF
        GO TO 100
      ENDIF
C
C LOOK FOR COEFFICIENT LINE
C
  150 READ(7,1000)CHAR
      IF(CHAR(3).NE.'N')GO TO 150             !NOT FOUND YET
      NRD=4
      IF(CHAR(42).NE.' ')NRD=6      
C
  160 READ(7,1020)N,(CC(I),I=1,NRD)                   !COEFFS
 1020 FORMAT(I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)
C
      IF(N.GT.0)THEN
        NCF(NION)=NCF(NION)+1
        IF(NCF(NION).GT.MAXCF)STOP'*** INCREASE PARAMETER MAXCF'
        DO I=1,NRD
          CF(I,NCF(NION),NION)=CC(I)
        ENDDO
        GO TO 160
      ELSE
        GO TO 100                             !LOOK FOR A NEW HEADER
      ENDIF
C
C NO MORE DATA SO CHECK COMPLETENESS
C
 9999 DO IN=0,INX
        DO IZ=IN+1,IZX
          DO IM=1,IMX(IN)
            IF(INDX(IM,IZ,IN).EQ.0.AND.
     X             (IZ.LE.30.OR.IZ.EQ.36.OR.IZ.EQ.42.OR.IZ.EQ.54))
     X         WRITE(*,*)'***MISSING DATASET FOR Z,N,M=',IZ,IN,IM
          ENDDO
        ENDDO
      ENDDO
C
C****************************************************
C NOW WRITE IT ALL BACK OUT AGAIN ***USER SPECIFIC***
C****************************************************
C
C FIRST, HEADER INFO
C
      CALL DATE_AND_TIME(DATE)  !F90 - COMMENT-OUT IF USUNG F77.
C
      WRITE(8,2222)DATE
 2222 FORMAT('RR RATE COEFFICIENT FITS (C)',A8,' N. R. BADNELL,',
     X' DEPARTMENT OF PHYSICS, UNIVERSITY OF STRATHCLYDE,',
     X' GLASGOW G4 0NG, UK.'/)
C
C FOR EACH METASTABLE
C
c      DO 777 M=1,METAX
C
C THEN, ALL C-COEFFICIENTS
C
      WRITE(8,2000)
 2000 FORMAT('  Z','  N','  M','  W',2X,4X,'A',8X,'B',8X,'T0',       !***OPTIONAL HEADER
     X       9X,'T1',8X,'C',8X,'T2')
C
      DO IN=0,INX
        DO 775 IZ=IN+1,IZX
          DO M=1,IMX(IN)
            J=INDX(M,IZ,IN)
            IF(J.EQ.0)GO TO 775
            NWR=4
            IF(CF(6,1,J).GT.1.D-10)NWR=6
            DO N=1,NCF(J)
              WRITE(8,2001)IZ,IN,M,IWGHT(J),(CF(I,N,J),I=1,NWR)
 2001         FORMAT(4I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)   !*** USER SPECIFIC
            ENDDO
          ENDDO
  775   CONTINUE
      ENDDO
C
c  777 CONTINUE
C
      STOP 'MASTER COEFFICIENT LIST IN FILE clist'
      END
