C N. R. BADNELL   BURG      UoS v1.4                   10/03/20
C
      PROGRAM MAIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C BURGESS PLOTS
C   ITYPE =1 ELECTRIC DIPOLE
C   ITYPE =2 ALLOWED (NON-ELECTRIC DIPOLE, NON-EXCHANGE)
C   ITYPE =3 EXCHANGE/FORBIDDEN
C   ITYPE =4 ELECTRIC DIPOLE, VERY SMALL GF
C   C  =BURGESS C-VALUE
C   C1,C2 =ALTERNATIVE TREATMENT FOR ITYPE=3
C   DE =EXCITATION ENERGY (RYD) !INPUT OPTIONAL, READ FROM FILE xout
C
C THE INFINITE ENERGY (X=1) Y-VALUE, MAY ALREADY BE IN OMEGA.
C
C   GF =SYMMETRIC OSCILLATOR STRENGTH (ITYPE=1 OR 4)
C    S = LINE STRENGTH
C   GR= SYMMETRIC RADIATIVE RATE
C    G= STAT WEIGTH THAT GF OR GR NEEDS TO BE MULTIPLIED TO MAKE
C       THEM SYMMETRIC, I.E. IF GF AND GR ARE NOT INPUT SYMMETRIC.
C   BORN =BORN LIMIT (ITYPE=2)
C   OCHKUR = OCHKUR LIMIT (ITYPE=3)
C
      LOGICAL EX,EX4,EX5
C
      PARAMETER (MXPTS=35001)
      DIMENSION X(MXPTS),Y(MXPTS),E(MXPTS),O(MXPTS)
      NAMELIST /BURG/ITYPE,C,C1,C2,DE,GF,BORN,OCHKUR,S,ISPN,G,GR
C
      INQUIRE(FILE='dbin',EXIST=EX5)
      IF(EX5)OPEN(5,FILE='dbin',STATUS='OLD')
      OPEN(6,FILE='dbout',STATUS='UNKNOWN')
      OPEN(7,FILE='xbout',STATUS='UNKNOWN')    !BURGESS PLOT DATA
C
      INQUIRE(FILE='xout',EXIST=EX)
      IF(EX)THEN
        OPEN(8,FILE='xout',STATUS='OLD')       !EXTRACT FROM OMEGA
        WRITE(*,*)'Opening file xout'
        EX4=.FALSE.                            !*OR* ADF04 (T3) upsout
      ELSE
        INQUIRE(FILE='x4out',EXIST=EX4)
        IF(EX4)THEN
          OPEN(8,FILE='x4out',STATUS='OLD')    !EXTRACT FROM ADF04 T1/5
          WRITE(*,*)'Opening file x4out'
        ELSE
          STOP 'NO COLLISION DATA FOUND'
        ENDIF
      ENDIF
C
C PRELIMINARY READ OF TRANSITION & ENERGY, CAN BE OVERRIDDEN BY NAMELIST
C
      READ(8,100)IS,JS,DE
C
      ITYPE=0
      GF=0.0
      S=0.0
      GR=0.0
      G=0.0
      BORN=0.0
      OCHKUR=0.0
      C=3.0
      C1=0.0
      C2=0.0
      ISPN=2
C
      IF(EX5)READ(5,BURG,END=1,ERR=1)
C
   1  IF(DE.LE.0.0)THEN
        WRITE(6,*)'DE MUST BE GREATER THAN ZERO, =', DE
        STOP 'INVALID TRANSITION ENERGY'
      ENDIF
C
C PRELIMINARY WRITE OF HEADER
C
      WRITE(7,150)IS,JS,DE
C
      I=0
      EI=0.0
      E0=0.0
C
   3  IF(EX)THEN
        READ(8,*,END=5)E0,EI,E(I+1),O(I+1)
        IF(E0.LT.1.E6.AND.EI.LT.1.E20.AND.O(I+1).LT.0.0)GO TO 3
      ELSE                                                     !THEN EX4
        READ(8,*,END=5)E(I+1),O(I+1)
        EI=E(I+1)
        IF(EI.LT.1.E20.AND.O(I+1).LT.0.0)GO TO 3
      ENDIF
C
      I=I+1
      IF(I.GE.MXPTS)THEN
        WRITE(6,*)'DIMENSION EXCEEDED, INCREASE MXPTS'
      ELSE
        GO TO 3
      ENDIF
C
   5  NPTS=I
      IF(EX4.AND.E(1).GE.100.)THEN                      !PROBABLY KELVIN
        WRITE(*,*)'Energies in Kelvins detected in x4out, converting...'
        DO I=1,NPTS-1
          E(I)=E(I)/.15789E6
        ENDDO
        IF(E(NPTS).LT.1.E20)E(NPTS)=E(NPTS)/.15789E6        !NO INFINITE
      ENDIF
C
      IF(ITYPE.LE.0.OR.ITYPE.GT.4)THEN
        ETEST=-1
        IF(EX)THEN
          IF(E0.GE.1.E6.OR.EI.GT.1.E20)ETEST=1
        ELSE
          IF(E(NPTS).GE.1.E20)ETEST=1
        ENDIF
        IF(ETEST.GT.0)THEN
          ITYPE=3
          IF(O(NPTS).LT.0.)ITYPE=1
          IF(O(NPTS).GT.0.)ITYPE=2
        ELSE
          WRITE(6,*)'CANNOT DETERMINE ITYPE'
          STOP 'INVALID ITYPE'
        ENDIF
      ENDIF
C
      GO TO (10,20,30,10),ITYPE
C
C
C TYPE=1 OR 4
C
  10  IF(C.LE.1.0)THEN
      WRITE(6,*)'C MUST BE .GT. UNITY, C=',C
      STOP 'INVALID C PARAMETER'
      ENDIF
      IF(ITYPE.EQ.1)EE=EXP(1.0)
      IF(ITYPE.EQ.4)EE=C
C
      DO 11 I=1,NPTS
        IF(E(I).GE.0.0)THEN
          X(I)=1.0-LOG(C)/LOG(E(I)/DE+C)
          Y(I)=O(I)/LOG(E(I)/DE+EE)
        ELSE
          X(I)=0.0
          Y(I)=0.0
        ENDIF
  11  CONTINUE
C
      IF(E0.GE.1.E6)THEN    !FOR OMEGA
        X(NPTS)=1.
        Y(NPTS)=ABS(O(NPTS))/LOG(E0)
      ENDIF
      IF(EI.GT.1.E20)THEN   !FOR UPSILON
        X(NPTS)=1.
        Y(NPTS)=ABS(O(NPTS))
      ENDIF
C
      IF(GR.GT.0.0.OR.GF.GT.0.0.OR.S.GT.0.0)THEN
        IF(X(NPTS).NE.1.)THEN
          NPTS=NPTS+1
          X(NPTS)=1.0
        ENDIF
        IF(GF.GT.0.0)Y(NPTS)=4.*GF/DE
        IF(S.GT.0.0)Y(NPTS)=4.*S/3.
        IF(GR.GT.0.0)Y(NPTS)=4.*1.245E-10*GR/DE**3
        IF(G.GT.0.0)Y(NPTS)=Y(NPTS)*G
      ENDIF
      GO TO 50
C
C TYPE=2
C
  20  IF(C.LE.0.0)THEN
        WRITE(6,*)'C MUST BE POSITIVE, C=',C
        STOP 'INVALID C PARAMETER'
      ENDIF
C
      DO 21 I=1,NPTS
        IF(E(I).GE.0.0)THEN
          X(I)=(E(I)/DE)/(E(I)/DE+C)
          Y(I)=O(I)
        ELSE
          X(I)=0.0
          Y(I)=0.0
        ENDIF
  21  CONTINUE
C
      IF(E0.GE.1.E6)THEN    !FOR OMEGA
        X(NPTS)=1.
        Y(NPTS)=O(NPTS)
      ENDIF
      IF(EI.GT.1.E20)THEN   !FOR UPSILON
        X(NPTS)=1.
        Y(NPTS)=O(NPTS)
      ENDIF
C
      IF(BORN.GT.0.0)THEN
        IF(X(NPTS).NE.1.)NPTS=NPTS+1
        X(NPTS)=1.0
        Y(NPTS)=BORN
      ENDIF
      GO TO 50
C
C TYPE=3
C
  30  IF(C.GT.0.0)THEN
        C1=C
        C2=DE
      ELSE
        IF(C1.LE.0.0.OR.C2.LE.0.0)THEN
          WRITE(6,*)'REQUIRE C1 & C2 POSITIVE; C1,C2=',C1,C2
          STOP 'INVALID C1,C2 PARAMETERS'
        ENDIF
      ENDIF
C
      DO 31 I=1,NPTS
        IF(E(I).GE.0.0)THEN
          X(I)=(E(I)/C2)/(E(I)/C2+C1)
          Y(I)=(E(I)/C2+1.0)**ISPN*O(I)
        ELSE
          X(I)=0.0
          Y(I)=0.0
        ENDIF
  31  CONTINUE
C
      IF(E0.GE.1.E6)THEN
        X(NPTS)=1.
        Y(NPTS)=O(NPTS)*E0**ISPN
      ENDIF
      IF(EI.GT.1.E20)THEN   !FOR UPSILON
        X(NPTS)=1.
        Y(NPTS)=O(NPTS)
      ENDIF
C
      IF(OCHKUR.GT.0.0)THEN
        IF(X(NPTS).NE.1.)NPTS=NPTS+1
        X(NPTS)=1.0
        Y(NPTS)=OCHKUR/C2**2
      ENDIF
C
C WRITE REDUCED X-Y PARAMETERS
C
  50  DO 60 I=1,NPTS
        WRITE(7,200)X(I),Y(I)
  60  CONTINUE
C
 100  FORMAT(1X,I3,2X,I4,20X,F12.6)
 150  FORMAT('#',I3,' -',I4,' TRANSITION, ENERGY=',F12.6,' RYD')
 200  FORMAT(1X,F12.6,1PE12.5)
C
      STOP 'Burgess scaled OMEGA/UPSILON IN xbout'
      END
