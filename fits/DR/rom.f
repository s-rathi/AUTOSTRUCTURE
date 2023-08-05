      PROGRAM ROM                                                   ! 18/03/05
      IMPLICIT REAL*8 (A-H,O-Z)
C
C EVALUATE DR FITS OF ROMANIK, APJ V330 PP1022-35 (1988),
C
C ADD NZA: .GT. 0 USES ADAS Z-SCALED TEMPS, .LT.0 INTERACTIVE.
C
      PARAMETER (NDIM28=19)
      PARAMETER (CONEV=13.606)
      PARAMETER (CONK=.1579E6)
C
      DIMENSION THTIC(NDIM28),C(10),E(10)
      DATA
     XTHTIC/1.0E1,2.0E1,5.0E1,1.0E2,2.0E2,5.0E2,1.0E3,2.0E3,5.0E3,1.0E4
     X    ,2.0E4,5.0E4,1.0E5,2.0E5,5.0E5,1.0E6,2.0E6,5.0E6,1.0E7/
C
      OPEN(5,FILE='romin')
      OPEN(6,FILE='romout')
C
      READ(5,*) NZA, NFIT,(C(I),I=1,NFIT),(E(I),I=1,NFIT)
C
      IF(NZA.LT.0)THEN
   5    WRITE(*,*)'INPUT TEMP (K.gt.0, eV.lt.0, =0 to exit)'
        READ(*,*)TT
        IF(TT.EQ.0.)STOP 'BYE'
        IF(TT.LT.0.)THEN
          TEMP=-TT/CONEV
        ELSE
          TEMP=TT/CONK
        ENDIF         
        ALF=0.
        DO J=1,NFIT
          ALF=ALF+C(J)*EXP(-E(J)/TEMP)
        ENDDO
        ALF=ALF/SQRT(TEMP**3)
        WRITE(*,*)'RATE COEF.= ',ALF
        GO TO 5
      ELSE
        WRITE(6,731)
        NZA2=NZA*NZA
        DO I=1,NDIM28
          TT=NZA2*THTIC(I)
          TEMP=TT/CONK
          ALF=0.
          DO J=1,NFIT
            ALF=ALF+C(J)*EXP(-E(J)/TEMP)
          ENDDO
          ALF=ALF/SQRT(TEMP**3)
          WRITE(6,732)TT,ALF,TEMP*CONEV
        ENDDO
      ENDIF
C
 731  FORMAT(//'    T(K) ',4X,'ALF(TOT)',4X,' T(EV)')
 732  FORMAT(1PE10.2,2(1X,E10.2))
C
      END

