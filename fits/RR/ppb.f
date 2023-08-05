      PROGRAM PPB                                                   ! 29/09/05
      IMPLICIT REAL*8 (A-H,O-Z)
C
C EVALUATE RR FITS OF PEQUIGNOT, PETITJOHN & BOISSON, A&A V251 PP680-8 (1991).
C ADD NZA: .GT. 0 USES ADAS Z-SCALED TEMPS, .LT.0 INTERACTIVE.
C
      PARAMETER (NDIM28=19)
      PARAMETER (CONEV=8.61042E-5)
C
      DIMENSION THTIC(NDIM28)
      DATA
     XTHTIC/1.0E1,2.0E1,5.0E1,1.0E2,2.0E2,5.0E2,1.0E3,2.0E3,5.0E3,1.0E4
     X    ,2.0E4,5.0E4,1.0E5,2.0E5,5.0E5,1.0E6,2.0E6,5.0E6,1.0E7/
C
      OPEN(5,FILE='din')
      OPEN(6,FILE='dout')
C
      READ(5,*) NZA,A,B,C,D
C
      IF(NZA.LT.0)THEN
   5    WRITE(*,*)'INPUT TEMP (K.gt.0, eV.lt.0, =0 to exit)'
        READ(*,*)TT
        IF(TT.EQ.0.)STOP 'BYE'
        IF(TT.LT.0.)THEN
          TEMP=-TT/CONEV
        ELSE
          TEMP=TT
        ENDIF         
        TT=TT*1.D-4/NZA**2
        ALF=1.D-13*NZA*A*TT**B/(1.0D0+C*TT**D)
        WRITE(*,*)'RATE COEF.= ',ALF
        GO TO 5
      ELSE
        WRITE(6,731)
        NZA2=NZA*NZA
        DO I=1,NDIM28
        TEMP=NZA2*THTIC(I)
        TT=TEMP*1.D-4/NZA2
        ALF=1.D-13*NZA*A*TT**B/(1.0D0+C*TT**D)
        TT=TEMP/CONEV
        WRITE(6,732)TEMP,ALF,TT
        ENDDO
      ENDIF
C
 731  FORMAT(//'    T(K) ',4X,'ALF(TOT)',4X,' T(EV)')
 732  FORMAT(1PE10.2,2(1X,E10.2))
C
       END
