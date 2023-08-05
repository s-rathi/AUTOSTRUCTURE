      PROGRAM XTRCTPI
      IMPLICIT REAL*8(A-H,O-Z)
C
C     EXTRACT A PARTIAL/TOTAL FROM A STANDARD ADAS ADF39PX/TX FILE
C     SET FINAL-STATE INDEX=0 FOR TOTALS.
C
      CHARACTER LAB2*2,LAB4*4
      PARAMETER (MXENG=9999)
      DIMENSION ENERG(MXENG),PCS(MXENG)
C
      OPEN(7,FILE='xout',STATUS='UNKNOWN')
C
      WRITE(*,*)'ENTER TRANSITION REQUIRED (i,f =0 totals)'
      READ(*,*)IT1,IT2  !,L  !FOR ASYMPTOTIC SCALING OF PARTIAL PI XSCTN
C
      IF(IT1.LE.0)STOP '*** NO VALID TRANSITION SPECIFIED, EXITING!'
      IF(IT2.LT.0)IT2=0
C
      IF(IT2.EQ.0)THEN                                           !TOTALS
        DE0=0.D0             !ELECTRON ENERGY ALREADY RELATIVE TO GROUND
        L=0                      !AN S-ELECTRON WILL EVENTUALLY DOMINATE
        OPEN(1,FILE='adf39tx',STATUS='OLD')
      ELSE                                                     !PARTIALS
        WRITE(*,*)'  OPTIONALLY, ENTER A.M. OF TARGET ELECTRON & ',
     X            'ENERGY OF F ABOVE ION GROUND STATE (.LE. TO SKIP):'
        READ(*,*)L,DE0
        IF(DE0.LT.0.D0)DE=0.D0
        IF(L.LT.0)L=0
        OPEN(1,FILE='adf39px',STATUS='OLD')
      ENDIF
C
      READ(1,208)LAB2,NZ,LAB4,NENG,(J,I=1,NENG)      
 208  FORMAT(A2,I2,5X,A4,21X,5X,I4/37X,2X            
     X               ,10(I4,8X)/(33X,10(6X,I4,2X)))  
C
      IF(NENG.GT.MXENG)THEN
        WRITE(*,*)'***INCREASE ,XENG TO: ',NENG
        STOP
      ENDIF
C
      READ(1,*)(ENERG(I),I=1,NENG)                   
C 207  FORMAT((31X,10D12.5))                         
C
      READ(1,209)(J,I=1,NENG)                        
 209  FORMAT(39X,10(I4,8X)/(33X,10(6X,I4,2X)))       
      READ(1,209)
C
C LOOP OVER ALL TRANSITIONS
C
      DO I=1,9999999
        READ(1,203,end=100)IRSL,IWP,IND0,DEL,(PCS(N),N=1,NENG)    
 203    FORMAT(3I5,1X,D15.6,10D12.3/(31X,10D12.3))        
        IF(IRSL.EQ.0)GO TO 100                              !END OF DATA
        IF(IRSL.EQ.IT1.AND.IND0.EQ.IT2)GO TO 101            !WE ARE DONE
      ENDDO
C
 100  WRITE(*,*)'REQUESTED TRANSITION NOT FOUND ON FILE...'
      STOP
C
C WRITE IT OUT
C
 101  WRITE(7,200)IT1,IT2,DEL                          !DEL INCLUDES DE0
 200  FORMAT('#',I3,' -',I4,' TRANSITION, ENERGY=',F12.6)
C
      ASY=3.5D0+L
      DO N=1,NENG
        PP=ABS(PCS(N))
        IF(PP.LT.1.D-10)PP=PP*1.D18
        EP=ENERG(N)+DEL                                   !PHOTON ENERGY
        E=ENERG(N)+DE0                                  !ELECTRON ENERGY
        WRITE(7,201)E,EP,PP,PP*EP**ASY
      ENDDO
 201  FORMAT(1P,2E14.5,2E12.4)
C
      STOP 'PI XSCTN IN xout'
C
      END
