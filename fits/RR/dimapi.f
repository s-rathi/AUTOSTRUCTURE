C      PROGRAM DIMAPI
C
C EVALUATE PI FITS OF VERNER ET AL AP.J. V465, 487-98 (1996)
C
C      NENG.GT. READ UNSCALED ELECTRON ENERGY IN RYD.
C          .LE.0 USE DEFAULT UNSCALED AUTOS ELECTRON MESH: COARSE=0, FINE<0.
C      NZ .EQ. RESIDUAL CHARGE TO SCALE DEFAULT ENERGIES.
C      TEAPOT .NE. 0: IONIZATION POTENTIAL ADDED TO ENERG TO GET
C            PHOTON ENERGY. SET ZERO IF PHOTON ENERGY READ-IN.
C            >0 EV, <0 RYD.
C
C
      PARAMETER (MXTABC=25)            !SET FOR EDUM IN DATA BELOW
      PARAMETER (MXTABF=45)            !SET FOR EDUM IN DATA BELOW
C
      DIMENSION ENERG(MXTABF),EDUMC(MXTABC),EDUMF(MXTABF)
C
C DEFAULT MESH, MXTABC=25
      DATA EDUMC/0.0D+0,2.1D-6,4.6D-6,1.0D-5,2.1D-5,4.6D-5,            
     X           1.0D-4,2.1D-4,4.6D-4,1.0D-3,2.1D-3,4.6D-3,            
     X           1.0D-2,2.1D-2,4.6D-2,1.0D-1,2.1D-1,4.6D-1,
     X           1.0D+0,2.1D+0,4.6D+0,1.0D+1,2.1D+1,4.6D+1,1.0D+2/  
C TEST FINER MESH, MXTABF=45  
      DATA EDUMF/0.0D+0,1.6D-6,2.5D-6,4.0D-6,6.3D-6,              
     X           1.0D-5,1.6D-5,2.5D-5,4.0D-5,6.3D-5,              
     X           1.0D-4,1.6D-4,2.5D-4,4.0D-4,6.3D-4,
     X           1.0D-3,1.6D-3,2.5D-3,4.0D-3,6.3D-3,
     X           1.0D-2,1.6D-2,2.5D-2,4.0D-2,6.3D-2,
     X           1.0D-1,1.6D-1,2.5D-1,4.0D-1,6.3D-1,
     X           1.0D+0,1.6D+0,2.5D+0,4.0D+0,6.3D+0,
     X           1.0D+1,1.6D+1,2.5D+1,4.0D+1,6.3D+1,
     X           1.0D+2,1.6D+2,2.5D+2,4.0D+2,6.3D+2/
C TEST COARSER MESH, MXTABC=10 OR 13
c      DATA EDUMC/0.D0,1.D-6,1.D-5,1.D-4,1.D-3,1.D-2,1.D-1,1.D0,1.D1,1.D2/
c     X,1.D3,1.D4,1.D5/                                                 
C
      NAMELIST/DIMAPI/NZ,SIG0,E0,Y0,Y1,YA,YW,P,L,NENG,TEAPOT
C
      OPEN(5,FILE='dinpi')
      OPEN(6,FILE='doutpi')
C
      NZ=1
      SIG0=-1.
      E0=-1.
      Y0=0.0
      Y1=0.0
      YA=-1.
      YW=-1.
      P=-1.
      L=0
      NENG=0
      TEAPOT=0
C
      READ(5,DIMAPI)
C
      IF(SIG0.LT.0.OR.E0.LT.0.OR.YA.LT.0.OR.YW.LT.0.OR.P.LT.0)THEN
        WRITE(6,*)'FIT PARAMETER NOT SET!'
        STOP 'FIT PARAMETER NOT SET!'
      ENDIF
C
      IF(NENG.GT.MXTABF)STOP 'INCREASE MXTABF TO NENG'
      IF(NENG.GT.0)READ(5,*)(ENERG(N),N=1,NENG)
C
      IF(TEAPOT.GT.0)TEAPOT=TEAPOT/13.606
      TEAPOT=ABS(TEAPOT)
C
      NZ2=NZ*NZ
      IF(NENG.EQ.0)THEN
        NENG=MXTABC
        DO N=1,NENG
          ENERG(N)=EDUMC(N)*NZ2
        ENDDO
      ELSEIF(NENG.LT.0)THEN
        NENG=MXTABF
        DO N=1,NENG
          ENERG(N)=EDUMF(N)*NZ2
        ENDDO
      ENDIF
C
      DO N=1,NENG
        E=ENERG(N)+TEAPOT
        X=13.606*E/E0-Y0
        Y=SQRT(X*X+Y1*Y1)
        YP=Y**(0.5*P-5.5-L)
        F=YP*((X-1)**2+YW*YW)/(1+SQRT(Y/YA))**P
        SIGMA=SIG0*F
        WRITE(6,200)ENERG(N),E,SIGMA,SIGMA*E**(3.5+L)
      ENDDO
C
 200  FORMAT(1P,2E14.5,2E12.4)
C
      END

