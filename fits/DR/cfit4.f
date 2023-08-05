C                                                               21/11/14
      PROGRAM CFIT4
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     PERFORM A NON-LINEAR LEAST-SQUARES FIT TO A RATE COEFFICIENT.
C     USES PAIRS OF COEFFICIENTS TO FORM SUM_I A_I*EXP(-B_I/T)/T**1.5
C
C INPUT (INTERACTIVE): NO. OF FREE FITTING PARAMETERS FOLLOWED BY
C     0 - 4 NON-ZERO TEMP INDEXES TO FIX ADDITIONAL PAIRS AT
C     THESE TEMP(S). IF NOT WANTED ENTER 0(s) INSTEAD
C
C     CAN TOGGLE NTEMP (FIT) AND TEMPR (READ) IN cfin
C     ALSO FLAG NSKP OF TEMPS.
C
      PARAMETER (NUMT=150)
      PARAMETER (NMETA=6)
C
C      CHARACTER ANS*1
C
      DIMENSION TEMP(NUMT),ADR(NUMT),SIG(NUMT),A(NUMT),IA(NUMT)
      DIMENSION COVAR(NUMT,NUMT),ALPHA(NUMT,NUMT),DUM(NUMT)
      DIMENSION ADRF(NUMT),TA(NMETA),IWJ(NMETA)
C
      EXTERNAL FUNCS
C
      OPEN(7,FILE='cfin')
      OPEN(8,FILE='cfout')
C
      DO N=1,NUMT
        TEMP(N)=0.0
        ADR(N)=0.0
      ENDDO
      NTEMP=-1
      NTEMPR=-1
      NSKP=-1
C
      WRITE(*,*)'Enter initial state index [1 for ground etc.]:'
      READ(*,*)META
C
      IF(META.GT.NMETA)STOP 'INCREASE DIMENSION NMETA'
C
      IF(META.GT.0)READ(7,*)NZ,NE,(IWJ(M),M=1,META)
C
      IF(META.LE.0)THEN
        META=1
        IWJ(1)=0
        NZ=0
        NE=0
      ENDIF
C
      IF(NZ.EQ.0)THEN
         WRITE(8,104)
      ELSE
         WRITE(8,102)NZ,NE,META,IWJ(META)
      ENDIF
C
   1  READ(7,101,END=5)N1,N2,N3
c      IF(N1.LE.0)GO TO 5
      IF(NTEMP.GT.0.AND.N1.NE.NTEMP)
     *                       STOP'MIS-MATCH ON INPUT NTEMPS'
      IF(NTEMPR.GT.0.AND.N2.GT.0.AND.N2.NE.NTEMPR)
     *                       STOP'MIS-MATCH ON INPUT NTEMPR'
      IF(NSKP.GT.0.AND.N3.GT.0.AND.N3.NE.NSKP)
     *                       STOP'MIS-MATCH ON INPUT NSKP'
C
      NTEMP=N1
      NTEMPR=N2
      NSKP=N3
C
      IF(NTEMP.GT.NUMT)STOP 'INCREASE NUMT TO NTEMP'
      IF(NTEMPR.GT.NUMT)STOP 'INCREASE NUMT TO NTEMPR'
      IF(NTEMP.EQ.0)NTEMP=19
      IF(NTEMPR.EQ.0)NTEMPR=NTEMP
      IF(NSKP.LT.1)NSKP=1
C
      TM=-1.
      NM=0
      N=0
      DO N0=1,NTEMPR
        READ(7,*)TT,(TA(J),J=1,META)
        IF(N0.LE.NTEMP)THEN
          IF((N0+1)/NSKP.GT.N)THEN
            N=N+1
            IF(TEMP(N).GT.0.0.AND.TT.NE.TEMP(N))
     *                      STOP'MIS-MATCH ON INPUT TEMPS'
            TEMP(N)=TT
            ADR(N)=ADR(N)+TA(META)
            IF(ADR(N).GT.TM)THEN
              NM=N
              TM=ADR(N)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      GO TO 1
C
   5  NTEMP=N
C
      DO N=1,NTEMP
        ADR(N)=ADR(N)*SQRT(TEMP(N))*TEMP(N)
        SIG(N)=1.
      ENDDO
C
      ANM=ADR(NM)
      BNM=TEMP(NM)
C
  10  WRITE(*,*)
      WRITE(*,*)'ENTER No. OF PAIRS OF FREE FITTING PARAMETERS',
     X' [.le. 0 to exit]:'
      READ(*,*)NFIT
C
      IF(NFIT.LE.0)STOP 'OUTPUT IN FILE cfout'
C
      NFIT=2*NFIT
      IF(NFIT.GT.NTEMP)STOP 'TOO MANY COEFFICIENTS'
C
  15  WRITE(*,*)'USE 1 - 4 FIXED PAIRS AT (LOW) T INDEX?',
     X' [e.g. 1 5 9 0, or 0 0 0 0 for none]:'
      READ(*,*)I0,I1,I2,I3
      IF(I0.GT.0)THEN
        IF(ADR(I0)*ADR(I0+1).LE.0.0)THEN
          WRITE(*,*)'***CANNOT FIX THIS TEMP POINT,',I0,
     X    ', TRY A HIGHER T INDEX...'
          WRITE(*,*)
          GO TO 15
        ENDIF
        B0=TEMP(I0)*TEMP(I0+1)*LOG(ADR(I0)/ADR(I0+1))
     X /(TEMP(I0)-TEMP(I0+1))
        A0=ADR(I0+1)*EXP(B0/TEMP(I0+1))
      ELSE
        A0=0.0
        B0=0.0
      ENDIF
C
      IF(I1.GT.0)THEN
        AD1=ADR(I1)
        AD2=ADR(I1+1)
        IF(I0.GT.0)THEN
          AD1=AD1-A0*EXP(-B0/TEMP(I1))
          AD2=AD2-A0*EXP(-B0/TEMP(I1+1))
        ENDIF
        IF(AD1*AD2.LE.0.0)THEN
          WRITE(*,*)'***CANNOT FIX THIS TEMP POINT,',I1,
     X    ', TRY A HIGHER T INDEX...'
          WRITE(*,*)
          GO TO 15
        ENDIF
        B1=TEMP(I1)*TEMP(I1+1)*LOG(AD1/AD2)
     X /(TEMP(I1)-TEMP(I1+1))
        A1=AD2*EXP(B1/TEMP(I1+1))
      ELSE
        A1=0.0
        B1=0.0
      ENDIF
C
      IF(I2.GT.0)THEN
        AD1=ADR(I2)
        AD2=ADR(I2+1)
        IF(I0.GT.0)THEN
          AD1=AD1-A0*EXP(-B0/TEMP(I2))
          AD2=AD2-A0*EXP(-B0/TEMP(I2+1))
        ENDIF
        IF(I1.GT.0)THEN
          AD1=AD1-A1*EXP(-B1/TEMP(I2))
          AD2=AD2-A1*EXP(-B1/TEMP(I2+1))
        ENDIF
        IF(AD1*AD2.LE.0.0)THEN
          WRITE(*,*)'***CANNOT FIX THIS TEMP POINT,',I2,
     X    ', TRY A HIGHER T INDEX...'
          WRITE(*,*)
          GO TO 15
        ENDIF
        B2=TEMP(I2)*TEMP(I2+1)*LOG(AD1/AD2)
     X /(TEMP(I2)-TEMP(I2+1))
        A2=AD2*EXP(B2/TEMP(I2+1))
      ELSE
        A2=0.0
        B2=0.0
      ENDIF
C
      IF(I3.GT.0)THEN
        AD1=ADR(I3)
        AD2=ADR(I3+1)
        IF(I0.GT.0)THEN
          AD1=AD1-A0*EXP(-B0/TEMP(I3))
          AD2=AD2-A0*EXP(-B0/TEMP(I3+1))
        ENDIF
        IF(I1.GT.0)THEN
          AD1=AD1-A1*EXP(-B1/TEMP(I3))
          AD2=AD2-A1*EXP(-B1/TEMP(I3+1))
        ENDIF
        IF(I2.GT.0)THEN
          AD1=AD1-A2*EXP(-B2/TEMP(I3))
          AD2=AD2-A2*EXP(-B2/TEMP(I3+1))
        ENDIF
        IF(AD1*AD2.LE.0.0)THEN
          WRITE(*,*)'***CANNOT FIX THIS TEMP POINT,',I3,
     X    ', TRY A HIGHER T INDEX...'
          WRITE(*,*)
          GO TO 15
        ENDIF
        B3=TEMP(I3)*TEMP(I3+1)*LOG(AD1/AD2)
     X /(TEMP(I3)-TEMP(I3+1))
        A3=AD2*EXP(B3/TEMP(I3+1))
      ELSE
        A3=0.0
        B3=0.0
      ENDIF
C
      DO N=1,NTEMP
        ADRF(N)=ADR(N)
      ENDDO
      IF(I0.GT.0)THEN   !REMOVE FIXED LOW-T CONTRIB
        DO N=1,NTEMP
          ADRF(N)=ADRF(N)-A0*EXP(-B0/TEMP(N))
        ENDDO
      ENDIF
      IF(I1.GT.0)THEN   !REMOVE FIXED LOW-T CONTRIB
        DO N=1,NTEMP
          ADRF(N)=ADRF(N)-A1*EXP(-B1/TEMP(N))
        ENDDO
      ENDIF
      IF(I2.GT.0)THEN   !REMOVE FIXED LOW-T CONTRIB
        DO N=1,NTEMP
          ADRF(N)=ADRF(N)-A2*EXP(-B2/TEMP(N))
        ENDDO
      ENDIF
      IF(I3.GT.0)THEN   !REMOVE FIXED LOW-T CONTRIB
        DO N=1,NTEMP
          ADRF(N)=ADRF(N)-A3*EXP(-B3/TEMP(N))
        ENDDO
      ENDIF
C
      DO I=1,NFIT,2
        IA(I)=1
        IA(I+1)=1
        A(I)=ANM
        A(I+1)=BNM
      ENDDO
C
      ALAMDA=-1.
      N=0
C
  20  CALL MRQMIN(TEMP,ADRF,SIG,NTEMP,A,IA,NFIT,COVAR,ALPHA,NUMT,
     *            CHISQ,XFUNCS,ALAMDA)
C
      N=N+1
      WRITE(99,*)'CHISQ=',CHISQ,'  ALAMDA=',ALAMDA
      IF(ALAMDA.GT.1.E4.OR.N.GT.1000)THEN
C       WRITE(*,*)'CHISQ=',CHISQ,'  ALAMDA=',ALAMDA
C       WRITE(*,*)'CONVERGED SUFFICIENTLY, Y OR N ?'
C       READ(*,*)ANS
C       IF(ANS.EQ.'N'.OR.ANS.EQ.'n')GO TO 20
        WRITE(*,*)'CHISQ=',CHISQ
        WRITE(*,*)' '
      ELSE
        GO TO 20
      ENDIF
C
C ORDER ON EXP COEFFICIENTS AND WRITE-OUT RESULTS
C
      DO I=1,NFIT,2
        ID=(I+1)/2
        DUM(ID)=A(I+1)
      ENDDO
C
      CALL ORDER(DUM,IA,NFIT/2,1)
C
      I00=0
      IF(I0.GT.0)THEN
        I00=I00+1
        WRITE(*,204)I00,I0
        WRITE(8,204)I00,I0
      ELSE
        WRITE(*,*)' '
        WRITE(8,*)' '
      ENDIF
      IF(I1.GT.0)THEN
        I00=I00+1
        WRITE(*,204)I00,I1
        WRITE(8,204)I00,I1
      ELSE
        WRITE(*,*)' '
        WRITE(8,*)' '
      ENDIF
      IF(I2.GT.0)THEN
        I00=I00+1
        WRITE(*,204)I00,I2
        WRITE(8,204)I00,I2
C      ELSE
C        WRITE(8,*)' '
      ENDIF
      IF(I3.GT.0)THEN
        I00=I00+1
        WRITE(*,204)I00,I3
        WRITE(8,204)I00,I3
C      ELSE
C        WRITE(8,*)' '
      ENDIF
C
      WRITE(*,202)
      WRITE(8,202)
C
      I00=0
      IF(I0.GT.0)THEN
        I00=I00+1
        WRITE(*,203)I00,A0,B0
        WRITE(8,203)I00,A0,B0
      ENDIF
      IF(I1.GT.0)THEN
        I00=I00+1
        WRITE(*,203)I00,A1,B1
        WRITE(8,203)I00,A1,B1
      ENDIF
      IF(I2.GT.0)THEN
        I00=I00+1
        WRITE(*,203)I00,A2,B2
        WRITE(8,203)I00,A2,B2
      ENDIF
      IF(I3.GT.0)THEN
        I00=I00+1
        WRITE(*,203)I00,A3,B3
        WRITE(8,203)I00,A3,B3
      ENDIF
      DO I=1,NFIT,2
        II=(I+1)/2
        ID=2*IA(II)
        WRITE(*,203)I00+II,A(ID-1),A(ID)
        WRITE(8,203)I00+II,A(ID-1),A(ID)
      ENDDO
C
      WRITE(*,201)
      WRITE(8,201)
      DO N=1,NTEMP
        T3=TEMP(N)*SQRT(TEMP(N))
        CALL FUNCS(TEMP(N),A,ADRF(N),DUM,NFIT)
        IF(I0.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A0*EXP(-B0/TEMP(N))
        ENDIF
        IF(I1.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A1*EXP(-B1/TEMP(N))
        ENDIF
        IF(I2.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A2*EXP(-B2/TEMP(N))
        ENDIF
        IF(I3.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A3*EXP(-B3/TEMP(N))
        ENDIF
        T=0.0
        IF(ADR(N).GT.0.0)T=ABS(ADR(N)-ADRF(N))/ADR(N)
        WRITE(*,200)TEMP(N),ADR(N)/T3,ADRF(N)/T3,T
        WRITE(8,200)TEMP(N),ADR(N)/T3,ADRF(N)/T3,T
      ENDDO
C
C CHECK ACCURACY OF ROUNDED COEFFICIENTS
C
      if(ntemp.lt.0)then
      IF(I0.GT.0)THEN
        WRITE(98,100)A0,B0
        BACKSPACE(98)
        READ(98,100)A0,B0
      ENDIF
      IF(I1.GT.0)THEN
        WRITE(98,100)A1,B1
        BACKSPACE(98)
        READ(98,100)A1,B1
      ENDIF
      IF(I2.GT.0)THEN
        WRITE(98,100)A2,B2
        BACKSPACE(98)
        READ(98,100)A2,B2
      ENDIF
      IF(I3.GT.0)THEN
        WRITE(98,100)A3,B3
        BACKSPACE(98)
        READ(98,100)A3,B3
      ENDIF
      DO I=1,NFIT
        WRITE(98,100)A(I)
        BACKSPACE(98)
        READ(98,100)A(I)
      ENDDO
      WRITE(*,201)
      WRITE(8,201)
      DO N=1,NTEMP
        CALL FUNCS(TEMP(N),A,ADRF(N),DUM,NFIT)
        IF(I0.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A0*EXP(-B0/TEMP(N))
        ENDIF
        IF(I1.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A1*EXP(-B1/TEMP(N))
        ENDIF
        IF(I2.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A2*EXP(-B2/TEMP(N))
        ENDIF
        IF(I3.GT.0)THEN   !ADD FIXED LOW-T CONTRIB
          ADRF(N)=ADRF(N)+A3*EXP(-B3/TEMP(N))
        ENDIF
        T=0.0
        IF(ADR(N).GT.0.0)T=ABS(ADR(N)-ADRF(N))/ADR(N)
        T3=TEMP(N)*SQRT(TEMP(N))
        WRITE(*,200)TEMP(N),ADR(N)/T3,ADRF(N)/T3,T
        WRITE(8,200)TEMP(N),ADR(N)/T3,ADRF(N)/T3,T
      ENDDO
      endif
C
C
      GO TO 10
C
 100  FORMAT(1P,2E10.3)
 101  FORMAT(3I5)
 102  FORMAT('Z=',I2,1X,'N=',I2,1X,'M=',I2,1X,'W=',I2)
 104  FORMAT('Z=XX',1X,'N=YY',1X,'M=MM',1X,'W=WW')
 200  FORMAT(1PE10.2,2E11.2,E15.2)
 201  FORMAT(//3X,'TEMP(K)',3X,'ADR(ORG)',3X,'ADR(FIT)',9X,'DIFF')
 202  FORMAT('  N','  FITTING COEFFICIENTS')
 203  FORMAT(I3,1PE11.3,E12.3)
 204  FORMAT(I3,'  FIXED COEFFICIENT AT T INDEX',I3)
      END
C******************************************************************
      SUBROUTINE FUNCS(X,A,Y,DYDA,NA)
      INTEGER NA
      REAL*8 X,Y,A(NA),DYDA(NA)
      INTEGER I
      REAL*8 ARG,EX
C
      Y=0.
      DO I=1,NA,2
        ARG=A(I+1)/X
        if(-arg.lt.70.)then
        EX=EXP(-ARG)
        else
        ex=0.0
        endif
        Y=Y+A(I)*EX
        DYDA(I)=EX
        DYDA(I+1)=-A(I)*EX/X
      ENDDO
C
      RETURN
      END
      SUBROUTINE covsrt(covar,npc,ma,ia,mfit)    !v2.08
      INTEGER ma,mfit,npc,ia(ma)
      REAL*8 covar(npc,npc)
      INTEGER i,j,k
      REAL*8 swap
      do i=mfit+1,ma
      do j=1,i
      covar(i,j)=0.
      covar(j,i)=0.
      enddo
      enddo
      k=mfit
      do j=ma,1,-1
      if(ia(j).ne.0)then
      do i=1,ma
      swap=covar(i,k)
      covar(i,k)=covar(i,j)
      covar(i,j)=swap
      enddo
      do i=1,ma
      swap=covar(k,i)
      covar(k,i)=covar(j,i)
      covar(j,i)=swap
      enddo
      k=k-1
      endif
      enddo
      return
      END
      SUBROUTINE gaussj(a,n,np,b,m,mp)   !v2.08
      INTEGER m,mp,n,np,NMAX
      REAL*8 a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),
     * ipiv(NMAX)
      REAL*8 big,dum,pivinv
      do j=1,n
      ipiv(j)=0
      enddo
      do i=1,n
      big=0.
      do j=1,n
      if(ipiv(j).ne.1)then
      do k=1,n
      if (ipiv(k).eq.0) then
      if (abs(a(j,k)).ge.big)then
      big=abs(a(j,k))
      irow=j
      icol=k
      endif
      else if (ipiv(k).gt.1) then
      stop 'singular matrix in gaussj'
      endif
      enddo
      endif
      enddo
      ipiv(icol)=ipiv(icol)+1
      if (irow.ne.icol) then
      do l=1,n
      dum=a(irow,l)
      a(irow,l)=a(icol,l)
       a(icol,l)=dum
      enddo
      do l=1,m
      dum=b(irow,l)
      b(irow,l)=b(icol,l)
      b(icol,l)=dum
      enddo
      endif
      indxr(i)=irow
      indxc(i)=icol
      if (a(icol,icol).eq.0.) stop 'singular matrix in gaussj'
      pivinv=1./a(icol,icol)
      a(icol,icol)=1.
      do l=1,n
      a(icol,l)=a(icol,l)*pivinv
      enddo
      do l=1,m
      b(icol,l)=b(icol,l)*pivinv
      enddo
      do ll=1,n
      if(ll.ne.icol)then
      dum=a(ll,icol)
      a(ll,icol)=0.
      do l=1,n
      a(ll,l)=a(ll,l)-a(icol,l)*dum
      enddo
      do l=1,m
      b(ll,l)=b(ll,l)-b(icol,l)*dum
      enddo
      endif
      enddo
      enddo
      do l=n,1,-1
      if(indxr(l).ne.indxc(l))then
      do k=1,n
      dum=a(k,indxr(l))
      a(k,indxr(l))=a(k,indxc(l))
      a(k,indxc(l))=dum
      enddo
      endif
      enddo
      return
      END
      SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,  !v2.08
     * chisq,xfuncs)
      INTEGER ma,nalp,ndata,ia(ma),MMAX
      REAL*8 chisq,a(ma),alpha(nalp,nalp),beta(ma),sig(ndata),x(ndata),
     * y(ndata),xfuncs
c      EXTERNAL funcs
      PARAMETER (MMAX=20)
      INTEGER mfit,i,j,k,l,m
      REAL*8 dy,sig2i,wt,ymod,dyda(MMAX)
      mfit=0
      do j=1,ma
      if (ia(j).ne.0) mfit=mfit+1
      enddo
      do j=1,mfit
      do k=1,j
      alpha(j,k)=0.
      enddo
      beta(j)=0.
      enddo
      chisq=0.
      do i=1,ndata
      call funcs(x(i),a,ymod,dyda,ma)
      sig2i=1./(sig(i)*sig(i))
      dy=y(i)-ymod
      j=0
      do l=1,ma
      if(ia(l).ne.0) then
      j=j+1
      wt=dyda(l)*sig2i
      k=0
      do m=1,l
      if(ia(m).ne.0) then
      k=k+1
      alpha(j,k)=alpha(j,k)+wt*dyda(m)
      endif
      enddo
      beta(j)=beta(j)+dy*wt
      endif
      enddo
      chisq=chisq+dy*dy*sig2i
      enddo
      do j=2,mfit
      do k=1,j-1
      alpha(k,j)=alpha(j,k)
      enddo
      enddo
      return
      END
      SUBROUTINE mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,    !v2.08
     * chisq,xfuncs,alamda)
      INTEGER ma,nca,ndata,ia(ma),MMAX
      REAL*8 alamda,chisq,xfuncs,a(ma),alpha(nca,nca),covar(nca,nca),
     * sig(ndata),x(ndata),y(ndata)
      PARAMETER (MMAX=20)
      INTEGER j,k,l,mfit
      REAL*8 ochisq,atry(MMAX),beta(MMAX),da(MMAX)
      SAVE ochisq,atry,beta,da,mfit
      if(alamda.lt.0.)then
      mfit=0
      do j=1,ma
      if (ia(j).ne.0) mfit=mfit+1
      enddo
      alamda=0.001
      call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq,xfuncs)
      ochisq=chisq
      do j=1,ma
      atry(j)=a(j)
      enddo
      endif
      do j=1,mfit
      do k=1,mfit
      covar(j,k)=alpha(j,k)
      enddo
      covar(j,j)=alpha(j,j)*(1.+alamda)
      da(j)=beta(j)
      enddo
      call gaussj(covar,mfit,nca,da,1,1)
      if(alamda.eq.0.)then
      call covsrt(covar,nca,ma,ia,mfit)
      call covsrt(alpha,nca,ma,ia,mfit)
      return
      endif
      j=0
      do l=1,ma
      if(ia(l).ne.0) then
      j=j+1
      atry(l)=a(l)+da(j)
      endif
      enddo
      call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq,xfuncs)
      if(chisq.lt.ochisq)then
      alamda=0.1*alamda
      ochisq=chisq
      do j=1,mfit
      do k=1,mfit
      alpha(j,k)=covar(j,k)
      enddo
      beta(j)=da(j)
      enddo
      do l=1,ma
      a(l)=atry(l)
      enddo
      else
      alamda=10.*alamda
      chisq=ochisq
      endif
      return
      END
      SUBROUTINE ORDER(EN,NORDER,NDIM,IUP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C
C
C-----------------------------------------------------------------------
C
C      RETURNS NORDER(I)=POINTER TO I-TH ENERGY IN EN ARRAY,
C      IUP=1 FOR ASCENDING ENERGIES, IUP=-1 FOR DESCENDING ENERGIES
C
C-----------------------------------------------------------------------
      DIMENSION EN(NDIM),NORDER(NDIM)
C-----------------------------------------------------------------------
      DO 40 K = 1,NDIM
        J = K
        J1 = J - 1
        IF (J1.EQ.0) GOTO 30
        E = EN(J) + IUP*1.0D-7
        DO 20 I = 1,J1
          IF (J.LT.K) GOTO 10
          N = NORDER(I)
          IF (IUP.GT.0 .AND. E.GT.EN(N)) GOTO 20
          IF (IUP.LT.0 .AND. E.LT.EN(N)) GOTO 20
   10     CONTINUE
          NORDER(J) = NORDER(J-1)
          J = J - 1
   20   CONTINUE
   30   NORDER(J) = K
   40 CONTINUE
C
      END
