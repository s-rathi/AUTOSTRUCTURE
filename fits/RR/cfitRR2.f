C                                                            13/12/06
      PROGRAM CFITRR2
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     PERFORM A NON-LINEAR LEAST-SQUARES FIT TO A RATE COEFFICIENT.
C     EVALUATE RR FIT FORM OF VERNER & FERLAND, APJS V103 PP467-73 (1996),
C     WITH MODIFIED B-FACTOR.
C
C
      PARAMETER (NUMT=150)
      PARAMETER (NMETA=10)
C
      CHARACTER ANS*1,TST*2,CARD*80
      LOGICAL EX
C
      DIMENSION TEMP(NUMT),ARR(NUMT),SIG(NUMT),A(NUMT),IA(NUMT)
      DIMENSION COVAR(NUMT,NUMT),ALPHA(NUMT,NUMT),DUM(NUMT)
      DIMENSION ARRF(NUMT),TA(NMETA),IWJ(NMETA)
C
      EXTERNAL FUNCS
C
      OPEN(7,FILE='cfin')
      OPEN(8,FILE='cfout')
C
      INQUIRE(FILE='adasout',EXIST=EX)
C
      IF(EX)THEN                           !EXTRACT RATES
        OPEN(9,FILE='adasout')
        DO I=1,10000
          READ(9,150,END=999)TST
  150     FORMAT(A2)
          IF(TST.EQ.'Z=')THEN
            BACKSPACE(9)
            READ(9,729)NZ,NE,(IWJ(M),M=1,NMETA)
  729       FORMAT(2X,I2,3X,I2,3X,20(I3))
            DO M=1,NMETA
              IF(IWJ(M).EQ.0)THEN
                META=M-1
                GO TO 151
              ENDIF
            ENDDO
            META=NMETA
  151       WRITE(7,*)NZ,NE,(IWJ(M),M=1,META)
            WRITE(7,*)' '
            DO J=1,4
              READ(9,*)
            ENDDO
            NN=0
            DO N=1,NUMT
              READ(9,150)TST
              IF(TST.NE.'C-')THEN
                BACKSPACE(9)
                READ(9,731)CARD
  731           FORMAT(A80)
                WRITE(7,731)CARD
                NN=NN+1
              ELSE
                REWIND(7)
                GO TO 2
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  999   WRITE(*,*)'*** UNABLE TO FIND Z, N IN adasout'
        STOP'ERROR: *** UNABLE TO FIND Z, N IN adasout'
      ELSE                                !ASSUME SUITABLE cfin EXISTS
        NN=19
        META=0
      ENDIF
C
   2  DO N=1,NUMT
        TEMP(N)=0.0
        ARR(N)=0.0
      ENDDO
      NTEMP=-1
      NTEMPR=-1
      metain=0
C
      WRITE(*,*)'Enter initial state index [1 for ground etc.]:'
      READ(*,*)METAIN
C
      IF(META.EQ.0)META=METAIN
      IF(METAIN.GT.META)THEN
        WRITE(*,*)'*** METASTABLE INDEX NOT ON FILE:',METAIN
        STOP'ERROR: *** METASTABLE INDEX NOT ON FILE'
      ELSE
        META=METAIN
      ENDIF
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
         WRITE(8,204)
      ELSE
         WRITE(8,206)NZ,NE,META,IWJ(META)
      ENDIF
C
   1  READ(7,101,END=5)N1,N2,N3,N4
      IF(N1.LT.0)GO TO 5
      IF(NTEMP.GT.0.AND.N1.NE.NTEMP)STOP'MIS-MATCH ON INPUT NTEMPS'
      IF(NTEMPR.GT.0.AND.N2.GT.0.AND.N2.NE.NTEMPR)
     *                       STOP'ERROR: MIS-MATCH ON INPUT NTEMPR'
C
      NTEMP=N1
      NTEMPR=N2
      IWT=N3
      NT2=N4
C
      IF(NTEMP.GT.NUMT)STOP 'INCREASE NUMT TO NTEMP'
      IF(NTEMPR.GT.NUMT)STOP 'INCREASE NUMT TO NTEMPR'
      IF(NTEMP.EQ.0)NTEMP=NN
      IF(NTEMPR.EQ.0)NTEMPR=NTEMP
      IF(NT2.EQ.0)NT2=(2*NTEMPR)/3
C
      DO N=1,NTEMPR
        READ(7,*)TT,(TA(J),J=1,META)
        IF(TEMP(N).GT.0.0.AND.TT.NE.TEMP(N))
     *                    STOP'ERROR: MIS-MATCH ON INPUT TEMPS'
        TEMP(N)=TT
        ARR(N)=ARR(N)+TA(META)
      ENDDO
      GO TO 1
C
   5  IWT=IWT+1
      DO N=1,NTEMP
        ARR(N)=LOG(ARR(N))
        SIG(N)=ARR(N)**IWT
      ENDDO
C
c      NFIT=1
C
c  10  WRITE(*,*)
c      WRITE(*,*)'ENTER TEMP INDEX OF T2',
c     X' [.le. 0 to exit]:'
C      READ(*,*)NT2         !***MINIMUM UNCOMMENT FOR INTERACTIVE USE
c
C
      IF(NT2.LE.0.OR.NT2.GT.NTEMPR)STOP 'OUTPUT IN FILE cfout'
C
      NFIT=6
      IF(NFIT.GT.NTEMP)STOP 'TOO MANY COEFFICIENTS'
C
      DO I=1,NFIT,6
        IA(I)=1
        IA(I+1)=1
        IA(I+2)=1
        IA(I+3)=1
        IA(I+4)=1
        IA(I+5)=1
        A(I)=1.d-10
        A(I+1)=.6
        A(I+2)=TEMP(1)
        A(I+3)=TEMP(NTEMPR)
        A(I+4)=.6
        A(I+5)=TEMP(NT2)
      ENDDO
C
      ALAMDA=-1.
      N=0
      CHISQO=0.
C
  20  CALL MRQMIN(TEMP,ARR,SIG,NTEMP,A,IA,NFIT,COVAR,ALPHA,NUMT,
     *            CHISQ,XFUNCS,ALAMDA)
C
      N=N+1
c      WRITE(99,*)N,'CHISQ=',CHISQ,'  ALAMDA=',ALAMDA
c      DO I=1,NFIT,6
c        WRITE(99,203)I,A(I),A(I+1),A(I+2),A(I+3),A(I+4),A(I+5)
c      ENDDO
c      CALL FLUSH(99)
      IF(ALAMDA.GT.1.E4.OR.N.GT.1000)THEN
c        WRITE(*,*)N,'FIT FAILURE: CHISQO, CHISQ =',CHISQO,CHISQ
      ELSE
        CHISQO=CHISQ
        GO TO 20
      ENDIF
C
      WRITE(8,202)
      DO I=1,NFIT,6
        II=(I+5)/6
        WRITE(8,203)II,A(I),A(I+1),A(I+2),A(I+3),A(I+4),A(I+5)
      ENDDO
      WRITE(8,201)
      DO N=1,NTEMPR
        CALL FUNCS(TEMP(N),A,ARRF(N),DUM,NFIT,IFLAG)
        T=0.0
        IF(EXP(ARR(N)).GT.0.)T=ABS(EXP(ARR(N))-EXP(ARRF(N)))/EXP(ARR(N))
        WRITE(8,200)TEMP(N),EXP(ARR(N)),EXP(ARRF(N)),T
        IF(T.GT..06)WRITE(*,205)N,T
      ENDDO
C
c      IF(NZ.EQ.0)STOP 'OUTPUT IN FILE cfout'
c      GO TO 10
C
      STOP 'OUTPUT IN FILE cfout'
C
 100  FORMAT(E10.2,1X,10E10.2)
 101  FORMAT(4I5)
 200  FORMAT(1PE10.2,2E11.2,E15.2)
 201  FORMAT(//3X,'TEMP(K)',3X,'ARR(ORG)',3X,'ARR(FIT)',9X,'DIFF')
 202  FORMAT('  N','  FITTING COEFFICIENTS: A, B, T0, T1, C, T2')
 203  FORMAT(I3,1PE11.3,0PF8.4,1P,2E11.3,0PF8.4,1P,E11.3)
 204  FORMAT('Z=XX',1X,'N=YY',1X,'M=MM',1X,'W=WW')
 205  FORMAT('WARNING: FIT FRACTIONAL ERROR:',I4,F7.3)
 206  FORMAT('Z=',I2,1X,'N=',I2,1X,'M=',I2,1X,'W=',I2)
      END
C******************************************************************
      SUBROUTINE FUNCS(X,A,Y,DYDA,NA,IFLAG)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER NA
      REAL*8 X,Y,A(NA),DYDA(NA)
      INTEGER I
      REAL*8 T0,T1
C
      IFLAG=0
      DO I=1,NA
        IF(A(I).LT.0)THEN
          IFLAG=-I
          write(99,*)'coeff index',i,'  .lt. 0'
          RETURN
        ENDIF
      ENDDO
      Y=0.
      DO I=1,NA,6
        T0=SQRT(X/A(I+2))
        T1=SQRT(X/A(I+3))
        B=A(I+1)+A(I+4)*EXP(-A(I+5)/X)       
        Y=Y+LOG(A(I))-LOG(T0)-(1.D0-B)*LOG(1.D0+T0)
     X                       -(1.D0+B)*LOG(1.D0+T1)
        DYDA(I)=1.D0/A(I)
        DYDA(I+1)=LOG(1.D0+T0)-LOG(1.D0+T1)
        DYDA(I+2)=.5D0/A(I+2)+.5D0*(1.D0-A(I+1))*T0/((1.D0+T0)*A(I+2))
        DYDA(I+3)=.5D0*(1.D0+A(I+1))*T1/((1.D0+T1)*A(I+3))
        DYDA(I+4)=EXP(-A(I+5)/X)*DYDA(I+1)
        DYDA(I+5)=-A(I+4)*DYDA(I+4)/X
      ENDDO
C
      RETURN
      END
C******************************************************************
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
      SUBROUTINE gaussj(a,n,np,b,m,mp,IFLAG)   !v2.08
      INTEGER m,mp,n,np,NMAX
      REAL*8 a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),
     * ipiv(NMAX),IFLAG
      REAL*8 big,dum,pivinv,eps
      PARAMETER (eps=1.d-300)    !smallest safe number machine representable
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
c      WRITE(99,*) 'singular matrix in gaussj'
      IFLAG=-1
      RETURN
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
      if (abs(a(icol,icol)).lt.eps) THEN
c      WRITE(99,*) 'singular matrix in gaussj'
      IFLAG=-1
      RETURN
      ENDIF
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
      INTEGER mfit,i,j,k,l,m,IFLAG
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
      OCHISQ=CHISQ
      chisq=0.
      do i=1,ndata
      call funcs(x(i),a,ymod,dyda,ma,IFLAG)
      IF(IFLAG.NE.0)THEN
        CHISQ=2*OCHISQ
        RETURN
      ENDIF
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
      INTEGER j,k,l,mfit,IFLAG
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
      IFLAG=1
      call gaussj(covar,mfit,nca,da,1,1,IFLAG)
      IF(IFLAG.LE.0)THEN
        CHISQ=2*OCHISQ
        RETURN
      ENDIF
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
