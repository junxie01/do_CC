      SUBROUTINE pcc(s1,r1,r2,nlag1,nlag2,nll,wu,n1,n2,ll,n11)
!
!     TIME DOMAIN PCC (BSSA,1999). Function returns real value as 
!     function of relative sample lag (see below).
!
!     r1 & r2 contain the phasors (envelope normalized analytic signals) 
!     for the pilot and trace.
!     n1 & n2 are the corresponding number of samples.
!     r1 is shifted with respect to r2, i.e., a positive lag means
!     that r1 is shifted to larger time. The time of r1(1) equals
!     the time at r2(n11) and nlag=n11+nl, where nl is the sample
!     lag (positive or negative integer value). nl=0 means zero lag.
!      
!     Partial overlap is permitted and depends on nlag.
!     Complete overlap for 0 <= nlag < (n2-n1+1) : 
!     pcc[ r1(i) , r2(i+nlag) ] for i=1,..,i21
!     wu is the power of PCC. (wu=1 corresponds to PCC from
!     Schimmel, BSSA, 1999.)
!
!     last change: 14/12/2011 schimmel@ictja.csic.es
!
!     real wu,r1(1),r2(1),cosd,rdum,difarg
!     integer n1,n2,nnorm,nlag,i,nn

      parameter (nmax=4000000)
      complex r1(nmax),r2(nmax),c1,c2
      real s1(nmax)

      ll=0
      do nlag=nlag1,nlag2,nll
            ll=ll+1
            if (nlag.lt.1-n1)  stop 'No overlap at all!'
            if (nlag.ge.n2) stop 'No overlap at all!'
! 1st sample on 1st trace:
            i11=2-nlag
            if (i11.lt.1) i11=1
! 1st sample on 2nd trace:
            i12=nlag
            if (i12.lt.1) i12=1
! last sample on 1st trace 
            nn2=n2-i12
            nn1=n1-i11
            i21=min(nn1,nn2)+i11
            nn=i12
            d1=0.
            d2=0.
            do i=i11,i21
                    c1=r2(nn)
                    c2=r1(i)
                    d1=d1+cabs(c1+c2)
                    d2=d2+cabs(c1-c2)
                    nn=nn+1
            enddo
            rdum=d1-d2
            nnorm=i21-i11+1
            rdorm=rdum*.5/float(nnorm)
            if (wu.eq.1) then
                   rdum=rdorm
            elseif (wu.eq.2) then
                   rdum=rdorm*abs(rdorm)
            else
                   rdum=rdorm*abs(rdorm)**(wu-1)
            endif
            s1(ll)=rdum
! set outside loop.
            !if (nlag.eq.n11) write(*,*)rdum,' Zero LAG PCC'
      enddo
      return
      end
