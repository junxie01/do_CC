!-----------------------------------------------------------------
      SUBROUTINE analytic(s1,c1,nsamp)
!
! Compute analytic signal c1(nsamp) from real time series s1(nsamp).
! Frequency domain computation!
!
      parameter (nmax=4000000)
      real s1(nmax)
      complex c1(nmax)
!
      dt=1.
!
! Determine power for FFT:
      npow=1
      nsmp=2
 5    if (nsamp.le.nsmp) goto 10
      npow=npow+1
      nsmp=nsmp*2
      goto 5
 10   nsmp2=nsmp/2
!
      do 20 i=1,nsamp
 20   c1(i)=cmplx(s1(i),0.)
        do 30 i=nsamp+1,nsmp
 30   c1(i)=cmplx(0.,0.)
!
! FFT:
      call clogc(npow,c1,1.,dt)
!
! calculating the analytical signal:
      do i=2,nsmp2
          c1(i)=2.*c1(i)
          c1(nsmp+2-i)=cmplx(0.,0.)
      enddo
!       c1(1)=cmplx(0.,0.)
!     c1(nsmp+1)=cmplx(0.,0.)
!
! IFFT:
      call clogc(npow,c1,-1.,dt)
!
      return

      end
