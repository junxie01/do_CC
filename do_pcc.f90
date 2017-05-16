subroutine do_pcc(nsamp,sig1,sig2,sig,npt2,dt,id)
!parameter (max=21048576)
parameter (max=4000000)
complex trace(max),pilot(max) 
complex csig1(max),csig2(max) 
integer ns,n,npilot,ntrace,id
complex cdum
integer nlag1,nlag2
integer ll,nll/1/
real*8 twopi,pi
real sig1(max),sig2(max),sig(max)
real wu
sig=0
twopi=6.28318530717958
pi=twopi/2.
wu=1.0
call analytic(sig1,csig1,nsamp)
if(id==0)then ! id==0 do cross-correlation else do autocorrelation
      call analytic(sig2,csig2,nsamp)
else
      csig2=csig1
endif
pilot(1:nsamp)=csig1(1:nsamp)/cabs(csig1(1:nsamp))
if (id==0)then
      trace(1:nsamp)=csig2(1:nsamp)/cabs(csig2(1:nsamp))
else
      trace=pilot
endif
npilot=nsamp
ntrace=nsamp
nlag1=1-npt2
nlag2=1+npt2
nnn=nlag2-nlag1+1
call pcc(sig,pilot,trace,nlag1,nlag2,nll,wu,npilot,ntrace,ll,1)
end subroutine
