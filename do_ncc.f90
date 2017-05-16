! npts is the length of sig1 and sig2
! shft +1 is the length of sig_out
subroutine do_ncc(npts,sig1,sig2,sig_out,shft,dt)
integer,parameter :: nptsmax=4000000,nsmax=10000
integer shft,nk
real sig1(nptsmax),sig2(nptsmax),amax,dt
real sig_out(nptsmax)
complex,dimension(:),allocatable :: ss1,ss2
integer nft,i,npts,npow,nft2
nft=2;npow=1
amax=0
do while(nft.lt.npts)
   nft=nft*2
   npow=npow+1
enddo
nk=nft/2+1
allocate(ss1(nft),ss2(nft))
ss1=cmplx(0,0)
ss2=cmplx(0,0)
ss1(1:npts)=cmplx(sig1(1:npts),0)
ss2(1:npts)=cmplx(sig2(1:npts),0)
! do fft
call clogc(npow,ss1,1,dt)
call clogc(npow,ss2,1,dt)
! do cross-correlation
call cor(ss1,ss2,nft,nk,npow,dt)
nft2=nft/2-shft
do i=1,2*shft+1
   sig_out(2*shft+2-i)=-real(ss1(nft2+i))
enddo
deallocate(ss1,ss2)
return
end subroutine
     
subroutine cor(src,data,nft,nk,npow,dt)
complex src(nft),data(nft)
integer nft,npow,i,nk
real aa,dt
aa=-1
!aa=1
do i=1,nft
   src(i)=aa*src(i)*conjg(data(i))
   aa=-aa
enddo
src(nk+1:nft)=cmplx(0,0)
call clogc(npow,src,-1,dt)
return
end subroutine cor
