! npts is the length of sig1 and sig2
! shft +1 is the length of sig_out
subroutine do_ncc(npts,npow,shft,dt,sig1,sig2,sig_out)
integer,parameter :: nptsmax=4000000,nsmax=10000
integer shft
complex sig1(nptsmax),sig2(nptsmax)
real dt
real sig_out(nptsmax)
complex,dimension(:),allocatable :: ss1,ss2
integer nft,i,npts,npow,nft2
allocate(ss1(npts),ss2(npts))
ss1=cmplx(0,0)
ss2=cmplx(0,0)
ss1(1:npts)=sig1(1:npts)
ss2(1:npts)=sig2(1:npts)
call cor(ss1,ss2,npts)     !do cross-correlation
call clogc(npow,ss1,-1,dt) !do ifft
nft2=npts/2-shft
do i=1,2*shft+1
   sig_out(2*shft+2-i)=-real(ss1(nft2+i))
enddo
deallocate(ss1,ss2)
return
end subroutine
     
subroutine cor(src,data,nft)
complex src(nft),data(nft)
integer nft,npow,i,nk
real aa,dt
aa=-1
do i=1,nft
   src(i)=aa*src(i)*conjg(data(i))
   aa=-aa
enddo
return
end subroutine cor
