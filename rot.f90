subroutine rot9com(sachead,sig)
use sacio
type(sac_head) :: sachead
integer,parameter :: nmax=4000000
real,dimension(nmax,9) :: sig,sigout
real,dimension(4,4) :: trans
real :: az,baz,sbaz,saz,caz,cbaz
az=sachead%az*atan2(1,1)*4/180
baz=sachead%baz*atan2(1,1)*4/180
npts=sachead%npts
saz=sin(az)
caz=cos(az)
sbaz=sin(baz)
cbaz=cos(baz)
trans(1,1)=-  caz*cbaz
trans(1,2)=   caz*sbaz
trans(1,3)=-  saz*sbaz
trans(1,4)=   saz*cbaz

trans(2,1)=-  saz*sbaz
trans(2,2)=-  saz*cbaz
trans(2,3)=-  caz*cbaz
trans(2,4)=-  caz*sbaz

trans(3,1)=-  caz*sbaz
trans(3,2)=   caz*cbaz
trans(3,3)=-  saz*cbaz
trans(3,4)=   saz*sbaz

trans(4,1)=   saz*cbaz
trans(4,2)=-  saz*sbaz
trans(4,3)=-  caz*sbaz
trans(4,4)=   caz*cbaz
sigout(1:npts,9)=sig(1:npts,9)*trans(1,1)+sig(1:npts,8)*trans(1,2)
                +sig(1:npts,5)*trans(1,3)+sig(1:npts,6)*trans(1,4)

sigout(1:npts,5)=sig(1:npts,9)*trans(2,1)+sig(1:npts,8)*trans(2,2)
                +sig(1:npts,5)*trans(2,3)+sig(1:npts,6)*trans(2,4)

sigout(1:npts,8)=sig(1:npts,9)*trans(3,1)+sig(1:npts,8)*trans(3,2)
                +sig(1:npts,5)*trans(2,3)+sig(1:npts,6)*trans(2,4)

sigout(1:npts,6)=sig(1:npts,9)*trans(4,1)+sig(1:npts,8)*trans(4,2)
                +sig(1:npts,5)*trans(4,3)+sig(1:npts,6)*trans(4,4)

sigout(1:npts,4)= sig(1:npts,4)* caz + sig(1:npts,7)* saz
sigout(1:npts,3)= sig(1:npts,2)*sbaz - sig(1:npts,3)*cbaz
sigout(1:npts,7)=-sig(1:npts,4)* saz + sig(1:npts,7)* caz
sigout(1:npts,2)=-sig(1:npts,2)*cbaz - sig(1:npts,3)*sbaz
end subroutine
